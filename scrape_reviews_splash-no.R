library(tidyverse)
library(rvest)
library(splashr)
library(magrittr)
library(pbapply)
# library(RSelenium)

# set up Selenium server
remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4445L, browserName = "chrome")
remDr$open()

hotel_scrape <- function(url) {

remDr$navigate(url)
base_html <- read_html(remDr$getPageSource()[[1]])
  
# base_html <- url %>%
#  read_html()

name <- base_html %>% 
  html_nodes("h1#HEADING.ui_header.h1") %>% 
  html_text()

address <- base_html %>% html_nodes("span.detail") %>% html_text()
ranking <- base_html %>% html_nodes("span.header_popularity.popIndexValidation") %>% html_text()
reviews_bubble <- base_html %>% html_nodes("span.ui_bubble_rating") %>% html_attr("alt")
reviews_total <- base_html %>% html_nodes("span.reviewCount") %>% html_text()
reviews_range <- base_html %>% html_nodes("span.row_count.row_cell") %>% html_text()

hotel_details <- tibble(name, url, address, ranking, 
                        review_average = reviews_bubble[1], 
                        reviews_total,
                        excellent = reviews_range[1],
                        very_good = reviews_range[2],
                        average = reviews_range[3],
                        poor = reviews_range[4],
                        terrible = reviews_range[5]
                        )

# locate the last page number of all the reviews
page_last <- url %>% 
  read_html() %>% 
  html_nodes("a.pageNum.last.taLnk") %>%
  html_attr("data-page-number") %>% 
  .[1] %>%
  as.integer()

page_reviews <- function(url, page_no) {
  # split the url in two for the page insertions
  # find the split point
  pos <- str_locate(url,"Reviews-")[2]
  
  # split into front and back
  front_url <- substr(url, 1, pos)
  back_url <- substr(url, pos+1, nchar(url))
  
  # create the insert for next pages
  insert <- paste0("or", as.integer((page_no-1)*5),"-")
  
  # reconstruct the url
  page_url <- ifelse(page_no == 1,paste0(front_url, back_url), paste0(front_url, insert, back_url))
  
  # grab the reviews
  page <- read_reviews(page_url)
  
  return(page)
}


read_reviews <- function(url) {
  
  reviews <- splash_local %>%
    splash_response_body(TRUE) %>%
    splash_enable_javascript(TRUE) %>%
    splash_plugins(TRUE) %>%
    splash_user_agent(ua_macos_chrome) %>%
    splash_go(url) %>%
    splash_wait(runif(1, 1.0, 2.0)) %>%
    splash_focus("span.taLnk.ulBlueLinks") %>%
    element_click() %>%
    splash_wait(runif(1,1.0,2.0)) %>%
    splash_html() %>%
    html_nodes("#REVIEWS")
  
  rev_len <- length(reviews)
  
  while(rev_len == 0) {  reviews <- splash_local %>%
    splash_response_body(TRUE) %>%
    splash_enable_javascript(TRUE) %>%
    splash_plugins(TRUE) %>%
    splash_user_agent(ua_macos_chrome) %>%
    splash_go(url) %>%
    splash_wait(runif(1, 1.0, 2.0)) %>%
    splash_html() %>%
    html_nodes("#REVIEWS");
  rev_len <- length(reviews);}
  
  id <- reviews %>%
    html_nodes(".quote a") %>%
    html_attr("id")
  
  quote <- reviews %>%
    html_nodes(".quote span") %>%
    html_text()
  
  rating <- reviews %>% 
    html_nodes("span") %>%
    html_attr("class")
  
  rating <- rating[grepl("bubble_",rating)]
  rating <- gsub("[A-Za-z_ ]","",rating)
  rating <- as.numeric(rating)
  rating <- rating/10
  
  date <- reviews %>%
    html_nodes("span.ratingDate") %>%
    html_attr("title") %>%
    strptime("%d %b %Y")%>%
    as.POSIXct()
  
  review <- reviews %>% 
    html_nodes("p.partial_entry") %>% html_text()
  
  # lookout for mgr replies
  has_mgr <- reviews %>% 
    html_nodes("div.mgrRspnInline p.partial_entry") %>% 
    html_text()
  
  # if mgr replies find where they are then delete them
  if(length(has_mgr)>0) {
    remove <- substr(has_mgr,1,10) %>% 
      map(~grep(.x, review)) %>% 
      flatten() %>%
      unique() %>% 
      as.integer()
    
    review <- review[-remove]
  }
  
  join_url <- gsub("-or[^A-Za-z-]+","",url)
  
  df <- tibble(url = rep(join_url, 5), id, quote, rating, date, review)
  
  return(df)
}

top_100 <- pblapply(seq(1,20), page_reviews, url = url) %>% 
  bind_rows()

details <- top_100 %>% left_join(hotel_details, by = c("url" = "url"))

return(details)
}

possible_scrape <- possibly(hotel_scrape, otherwise = NA_real_)

big_scrape <- map_dfr(pages_all$links[1:30], possible_scrape)
