library(tidyverse)
library(rvest)
library(magrittr)
library(pbapply)
library(RSelenium)

# start selenium in the terminal
# install: docker pull selenium/standalone-chrome
# run: docker run -d -p 4445:4444 selenium/standalone-chrome
# stop: docker stop $(docker ps -q)

pages_all <- readRDS("data/pages_all.rds")

hotel_scrape <- function(url) {
# set up Selenium server
remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4445L, browserName = "chrome")
remDr$open(silent = TRUE)

# go to url and wait for between 1 and 2 seconds
remDr$navigate(url)
Sys.sleep(runif(1,1,2))

# read the page in via rvest::read_html  
base_html <- read_html(remDr$getPageSource()[[1]])

# close down the selenium thing. ALWAYS DO THIS
remDr$close()  

# get the name of the hotel
name <- base_html %>% 
  html_nodes("h1#HEADING.ui_header.h1") %>% 
  html_text()

# pull a bunch of attributes from the front page
# address needs some work for more than one line
address <- base_html %>% html_nodes("span.detail") %>% html_text()
ranking <- base_html %>% html_nodes("span.header_popularity.popIndexValidation") %>% html_text()
reviews_bubble <- base_html %>% html_nodes("span.ui_bubble_rating") %>% html_attr("alt")
reviews_total <- base_html %>% html_nodes("span.reviewCount") %>% html_text()
reviews_range <- base_html %>% html_nodes("span.row_count.row_cell") %>% html_text()

# lightly munge into rectangular data
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

# this is the workhorse function to read the url, split it and get top 100
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

# this takes any single page, clicks more, and reads in the reviews and data
read_reviews <- function(url) {
  
  # open the remote driver
  remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4445L, browserName = "chrome")
  remDr$open(silent = TRUE)

  # go to the webpage and wait for it to load
  remDr$navigate(url)
  Sys.sleep(runif(1,1,2))
  
  # create R objects from the website elements
  webElem <- remDr$findElement(using = 'css selector', "span.taLnk.ulBlueLinks")
  webElem$clickElement()
  Sys.sleep(runif(1,1,2))
  new_webElem <- remDr$findElement(using = 'css selector', "p.partial_entry")

reviews <- read_html(remDr$getPageSource()[[1]]) %>% 
  html_nodes("#REVIEWS")

# first read the reviews themselves
  review <- reviews %>% 
    html_nodes("p.partial_entry") %>% html_text()

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
  
  # create the url to do the join later
  join_url <- gsub("-or[^A-Za-z-]+","",url)
  
  # make the tibble with all the data
  df <- tibble(url = rep(join_url, 5), id, quote, rating, date, review)
  
  # close selenium. ALWAYS DO THIS
  remDr$close()  
  
  return(df)
}

top_100 <- pblapply(seq(1,20), page_reviews, url = url) %>% 
  bind_rows()

details <- top_100 %>% left_join(hotel_details, by = c("url" = "url"))

return(details)
}

# possible_scrape <- possibly(hotel_scrape, otherwise = NA_real_)

# big_scrape <- map_dfr(pages_all$links[1:30], possible_scrape)
