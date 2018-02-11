
library(tidyverse)
library(rvest)
library(magrittr)
library(pbapply)
library(parallel)

url <- "https://www.tripadvisor.co.uk/Hotel_Review-g190756-d192010-Reviews-The_Palace_Hotel-Buxton_Derbyshire_England.html"

# locate the last page number of all the reviews
page_last <- url %>% 
  read_html() %>% 
  html_nodes("span") %>%
  html_attr("data-page-number") %>% 
  as.integer()

page_last <- max(page_last[!is.na(page_last)])

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

reviews <- url %>%
  read_html() %>%
  html_nodes("#REVIEWS .innerBubble")

rev_len <- length(reviews)
  
while(rev_len == 0) {reviews <- url %>%
  read_html() %>%
  html_nodes("#REVIEWS .innerBubble");
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
  html_nodes(".partial_entry") %>%
  html_text()

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

df <- tibble(rep(url, 5), id, quote, rating, date, review)

return(df)
}

top_100 <- pblapply(seq(1:20), page_reviews, url = url, cl = 4) %>% 
  bind_rows()


