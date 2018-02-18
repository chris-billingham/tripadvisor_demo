library(tidyverse)
library(rvest)
library(splashr)
library(magrittr)
library(pbapply)
library(parallel)

# create your failed string
# this is the first hotel on page 1, if this shows outside page 1 try again
failed <- hotel_links(search_url, 1)[1,1] %>% 
  as.character()

# search string
search_url <- 'https://www.tripadvisor.co.uk/Search?q=premier+inn&ssrc=h#&ssrc=h&o=0'

# work out how many pages of hotels we've got
page_last <- search_url %>% 
  read_html() %>% 
  html_nodes("a.pageNum") %>%
  html_text() %>%
  as.integer() %>%
  max()

# take the search url and page number and get the links on that page
hotel_links <- function(url, page_no) {
  url_page <- paste0(substr(url, 1, nchar(url)-1), as.character((page_no - 1) * 30))
  
  raw_links <- splash_local %>%
    splash_response_body(TRUE) %>%
    splash_enable_javascript(TRUE) %>%
    splash_plugins(TRUE) %>%
    splash_user_agent(ua_macos_chrome) %>%
    splash_go(url_page) %>%
    splash_wait(runif(1, 5.0, 10.0)) %>%
    splash_html() %>%
    html_nodes("div.all-results div.result_wrap") %>%
    html_attr("onclick")
  
  links <- raw_links %>% map_chr(parse_url)
  
  # check the failed string, if failed redo
  if(page_no > 1) {
  while(links[1] == failed | ) {  raw_links <- splash_local %>%
    splash_response_body(TRUE) %>%
    splash_enable_javascript(TRUE) %>%
    splash_plugins(TRUE) %>%
    splash_user_agent(ua_macos_chrome) %>%
    splash_go(url_page) %>%
    splash_wait(runif(1, 5.0, 10.0)) %>%
    splash_html() %>%
    html_nodes("div.all-results div.result_wrap") %>%
    html_attr("onclick");
    links <- raw_links %>% map_chr(parse_url);}
  }
  
  # tibble
  df <- tibble(links)
  
  return(df)
}

# take in a raw links string and convert it into a proper url
parse_url <- function(string) {
  
  start_pos <- str_locate(string, "/Hotel")[1]
  end_pos <- str_locate(string, "html?")[2]
  
  new_url <- paste0('https://www.tripadvisor.co.uk', substr(string, start_pos, end_pos))
  return(new_url)
}

pages_1_10 <- pblapply(seq(1:10), hotel_links, url = search_url) %>% 
  bind_rows()

pages_11_20 <- pblapply(seq(11:20), hotel_links, url = search_url) %>% 
  bind_rows()

pages_21_30 <- pblapply(seq(21:30), hotel_links, url = search_url) %>% 
  bind_rows()

pages_11_20 <- pblapply(seq(31:34), hotel_links, url = search_url) %>% 
  bind_rows()
