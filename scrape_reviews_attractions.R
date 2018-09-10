library(tidyverse)
library(rvest)
library(magrittr)
library(pbapply)
library(RSelenium)

docker <- Sys.which("docker")
system2(command = docker,  args = c("run -d -p 4445:4444 selenium/standalone-chrome"))

# load in the list of pages from scrape_links.r
pages_all <- readRDS("data/pages_all.rds")
# notes

# this function takes a tripadvisor url for a venue and what review
# page number you want. then it creates a bespoke url for the individual page
# it then sends that url to read_reviews to grab the reviews from it
page_reviews <- function(url, page_no) {
  # split the url in two for the page insertions
  # find the split point
  pos <- str_locate(url,"Reviews-")[2]
  
  # split into front and back
  front_url <- substr(url, 1, pos)
  back_url <- substr(url, pos+1, nchar(url))
  
  # create the insert for next pages (attractions is 10 per page)
  insert <- paste0("or", as.integer((page_no-1)*10),"-")
  
  # reconstruct the url
  page_url <- ifelse(page_no == 1,paste0(front_url, back_url), paste0(front_url, insert, back_url))
  
  # grab the reviews in a dataframe
  page <- read_reviews(page_url)
  
  # return the dataframe
  return(page)
}

# this function will open up a reviews page, click more if we need to click more
# then pull in all the reviews and data about the reviews
read_reviews <- function(url) {
  # test for whether RSelenium session is open
  if(is.null(unlist(remDr$getSessions()[1]))) {stop("No RSelenium Session open")}
  
  # go to the webpage and wait for it to load
  remDr$navigate(url)
  Sys.sleep(runif(1,1,2))
  
  # we're now going to check whether it's possible to click ...More
  # first we're going to scrape the unadulterated page
  check_more <- read_html(remDr$getPageSource()[[1]]) %>% 
    html_nodes("#REVIEWS") %>%
    html_nodes("p.partial_entry") %>% 
    html_text()
  
  # now we grepl for ...More and sum how many we find, if it's zero then there's none
  more_num <- sum(grepl("...More", check_more))
  
  # providing we can click ...More, click it.
  if(more_num > 0) {
    # find the web element on the page which is the ...More link
    webElem <- remDr$findElement(using = 'css selector', "span.taLnk.ulBlueLinks")
    
    # click it and wait
    webElem$clickElement()
    Sys.sleep(runif(1,1,2))
  }
  
  # now we can read the entire page as ...More has been exposed
  reviews <- read_html(remDr$getPageSource()[[1]]) %>% 
    html_nodes("#REVIEWS")
  
  # first read the reviews themselves, if we can expand we have expanded
  review <- reviews %>% 
    html_nodes("p.partial_entry") %>% 
    html_text()
  
  # what is the id of the person leaving the review
  id <- reviews %>%
    html_nodes(".quote a") %>%
    html_attr("id")
  
  # what is the quote at the top of the review
  quote <- reviews %>%
    html_nodes(".quote span") %>%
    html_text()
  
  # what is the rating they've give then review
  rating <- reviews %>% 
    html_nodes("span") %>%
    html_attr("class")
  
  # annoyingly tripadvisor has this bubble_ think which turns 5 into bubble_50
  # and 3 into bubble_30 and so. let's rip that out. this could be fully piped
  rating <- rating[grepl("bubble_",rating)]
  rating <- gsub("[A-Za-z_ ]","",rating)
  rating <- as.numeric(rating)
  rating <- rating/10
  # rating <- rating[seq(1,19,2)]
  
  # get the date of the review and get it into a good format
  # all the dates are actual datatimes of midnight, we don't need that
  date <- reviews %>%
    html_nodes("span.ratingDate") %>%
    html_attr("title") %>%
    strptime("%d %b %Y")%>%
    as.POSIXct()
  
  # date <- date[seq(1,19,2)]
  # managers can leave replies to people's review, annoyingly this is captured
  # above when we pull p.partial_entry. so we're going to bodge that out.
  # we can speficially identify the manager's review. so pull those on their own
  has_mgr <- reviews %>% 
    html_nodes("div.mgrRspnInline p.partial_entry") %>% 
    html_text()
  
  # if mgr replies (string match on first 10 characters), we find where they 
  # are then delete them
  # there's probably a better way to do this but this works
  if(length(has_mgr)>0) {
    remove <- substr(has_mgr,1,10) %>% 
      map(~grep(.x, review, fixed = TRUE)) %>% 
      flatten() %>%
      unique() %>% 
      as.integer()
    
    review <- review[-remove]
  }
  
  # recreate the main url to do the join later
  join_url <- gsub("-or[^A-Za-z-]+","",url)
  
  # to keep the loop going occasionally things will keel over because (usually)
  # the review is coming back with more or less than 5 reviews. the code is setup
  # to only want 5 (could fix?). so these two if clauses deal with that in a
  # known way.
  
  reps <- length(id)
  
  if(length(review) < reps) {
    fill <- rep("<blank>", reps-length(review))
    review <- c(review,fill)
  }
  if(length(review) > reps) {
    review <- review[1:reps]
  }
  # make the tibble with all the data
  df <- tibble(url = rep(join_url, reps), id, quote, date, rating, review)
 
  # send it on back
  return(df)
}

# this is the main scrape function for a whole hotel.
# it will read all the "top of the page" information, then cycle through the first
# 20 pages of reviews and pull those. we use pbapply because i love progress bars
# if reviews is set to FALSE it doesn't read the reviews
attaction_scrape <- function(url, reviews = TRUE) {
  # test for whether RSelenium session is open
  if(is.null(unlist(remDr$getSessions()[1]))) {stop("No RSelenium Session open")}

  # go to url and wait for between 1 and 2 seconds
  remDr$navigate(url)
  Sys.sleep(runif(1,1,2))
    
  # read the page in via rvest::read_html  
  base_html <- read_html(remDr$getPageSource()[[1]])

  # get the name of the hotel
  name <- base_html %>% 
    html_nodes("h1#HEADING.ui_header.h1") %>% 
    html_text()

  # pull a bunch of attributes from the front page
  address <- base_html %>% 
    html_nodes("span.detail") %>% 
    html_text() %>%
    paste(., collapse = ";")
  
  geo <- base_html %>% 
    html_nodes("img.mapImg") %>%
    html_attr("src") %>%
    .[1] %>%
    substr(., str_locate(., "\\|")+1, nchar(.)) %>%
    substr(., 1, str_locate(., "\\&")-1)
  
  amenities <- base_html %>% 
    html_nodes("div.highlightedAmenity.detailListItem") %>% 
    html_text() %>%
    paste(., collapse = ";")
  
  star_rating <- base_html %>% 
    html_nodes("div.starRating.detailListItem") %>% 
    html_text()
  
  if (length(star_rating) == 0) {star_rating <- "<BLANK>"}

  # this is the "local area" ranking. handy
  ranking <- base_html %>% 
    html_nodes("span.header_popularity.popIndexValidation") %>% 
    html_text() 
  
  if (length(ranking) == 0) {ranking <- "<BLANK>"}
  
  # get the average review rating
  reviews_bubble <- base_html %>% 
    html_nodes("span.ui_bubble_rating") %>% 
    html_attr("alt")
  
  # get the total number of reviews
  reviews_total <- base_html %>% 
    html_nodes("span.reviewCount") %>% 
    html_text() %>%
    .[1]
  
  # get the review range percentages, will require sorting in the next bit 
  reviews_range <- base_html %>% 
    html_nodes("span.row_count.row_cell") %>% 
    html_text()

  # find the last page of reviews
  max_review <- base_html %>%
    html_nodes("a.pageNum.last.taLnk") %>%
    html_text()
  
  # lightly munge into rectangular data, we split out the review rations as well
  # this creates the overall information at the hotel level
  attaction_details <- tibble(name, url, address, geo, amenities, star_rating, ranking, 
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
  
  # here we check whether we want to get the reviews or not. if we do run through page_reviews
  # if not then don't and shuffle hotel_details into details
  if(reviews == TRUE) {
    # now to get the top 100 reviews, we've set this up with the previous functions
    # this takes any single page, clicks more, and reads in the reviews and data
    # we do this 20 times to get the 100. current time per run ~90 seconds
    top_100 <- pblapply(seq(1,1000), page_reviews, url = url) %>% 
      bind_rows()
  
    # now we append on all the hotel details from before. this is a touch inelegant
    # as we have a tonne of repeated data however for the set up we have here
    # i would rather return a full data frame and then deal with the repeats later
    details <- top_100 %>% 
      left_join(attaction_details, by = c("url" = "url"))
  } else {
    details <- attaction_details
  }
  # send back the details
  return(details)
}

# unfortuantely webscraping has a tendancy to error for various reasons
# when you're using map/apply an error can kill your entire pipeline
# with purrr you could use possible/safely however because we like progress
# bars i've used pblapply which i don't know how to error out of
# so scrape_and_save does a few things. it reads in the latest data, runs
# hotel_scrape on a single entry, combines it with the old data, then saves
# that back to your data directory.
# this means that whenever you get an error the most you've lost is one iteration
# we print out what number we're up to so we can restart from where it broke
scrape_and_save_attaction_reviews <- function(number) {
  # where are we up to
  print(number)
  
  # check if we have the big_scrape file. if we do append if we don't create
  if(file.exists("data/big_scrape_attractions.rds")) {
    
    # load in the old file
    load <- readRDS("data/big_scrape_attractions.rds")
    
    # do a single scrape
    scrape <- map_dfr(pages_all$links[number], attaction_scrape)
    
    # push back together
    combine <- bind_rows(load, scrape)
    
    # save off
    saveRDS(combine,"data/big_scrape_attractions.rds")
  } else {
    # run the very first link
    big_scrape <- map_dfr(pages_all$links[1], attaction_scrape)
    
    # save off
    saveRDS(big_scrape, "data/big_scrape2.rds")
  }
}

# right this is the loop
# first we set up the RSelenium session, getting to this point was UNFUN
remDr <- remoteDriver(remoteServerAddr = "localhost",
                      port = 4445L,
                      browserName = "chrome")
remDr$open(silent = TRUE)

# start the scrape loop
seq(1,5) %>% 
  map(scrape_and_save_attaction_reviews)

# close the session
remDr$close()  

# fin

