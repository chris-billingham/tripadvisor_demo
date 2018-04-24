library(tidyverse)
library(magrittr)

# read in the data
big_scrape <- readRDS("data/all_hotels.rds")

# split the reviews from the hotel details
hotel_reviews <- big_scrape[,1:6]
hotel_details <- big_scrape[, -c(2:6)] %>% 
  unique()

# helper function
convert_pct <- function(chr) {
  fixed <- chr %>%
    gsub("%", "", .) %>%
    as.numeric()/100
  
  return(fixed)
}

# convert hotel pcts into numerics
hotel_details %<>% mutate_at(vars(7:11), convert_pct)
hotel_details$reviews_total <- as.numeric(gsub("[^0-9]", "", hotel_details$reviews_total))
hotel_details$review_average <- as.numeric(gsub(" of 5 bubbles", "", hotel_details$review_average))

# postcode shenanigans, find the end bit, work out the start, deal with the short ones by adding spaces
postcode_loc <- str_locate(hotel_details$address, " [0-9][A-Z][A-Z]")
postcode <- trimws(substr(hotel_details$address, unname(postcode_loc[,1])-4, unname(postcode_loc[,2])))
if(str_count(postcode, " ")>1) {substr(postcode, 3, nchar(postcode))}

# ranking
of_loc <- str_locate(hotel_details$ranking, " of ")
in_loc <- str_locate(hotel_details$ranking, " Hotels in ")
position_ranking <- as.numeric(substr(hotel_details$ranking, 2, of_loc[,1]-1))
total_place <- as.numeric(gsub(",", "", substr(hotel_details$ranking, of_loc[,2], in_loc[,1]-1)))
place <- substr(hotel_details$ranking, in_loc[,2]+1, nchar(hotel_details$ranking))
