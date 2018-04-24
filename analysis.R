library(tidyverse)
library(magrittr)

# read in the data
big_scrape <- readRDS("data/all_hotels.rds")

# split the reviews from the hotel details
hotel_details <- big_scrape[,1:14] %>% 
  unique()

hotel_reviews <- big_scrape[, -c(2:14)]


# helper function
convert_pct <- function(chr) {
  fixed <- chr %>%
    gsub("%", "", .) %>%
    as.numeric()/100
  
  return(fixed)
}

# convert hotel pcts into numerics
hotel_details %<>% mutate_at(vars(10:14), convert_pct)
hotel_details$reviews_total <- as.numeric(gsub("[^0-9]", "", hotel_details$reviews_total))
hotel_details$review_average <- as.numeric(gsub(" of 5 bubbles", "", hotel_details$review_average))
hotel_details$true_review_average <- (hotel_details$excellent * hotel_details$reviews_total * 5 +
  hotel_details$very_good * hotel_details$reviews_total * 4 +
  hotel_details$average * hotel_details$reviews_total * 3 +
  hotel_details$poor * hotel_details$reviews_total * 2 +
  hotel_details$terrible * hotel_details$reviews_total)/hotel_details$reviews_total

# locations
map <- as.tibble(str_split(hotel_details$geo, ",", simplify = TRUE))
colnames(map) <- c("lat", "lng")
map$lat %<>% as.numeric()
map$lng %<>% as.numeric()

# put them back in
hotel_details %<>% bind_cols(map)

# some light visuals
ggplot(hotel_details, aes(lng, lat)) + 
  geom_point(colour = excellent) + 
  xlim(c(-8,2)) + 
  ylim(c(50,58))

# ranking
of_loc <- str_locate(hotel_details$ranking, " of ")
in_loc <- str_locate(hotel_details$ranking, " Hotels in ")
position_ranking <- as.numeric(substr(hotel_details$ranking, 2, of_loc[,1]-1))
total_place <- as.numeric(gsub(",", "", substr(hotel_details$ranking, of_loc[,2], in_loc[,1]-1)))
place <- substr(hotel_details$ranking, in_loc[,2]+1, nchar(hotel_details$ranking))

ranking <- tibble(position_ranking, total_place, pct_place = position_ranking/total_place, place)

hotel_details %<>% bind_cols(ranking)

# inspection
hotel_details %>% 
  filter(total_place > 10) %>% 
  ggplot(aes(pct_place, true_review_average)) + 
    geom_point()

# tally for places
place_ranking <- hotel_details %>% 
  group_by(place) %>% 
  summarise(volume = n(), total_hotels = as.integer(mean(total_place)), av_rank = mean(pct_place))

