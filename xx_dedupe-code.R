library(tidyverse)

# de dupe code

big_scrape_attractions <- readRDS("~/Documents/R/tripadvisor_demo/data/big_scrape_attractions.rds")
big_scrape_attractions_edit <- big_scrape_attractions %>% 
  mutate(review_edit = as.integer(gsub("[^0-9]+", "", reviews_total))) %>% 
  arrange(id, review_edit) %>% 
  group_by(id) %>% 
  mutate(id_id = row_number()) %>%
  filter(id_id == 1)

saveRDS(big_scrape_attractions_edit, "2019-01-14_big-scrape-attractions.rds")

