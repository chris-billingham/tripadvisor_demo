# this creates a tibble to scrape
# if you have specific urls that you want to go to

pages_all <- c("https://www.tripadvisor.co.uk/Attraction_Review-g186419-d207144-Reviews-LEGOLAND_Windsor_Resort-Windsor_Windsor_and_Maidenhead_Berkshire_England.html",
               "https://www.tripadvisor.co.uk/Attraction_Review-g642238-d216468-Reviews-THORPE_PARK_Resort-Chertsey_Surrey_England.html",
               "https://www.tripadvisor.co.uk/Attraction_Review-g580409-d216483-Reviews-Chessington_World_of_Adventures_Resort-Chessington_Surrey_England.html",
               "https://www.tripadvisor.co.uk/Attraction_Review-g504160-d503030-Reviews-Drayton_Manor_Park-Tamworth_Staffordshire_England.html",
               "https://www.tripadvisor.co.uk/Attraction_Review-g503839-d215576-Reviews-Paultons_Park-Romsey_Hampshire_England.html"
)

pages_all <- as.tibble(pages_all)
colnames(pages_all) <- "links"
