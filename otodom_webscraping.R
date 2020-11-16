library("rvest")
library("dplyr")
library("stringr")
library("jsonlite")

link <- "https://www.otodom.pl/sprzedaz/mieszkanie/warszawa/?search%5Bregion_id%5D=7&search%5Bsubregion_id%5D=197&search%5Bcity_id%5D=26&page="
pages <- 1:500

auctions_urls <- c()

for (i in 1:length(pages)) {
  
temp_url <- paste0(link,pages[i]) %>%
  read_html() %>%
  html_nodes(xpath = '//h3/a') %>%
  html_attr('href')

auctions_urls <- c(temp_url,auctions_urls)

}

get_flat_details <- function(link) {
  
  # Extract offer title
  title <- link %>%
    read_html() %>%
    html_node(xpath = '//h1[@class="css-1ld8fwi"]') %>%
    html_text()
  
  # Extract offer price
  price <- link %>%
    read_html() %>%
    html_node(xpath = '//div[@class="css-1vr19r7"]') %>%
    html_text() %>%
    stringr::str_extract('([0-9]+ [0-9]+)|[0-9]+') %>%
    stringr::str_replace(' ','') %>%
    as.numeric()
  
  # Extract other elements e.g. area, rooms number and market
  details <- link %>%
    read_html() %>%
    html_nodes(xpath = '//section[@class="section-overview"]/div[@class="css-dwmx8v-Fe"]/ul/li') %>%
    html_text()
  area <- as.numeric(str_replace(str_extract(details[1],'([0-9]+,[0-9]+)|[0-9]+'),',','.'))
  rooms <- as.numeric(str_extract(details[2],'[0-9]+'))
  market <- str_split(details[3], ' ',simplify = T)[2]
  
  # Extract geo location data from JSON
  geo_data <- link %>%
    read_html() %>%
    html_node(xpath = '//script[@type="application/ld+json"]') %>%
    html_text() %>%
    fromJSON()
  
  city <- data.frame(geo_data$itemListElement)[3,3][,2]
  district <- data.frame(geo_data$itemListElement)[4,3][,2]

  temp_data <- data.frame(link, title, price, area,rooms,market, city,district)
  
  return(temp_data)
  
}


auction_data <- sapply(auctions_urls,get_flat_details,simplify = T)

auction_data_df <- do.call(rbind, auction_data)


auction_data_df %>%
  mutate(price_per_m = price / area ) %>%
  group_by(district) %>%
  summarise(median(price_per_m))




