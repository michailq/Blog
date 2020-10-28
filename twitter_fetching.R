

### Load the package

library(twitteR)
library(stringr)
library(dplyr)
library(lubridate)
library(ggplot2)

### Specify API credentials for Twitter App

consumer_key <- "0gAXkQavJhY42JPQbuD3o5zUr"
consumer_secret <- "EPmFC4GwFGL0ElVYcjfssNbQAARSXRjGlpnvJ9jOE2myr222JV"
access_token <- "1160628134-lxB34oE67oeec4jZ6TDvAEXl1ss0Yb3i1a3jLYv"
access_secret <- "FNMHt3yI7RrocLAMzhADM87L9EPnwIJKm5t2YFOGI61DM"

### Open connection and authenticate
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

### Retrieve all tweets from Ministry of Health twitter accounts
tw <- twitteR::searchTwitter('from:MZ_GOV_PL', 
                            #since = '2020-06-01',
                            n=5000, 
                            retryOnRateLimit = 100)
d <- twitteR::twListToDF(tw)

### Data formatting and extraction
d1 <- d %>% 
  filter(str_detect(d$text,'^Mamy.')) %>%
  mutate(
    tweetdate= ymd(as.Date(created)),
    number_of_cases = as.numeric(
      str_replace(str_extract(text, '([0-9]+ [0-9]+)|[0-9]+'),' ','')))

### Plot data on the simple graph

g <- ggplot(d1, aes(x=tweetdate,y=number_of_cases)) +
  geom_col(fill = 'blue') +
  ggtitle("Number of Covid positive cases") +
  xlab("Date") + ylab("Number of positive cases")
g
