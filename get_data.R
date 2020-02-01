library(dplyr)
library(tidyr)
library(ggplot2)
library(googlesheets4)

link <- "https://docs.google.com/spreadsheets/d/1yZv9w9zRKwrGTaR-YzmAqMefw4wMlaXocejdxZaTs6w/htmlview?usp=sharing&sle=true"
metadata <- sheets_get(link)

# number and order of columns differ before "Jan26_11am"
metadata$sheets$name[metadata$sheets$name=="Jan26_11am"]
good_format_after <- which(metadata$sheets$name=="Jan27_9am")

df <- sheets_read(link, sheet = 1)
for (i in 2:good_format_after) {
  df <- rbind(gdf, sheets_read(link, sheet = i))
}

glimpse(df)

df %>% 
  rename(province = `Province/State`,
         country = `Country/Region`,
         date = `Last Update`,
         confirmed = Confirmed,
         deaths = Deaths,
         recovered = Recovered
  ) %>% 
  mutate(confirmed = ifelse(is.na(confirmed), 0, confirmed)) %>% 
  mutate(deaths = ifelse(is.na(deaths), 0, deaths)) %>% 
  mutate(recovered = ifelse(is.na(recovered), 0, recovered)) -> dfc

dfc$net_infected <- dfc$confirmed - dfc$deaths - dfc$recovered  

dfc %>% 
  group_by(date) %>% 
  summarize(confirmed = sum(confirmed),
            deaths = sum(deaths),
            recovered = sum(recovered),
            net_infected = sum(net_infected)
  ) %>% 
  filter(confirmed > 10) -> dfa

##########################################################
# Downloading Google Trends
##########################################################

library(gtrendsR)

keywords <- c("coronavirus", "china", "sars", "flu")
country <- c("US")
time <- ("2020-01-01 2020-01-31")
channel <- "news"

trends <- gtrends(keywords, gprop = channel, geo = country, time = time)

#select only interst over time 
trends$interest_over_time %>% 
  mutate(hits = replace(hits, hits == "<1", 0)) %>% 
  mutate(hits = as.integer(hits)) %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(time = as.Date(time)) -> time_trend

head(time_trend)


##########################################################
# Downloading Stock Market Data
##########################################################

library(tidyquant)

sp500_names <- tq_index("S&P500")

sp500 <- tq_get(sp500_names$symbol,
          from = "2020-01-01",
          to = "2020-01-31",
          get = "stock.prices")
