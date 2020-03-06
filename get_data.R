library(dplyr)
library(tidyr)
library(ggplot2)
library(googlesheets4)
library(lubridate)

link <- "https://docs.google.com/spreadsheets/d/1yZv9w9zRKwrGTaR-YzmAqMefw4wMlaXocejdxZaTs6w/htmlview?usp=sharing&sle=true"
metadata <- sheets_get(link)

# number and order of columns differ before "Jan26_11am"
metadata$sheets$name[metadata$sheets$name=="Jan26_11am"]
good_format_after <- which(metadata$sheets$name=="Jan27_9am")

df <- sheets_read(link, sheet = 1)
for (i in 2:good_format_after) {
  dftemp <- sheets_read(link, sheet = i)
  dftemp <- dftemp[,1:6]
  df <- rbind(df, dftemp)
}
###########################
df <- sheets_read(link, sheet = 5)
bind_rows(df, sheets_read(link, sheet = 2))
for (i in 6:length(metadata$sheets$name)) {
  df <- bind_rows(df, sheets_read(link, sheet = i))
}

# new time series data
ts_link <- "https://docs.google.com/spreadsheets/d/1UF2pSkFTURko2OvfHWWlFpDFAr1UxCBA4JLwlSP6KFo/htmlview?usp=sharing&sle=true"
ts <- sheets_read(ts_link, sheet = 2)

ts %>%
  gather("time", "confirmed", -c(1:5)) %>% 
  rename(province = `Province/State`,
         country = `Country/Region`,
         #firstcase = `First confirmed date in country (Est.)`,
         ) %>% 
  mutate(time = mdy_hm(time)) -> tsl

# take only most recent snapshot by day
timestamps <- unique(tsl$time)
tsl$day <- as.Date(tsl$time)

tsl %>% 
  group_by(day) %>% 
  dplyr::filter(time == max(time)) -> tslu
 
unique(tslu$time) 

# aggregate quantities per day over provinces
tslu %>% 
  group_by(day) %>% 
  summarize(confirmed = sum(confirmed))

##########################

glimpse(df)
columns <- c("province", "country", "date", "confirmed", "deaths", "recovered")

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
  mutate(recovered = ifelse(is.na(recovered), 0, recovered)) %>% 
  mutate(date = replace(date, is.na(date), as.Date("2020-01-21"))) %>%
  select(columns) -> dfc 

dfc$net_infected <- dfc$confirmed - dfc$deaths - dfc$recovered  

dfc %>% 
  mutate(date = as.Date(date)) %>% 
  group_by(date) %>% # reduces granularity to days w/o time
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

keywords <- c("coronavirus", "china", "superbowl", "sanders", "trump")
country <- c("US")
keywords <- c("coronavirus", "china", "webasto", "thüringen", "trump")
country <- c("DE")
time <- ("2020-01-01 2020-02-12")
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
          to = "2020-02-28",
          get = "stock.prices")
