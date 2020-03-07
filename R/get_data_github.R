library(tidyverse)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

##########################################################
# Download Johns Hopkins CSSE Data from GitHub
##########################################################

links <- c("https://raw.github.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv",
           "https://raw.github.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv",
           "https://raw.github.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"
           )

confirmed <- read_csv(links[1])
deaths <- read_csv(links[2])
recovered <- read_csv(links[3])
write_csv(confirmed, paste("data\\individual\\", Sys.Date(), "_confirmed.csv", sep =""))
write_csv(deaths, paste("data\\individual\\", Sys.Date(), "_deaths.csv", sep =""))
write_csv(recovered, paste("data\\individual\\", Sys.Date(), "_recovered.csv", sep =""))

dl <- list(confirmed, deaths, recovered)

names(dl) <- c("confirmed", "deaths", "recovered")

dl$confirmed %>% 
  gather("time", "confirmed", -c(1:4)) %>% 
    mutate(time = mdy(time)) -> df

dl$deaths %>% 
  gather("time", "deaths", -c(1:4)) %>% 
  mutate(time = mdy(time)) %>% 
  select(deaths) %>% 
  cbind(df) -> df

dl$recovered %>% 
  gather("time", "recovered", -c(1:4)) %>% 
  mutate(time = mdy(time)) %>% 
  select(recovered) %>% 
  cbind(df) -> df

# data cleaning and preparation
# full data set
df %>% 
  rename(province = `Province/State`,
         country = `Country/Region`,
         latitude = Lat,
         longitude = Long) %>%  
  mutate(confirmed = ifelse(is.na(confirmed), 0, confirmed)) %>% 
  mutate(deaths = ifelse(is.na(deaths), 0, deaths)) %>% 
  mutate(recovered = ifelse(is.na(recovered), 0, recovered)) -> dfc

dfc$netinfected <- dfc$confirmed - dfc$deaths - dfc$recovered  

dfc %>%
  select(c("province", "country", "latitude", "longitude", "time", "confirmed", "recovered", "deaths", "netinfected")) -> dfc

write_csv(dfc, paste("data\\total\\", Sys.Date(), "_total.csv", sep = ""))


# generate aggregate data set over all regions per single date
dfc %>% 
  group_by(time) %>% # reduces granularity to days w/o time
  summarize(confirmed = sum(confirmed),
            deaths = sum(deaths),
            recovered = sum(recovered),
            netinfected = sum(netinfected)
  ) -> df_single_day

# aggregate by country
dfc %>% 
  group_by(time, country) %>%
  summarize(confirmed = sum(confirmed),
            deaths = sum(deaths),
            recovered = sum(recovered),
            netinfected = sum(netinfected)
  ) -> df_single_day_country

# split China vs. rest of world data sets
dfc %>% 
  filter(country == "Mainland China") -> dfc_china

dfc %>% 
  filter(country != "Mainland China") -> dfc_excl_china

# generate aggregate data set over for two separate dfs
dfc_china %>% 
  group_by(time) %>% # reduces granularity to days w/o time
  summarize(confirmed = sum(confirmed),
            deaths = sum(deaths),
            recovered = sum(recovered),
            netinfected = sum(netinfected)
  ) -> df_single_day_china

dfc_excl_china %>% 
  group_by(time) %>% # reduces granularity to days w/o time
  summarize(confirmed = sum(confirmed),
            deaths = sum(deaths),
            recovered = sum(recovered),
            netinfected = sum(netinfected)
  ) -> df_single_day_excl_china

##########################################################
# Download Google Trends
##########################################################

library(gtrendsR)

keywords <- c("coronavirus", "china", "superbowl", "sanders", "trump")
country <- c("US")
keywords <- c("coronavirus", "china", "webasto", "thüringen", "trump")
country <- c("DE")
time <- ("2020-01-01 2020-03-06")
channel <- "web"

trends <- gtrends(keywords, gprop = channel, geo = country, time = time)

#select only interst over time 
trends$interest_over_time %>% 
  mutate(hits = replace(hits, hits == "<1", 0)) %>% 
  mutate(hits = as.integer(hits)) %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(time = as.Date(time)) -> time_trend


##########################################################
# Download Stock Market Data
##########################################################

library(tidyquant)

sp500_names <- tq_index("S&P500")

sp500 <- tq_get(sp500_names$symbol,
                from = "2020-01-01",
                to = "2020-03-06",
                get = "stock.prices")
