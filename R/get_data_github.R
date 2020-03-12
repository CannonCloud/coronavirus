library(tidyverse)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(countrycode)

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

## create modified data sets for didactical reasons for use by students
# create ID
dfc_id <- tibble::rowid_to_column(dfc, "id")

dfc %>% 
  tibble::rowid_to_column("id") %>% 
  # create NA's for all zeros (to be reversed by students)
  na_if(0) -> dfc_id

# create df with only confirmed cases (base df)
dfc_id %>% 
  select(c("id", "province", "country", "latitude", "longitude", "time", "confirmed")) %>% 
  write_csv("data\\modified_covid\\confirmed.csv")

# create individual csv's for recovered and deaths
dfc_id %>% 
  select(c("id", "deaths")) %>% 
  write_csv("data\\modified_covid\\deaths.csv")

dfc_id %>% 
  select(c("id", "recovered")) %>% 
  write_csv("data\\modified_covid\\recovered.csv")



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

# reproducible routine for downloading and saving trends data for the project
gtrend_list <- gtrends("coronavirus", gprop = "web", geo = "", time = "2020-01-01 2020-03-12")

gtrend_list$interest_over_time %>% 
  mutate(hits = replace(hits, hits == "<1", 0)) %>% 
  mutate(hits = as.integer(hits)) %>% 
  mutate(date = as.Date(date)) %>% 
  select(c("date", "hits", "geo", "keyword", "gprop")) -> gtrend_df

write_csv(gtrend_df, paste("data\\gtrends\\", "gtrends.csv", sep = ""))

##########################################################
# Download Stock Market Data
##########################################################

library(tidyquant)

sp500_names <- tq_index("S&P500")

sp500 <- tq_get(sp500_names$symbol,
                from = "2020-01-01",
                to = "2020-03-12",
                get = "stock.prices")

# reproducible routine for downloading and saving trends data for the project
selected_stocks <- c("^GSPC", "^GDAXI", "NFLX", "LHA.DE", "GILD", "MRNA", "NCLH")
stock_df <- tq_get(selected_stocks,
       from = "2020-01-01",
       to = "2020-03-12",
       get = "stock.prices")

write_csv(stock_df, paste("data\\stocks\\", "stocks.csv", sep = ""))

##########################################################
# Download and Save Country Lookup Table
##########################################################

country_lookup <- read_csv("https://raw.github.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv")
write_csv(country_lookup, paste("data\\misc\\", "country_lookup.csv", sep = ""))


##########################################################
# Generate Prediction Data Set
##########################################################

# health preparedness index from https://www.ghsindex.org/
health <- read.csv("data/misc/health_index.csv", sep = ";")

# import population data
pop <- read_csv("https://raw.github.com/datasets/population/master/data/population.csv")
pop %>% 
  filter(Year == 2016) %>% 
  select(c(`Country Code`, "Value")) %>% 
  rename(countrycode = `Country Code`,
         population = Value) -> pop_c

# create countrycode to match data sets
health %>% 
  mutate(countrycode = countrycode(country,
                                   origin = "country.name",
                                   destination = "iso3c")) -> health

# aggregate by country and time
dfc %>% 
  group_by(country, time) %>% 
  summarize(confirmed = sum(confirmed),
            deaths = sum(deaths),
            recovered = sum(recovered),
            netinfected = sum(netinfected),
            lat = mean(latitude),
            lon = mean(longitude)) %>% 
  mutate(countrycode = countrycode(country,
                                   origin = "country.name",
                                   destination = "iso3c")) -> dfc_grouped

# get date of first confirmed case
dfc_grouped %>% 
  group_by(country) %>% 
  filter(confirmed != 0) %>% 
  arrange(time) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(countrycode, time) %>% 
  rename(first_confirmed = time) -> date_first_occurence

# add first confirmed date to df and calculate days since outbreak in each country
dfc_grouped %>% 
  left_join(date_first_occurence,
            by = "countrycode") %>% 
  mutate(days_outbreak = time - first_confirmed,
         days_outbreak = replace(days_outbreak, which(days_outbreak < 0), 0)) -> dfc_grouped_add
  


stock_df %>% 
  select(c("symbol", "date", "adjusted")) %>% 
  pivot_wider(names_from = symbol, values_from = adjusted) %>% 
  rename(time = date,
         SP500 = `^GSPC`,
         DAX30 = `^GDAXI`) -> stocks_wide


# merge all data sets
dfc_grouped_add %>% 
  left_join(stocks_wide, by = "time") %>% 
  left_join(gtrend_df %>%
              rename(time = date,
                     searchindex = hits) %>% 
              select(c("time", searchindex)),
            by = "time") %>% 
  left_join(health[,c("health_index", "countrycode")], by = "countrycode") %>% 
  left_join(pop_c, by = "countrycode") %>% 
  tibble::rowid_to_column("id") -> dfc_predict 

# check which countries aren't included in health_index/population data
dfc_predict %>% 
  filter(is.na(population)) %>% 
  pull(country) %>% 
  unique()

# move old prediction file to archive, rename it and write new one
file.copy("data\\predict\\predict.csv", "data\\predict\\archive")
as.Date(file.info("data\\predict\\predict.csv")$mtime)
file.rename("data\\predict\\archive\\predict.csv",
            paste("data\\predict\\archive\\", as.Date(file.info("data\\predict\\predict.csv")$mtime), "_predict.csv", sep = ""))
write_csv(dfc_predict, paste("data\\predict\\", "predict.csv", sep = ""))
