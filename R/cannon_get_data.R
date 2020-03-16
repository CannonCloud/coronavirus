library(tidyverse)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(countrycode)
library(magrittr)

##########################################################
# Download Johns Hopkins CSSE Data from GitHub
##########################################################

var_list <- list("Confirmed", "Deaths", "Recovered")

#create list of links
create_jh_link <- function(x) {
  paste0("https://raw.github.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-", x, ".csv")
}

links <- lapply(var_list, create_jh_link)

#read links to create datasets
jh_data <- lapply(links, read_csv)
names(jh_data) <- var_list

#save data
lapply(1:length(jh_data), function(i) write_csv(jh_data[[i]], 
                                                path = paste0("data/", Sys.Date(), "_", names(jh_data[i]), ".csv")))

#make data long, and merge variables
long_data <- lapply(jh_data, gather, key = "time", value = "variable", -c(1:4))

#I can't figure out how to lapply this since I can't pass a list of strings to the rename argument...
long_data$Confirmed %<>% rename(confirmed = variable)
long_data$Deaths    %<>% rename(deaths    = variable)
long_data$Recovered %<>% rename(recovered = variable)

panel_data <-long_data

#merge data on province/state, country/region, time
panel_data %<>% reduce(full_join, by = c("Province/State", "Country/Region", "Lat", "Long", "time"  ))

# data cleaning and preparation
# full data set
panel_data %<>% 
  rename(province = `Province/State`,
         country = `Country/Region`,
         latitude = Lat,
         longitude = Long) %>%  
  mutate(time = mdy(time),
         netinfected = confirmed - deaths - recovered)

write_csv(panel_data, paste0("data/", Sys.Date(), "_total.csv"))

# generate aggregate data set over all regions per single date
sum_CDRN <- function(x) {
  summarize(x, confirmed = sum(confirmed),
            deaths = sum(deaths),
            recovered = sum(recovered),
            netinfected = sum(netinfected))
}

panel_data_sum_all <- group_by(panel_data, time) %>%
  sum_CDRN()

write_csv(panel_data_sum_all, paste0("data/", Sys.Date(), "_summed.csv"))

panel_data_sum_country <- group_by(panel_data, country, time) %>%
  sum_CDRN()

write_csv(panel_data_sum_country, paste0("data/", Sys.Date(), "_country_sum.csv"))



##########################################################
# Download and Save Country Lookup Table
##########################################################

country_lookup <- read_csv("https://raw.github.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv")
write_csv(country_lookup, paste0("data/", "country_lookup.csv", sep = ""))


##########################################################
# Generate Prediction Data Set
##########################################################

# health preparedness index from https://www.ghsindex.org/
health <- read.csv("data/health_index.csv", sep = ";")

# import population data
pop <- read_csv("https://raw.github.com/datasets/population/master/data/population.csv")
pop_c <- filter(pop, Year == 2016) %>% 
  select(c(`Country Code`, "Value")) %>% 
  rename(countrycode = `Country Code`,
         population = Value)

# create countrycode to match data sets
health %<>% 
  mutate(countrycode = countrycode(country,
                                   origin = "country.name",
                                   destination = "iso3c"))

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

# get date of number of cases larger than 100
dfc_grouped %>% 
  group_by(country) %>% 
  filter(confirmed >= 100) %>% 
  arrange(time) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(countrycode, time) %>% 
  rename(datelarger100 = time) -> date_larger_100

# add first confirmed date to df and calculate days since outbreak in each country
dfc_grouped %>% 
  left_join(date_first_occurence,
            by = "countrycode") %>% 
  left_join(date_larger_100,
            by = "countrycode") %>% 
  mutate(days_outbreak = time - first_confirmed,
         days_outbreak = replace(days_outbreak, which(days_outbreak < 0), 0),
         days_larger_100 = time - datelarger100,
         days_larger_100 = replace(days_larger_100, which(days_larger_100 < 0), 0)) -> dfc_grouped_add

stock_df %>% 
  select(c("symbol", "date", "adjusted")) %>% 
  pivot_wider(names_from = symbol, values_from = adjusted) %>% 
  rename(time = date,
         SP500 = `^GSPC`,
         DAX30 = `^GDAXI`) -> stocks_wide

# get SARS data from https://www.kaggle.com/imdevskp/sars-outbreak-2003-complete-dataset/data
sars <- read.csv("data/misc/sars.csv")
sars %>% 
  rename(time = Date,
         country = Country,
         sars_confirmed = Cumulative.number.of.case.s.,
         sars_deaths = Number.of.deaths,
         sars_recovered = Number.recovered) %>% 
  mutate(time = as.Date(time),
         sars_netinfected = sars_confirmed - sars_deaths - sars_recovered,
         countrycode = countrycode(country,
                                   origin = "country.name",
                                   destination = "iso3c")) -> df_sars

# idea: get overall SARS outbreak curve over all countries
df_sars %>% 
  group_by(time) %>% 
  summarize(sars_confirmed = sum(sars_confirmed),
            sars_deaths = sum(sars_deaths),
            sars_recovered = sum(sars_recovered),
            sars_netinfected = sum(sars_netinfected)) %>% 
  tibble::rowid_to_column("sars_days_since_outbreak") %>% 
  rename(days_larger_100 = sars_days_since_outbreak,
         sars_time = time) -> df_sars_grouped # inconsistent naming, but need that for merging later


df_sars_grouped %>%
  ggplot(aes(x = sars_days_since_outbreak, y = sars_recovered)) +
  geom_point()
# this uncovers a serious measurement error in the data. Netinfected only started counting at >1000 infections -> distorts netinfected
# solution: approximate function (tried polynomial, which is definitely not suitable)
# next try: gaussian quadrature



# # get date of first confirmed case for sars
# df_sars %>% 
#   group_by(countrycode) %>% 
#   filter(sars_confirmed != 0) %>% 
#   arrange(time) %>% 
#   slice(1) %>% 
#   ungroup() %>% 
#   select(countrycode, time) %>% 
#   rename(sars_first_confirmed = time) -> sars_date_first_occurence
# 
# # get date of number of cases larger than 100 for sars
# df_sars %>% 
#   group_by(countrycode) %>% 
#   filter(sars_confirmed >= 100) %>% 
#   arrange(time) %>% 
#   slice(1) %>% 
#   ungroup() %>% 
#   select(countrycode, time) %>% 
#   rename(sars_datelarger100 = time) -> sars_date_larger_100
# 
# # add first confirmed date to df and calculate days since outbreak in each country
# df_sars %>% 
#   left_join(sars_date_first_occurence,
#             by = "countrycode") %>% 
#   left_join(sars_date_larger_100,
#             by = "countrycode") %>% 
#   mutate(sars_days_outbreak = time - sars_first_confirmed,
#          sars_days_outbreak = replace(sars_days_outbreak, which(sars_days_outbreak < 0), 0),
#          sars_days_larger_100 = time - sars_datelarger100,
#          sars_days_larger_100 = replace(sars_days_larger_100, which(sars_days_larger_100 < 0), 0)) %>% 
#   rename(sars_time = time)-> dfc_sars_grouped_add




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
  mutate(days_outbreak = as.numeric(days_outbreak),
         days_larger_100 = as.numeric(days_larger_100)) %>% 
  left_join(df_sars_grouped, by = "days_larger_100") %>% 
  mutate(sars_confirmed = replace_na(sars_confirmed, 0),
         sars_deaths = replace_na(sars_deaths, 0),
         sars_recovered = replace_na(sars_recovered, 0),
         sars_netinfected = replace_na(sars_netinfected, 0),
         confirmed_capita = confirmed/population,
         deaths_capita = deaths/population,
         recovered_capita = recovered/population,
         netinfected_capita = netinfected/population,
         sars_confirmed_capita = sars_confirmed/population,
         sars_deaths_capita = sars_deaths/population,
         sars_recovered_capita = sars_recovered/population,
         sars_netinfected_capita = sars_netinfected/population) %>%
  group_by(country) %>% 
  mutate(lag_confirmed = dplyr::lag(confirmed, n = 14, default = NA)) %>% 
  tibble::rowid_to_column("id") -> dfc_predict 

# check which countries aren't included in health_index/population data
dfc_predict %>% 
  filter(is.na(population)) %>% 
  pull(country) %>% 
  unique()

# move old prediction file to archive, rename it and write new one
file.copy("data\\predict\\predict.csv", "data\\predict\\archive")
file.rename("data\\predict\\archive\\predict.csv",
            paste("data\\predict\\archive\\", as.Date(file.info("data\\predict\\predict.csv")$mtime), "_predict.csv", sep = ""))
write_csv(dfc_predict, paste("data\\predict\\", "predict.csv", sep = ""))

© 2020 GitHub, Inc.
Terms
Privacy
Security
Status
Help

Contact GitHub
Pricing
API
Training
Blog
About

