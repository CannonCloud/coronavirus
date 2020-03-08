# Preliminaries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(magick) # for including a .png in plots
TA_logo <- image_read("R/plots/TA_logo.png")

## Part A: Exploratory Data Analysis
### 1. Visualize Google Searches and Stock Price Time Series

gtrend <- read.csv("data/gtrends/gtrends.csv")
str(gtrend)
gtrend$date <- as.Date(gtrend$date)

gtrend %>% 
  ggplot(aes(x = as.Date(date), y = hits)) +
  geom_line(color = "#3c4ee0") +
  geom_point(color = "#3c4ee0") +
  labs(x = "Time",
       y = "Relative Interest",
       title = "Google-ing Corona",
       subtitle = "Relative Number of Worldwide Google Search Queries for \"Coronavirus\"") +
    theme(plot.title = element_text(color = "#3c4ee0", face = 'bold'))
grid::grid.raster(TA_logo, x = 0.98, y = 0.01, just = c('right', 'bottom'), width = unit(1.5, 'inches'))

stocks <- read.csv("data/stocks/stocks.csv")
str(stocks)
stocks$date <- as.Date(stocks$date)

labs <- c("DAX", "S&P 500", "Gilead Sciences", "Lufthansa", "Moderna", "Norwegian Cruise Lines", "Netflix")
levels(stocks$symbol) <- labs

stocks %>% 
  ggplot(aes(x = date, y = adjusted)) +
    geom_line(color = "#3c4ee0") +
    geom_point(color = "#3c4ee0") +
    facet_wrap(~symbol, scales = "free_y") +
    labs(x = 'Date',
         y = "Adjusted Price",
         title = "Tourism Stocks Tanked, but Biotech Companies and Netflix Soared During the Coronavirus Crisis",
         subtitle = "Price Charts for Selected Indices and Companies") +
    theme(plot.title = element_text(color = "#3c4ee0", face = 'bold'))

grid::grid.raster(TA_logo, x = 0.98, y = 0.01, just = c('right', 'bottom'), width = unit(1.5, 'inches'))

### 2. Data Wrangling and Visualization of COVID-19-data

conf <- read.csv("data/modified_covid/confirmed.csv")
str(conf)

conf$time <- as.Date(conf$time)
conf$confirmed <- ifelse(is.na(conf$confirmed), 0, conf$confirmed)
sum(is.na(conf$confirmed))

conf %>% 
  group_by(time) %>% 
  summarize(confirmed = sum(confirmed)) -> conf_agg

# simple line plot (number of infected)
conf_agg %>% 
  ggplot(aes(x = time, y = confirmed)) +
  geom_line(color = "#3c4ee0") +
  geom_point(color = "#3c4ee0") +
  labs(x = 'Date',
       y = "Number of Infected",
       title = "Covid-19: Is it over yet?",
       subtitle = paste("Number of Infected by Time. Last update:", max(conf_agg$time))) +
  theme(plot.title = element_text(color = "#3c4ee0", face = 'bold'))

grid::grid.raster(TA_logo, x = 0.98, y = 0.01, just = c('right', 'bottom'), width = unit(1.5, 'inches'))

# stacked line plot with filled areas by country
# see https://www.economist.com/briefing/2020/03/05/what-the-world-has-learned-about-facing-covid-19

# 1. get top 6 countries according to last snapshot
conf %>% 
  filter(time == max(conf$time)) %>% 
  group_by(country) %>% 
  summarize(confirmed = sum(confirmed),
            time = max(conf$time)) %>% 
  arrange(desc(confirmed)) %>% 
  top_n(6, confirmed) %>% 
  pull(country) %>% 
  droplevels() -> top_countries

top_countries %>% 
  factor(levels = top_countries) -> top_countries

# 2. 
conf %>% 
  group_by(country, time) %>% 
  summarize(confirmed = sum(confirmed)) %>% 
  filter(country %in% top_countries) %>% 
  droplevels() -> conf_agg_time_country

arrange(conf_agg_time_country, factor(conf_agg_time_country$country, levels = top_countries)) -> temp
levels(temp$country)
temp2 %>% 
  droplevels.data.frame() %>% 
  arrange(factor(country, levels = top_countries)) -> temp2
levels(temp2$country)

conf_agg_time_country %>% 
  arrange(factor(country, levels = top_countries)) %>% 
  ggplot(aes(x = time, y = confirmed, fill = reorder(country, confirmed))) +
  geom_area(position = "stack", color = "white") +
  scale_fill_brewer(palette = "Blues") +
  labs(x = 'Date',
       y = "Number of Infected",
       title = "Covid-19: Spread Slows in China While Accelerating in Europe",
       subtitle = paste("Number of Infected by Time and Country. Last update:", max(conf_agg$time)),
       fill = "Country") +
    theme(plot.title = element_text(color = "#3c4ee0", face = 'bold'),
          legend.position = c(0.2, 0.7))
grid::grid.raster(TA_logo, x = 0.98, y = 0.01, just = c('right', 'bottom'), width = unit(1.5, 'inches'))


# generate total data set by merging three data sets by id
dead <- read.csv("data/modified_covid/deaths.csv")
recov <- read.csv("data/modified_covid/recovered.csv")

# merge using base
df_merged <- merge(conf, dead, by = "id")
df_merged <- merge(df_merged, recov, by = "id")

# merge using dplyr
conf %>% 
  inner_join(dead) %>% 
  inner_join(recov) -> df_merged

# remove NAs in the two new columns
df_merged$deaths <- ifelse(is.na(df_merged$deaths), 0, df_merged$deaths)
df_merged$recovered <- ifelse(is.na(df_merged$recovered), 0, df_merged$recovered)

# calculate net infected
df_merged$netinfected <- df_merged$confirmed - df_merged$deaths - df_merged$recovered

# aggregate by country and time
df_merged %>% 
  group_by(country, time) %>% 
  summarize(confirmed = sum(confirmed),
            deaths = sum(deaths),
            recovered = sum(recovered),
            netinfected = sum(netinfected)) -> df_merged_country_time

df_merged_country_time %>% 
  filter(country %in% top_countries) %>% # optional filter
  ggplot(aes(x = time)) +
    geom_line(aes(y = confirmed)) +
    geom_line(aes(y = netinfected), linetype = "dashed") +
    geom_line(aes(y = -c(deaths)), color = "red") +
    geom_line(aes(y = -c(recovered)), color = "blue") +
  facet_wrap(~country, scales = "free_y") +
  labs(x = 'Date',
       y = "Number of Patients",
       title = "Covid-19: Spread Slows in China While Accelerating in Other Countries",
       subtitle = paste("Number of Patients by Time and Country. Last update:", max(conf_agg$time)),
       caption = "Note: Confirmed and Net Infected positive. Deaths (red) and Recovered (blue) negative") +
  theme(plot.title = element_text(color = "#3c4ee0", face = 'bold'),
        plot.caption = element_text(hjust = 0, face = "italic"))
grid::grid.raster(TA_logo, x = 0.98, y = 0.01, just = c('right', 'bottom'), width = unit(1.5, 'inches'))

# better alternative way with legend
df_merged_country_time %>% 
  mutate(deaths = -deaths,
         recovered = -recovered) %>% 
  filter(country %in% top_countries) %>%
  pivot_longer(confirmed:netinfected, names_to = "status", values_to = "amount") %>%
  ggplot(aes(x = time, y = amount)) +
  geom_line(aes(color = status, linetype = status)) +
  scale_linetype_manual(values = c("solid", "solid", "dashed", "solid")) +
  scale_color_manual(values = c("black", "red", "black", "#3c4ee0")) +
  facet_wrap(~country, scales = "free_y") +
  labs(x = 'Date',
       y = "Number of Patients",
       title = "Covid-19: Spread Slows in China While Accelerating in Other Countries",
       subtitle = paste("Number of Patients by Time and Country. Last update:", max(conf_agg$time))) +
  theme(plot.title = element_text(color = "#3c4ee0", face = 'bold'),
        plot.caption = element_text(hjust = 0, face = "italic"))
grid::grid.raster(TA_logo, x = 0.98, y = 0.01, just = c('right', 'bottom'), width = unit(1.5, 'inches'))

# areaplot
df_merged_country_time %>% 
  mutate(deaths = -deaths,
         recovered = -recovered) %>% 
  filter(country %in% top_countries) %>%
  pivot_longer(confirmed:netinfected, names_to = "status", values_to = "amount") %>%
  ggplot(aes(x = time)) +
  geom_area(aes(y = amount, fill = status), color = "white", alpha = 0.7, position = position_dodge(0)) +
  scale_fill_manual(values = c("grey", "red", "black", "#3c4ee0")) +
  facet_wrap(~country, scales = "free_y") +
  labs(x = 'Date',
       y = "Number of Patients",
       title = "Covid-19: Spread Slows in China While Accelerating in Other Countries",
       subtitle = paste("Number of Patients by Time and Country. Last update:", max(conf_agg$time))) +
  theme(plot.title = element_text(color = "#3c4ee0", face = 'bold'),
        plot.caption = element_text(hjust = 0, face = "italic"))
grid::grid.raster(TA_logo, x = 0.98, y = 0.01, just = c('right', 'bottom'), width = unit(1.5, 'inches'))

# Split data set in China and Rest of World
# this exercise should not be included, since using facet_wrap with the full data set is (see above) more flexible
df_merged %>% 
  filter(country == "Mainland China") -> df_china

df_merged %>% 
  filter(country != "Mainland China") -> df_excl_china

df_china %>% 
  group_by(time) %>% # reduces granularity to days w/o time
  summarize(confirmed = sum(confirmed),
            deaths = sum(deaths),
            recovered = sum(recovered),
            netinfected = sum(netinfected)
  ) -> df_single_day_china

df_excl_china %>% 
  group_by(time) %>% # reduces granularity to days w/o time
  summarize(confirmed = sum(confirmed),
            deaths = sum(deaths),
            recovered = sum(recovered),
            netinfected = sum(netinfected)
  ) -> df_single_day_excl_china

# visualizing all four quantities in one plot (confirmed, deaths, recovered, netinfected) 
china <- df_single_day_china %>% 
  ggplot(aes(x = time)) +
    geom_line(aes(y = confirmed)) +
    geom_line(aes(y = netinfected), linetype = "dashed") +
    geom_line(aes(y = -c(deaths)), color = "red") +
    geom_line(aes(y = -c(recovered)), color = "blue")


# Barplot most recent date: by country
df_merged_country_time %>% 
  filter(time == max(conf$time)) %>% 
  filter(country %in% top_countries) %>% 
  arrange(desc(confirmed)) %>% 
  pivot_longer(confirmed:netinfected, names_to = "status", values_to = "amount") %>%
  ggplot(aes(x = reorder(country, amount))) +
  geom_bar(aes(weight = amount, fill = status), position = "dodge") +
  coord_flip() +
  scale_fill_manual(values = c("grey", "red", "black", "#3c4ee0")) +
  labs(x = 'Date',
       y = "Number of Patients",
       title = "Covid-19: Today's Patients' Status",
       subtitle = paste("Number of Patients by Country. Last update:", max(conf_agg$time))) +
  theme(plot.title = element_text(color = "#3c4ee0", face = 'bold'),
        plot.caption = element_text(hjust = 0, face = "italic"))
grid::grid.raster(TA_logo, x = 0.98, y = 0.01, just = c('right', 'bottom'), width = unit(1.5, 'inches'))

# calculate mortality rates
df_merged_country_time %>% 
  filter(time == max(conf$time)) %>% 
  mutate(mortality = deaths/netinfected) %>% 
  arrange(desc(mortality)) %>% 
  filter(country %in% top_countries) %>% 
#  head(20) %>% 
  ggplot(aes(x = reorder(country, mortality))) +
  geom_bar(aes(weight = mortality), fill = "#3c4ee0") +
  coord_flip() +
  labs(x = 'Date',
       y = "Number of Patients",
       title = "Covid-19: Mortality Rate Differs Drastically Depending Countries",
       subtitle = paste("Mortality Rate by Country. Last update:", max(conf_agg$time)),
       caption = "Note: Mortality rate calculated as deaths/netinfected") +
  theme(plot.title = element_text(color = "#3c4ee0", face = 'bold'),
        plot.caption = element_text(hjust = 0, face = "italic"))
grid::grid.raster(TA_logo, x = 0.98, y = 0.01, just = c('right', 'bottom'), width = unit(1.5, 'inches'))

### 3. Critical Thinking

### 4. Visualization with Maps
