# Preliminaries
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(maps)
library(countrycode)
library(leaflet)
library(geojsonio)
library(viridis)
library(magick) # for including a .png in plots
TA_logo <- image_read("R/plots/TA_logo.png")
library(gghighlight)
library(GGally)
library(ggrepel)


##########################################################
## Part A: Exploratory Data Analysis
##########################################################

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
  top_n(9, confirmed) %>% 
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
            netinfected = sum(netinfected),
            lat = mean(latitude),
            lon = mean(longitude)) -> df_merged_country_time

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
# df_merged %>% 
#   filter(country == "Mainland China") -> df_china
# 
# df_merged %>% 
#   filter(country != "Mainland China") -> df_excl_china
# 
# df_china %>% 
#   group_by(time) %>% # reduces granularity to days w/o time
#   summarize(confirmed = sum(confirmed),
#             deaths = sum(deaths),
#             recovered = sum(recovered),
#             netinfected = sum(netinfected)
#   ) -> df_single_day_china
# 
# df_excl_china %>% 
#   group_by(time) %>% # reduces granularity to days w/o time
#   summarize(confirmed = sum(confirmed),
#             deaths = sum(deaths),
#             recovered = sum(recovered),
#             netinfected = sum(netinfected)
#   ) -> df_single_day_excl_china


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
  mutate(mortality = deaths/confirmed) %>% 
  arrange(desc(mortality)) %>% 
#  filter(country %in% top_countries) %>% 
  head(35) %>% 
  ggplot(aes(x = reorder(country, mortality))) +
  geom_bar(aes(weight = mortality), fill = "#3c4ee0") +
  gghighlight(country %in% top_countries) +
  coord_flip() +
  labs(x = "Country",
       y = "Mortality Rate",
       title = "Covid-19: Mortality Rate Depends Drastically On Countries",
       subtitle = paste("Mortality Rate by Country. Countries with Most Confirmed Cases Highlighted. Last update:", max(conf_agg$time)),
       caption = "Note: Mortality rate calculated as deaths/confirmed") +
  theme(plot.title = element_text(color = "#3c4ee0", face = 'bold'),
        plot.caption = element_text(hjust = 0, face = "italic"))
grid::grid.raster(TA_logo, x = 0.98, y = 0.01, just = c('right', 'bottom'), width = unit(1.5, 'inches'))

### 3. Critical Thinking

### 4. Visualization with Maps

# prepare data set
df_merged_country_time %>% 
  mutate(mortality = deaths/confirmed,
         countrycode = countrycode(country, origin = "country.name", destination = "iso3c")) %>% 
  arrange(desc(mortality)) %>% 
  filter(time == max(conf$time)) -> df_maps

world_map <- map_data("world")
world_map %>% 
  mutate(countrycode = countrycode(region, origin = "country.name", destination = "iso3c")) -> world_map

world_map %>% 
  left_join(df_maps %>%
              select(c("time", "confirmed", "deaths", "recovered", "netinfected", "mortality", "countrycode")),
            by = "countrycode") -> world_merged

world_merged %>% 
  group_by(region, countrycode, time) %>% 
  summarize(long_mean = mean(long),
            lat_mean = mean(lat),
            confirmed = median(confirmed),
            deaths = median(deaths),
            recovered = median(recovered),
            netinfected = median(netinfected),
            mortality = median(mortality)
            ) -> label_centroids

world_merged %>% 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = confirmed), color = "black") +
  scale_fill_gradient(low = "#3c4ee0", high = "red", na.value = NA) +
  theme_void() +
  labs(title = "Covid-19: Worldwide Spread of the Disease",
       subtitle = paste("Confirmed Cases by Country. Last update:", max(conf_agg$time)),
       caption = "",
       fill = "Confirmed Cases") +
  theme(plot.title = element_text(color = "#3c4ee0", face = 'bold'))
grid::grid.raster(TA_logo, x = 0.98, y = 0.01, just = c('right', 'bottom'), width = unit(1.5, 'inches'))
  
world_merged %>% 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = mortality), color = "black") +
  scale_fill_gradient(low = "#3c4ee0", high = "red", na.value = NA) +
  theme_void() +
  labs(title = "Covid-19: Mortality Rate Varies Largely",
       subtitle = paste("Mortality Rate by Country. Last update:", max(conf_agg$time)),
       caption = "",
       fill = "Mortality Rate") +
  theme(plot.title = element_text(color = "#3c4ee0", face = 'bold'))
grid::grid.raster(TA_logo, x = 0.98, y = 0.01, just = c('right', 'bottom'), width = unit(1.5, 'inches'))

# Europe only
eu_countries <- c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST",
                  "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA",
                  "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROU", "SVK",
                  "SVN", "ESP", "SWE", "GBR")

country_lookup <- read.csv("data/misc/country_lookup.csv")
country_lookup %>% 
  filter(region == "Europe") %>%
  filter(alpha.3 != "RUS")  %>%         
  pull(alpha.3) -> cc_europe

world_merged %>% 
  filter(countrycode %in% cc_europe) %>% 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = confirmed), color = "black") +
  # geom_label_repel(data = label_centroids %>% filter(countrycode %in% cc_europe),
  #           aes(x = long_mean, y = lat_mean, label = confirmed, fontface = 2),
  #           inherit.aes = FALSE,
  #           segment.color = "white") +
  geom_label_repel(data = label_centroids %>%
                     filter(countrycode %in% cc_europe) %>%
                     mutate(label = paste(region, confirmed, sep = "\n")),
                   aes(x = long_mean, y = lat_mean, label = label),
                   inherit.aes = FALSE,
                   segment.color = "white",
                   alpha = 0.85,
                   size = 3) +
  scale_fill_gradient(low = "#3c4ee0", high = "red", na.value = NA) +
  theme_void() +
  labs(title = "Covid-19: Spread of the Disease in Europe",
       subtitle = paste("Confirmed Cases by Country. Last update:", max(conf_agg$time)),
       caption = "",
       fill = "Confirmed Cases") +
  theme(plot.title = element_text(color = "#3c4ee0", face = 'bold'))
grid::grid.raster(TA_logo, x = 0.98, y = 0.01, just = c('right', 'bottom'), width = unit(1.5, 'inches'))



# leaflet interactive map
# doesn't work properly so far
WorldCountry <- geojsonio::geojson_read("https://raw.github.com/johan/world.geo.json/master/countries.geo.json", what = "sp")

WorldCountry <- WorldCountry[WorldCountry$id %in% data$Country, ]

#Dataframe for choropleth map
Country <- c("Bulgaria","Pakistan","Turkey")
Projects <- c(2,1,6)
data <- data.frame(Country,Projects)

data_Map <- WorldCountry[WorldCountry$id %in% cc_europe, ]

#basemap
Map <- leaflet(WorldCountry) %>% addTiles() %>% addPolygons()

#set bin and color for choropleth map
pal <- colorNumeric("Reds", domain = label_centroids$mortality)

#set labels
labels <- sprintf(
  "<strong>%s</strong><br/>%g confirmed <sup></sup>",
  label_centroids$region, label_centroids$confirmed) %>% lapply(htmltools::HTML)

#add polygons,labels and mouse over effect
Map %>% addPolygons(
  fillColor = label_centroids$confirmed
)
  
Map %>% 
  addPolygons(
  fillColor = ~pal(label_centroids$mortality),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = '3',
  fillOpacity = 1,
  highlight = highlightOptions(
    weight = 5,
    color = "#666",
    dashArray = "",
    fillOpacity = 1,
    bringToFront = TRUE),
  label = rev(labels),
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px",
    direction = "auto")
)

##########################################################
## Part B: Prediction
##########################################################

library(recipes)

train <- read.csv("data/predict/predict.csv")
train %>% 
  select_if(is.numeric) %>% 
  cor() %>% 
  round(2)
corr_features <- c("days_outbreak", "confirmed", "deaths", "recovered", "netinfected",
              "SP500", "searchindex", "health_index", "population")


ggpairs(train %>%
#          select(c("days_outbreak", "confirmed", "deaths", "recovered", "netinfected",
#                   "SP500", "searchindex", "health_index", "population"))) +
  select(netinfected, days_outbreak, population, health_index, searchindex, sars_netinfected, SP500)) +
  labs(title = 'Correlation and Scatterplots for Select Features',
       subtitle = "Get an intuition about which variables might be important for prediction",
       x = "") +
  theme_light(base_size = 12) +
  theme(plot.title = element_text(color = "#3c4ee0", face = 'bold'),
        legend.position = "none")
# add TechAcademy Logo
grid::grid.raster(TA_logo, x = 0.055, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'inches'))

# notes
# - extremely small correlation between number of patients and S&P and searchindex
#   -> apparently stocks and searches are not driven by the actual numbers, but by something else
#      might conclude that reaction is not rational?
# - extremely high negative correlation btw. S&P and searchindex
#   -> stock value driven by interest in the topic coronavirus?

## SIMPLE REGRESSION

lm(confirmed ~ deaths, data = train) %>% 
  summary()

lm(SP500 ~ searchindex, data = train) %>% 
  summary()

lm(deaths ~ health_index, data = train) %>% 
  summary()

lm(deaths ~ population, data = train) %>% 
  summary()

lm(deaths ~ days_outbreak, data = train) %>% 
  summary()

lm(deaths ~ country, data = train) %>% 
  summary()

lm(deaths ~ sars_deaths, data = train) %>% 
  summary()

train %>% 
  select(deaths, days_outbreak, population, health_index, sars_deaths) -> train_select

train %>%
  select(id, deaths, days_outbreak, population, health_index, sars_deaths) %>% 
  filter(is.na(sars_deaths))

# problem: countries, which have NA population and health index
train %>% 
  filter(is.na(population)) %>% 
  pull(countrycode) %>% 
  unique() -> remove_countrycode

# clean train
train %>% 
  select(id, country, time, netinfected, deaths, days_outbreak, population, health_index, searchindex, sars_deaths, SP500) %>% 
  drop_na() -> train_c


m_reg1 <- lm(deaths ~ days_outbreak + population + health_index + sars_deaths, data = train_c) 
summary(m_reg1)
yhat <- predict(m_reg1)
train_pred <- cbind(train_c, yhat)
View(train_pred)

train_pred %>% 
  ggplot(aes(yhat, deaths)) +
  geom_point()

train_pred %>% 
  ggplot(aes(yhat, deaths)) +
  geom_line(aes(color = country))

train %>% 
  ggplot(aes(x = days_outbreak, y = confirmed)) +
  geom_line(aes(color = country))

## ALTERNATIVE: train on China, predict on other countries
# Notes:
# use per capita measures to make it independent of absolute values
# prediction of confirmed_capita and deaths_capita seems to work partly by the sars outbreak
# netinfected almost no correlation to sars_netinfected per capita
# predicting with sars_confirmed_capita works well for China, but doesn't make any sense, since most other countries didn't have a SARS outbreak

train %>% 
  filter(countrycode == "CHN") -> train_china 

train_china$netinfected_capita_bin <- as.numeric(cut_number(train_china$netinfected_capita,7))
train_china$sars_netinfected_capita_bin <- as.numeric(cut_number(train_china$sars_netinfected_capita,7))

ggpairs(train_china %>%
          select(confirmed_capita, sars_confirmed_capita, deaths_capita, sars_deaths_capita, days_outbreak, netinfected_capita, sars_netinfected_capita))

# testing different models
lm(confirmed_capita ~ sars_confirmed_capita + days_larger_100, data = train_china) %>% 
  summary()

lm(netinfected_capita ~ sars_netinfected_capita, data = train_china) %>% 
  summary()

lm(deaths_capita ~ sars_deaths_capita + days_larger_100, data = train_china) %>% 
  summary()

lm(confirmed_capita ~ days_larger_100 + deaths_capita, data = train_china) %>% 
  summary()

lm(lag_confirmed ~ days_larger_100 + deaths, data = train_china) %>% 
  summary()

# using one to predict
m_reg2 <- lm(confirmed_capita ~ days_larger_100, data = train_china)
yhat <- predict(m_reg2)
y_yhat_m_reg2 <- cbind(train_china$confirmed_capita, yhat)

# test
train %>% 
  filter(countrycode != "CHN") %>% 
  select(country, time, sars_confirmed_capita, days_larger_100, confirmed_capita, deaths_capita, confirmed, searchindex) %>% 
  drop_na() -> test_wo_china

summary(predict(m_reg2, test_wo_china))
yhat_test_wo_china <- predict(m_reg2, test_wo_china)
test_wo_china_yhat <- cbind(test_wo_china, yhat_test_wo_china)

test_wo_china_yhat %>% 
  ggplot(aes(days_larger_100, confirmed_capita)) +
  geom_point() +
  geom_line(aes(y = yhat_test_wo_china)) +
  facet_wrap(~country, scales = "free")







## ALTERNATIVE: train on half of top countries, predict on other top countries
train_countries <- top_countries[c(FALSE, TRUE)]
test_countries <- top_countries[c(TRUE, FALSE)]

train %>% 
  filter(country %in% train_countries) -> train_top

train %>% 
  filter(country %in% test_countries) -> test_top

# checking different models with simple regressions
lm(confirmed ~ days_larger_100 + health_index + population, data = train_top) %>% 
  summary()

lm(deaths ~ days_larger_100 + health_index + population, data = train_top) %>% 
  summary()

# using one to predict
m_reg3 <- lm(confirmed ~ days_larger_100 + health_index + population, data = train_top)
yhat <- predict(m_reg3)
y_yhat_m_reg3 <- cbind(train_top$confirmed, yhat)

# test

yhat_test_top <- predict(m_reg3, test_top)
test_top_yhat <- cbind(test_top, yhat_test_top)

test_top_yhat %>% 
  select(country, time, days_larger_100, population, health_index, deaths, yhat_test_top) %>% 
  View()

train_top %>% 
  ggplot(aes(days_larger_100, confirmed)) +
  geom_point() +
  geom_line(aes(y = yhat)) +
  facet_wrap(~country, scales = "free_y")

test_top_yhat %>% 
  ggplot(aes(days_larger_100, confirmed)) +
  geom_point() +
  geom_line(aes(y = yhat_test_top)) +
  facet_wrap(~country, scales = "free_y")





# save all plots
  plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
  plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
  plots.png.detials <- file.info(plots.png.paths)
  plots.png.detials <- plots.png.detials[order(plots.png.detials$mtime),]
  sorted.png.names <- gsub(plots.dir.path, "R/plots/", row.names(plots.png.detials), fixed=TRUE)
  numbered.png.names <- paste0("R/plots/", 1:length(sorted.png.names), ".png")
  
  file.copy(from=plots.png.paths, to="R/plots/")
  
  # Rename all the .png files as: 1.png, 2.png, 3.png, and so on.
  file.rename(from=sorted.png.names, to=numbered.png.names)
  

