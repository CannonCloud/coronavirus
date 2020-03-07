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


### 3. Critical Thinking

### 4. Visualization with Maps
