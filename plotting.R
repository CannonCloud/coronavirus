source("get_data_github.R")

#####################################################
# Coronavirus infected, deaths, recovered
#####################################################

# 1) Entire Data Set
# define data set
plot_data_single_day <- df_single_day_excl_china #df_single_day #df_single_day_china

plot_data_single_day %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = confirmed)) +
  geom_line(aes(y = netinfected), linetype = "dashed") +
  geom_line(aes(y = -c(deaths)), color = "red") +
  geom_line(aes(y = -c(recovered)), color = "blue") +
  labs(x = 'Date',
       y = "Number of Patients",
       title = "COVID-19: Global Number of Infected, Net Infected, Deaths, and Recovered Patients",
       subtitle = paste("Last Update:", max(df_single_day$time)))

# from long to wide
plot_data_single_day %>% 
  pivot_longer(confirmed:netinfected, names_to = "type", values_to = "amount") -> plot_data_single_day_long

plot_data_single_day_long %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = amount)) +
  geom_point(aes(y = amount)) +
  facet_wrap(.~ type, scales = "free_y") +
  labs(x = 'Date',
       y = "Number of Patients",
       title = "COVID-19 Global Statistics",
       subtitle = paste("Last Update:", max(df_single_day_long$time)))


#####################################################
# Coronavirus Maps
#####################################################

library(leaflet)
leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")

leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(dfc[1:7,], lng = dfc[1:7,]$longitude, lat = dfc[1:7,]$latitude) %>% 
  addPolygons(dfc[1:7,]$country) 
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
              color = ~pal(gdp_md_est))


#####################################################
# Google Trends
#####################################################

time_trend %>% 
  ggplot(aes(x = date, y = hits)) +
  geom_line() +
  geom_point() +
  facet_wrap(~keyword) +
  xlab("Time") +
  ylab("Relative Interest") +
  ggtitle(paste("Google Search Volume: Channel", channel))
  
  
#####################################################
# Stock Data
#####################################################  
  
symbols <- c("AAL", "DAL", "FDX", "LUV", "UAL", "AAPL")
  
sp500 %>%
  filter(symbol %in% symbols) %>% 
  ggplot(aes(x = date, y = adjusted)) +
  geom_line() +
  geom_point() +
  facet_wrap(~symbol, scales = "free_y") +
  labs(x = 'Date',
       y = "Adjusted Price",
       title = "Price Charts for Selected S&P Companies with China Exposure")
