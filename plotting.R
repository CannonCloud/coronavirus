source("get_data.R")

#####################################################
# Coronavirus infected, deaths, recovered
#####################################################

dfa %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = confirmed)) +
  geom_line(aes(y = net_infected), linetype = "dashed") +
  geom_line(aes(y = -c(deaths)), color = "red") +
  geom_line(aes(y = -c(recovered)), color = "blue") +
  labs(x = 'Date',
       y = "Number of People",
       title = "Coronavirus number of infected, deaths, recovered",
        subtitle = paste("Last Update:", max(dfa$date)))

# from long to wide
dfa %>% 
  pivot_longer(confirmed:net_infected, names_to = "type", values_to = "amount") -> dfaw

dfaw %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = amount)) +
  facet_wrap(.~ type, scales = "free_y") +
  labs(x = 'Date',
       y = "Number of People",
       title = "Coronavirus Statistics",
       subtitle = paste("Last Update:", max(dfaw$date)))
  
## new time series data

tslu %>% 
  ggplot(aes(x = time)) +
  geom_point(aes(y = confirmed)) +
 # facet_wrap(.~ type, scales = "free_y") +
  labs(x = 'Date',
       y = "Number of People",
       title = "Coronavirus Confimed Cases")


#####################################################
# Google Trends
#####################################################

time_trend %>% 
  ggplot(aes(x = date, y = hits)) +
  geom_line() +
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
  facet_wrap(~symbol, scales = "free_y") +
  labs(x = 'Date',
       y = "Adjusted Price",
       title = "Price Charts for Selected S&P Companies with China Exposure")
