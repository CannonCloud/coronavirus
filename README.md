# COVID-19 TechAcademy Data Science Project
In this repository, we're building an educational hands-on Data Science project for students who are participating in TechAcademy's curriculum.
We analyze the recent coronavirus outbreak and teach Data Science in Python or R along the way.

# Exercises
## Part A: Exploratory Data Analysis
### 1. Visualize Google Searches and Stock Price Time Series

- simple line plot for keyword "coronavirus" without any data wrangling
- Stock data: facet_wrap by share (Airlines, healthcare, slack)
- Gilead Sciences, Moderna, Lufthansa, overall index (S&P500)

### 2. Data Wrangling and Visualization of COVID-19-data

- Lineplot number infected (group_by time)
- Stacked Area Plot (Top 5 countries, see Economist). Use group_by country, date and arrange, filter

- generate total data set by merging confimed with deaths and recovered by ID
- calculate net infected
- Split data sets in China and Rest of World
- visualize four timeseries in two plots (china, rest of world)

- Barplot most recent date: by country
  - first plot absolute number of dead and recovered
  - second plot: mortality rate by country (top n) (either deaths/confirmed or deaths/recovered or deaths/netinfected)

### 3. Critical Thinking
- Why is there a kink in the total number of infected around Feb 10? -> Testing criteria changed
- Mortality rate: Why is it problematic to calculate the mortality rate during a pandemic? How would you calculate the true rate?
- How does the political attitude towards testing the population affect the number of infected and number of deaths? (Italy vs. US)

### 4. Visualization with Maps
Static Map
- select most recent snapshot
- World Maps: individual data points
- color by number of infected
- Include country borders and color by number of infected in each country (rwoldmap)  \

Dynamic Map
- Create dynamic world map, get detailed information by clicking on each data point (leaflet)

## Part B: Prediction
