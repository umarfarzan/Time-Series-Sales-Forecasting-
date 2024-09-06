pacman::p_load(
  tidyverse,
  forecast,
  urca,
  vars,
  mFilter,
  lmtest,
  tseries,
  stargazer,
  readr,
  ggplot2,
  reshape2,
  anomalize
)

df_poultry <- read.csv("sample1_sales_timeseries.csv", header = TRUE, colClasses = c(date="Date", store_nbr = "numeric", sales = "numeric", onpromotion = "numeric"))
df_meats <- read.csv("sample2_sales_timeseries.csv", header = TRUE, colClasses = c(date="Date", store_nbr = "numeric", sales = "numeric", onpromotion = "numeric"))
df_liquor_wine_beer <- read.csv("sample3_sales_timeseries.csv", header = TRUE, colClasses = c(date="Date", store_nbr = "numeric", sales = "numeric", onpromotion = "numeric"))
df_prepared_foods <- read.csv("sample4_sales_timeseries.csv", header = TRUE, colClasses = c(date="Date", store_nbr = "numeric", sales = "numeric", onpromotion = "numeric"))
df_frozen_foods <- read.csv("sample5_sales_timeseries.csv", header = TRUE, colClasses = c(date="Date", store_nbr = "numeric", sales = "numeric", onpromotion = "numeric"))






#This is reading code for Mac
#df_poultry <- read.csv("sample1_sales_timeseries.csv", header = TRUE, 
#                       colClasses = c(store_nbr = "numeric", sales = "numeric", onpromotion = "numeric"))
#df_poultry$date <- as.Date(df_poultry$date, format = "%m/%d/%y")

#df_meats <- read.csv("sample2_sales_timeseries.csv", header = TRUE, 
#                     colClasses = c(store_nbr = "numeric", sales = "numeric", onpromotion = "numeric"))
#df_meats$date <- as.Date(df_poultry$date, format = "%m/%d/%y")

#df_liquor_wine_beer <- read.csv("sample3_sales_timeseries.csv", header = TRUE, 
#                                colClasses = c(store_nbr = "numeric", sales = "numeric", onpromotion = "numeric"))
#df_liquor_wine_beer$date <- as.Date(df_liquor_wine_beer$date, format = "%m/%d/%y")

#df_prepared_foods <- read.csv("sample4_sales_timeseries.csv",  header = TRUE, 
#                              colClasses = c(store_nbr = "numeric", sales = "numeric", onpromotion = "numeric"))
#df_prepared_foods$date <- as.Date(df_prepared_foods$date, format = "%m/%d/%y")

#df_frozen_foods <- read.csv("sample5_sales_timeseries.csv",  header = TRUE, 
#                           colClasses = c(store_nbr = "numeric", sales = "numeric", onpromotion = "numeric"))
#df_frozen_foods$date <- as.Date(df_frozen_foods$date, format = "%m/%d/%y")


#daily

df_frozen_foods <- df_frozen_foods %>%
  group_by(date) %>%
  summarise(TOTAL_SALES = sum(sales), TOTAL_PROMOTIONS = sum(onpromotion)) %>%
  ungroup() %>%
  mutate(onpromotion = ifelse(TOTAL_PROMOTIONS > 0, "Yes", "No") %>% 
           factor(levels = c("Yes", "No")))

df_liquor_wine_beer <- df_liquor_wine_beer %>%
  group_by(date) %>%
  summarise(TOTAL_SALES = sum(sales), TOTAL_PROMOTIONS = sum(onpromotion)) %>%
  ungroup() %>%
  mutate(onpromotion = ifelse(TOTAL_PROMOTIONS > 0, "Yes", "No") %>% 
           factor(levels = c("Yes", "No")))

df_meats <- df_meats %>%
  group_by(date) %>%
  summarise(TOTAL_SALES = sum(sales), TOTAL_PROMOTIONS = sum(onpromotion)) %>%
  ungroup() %>%
  mutate(onpromotion = ifelse(TOTAL_PROMOTIONS > 0, "Yes", "No") %>% 
           factor(levels = c("Yes", "No")))

df_poultry <- df_poultry %>%
  group_by(date) %>%
  summarise(TOTAL_SALES = sum(sales), TOTAL_PROMOTIONS = sum(onpromotion)) %>%
  ungroup() %>%
  mutate(onpromotion = ifelse(TOTAL_PROMOTIONS > 0, "Yes", "No") %>% 
           factor(levels = c("Yes", "No")))

df_prepared_foods <- df_prepared_foods %>%
  group_by(date) %>%
  summarise(TOTAL_SALES = sum(sales), TOTAL_PROMOTIONS = sum(onpromotion)) %>%
  ungroup() %>%
  mutate(onpromotion = ifelse(TOTAL_PROMOTIONS > 0, "Yes", "No") %>% 
           factor(levels = c("Yes", "No")))


#time series
ts_meats <- ts(df_meats$TOTAL_SALES, frequency = 365, start = c(2013, 1))
ts_poultry <- ts(df_poultry$TOTAL_SALES, frequency = 365, start = c(2013, 1))
ts_frozen_foods <- ts(df_frozen_foods$TOTAL_SALES, frequency  = 365, start = c(2013, 1))
ts_liquor_wine_beer <- ts(df_liquor_wine_beer$TOTAL_SALES, frequency = 365, start = c(2013, 1))
ts_prepared_foods <- ts(df_prepared_foods$TOTAL_SALES, frequency = 365, start = c(2013, 1))



#EDA For Poultry ----
str(df_poultry)
head(df_poultry)

View(df_poultry)


df_poultry <- df_poultry %>%
  group_by(date) %>%
  summarise(TOTAL_SALES = sum(sales), TOTAL_PROMOTIONS = sum(onpromotion)) %>%
  ungroup() %>%
  mutate(onpromotion = ifelse(TOTAL_PROMOTIONS > 0, "Yes", "No") %>% 
           factor(levels = c("Yes", "No")))

ts_poultry <- ts(df_poultry$TOTAL_SALES, start=c(2013,1), 
                 frequency = 365)

plot.ts(ts_poultry, xlab = "Time", ylab = "Total Sales",
        main = "Sales of Poultry in Ecuador: 2013-2017")

#Decomposition
decomp_poultry_additive <- decompose(ts_poultry,type='additive')
plot(decomp_poultry_additive)

stl_decomp_poultry_additive <- stl(ts_poultry, s.window=15, t.window=365)
plot(stl_decomp_poultry_additive, main="STL Decomposition (Additive)" )

# Decompose the time series
decomp_poultry<- decompose(ts_poultry, type = 'multiplicative')
plot(decomp_poultry)

# STL decomposition
stl_decomp_poultry <- stl(ts_poultry, s.window = "periodic")
plot(stl_decomp_poultry, main = "(STL) Decomposition of multiplicative time series")


#CPT Analysis
cpt_mean_poultry <- cpt.mean(ts_poultry)
plot(cpt_mean_poultry, main = "Change point analysis of the mean for Poultry")

cpt_var_poultry <- cpt.var(ts_poultry)
plot(cpt_var_poultry, main = "Change point analysis of the variance for Poultry")

#Earthquake
proportion<-yday(as.Date('2016-04-16'))/366
plot.ts(ts_poultry, ylab = "Sales", main = "Sales of Poultry")

abline(v=2016.2923497, col="red")


# Define a function to identify peaks and troughs
find_peaks_troughs <- function(ts_data) {
  peaks <- which(diff(sign(diff(ts_data))) == -2) + 1
  troughs <- which(diff(sign(diff(ts_data))) == 2) + 1
  list(peaks = peaks, troughs = troughs)
}

# Function to filter, create time series, plot, and identify peaks and troughs
analyze_year <- function(year, data) {
  df_year <- data %>%
    filter(year(date) == year) %>%
    arrange(date)
  
  if (nrow(df_year) > 0) {  # Ensure there is data for the year
    ts_year <- ts(df_year$TOTAL_SALES, start = c(year, 1), frequency = 365)
    
    plot(ts_year, ylab = "Sales", main = paste("Sales of Poultry in", year))
    
    peaks_troughs <- find_peaks_troughs(ts_year)
    peaks <- peaks_troughs$peaks
    troughs <- peaks_troughs$troughs
    
    points(time(ts_year)[peaks], ts_year[peaks], col = "red", pch = 19)
    points(time(ts_year)[troughs], ts_year[troughs], col = "blue", pch = 19)
    
    cpt_meanvar <- cpt.meanvar(ts_year)
    change_points <- cpts(cpt_meanvar)
    
    print(paste("Year:", year, "Change points:", change_points))  # Debugging statement
    
    if (length(change_points) > 0) {
      abline(v = time(ts_year)[change_points], col = 'purple')
    }
  } else {
    print(paste("No data available for the year:", year))
  }
}

# Analyze each year from 2013 to 2017
years <- 2013:2017
for (year in years) {
  analyze_year(year, df_poultry)
}



#aggregate the weekly data

library(tidyverse)
library(lubridate)
library(zoo)

df_poultry$date <- as.Date(df_poultry$date, format = "%Y-%m-%d")

library(dplyr)

View(df_poultry)

#weekly


# Aggregate data by week
df_poultry_weekly <- df_poultry %>%
  mutate(week = floor_date(date, "week")) %>%
  group_by(week) %>%
  summarise(TOTAL_SALES = sum(TOTAL_SALES)) %>%
  ungroup()

# Create a time series object
ts_poultry_weekly <- ts(df_poultry_weekly$TOTAL_SALES, start = c(2013, 1), frequency = 52)

# Plot the weekly sales time series
plot(ts_poultry_weekly, ylab = "Weekly Sales", main = "Weekly Sales of Poultry")



# Add a column for the day of the week
df_poultry <- df_poultry %>%
  mutate(day_of_week = weekdays(date))




# Aggregate sales by month across all years
monthly_sales <- df_poultry %>%
  group_by(month = floor_date(date, "month")) %>%
  summarise(mean_sales = mean(TOTAL_SALES),
            total_sales = sum(TOTAL_SALES),
            sd_sales = sd(TOTAL_SALES)) %>%
  ungroup()

# Plot the mean sales by month
ggplot(monthly_sales, aes(x = month, y = mean_sales)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = mean_sales - sd_sales, ymax = mean_sales + sd_sales), width = 0.2) +
  labs(title = "Average Monthly Sales of Poultry",
       x = "Month",
       y = "Average Sales") +
  theme_minimal()



#Monthly Sales

# View the structure and summary of the data
str(df_poultry)
head(df_poultry)

# Extract day from date
df_poultry <- df_poultry %>%
  mutate(day = day(date), month = month(date, label = TRUE), year = year(date))

# Summarize average sales for each day of the month across all years
average_daily_sales <- df_poultry %>%
  group_by(day) %>%
  summarise(average_sales = mean(TOTAL_SALES)) %>%
  ungroup()

# Plot the average daily sales for each day of the month
ggplot(average_daily_sales, aes(x = day, y = average_sales)) +
  geom_bar(stat = "identity", fill = "blue", width = 0.7) +
  labs(title = "Average Sales of Prepared Foods by Day of the Month",
       x = "Day of the Month",
       y = "Average Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# Aggregate sales by day of the week across all years
weekly_sales <- df_poultry %>%
  group_by(day_of_week) %>%
  summarise(mean_sales = mean(TOTAL_SALES),
            total_sales = sum(TOTAL_SALES),
            sd_sales = sd(TOTAL_SALES)) %>%
  ungroup()

# Reorder the days of the week
levels_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
weekly_sales$day_of_week <- factor(weekly_sales$day_of_week, levels = levels_order)



# Plot the mean sales by day of the week
ggplot(weekly_sales, aes(x = day_of_week, y = mean_sales)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = mean_sales - sd_sales, ymax = mean_sales + sd_sales), width = 0.2) +
  labs(title = "Average Weekly Sales of Poultry by Day of the Week",
       x = "Day of the Week",
       y = "Average Sales") +
  theme_minimal()




#Plot influence of Promotions on Poultry Sales
ggplot(df_poultry, aes(x = onpromotion, y = TOTAL_SALES, fill = onpromotion)) +
  geom_boxplot() +
  labs(title = "Influence of promotions on daily sales of Poultry",
       subtitle = "Ecuador (2013-2017)",
       x = "Promotions",
       y = "Daily sales")

#trend analysis

# Calculate a simple moving average (SMA) for a 30-day window
df_poultry <- df_poultry %>%
  mutate(SMA_30 = zoo::rollmean(TOTAL_SALES, k = 30, fill = NA))

# Plot the moving average
ggplot(df_poultry, aes(x = date)) +
  geom_line(aes(y = TOTAL_SALES), color = "blue", alpha = 0.5) +
  geom_line(aes(y = SMA_30), color = "red") +
  labs(title = "30-Day Moving Average of Total Sales",
       x = "Date",
       y = "Total Sales")



# Fit a linear trend model
linear_model <- lm(TOTAL_SALES ~ date, data = df_poultry)

# Add the trend line to the plot
ggplot(df_poultry, aes(x = date, y = TOTAL_SALES)) +
  geom_line(color = "blue", alpha = 0.5) +
  geom_abline(intercept = coef(linear_model)[1], slope = coef(linear_model)[2], color = "red") +
  labs(title = "Linear Trend in Total Sales",
       x = "Date",
       y = "Total Sales")


# Plot with Loess smoothing
ggplot(df_poultry, aes(x = date, y = TOTAL_SALES)) +
  geom_line(color = "blue", alpha = 0.5) +
  geom_smooth(method = "loess", color = "red") +
  labs(title = "Loess Smoothing of Total Sales",
       x = "Date",
       y = "Total Sales")



# Load necessary library
library(TTR)

# Calculate a 30-day Exponential Moving Average (EMA)
df_poultry <- df_poultry %>%
  mutate(EMA_30 = EMA(TOTAL_SALES, n = 30))

# Plot the EMA
ggplot(df_poultry, aes(x = date)) +
  geom_line(aes(y = TOTAL_SALES), color = "blue", alpha = 0.5) +
  geom_line(aes(y = EMA_30), color = "red") +
  labs(title = "30-Day Exponential Moving Average of Total Sales",
       x = "Date",
       y = "Total Sales")



#anomaly detection
library("anomalize")
data_anomalized_poultry <- df_poultry %>%
  time_decompose(TOTAL_SALES, method = "stl", frequency = 365, trend = "auto", merge = TRUE) %>%
  anomalize(remainder, method = "iqr", max_anoms = 0.005) %>%
  time_recompose()

data_anomalized_poultry <- data_anomalized_poultry %>%
  mutate(anomaly = ifelse(anomaly == "Yes" & TOTAL_SALES < 500, "No", anomaly))

# Plotting the decomposed and recomposed data with anomalies
data_anomalized_poultry %>%
  plot_anomalies(time_recomposed = TRUE, alpha_dots = 1) +
  labs(title = "Anomaly Detection in Poultry Sales", x = "Date", y = "Total Sales")+
  theme(plot.title = element_text(hjust = 0.5))

#replacing values

decomp_stl_poultry_additive <- stl(ts_poultry, s.window = "periodic", 
                                   t.window=365)
trend_component <- decomp_stl_poultry_additive$time.series[, "trend"]
seasonal_component <- decomp_stl_poultry_additive$time.series[, "seasonal"]
anomaly_points <- which(data_anomalized_poultry$anomaly == "Yes")
anomaly_points
# Replace anomalies with the sum of trend and seasonal components
data_anomalized_poultry$TOTAL_SALES_replaced <- data_anomalized_poultry$TOTAL_SALES

for (i in anomaly_points) {
  # Ensure the trend and seasonal components are not NA
  data_anomalized_poultry$TOTAL_SALES_replaced[i] <- trend_component[i] + seasonal_component[i]
}


ts_poultry_cleaned<-ts(data_anomalized_poultry$TOTAL_SALES_replaced, frequency = 365, start=c(2013,1)) 
autoplot(ts_poultry)


#Stationariy Tests
adf.test(ts_liquor_wine_beer_transformed)
kpss.test(ts_liquor_wine_beer_transformed)
adf.test(ts_liquor_wine_beer_diff)
kpss.test(ts_liquor_wine_beer_diff)
pp.test(ts_liquor_wine_beer_diff)
pp.test(ts_liquor_wine_beer_transformed)

ts_poultry_diff<- diff(ts_poultry_cleaned)
plot(ts_poultry_diff)
plot(ts_poultry)

#EDA For Prepared Foods----
str(df_prepared_foods)
head(df_prepared_foods)

View(df_prepared_foods)

df_prepared_foods <- df_prepared_foods %>%
  group_by(date) %>%
  summarise(TOTAL_SALES = sum(sales), TOTAL_PROMOTIONS = sum(onpromotion)) %>%
  ungroup() %>%
  mutate(onpromotion = ifelse(TOTAL_PROMOTIONS > 0, "Yes", "No") %>% 
           factor(levels = c("Yes", "No")))

ts_prepared_foods <- ts(df_prepared_foods$TOTAL_SALES, start=c(2013,1), 
                        frequency = 365)

plot(ts_prepared_foods, xlab = "Time", ylab = "Total Sales",
     main = "Sales of prepared_foods in Ecuador: 2013-2017")

#Decomposition
decomp_prepared_foods_additive <- decompose(ts_prepared_foods,type='additive')
plot(decomp_prepared_foods_additive)

stl_decomp_prepared_foods_additive <- stl(ts_prepared_foods, s.window=15, t.window=365)
plot(stl_decomp_prepared_foods_additive, main="STL Decomposition (Additive)" )

# Decompose the time series
decomp_prepared_foods <- decompose(ts_prepared_foods, type = 'multiplicative')
plot(decomp_prepared_foods)

# STL decomposition
stl_decomp_prepared_foods <- stl(ts_prepared_foods, s.window = "periodic")
plot(stl_decomp_prepared_foods, main = "(STL) Decomposition of multiplicative time series")

#CPT Analysis
cpt_mean_prepared_foods <- cpt.mean(ts_prepared_foods)
plot(cpt_mean_prepared_foods, main = "Change point analysis of the mean for prepared_foods")

cpt_var_prepared_foods <- cpt.var(ts_prepared_foods)
plot(cpt_var_prepared_foods, main = "Change point analysis of the variance for prepared_foods")

#Earthquake
proportion<-yday(as.Date('2016-04-16'))/366
plot.ts(ts_prepared_foods, ylab = "Sales", main = "Sales of prepared_foods")

abline(v=2016.2923497, col="red")


# Define a function to identify peaks and troughs
find_peaks_troughs <- function(ts_data) {
  peaks <- which(diff(sign(diff(ts_data))) == -2) + 1
  troughs <- which(diff(sign(diff(ts_data))) == 2) + 1
  list(peaks = peaks, troughs = troughs)
}

# Function to filter, create time series, plot, and identify peaks and troughs
analyze_year <- function(year, data) {
  df_year <- data %>%
    filter(year(date) == year) %>%
    arrange(date)
  
  if (nrow(df_year) > 0) {  # Ensure there is data for the year
    ts_year <- ts(df_year$TOTAL_SALES, start = c(year, 1), frequency = 365)
    
    plot(ts_year, ylab = "Sales", main = paste("Sales of prepared_foods in", year))
    
    peaks_troughs <- find_peaks_troughs(ts_year)
    peaks <- peaks_troughs$peaks
    troughs <- peaks_troughs$troughs
    
    points(time(ts_year)[peaks], ts_year[peaks], col = "red", pch = 19)
    points(time(ts_year)[troughs], ts_year[troughs], col = "blue", pch = 19)
    
    cpt_meanvar <- cpt.meanvar(ts_year)
    change_points <- cpts(cpt_meanvar)
    
    print(paste("Year:", year, "Change points:", change_points))  # Debugging statement
    
    if (length(change_points) > 0) {
      abline(v = time(ts_year)[change_points], col = 'purple')
    }
  } else {
    print(paste("No data available for the year:", year))
  }
}

# Analyze each year from 2013 to 2017
years <- 2013:2017
for (year in years) {
  analyze_year(year, df_prepared_foods)
}



#aggregate the weekly data

library(tidyverse)
library(lubridate)
library(zoo)

df_prepared_foods$date <- as.Date(df_prepared_foods$date, format = "%Y-%m-%d")

library(dplyr)

View(df_prepared_foods)

#weekly


# Aggregate data by week
df_prepared_foods_weekly <- df_prepared_foods %>%
  mutate(week = floor_date(date, "week")) %>%
  group_by(week) %>%
  summarise(TOTAL_SALES = sum(TOTAL_SALES)) %>%
  ungroup()

# Create a time series object
ts_prepared_foods_weekly <- ts(df_prepared_foods_weekly$TOTAL_SALES, start = c(2013, 1), frequency = 52)

# Plot the weekly sales time series
plot(ts_prepared_foods_weekly, ylab = "Weekly Sales", main = "Weekly Sales of prepared_foods")



# Add a column for the day of the week
df_prepared_foods <- df_prepared_foods %>%
  mutate(day_of_week = weekdays(date))


# Aggregate sales by month across all years
monthly_sales <- df_prepared_foods %>%
  group_by(month = floor_date(date, "month")) %>%
  summarise(mean_sales = mean(TOTAL_SALES),
            total_sales = sum(TOTAL_SALES),
            sd_sales = sd(TOTAL_SALES)) %>%
  ungroup()

# Plot the mean sales by month
ggplot(monthly_sales, aes(x = month, y = mean_sales)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = mean_sales - sd_sales, ymax = mean_sales + sd_sales), width = 0.2) +
  labs(title = "Average Monthly Sales of prepared_foods",
       x = "Month",
       y = "Average Sales") +
  theme_minimal()



#Monthly Sales

# View the structure and summary of the data
str(df_prepared_foods)
head(df_prepared_foods)

# Extract day from date
df_prepared_foods <- df_prepared_foods %>%
  mutate(day = day(date), month = month(date, label = TRUE), year = year(date))

# Summarize average sales for each day of the month across all years
average_daily_sales <- df_prepared_foods %>%
  group_by(day) %>%
  summarise(average_sales = mean(TOTAL_SALES)) %>%
  ungroup()

# Plot the average daily sales for each day of the month
ggplot(average_daily_sales, aes(x = day, y = average_sales)) +
  geom_bar(stat = "identity", fill = "blue", width = 0.7) +
  labs(title = "Average Sales of Prepared Foods by Day of the Month",
       x = "Day of the Month",
       y = "Average Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# Aggregate sales by day of the week across all years
weekly_sales <- df_prepared_foods %>%
  group_by(day_of_week) %>%
  summarise(mean_sales = mean(TOTAL_SALES),
            total_sales = sum(TOTAL_SALES),
            sd_sales = sd(TOTAL_SALES)) %>%
  ungroup()

# Reorder the days of the week
levels_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
weekly_sales$day_of_week <- factor(weekly_sales$day_of_week, levels = levels_order)



# Plot the mean sales by day of the week
ggplot(weekly_sales, aes(x = day_of_week, y = mean_sales)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = mean_sales - sd_sales, ymax = mean_sales + sd_sales), width = 0.2) +
  labs(title = "Average Weekly Sales of prepared_foods by Day of the Week",
       x = "Day of the Week",
       y = "Average Sales") +
  theme_minimal()




#Plot influence of Promotions on prepared_foods Sales
ggplot(df_prepared_foods, aes(x = onpromotion, y = TOTAL_SALES, fill = onpromotion)) +
  geom_boxplot() +
  labs(title = "Influence of promotions on daily sales of prepared_foods",
       subtitle = "Ecuador (2013-2017)",
       x = "Promotions",
       y = "Daily sales")

#trend analysis

# Calculate a simple moving average (SMA) for a 30-day window
df_prepared_foods <- df_prepared_foods %>%
  mutate(SMA_30 = zoo::rollmean(TOTAL_SALES, k = 30, fill = NA))

# Plot the moving average
ggplot(df_prepared_foods, aes(x = date)) +
  geom_line(aes(y = TOTAL_SALES), color = "blue", alpha = 0.5) +
  geom_line(aes(y = SMA_30), color = "red") +
  labs(title = "30-Day Moving Average of Total Sales",
       x = "Date",
       y = "Total Sales")



# Fit a linear trend model
linear_model <- lm(TOTAL_SALES ~ date, data = df_prepared_foods)

# Add the trend line to the plot
ggplot(df_prepared_foods, aes(x = date, y = TOTAL_SALES)) +
  geom_line(color = "blue", alpha = 0.5) +
  geom_abline(intercept = coef(linear_model)[1], slope = coef(linear_model)[2], color = "red") +
  labs(title = "Linear Trend in Total Sales",
       x = "Date",
       y = "Total Sales")


# Plot with Loess smoothing
ggplot(df_prepared_foods, aes(x = date, y = TOTAL_SALES)) +
  geom_line(color = "blue", alpha = 0.5) +
  geom_smooth(method = "loess", color = "red") +
  labs(title = "Loess Smoothing of Total Sales",
       x = "Date",
       y = "Total Sales")



# Load necessary library
library(TTR)

# Calculate a 30-day Exponential Moving Average (EMA)
df_prepared_foods <- df_prepared_foods %>%
  mutate(EMA_30 = EMA(TOTAL_SALES, n = 30))

# Plot the EMA
ggplot(df_prepared_foods, aes(x = date)) +
  geom_line(aes(y = TOTAL_SALES), color = "blue", alpha = 0.5) +
  geom_line(aes(y = EMA_30), color = "red") +
  labs(title = "30-Day Exponential Moving Average of Total Sales",
       x = "Date",
       y = "Total Sales")

#anomaly detection
library("anomalize")
data_anomalized_food <- df_prepared_foods %>%
  time_decompose(TOTAL_SALES, method = "stl", frequency = 365, trend = "auto", merge = TRUE) %>%
  anomalize(remainder, method = "gesd", max_anoms = 0.005) %>%
  time_recompose()

# Plotting the decomposed and recomposed data with anomalies
data_anomalized_poultry %>%
  plot_anomalies(time_recomposed = TRUE, alpha_dots = 1) +
  labs(title = "Anomaly Detection in Poultry Sales", x = "Date", y = "Total Sales")+
  theme(plot.title = element_text(hjust = 0.5))

ts_prepared_foods <- ts(df_prepared_foods$TOTAL_SALES, frequency = 365)
decomp_stl_prepared_foods_additive <- stl(ts_prepared_foods, s.window = "periodic", t.window = 365)

trend_component_prepared <- decomp_stl_prepared_foods_additive$time.series[, "trend"]
seasonal_component_prepared <- decomp_stl_prepared_foods_additive$time.series[, "seasonal"]

anomaly_points_prepared <- c(642, 1214, 1548)

data_anomalized_food$TOTAL_SALES_replaced <- data_anomalized_food$TOTAL_SALES


for (i in anomaly_points_prepared) {
  data_anomalized_food$TOTAL_SALES_replaced[i] <- trend_component_prepared[i] + seasonal_component_prepared[i]
}

ts_prepared_foods_cleaned<-ts(data_anomalized_food$TOTAL_SALES_replaced, frequency = 365, start=c(2013,1)) 
autoplot(ts_prepared_foods_cleaned)


#Sationarity Test
#Stationariy Tests
adf.test(ts_prepared_foods )
kpss.test(ts_prepared_foods )
adf.test(ts_prepared_foods)
kpss.test(ts_prepared_foods)
pp.test(ts_prepared_foods)
pp.test(ts_prepared_foods )

ts_prepared_foods_diff<- diff(ts_prepared_foods)
plot(ts_prepared_foods)
plot(ts_prepared_foods_diff)




#EDA For Liquor, Wine and Beer ----

str(df_liquor_wine_beer)
head(df_liquor_wine_beer)

View(df_liquor_wine_beer)

df_liquor_wine_beer <- df_liquor_wine_beer %>%
  group_by(date) %>%
  summarise(TOTAL_SALES = sum(sales), TOTAL_PROMOTIONS = sum(onpromotion)) %>%
  ungroup() %>%
  mutate(onpromotion = ifelse(TOTAL_PROMOTIONS > 0, "Yes", "No") %>% 
           factor(levels = c("Yes", "No")))

ts_liquor_wine_beer <- ts(df_liquor_wine_beer$TOTAL_SALES, start=c(2013,1), 
                          frequency = 365)

plot(ts_liquor_wine_beer, xlab = "Time", ylab = "Total Sales",
     main = "Sales of liquor_wine_beer in Ecuador: 2013-2017")

#Decomposition
decomp_liquor_wine_beer_additive <- decompose(ts_liquor_wine_beer,type='additive')
plot(decomp_liquor_wine_beer_additive)

stl_decomp_liquor_wine_beer_additive <- stl(ts_liquor_wine_beer, s.window=15, t.window=365)
plot(stl_decomp_liquor_wine_beer_additive, main="STL Decomposition (Additive)" )

# Decompose the time series
decomp_liquor_wine_beer <- decompose(ts_liquor_wine_beer, type = 'multiplicative')
plot(decomp_liquor_wine_beer)

# STL decomposition
stl_decomp_liquor_wine_beer <- stl(ts_liquor_wine_beer, s.window = "periodic")
plot(stl_decomp_liquor_wine_beer, main = "(STL) Decomposition of multiplicative time series")

#CPT Analysis
cpt_mean_liquor_wine_beer <- cpt.mean(ts_liquor_wine_beer)
plot(cpt_mean_liquor_wine_beer, main = "Change point analysis of the mean for liquor_wine_beer")

cpt_var_liquor_wine_beer <- cpt.var(ts_liquor_wine_beer)
plot(cpt_var_liquor_wine_beer, main = "Change point analysis of the variance for liquor_wine_beer")

#Earthquake
proportion<-yday(as.Date('2016-04-16'))/366
plot.ts(ts_liquor_wine_beer, ylab = "Sales", main = "Sales of liquor_wine_beer")

abline(v=2016.2923497, col="red")


# Define a function to identify peaks and troughs
find_peaks_troughs <- function(ts_data) {
  peaks <- which(diff(sign(diff(ts_data))) == -2) + 1
  troughs <- which(diff(sign(diff(ts_data))) == 2) + 1
  list(peaks = peaks, troughs = troughs)
}

# Function to filter, create time series, plot, and identify peaks and troughs
analyze_year <- function(year, data) {
  df_year <- data %>%
    filter(year(date) == year) %>%
    arrange(date)
  
  if (nrow(df_year) > 0) {  # Ensure there is data for the year
    ts_year <- ts(df_year$TOTAL_SALES, start = c(year, 1), frequency = 365)
    
    plot(ts_year, ylab = "Sales", main = paste("Sales of liquor_wine_beer in", year))
    
    peaks_troughs <- find_peaks_troughs(ts_year)
    peaks <- peaks_troughs$peaks
    troughs <- peaks_troughs$troughs
    
    points(time(ts_year)[peaks], ts_year[peaks], col = "red", pch = 19)
    points(time(ts_year)[troughs], ts_year[troughs], col = "blue", pch = 19)
    
    cpt_meanvar <- cpt.meanvar(ts_year)
    change_points <- cpts(cpt_meanvar)
    
    print(paste("Year:", year, "Change points:", change_points))  # Debugging statement
    
    if (length(change_points) > 0) {
      abline(v = time(ts_year)[change_points], col = 'purple')
    }
  } else {
    print(paste("No data available for the year:", year))
  }
}

# Analyze each year from 2013 to 2017
years <- 2013:2017
for (year in years) {
  analyze_year(year, df_liquor_wine_beer)
}



#aggregate the weekly data

library(tidyverse)
library(lubridate)
library(zoo)

df_liquor_wine_beer$date <- as.Date(df_liquor_wine_beer$date, format = "%Y-%m-%d")

library(dplyr)

View(df_liquor_wine_beer)

#weekly


# Aggregate data by week
df_liquor_wine_beer_weekly <- df_liquor_wine_beer %>%
  mutate(week = floor_date(date, "week")) %>%
  group_by(week) %>%
  summarise(TOTAL_SALES = sum(TOTAL_SALES)) %>%
  ungroup()

# Create a time series object
ts_liquor_wine_beer_weekly <- ts(df_liquor_wine_beer_weekly$TOTAL_SALES, start = c(2013, 1), frequency = 52)

# Plot the weekly sales time series
plot(ts_liquor_wine_beer_weekly, ylab = "Weekly Sales", main = "Weekly Sales of liquor_wine_beer")



# Add a column for the day of the week
df_liquor_wine_beer <- df_liquor_wine_beer %>%
  mutate(day_of_week = weekdays(date))


# Aggregate sales by month across all years
monthly_sales <- df_liquor_wine_beer %>%
  group_by(month = floor_date(date, "month")) %>%
  summarise(mean_sales = mean(TOTAL_SALES),
            total_sales = sum(TOTAL_SALES),
            sd_sales = sd(TOTAL_SALES)) %>%
  ungroup()

# Plot the mean sales by month
ggplot(monthly_sales, aes(x = month, y = mean_sales)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = mean_sales - sd_sales, ymax = mean_sales + sd_sales), width = 0.2) +
  labs(title = "Average Monthly Sales of liquor_wine_beer",
       x = "Month",
       y = "Average Sales") +
  theme_minimal()



#Monthly Sales

# View the structure and summary of the data
str(df_liquor_wine_beer)
head(df_liquor_wine_beer)

# Extract day from date
df_liquor_wine_beer <- df_liquor_wine_beer %>%
  mutate(day = day(date), month = month(date, label = TRUE), year = year(date))

# Summarize average sales for each day of the month across all years
average_daily_sales <- df_liquor_wine_beer %>%
  group_by(day) %>%
  summarise(average_sales = mean(TOTAL_SALES)) %>%
  ungroup()

# Plot the average daily sales for each day of the month
ggplot(average_daily_sales, aes(x = day, y = average_sales)) +
  geom_bar(stat = "identity", fill = "blue", width = 0.7) +
  labs(title = "Average Sales of Prepared Foods by Day of the Month",
       x = "Day of the Month",
       y = "Average Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# Aggregate sales by day of the week across all years
weekly_sales <- df_liquor_wine_beer %>%
  group_by(day_of_week) %>%
  summarise(mean_sales = mean(TOTAL_SALES),
            total_sales = sum(TOTAL_SALES),
            sd_sales = sd(TOTAL_SALES)) %>%
  ungroup()

# Reorder the days of the week
levels_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
weekly_sales$day_of_week <- factor(weekly_sales$day_of_week, levels = levels_order)



# Plot the mean sales by day of the week
ggplot(weekly_sales, aes(x = day_of_week, y = mean_sales)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = mean_sales - sd_sales, ymax = mean_sales + sd_sales), width = 0.2) +
  labs(title = "Average Weekly Sales of liquor_wine_beer by Day of the Week",
       x = "Day of the Week",
       y = "Average Sales") +
  theme_minimal()




#Plot influence of Promotions on liquor_wine_beer Sales
ggplot(df_liquor_wine_beer, aes(x = onpromotion, y = TOTAL_SALES, fill = onpromotion)) +
  geom_boxplot() +
  labs(title = "Influence of promotions on daily sales of liquor_wine_beer",
       subtitle = "Ecuador (2013-2017)",
       x = "Promotions",
       y = "Daily sales")

#trend analysis

# Calculate a simple moving average (SMA) for a 30-day window
df_liquor_wine_beer <- df_liquor_wine_beer %>%
  mutate(SMA_30 = zoo::rollmean(TOTAL_SALES, k = 30, fill = NA))

# Plot the moving average
ggplot(df_liquor_wine_beer, aes(x = date)) +
  geom_line(aes(y = TOTAL_SALES), color = "blue", alpha = 0.5) +
  geom_line(aes(y = SMA_30), color = "red") +
  labs(title = "30-Day Moving Average of Total Sales",
       x = "Date",
       y = "Total Sales")



# Fit a linear trend model
linear_model <- lm(TOTAL_SALES ~ date, data = df_liquor_wine_beer)

# Add the trend line to the plot
ggplot(df_liquor_wine_beer, aes(x = date, y = TOTAL_SALES)) +
  geom_line(color = "blue", alpha = 0.5) +
  geom_abline(intercept = coef(linear_model)[1], slope = coef(linear_model)[2], color = "red") +
  labs(title = "Linear Trend in Total Sales",
       x = "Date",
       y = "Total Sales")


# Plot with Loess smoothing
ggplot(df_liquor_wine_beer, aes(x = date, y = TOTAL_SALES)) +
  geom_line(color = "blue", alpha = 0.5) +
  geom_smooth(method = "loess", color = "red") +
  labs(title = "Loess Smoothing of Total Sales",
       x = "Date",
       y = "Total Sales")



# Load necessary library
library(TTR)

# Calculate a 30-day Exponential Moving Average (EMA)
df_liquor_wine_beer <- df_liquor_wine_beer %>%
  mutate(EMA_30 = EMA(TOTAL_SALES, n = 30))

# Plot the EMA
ggplot(df_liquor_wine_beer, aes(x = date)) +
  geom_line(aes(y = TOTAL_SALES), color = "blue", alpha = 0.5) +
  geom_line(aes(y = EMA_30), color = "red") +
  labs(title = "30-Day Exponential Moving Average of Total Sales",
       x = "Date",
       y = "Total Sales")

#anomaly detection
library("anomalize")
data_anomalized_lwr <- df_liquor_wine_beer %>%
  time_decompose(TOTAL_SALES, method = "stl", frequency = 365, trend = "auto", merge = TRUE) %>%
  anomalize(remainder, method = "gesd", max_anoms = 0.005) %>%
  time_recompose()


# Plotting the decomposed and recomposed data with anomalies
data_anomalized_lwr %>%
  plot_anomalies(time_recomposed = TRUE, alpha_dots = 1) +
  labs(title = "Anomaly Detection in Poultry Sales", x = "Date", y = "Total Sales")+
  theme(plot.title = element_text(hjust = 0.5))

#replacing values

decomp_stl_liquor_wine_beer_additive <- stl(ts_liquor_wine_beer, s.window = "periodic", 
                                            t.window=365)
trend_component_lwr <- decomp_stl_liquor_wine_beer_additive$time.series[, "trend"]
seasonal_component_lwr <- decomp_stl_liquor_wine_beer_additive$time.series[, "seasonal"]
anomaly_points_lwr <- which(data_anomalized_lwr$anomaly == "Yes")
anomaly_points_lwr
# Replace anomalies with the sum of trend and seasonal components
data_anomalized_lwr$TOTAL_SALES_replaced <- data_anomalized_lwr$TOTAL_SALES

for (i in anomaly_points) {
  # Ensure the trend and seasonal components are not NA
  data_anomalized_lwr$TOTAL_SALES_replaced[i] <- trend_component[i] + seasonal_component[i]
}


ts_liquor_wine_beer_cleaned<-ts(data_anomalized_lwr$TOTAL_SALES_replaced, frequency = 365, start=c(2013,1)) 
autoplot(ts_liquor_wine_beer)

#Sationarity Test
#Stationariy Tests
adf.test(ts_liquor_wine_beer )
kpss.test(ts_liquor_wine_beer )
adf.test(ts_liquor_wine_beer)
kpss.test(ts_liquor_wine_beer)
pp.test(ts_liquor_wine_beer)
pp.test(ts_liquor_wine_beer )

ts_liquor_wine_beer_diff<- diff(ts_liquor_wine_beer)
plot(ts_liquor_wine_beer)
plot(ts_liquor_wine_beer_diff)


install.packages("strucchange")
library(strucchange)

# Create or load your time series data
set.seed(123)

# Detect breakpoints in the time series
breakpoints_result <- breakpoints(ts_liquor_wine_beer ~ 1)

# Summarize the breakpoints
summary(breakpoints_result)

# Plot the time series with breakpoints
plot(time_series_data, main = "Time Series with Breakpoints")
lines(breakpoints_result, col = "red")


#EDA For Frozen Foods ----

str(df_frozen_foods)
head(df_frozen_foods)

View(df_frozen_foods)

df_frozen_foods <- df_frozen_foods %>%
  group_by(date) %>%
  summarise(TOTAL_SALES = sum(sales), TOTAL_PROMOTIONS = sum(onpromotion)) %>%
  ungroup() %>%
  mutate(onpromotion = ifelse(TOTAL_PROMOTIONS > 0, "Yes", "No") %>% 
           factor(levels = c("Yes", "No")))

ts_frozen_foods <- ts(df_frozen_foods$TOTAL_SALES, start=c(2013,1), 
                      frequency = 365)

plot(ts_frozen_foods, xlab = "Time", ylab = "Total Sales",
     main = "Sales of frozen_foods in Ecuador: 2013-2017")

#Decomposition
decomp_frozen_foods_additive <- decompose(ts_frozen_foods,type='additive')
plot(decomp_frozen_foods_additive)

stl_decomp_frozen_foods_additive <- stl(ts_frozen_foods, s.window=15, t.window=365)
plot(stl_decomp_frozen_foods_additive, main="STL Decomposition (Additive)" )

# Decompose the time series
decomp_frozen_foods <- decompose(ts_frozen_foods, type = 'multiplicative')
plot(decomp_frozen_foods)

# STL decomposition
stl_decomp_frozen_foods <- stl(ts_frozen_foods, s.window = "periodic")
plot(stl_decomp_frozen_foods, main = "(STL) Decomposition of multiplicative time series")

#CPT Analysis
cpt_mean_frozen_foods <- cpt.mean(ts_frozen_foods)
plot(cpt_mean_frozen_foods, main = "Change point analysis of the mean for frozen_foods")

cpt_var_frozen_foods <- cpt.var(ts_frozen_foods)
plot(cpt_var_frozen_foods, main = "Change point analysis of the variance for frozen_foods")

#Earthquake
proportion<-yday(as.Date('2016-04-16'))/366
plot.ts(ts_frozen_foods, ylab = "Sales", main = "Sales of frozen_foods")

abline(v=2016.2923497, col="red")


# Define a function to identify peaks and troughs
find_peaks_troughs <- function(ts_data) {
  peaks <- which(diff(sign(diff(ts_data))) == -2) + 1
  troughs <- which(diff(sign(diff(ts_data))) == 2) + 1
  list(peaks = peaks, troughs = troughs)
}

# Function to filter, create time series, plot, and identify peaks and troughs
analyze_year <- function(year, data) {
  df_year <- data %>%
    filter(year(date) == year) %>%
    arrange(date)
  
  if (nrow(df_year) > 0) {  # Ensure there is data for the year
    ts_year <- ts(df_year$TOTAL_SALES, start = c(year, 1), frequency = 365)
    
    plot(ts_year, ylab = "Sales", main = paste("Sales of frozen_foods in", year))
    
    peaks_troughs <- find_peaks_troughs(ts_year)
    peaks <- peaks_troughs$peaks
    troughs <- peaks_troughs$troughs
    
    points(time(ts_year)[peaks], ts_year[peaks], col = "red", pch = 19)
    points(time(ts_year)[troughs], ts_year[troughs], col = "blue", pch = 19)
    
    cpt_meanvar <- cpt.meanvar(ts_year)
    change_points <- cpts(cpt_meanvar)
    
    print(paste("Year:", year, "Change points:", change_points))  # Debugging statement
    
    if (length(change_points) > 0) {
      abline(v = time(ts_year)[change_points], col = 'purple')
    }
  } else {
    print(paste("No data available for the year:", year))
  }
}

# Analyze each year from 2013 to 2017
years <- 2013:2017
for (year in years) {
  analyze_year(year, df_frozen_foods)
}



#aggregate the weekly data

library(tidyverse)
library(lubridate)
library(zoo)

df_frozen_foods$date <- as.Date(df_frozen_foods$date, format = "%Y-%m-%d")

library(dplyr)

View(df_frozen_foods)

#weekly


# Aggregate data by week
df_frozen_foods_weekly <- df_frozen_foods %>%
  mutate(week = floor_date(date, "week")) %>%
  group_by(week) %>%
  summarise(TOTAL_SALES = sum(TOTAL_SALES)) %>%
  ungroup()

# Create a time series object
ts_frozen_foods_weekly <- ts(df_frozen_foods_weekly$TOTAL_SALES, start = c(2013, 1), frequency = 52)

# Plot the weekly sales time series
plot(ts_frozen_foods_weekly, ylab = "Weekly Sales", main = "Weekly Sales of frozen_foods")



# Add a column for the day of the week
df_frozen_foods <- df_frozen_foods %>%
  mutate(day_of_week = weekdays(date))


# Aggregate sales by month across all years
monthly_sales <- df_frozen_foods %>%
  group_by(month = floor_date(date, "month")) %>%
  summarise(mean_sales = mean(TOTAL_SALES),
            total_sales = sum(TOTAL_SALES),
            sd_sales = sd(TOTAL_SALES)) %>%
  ungroup()

# Plot the mean sales by month
ggplot(monthly_sales, aes(x = month, y = mean_sales)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = mean_sales - sd_sales, ymax = mean_sales + sd_sales), width = 0.2) +
  labs(title = "Average Monthly Sales of frozen_foods",
       x = "Month",
       y = "Average Sales") +
  theme_minimal()



#Monthly Sales

# View the structure and summary of the data
str(df_frozen_foods)
head(df_frozen_foods)

# Extract day from date
df_frozen_foods <- df_frozen_foods %>%
  mutate(day = day(date), month = month(date, label = TRUE), year = year(date))

# Summarize average sales for each day of the month across all years
average_daily_sales <- df_frozen_foods %>%
  group_by(day) %>%
  summarise(average_sales = mean(TOTAL_SALES)) %>%
  ungroup()

# Plot the average daily sales for each day of the month
ggplot(average_daily_sales, aes(x = day, y = average_sales)) +
  geom_bar(stat = "identity", fill = "blue", width = 0.7) +
  labs(title = "Average Sales of Prepared Foods by Day of the Month",
       x = "Day of the Month",
       y = "Average Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# Aggregate sales by day of the week across all years
weekly_sales <- df_frozen_foods %>%
  group_by(day_of_week) %>%
  summarise(mean_sales = mean(TOTAL_SALES),
            total_sales = sum(TOTAL_SALES),
            sd_sales = sd(TOTAL_SALES)) %>%
  ungroup()

# Reorder the days of the week
levels_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
weekly_sales$day_of_week <- factor(weekly_sales$day_of_week, levels = levels_order)



# Plot the mean sales by day of the week
ggplot(weekly_sales, aes(x = day_of_week, y = mean_sales)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = mean_sales - sd_sales, ymax = mean_sales + sd_sales), width = 0.2) +
  labs(title = "Average Weekly Sales of frozen_foods by Day of the Week",
       x = "Day of the Week",
       y = "Average Sales") +
  theme_minimal()




#Plot influence of Promotions on frozen_foods Sales
ggplot(df_frozen_foods, aes(x = onpromotion, y = TOTAL_SALES, fill = onpromotion)) +
  geom_boxplot() +
  labs(title = "Influence of promotions on daily sales of frozen_foods",
       subtitle = "Ecuador (2013-2017)",
       x = "Promotions",
       y = "Daily sales")

#trend analysis

# Calculate a simple moving average (SMA) for a 30-day window
df_frozen_foods <- df_frozen_foods %>%
  mutate(SMA_30 = zoo::rollmean(TOTAL_SALES, k = 30, fill = NA))

# Plot the moving average
ggplot(df_frozen_foods, aes(x = date)) +
  geom_line(aes(y = TOTAL_SALES), color = "blue", alpha = 0.5) +
  geom_line(aes(y = SMA_30), color = "red") +
  labs(title = "30-Day Moving Average of Total Sales",
       x = "Date",
       y = "Total Sales")



# Fit a linear trend model
linear_model <- lm(TOTAL_SALES ~ date, data = df_frozen_foods)

# Add the trend line to the plot
ggplot(df_frozen_foods, aes(x = date, y = TOTAL_SALES)) +
  geom_line(color = "blue", alpha = 0.5) +
  geom_abline(intercept = coef(linear_model)[1], slope = coef(linear_model)[2], color = "red") +
  labs(title = "Linear Trend in Total Sales",
       x = "Date",
       y = "Total Sales")


# Plot with Loess smoothing
ggplot(df_frozen_foods, aes(x = date, y = TOTAL_SALES)) +
  geom_line(color = "blue", alpha = 0.5) +
  geom_smooth(method = "loess", color = "red") +
  labs(title = "Loess Smoothing of Total Sales",
       x = "Date",
       y = "Total Sales")



# Load necessary library
library(TTR)

# Calculate a 30-day Exponential Moving Average (EMA)
df_frozen_foods <- df_frozen_foods %>%
  mutate(EMA_30 = EMA(TOTAL_SALES, n = 30))

# Plot the EMA
ggplot(df_frozen_foods, aes(x = date)) +
  geom_line(aes(y = TOTAL_SALES), color = "blue", alpha = 0.5) +
  geom_line(aes(y = EMA_30), color = "red") +
  labs(title = "30-Day Exponential Moving Average of Total Sales",
       x = "Date",
       y = "Total Sales")

#anomaly detection
library("anomalize")
data_anomalized_frozen_foods <- df_frozen_foods %>%
  time_decompose(TOTAL_SALES, method = "stl", frequency = 365, trend = "auto", merge = TRUE) %>%
  anomalize(remainder, method = "gesd", max_anoms = 0.005) %>%
  time_recompose()

log10_ts_frozen_foods <- log10(ts_frozen_foods)
decomp_stl_log10_frozen_foods <- stl(log10_ts_frozen_foods, s.window = "periodic", t.window = 365)
log10_trend_component_frozen_foods <- decomp_stl_log10_frozen_foods$time.series[, "trend"]
log10_seasonal_component_frozen_foods <- decomp_stl_log10_frozen_foods$time.series[, "seasonal"]
anomaly_points_frozen_foods <- which(data_anomalized_frozen_foods$anomaly == "Yes")
data_anomalized_frozen_foods$TOTAL_SALES_replaced <- data_anomalized_frozen_foods$TOTAL_SALES

for (i in anomaly_points_frozen_foods) {
  log10_trend_season_sum_frozen_foods <- log10_trend_component_frozen_foods[i] + 
    log10_seasonal_component_frozen_foods[i]
  data_anomalized_frozen_foods$TOTAL_SALES_replaced[i] <- 10^log10_trend_season_sum_frozen_foods
}

ts_frozen_foods_cleaned<-ts(data_anomalized_frozen_foods$TOTAL_SALES_replaced, 
                            frequency = 365, start=c(2013,1)) 


#Sationarity Test
#Stationariy Tests
adf.test(ts_frozen_foods )
kpss.test(ts_frozen_foods )
adf.test(ts_frozen_foods)
kpss.test(ts_frozen_foods)
pp.test(ts_frozen_foods)
pp.test(ts_frozen_foods )

ts_frozen_foods_diff<- diff(ts_frozen_foods)
plot(ts_frozen_foods)
plot(ts_frozen_foods_diff)


install.packages("strucchange")
library(strucchange)

# Create or load your time series data
set.seed(123)

# Detect breakpoints in the time series
breakpoints_result <- breakpoints(ts_frozen_foods ~ 1)

# Summarize the breakpoints
summary(breakpoints_result)

# Plot the time series with breakpoints
plot(time_series_data, main = "Time Series with Breakpoints")
lines(breakpoints_result, col = "red")




#EDA For meats ----

str(df_meats)
head(df_meats)

View(df_meats)

df_meats <- df_meats %>%
  group_by(date) %>%
  summarise(TOTAL_SALES = sum(sales), TOTAL_PROMOTIONS = sum(onpromotion)) %>%
  ungroup() %>%
  mutate(onpromotion = ifelse(TOTAL_PROMOTIONS > 0, "Yes", "No") %>% 
           factor(levels = c("Yes", "No")))

ts_meats <- ts(df_meats$TOTAL_SALES, start=c(2013,1), 
               frequency = 365)

plot(ts_meats, xlab = "Time", ylab = "Total Sales",
     main = "Sales of meats in Ecuador: 2013-2017")

#Decomposition
decomp_meats_additive <- decompose(ts_meats,type='additive')
plot(decomp_meats_additive)

stl_decomp_meats_additive <- stl(ts_meats, s.window=15, t.window=365)
plot(stl_decomp_meats_additive, main="STL Decomposition (Additive)" )

# Decompose the time series
decomp_meats <- decompose(ts_meats, type = 'multiplicative')
plot(decomp_meats)

# STL decomposition
stl_decomp_meats <- stl(ts_meats, s.window = "periodic")
plot(stl_decomp_meats, main = "(STL) Decomposition of multiplicative time series")

#CPT Analysis
cpt_mean_meats <- cpt.mean(ts_meats)
plot(cpt_mean_meats, main = "Change point analysis of the mean for meats")

cpt_var_meats <- cpt.var(ts_meats)
plot(cpt_var_meats, main = "Change point analysis of the variance for meats")

#Earthquake
proportion<-yday(as.Date('2016-04-16'))/366
plot.ts(ts_meats, ylab = "Sales", main = "Sales of meats")

abline(v=2016.2923497, col="red")


# Define a function to identify peaks and troughs
find_peaks_troughs <- function(ts_data) {
  peaks <- which(diff(sign(diff(ts_data))) == -2) + 1
  troughs <- which(diff(sign(diff(ts_data))) == 2) + 1
  list(peaks = peaks, troughs = troughs)
}

# Function to filter, create time series, plot, and identify peaks and troughs
analyze_year <- function(year, data) {
  df_year <- data %>%
    filter(year(date) == year) %>%
    arrange(date)
  
  if (nrow(df_year) > 0) {  # Ensure there is data for the year
    ts_year <- ts(df_year$TOTAL_SALES, start = c(year, 1), frequency = 365)
    
    plot(ts_year, ylab = "Sales", main = paste("Sales of meats in", year))
    
    peaks_troughs <- find_peaks_troughs(ts_year)
    peaks <- peaks_troughs$peaks
    troughs <- peaks_troughs$troughs
    
    points(time(ts_year)[peaks], ts_year[peaks], col = "red", pch = 19)
    points(time(ts_year)[troughs], ts_year[troughs], col = "blue", pch = 19)
    
    cpt_meanvar <- cpt.meanvar(ts_year)
    change_points <- cpts(cpt_meanvar)
    
    print(paste("Year:", year, "Change points:", change_points))  # Debugging statement
    
    if (length(change_points) > 0) {
      abline(v = time(ts_year)[change_points], col = 'purple')
    }
  } else {
    print(paste("No data available for the year:", year))
  }
}

# Analyze each year from 2013 to 2017
years <- 2013:2017
for (year in years) {
  analyze_year(year, df_meats)
}



#aggregate the weekly data

library(tidyverse)
library(lubridate)
library(zoo)

df_meats$date <- as.Date(df_meats$date, format = "%Y-%m-%d")

library(dplyr)

View(df_meats)

#weekly


# Aggregate data by week
df_meats_weekly <- df_meats %>%
  mutate(week = floor_date(date, "week")) %>%
  group_by(week) %>%
  summarise(TOTAL_SALES = sum(TOTAL_SALES)) %>%
  ungroup()

# Create a time series object
ts_meats_weekly <- ts(df_meats_weekly$TOTAL_SALES, start = c(2013, 1), frequency = 52)

# Plot the weekly sales time series
plot(ts_meats_weekly, ylab = "Weekly Sales", main = "Weekly Sales of meats")



# Add a column for the day of the week
df_meats <- df_meats %>%
  mutate(day_of_week = weekdays(date))


# Aggregate sales by month across all years
monthly_sales <- df_meats %>%
  group_by(month = floor_date(date, "month")) %>%
  summarise(mean_sales = mean(TOTAL_SALES),
            total_sales = sum(TOTAL_SALES),
            sd_sales = sd(TOTAL_SALES)) %>%
  ungroup()

# Plot the mean sales by month
ggplot(monthly_sales, aes(x = month, y = mean_sales)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = mean_sales - sd_sales, ymax = mean_sales + sd_sales), width = 0.2) +
  labs(title = "Average Monthly Sales of meats",
       x = "Month",
       y = "Average Sales") +
  theme_minimal()



#Monthly Sales

# View the structure and summary of the data
str(df_meats)
head(df_meats)

# Extract day from date
df_meats <- df_meats %>%
  mutate(day = day(date), month = month(date, label = TRUE), year = year(date))

# Summarize average sales for each day of the month across all years
average_daily_sales <- df_meats %>%
  group_by(day) %>%
  summarise(average_sales = mean(TOTAL_SALES)) %>%
  ungroup()

# Plot the average daily sales for each day of the month
ggplot(average_daily_sales, aes(x = day, y = average_sales)) +
  geom_bar(stat = "identity", fill = "blue", width = 0.7) +
  labs(title = "Average Sales of Prepared Foods by Day of the Month",
       x = "Day of the Month",
       y = "Average Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# Aggregate sales by day of the week across all years
weekly_sales <- df_meats %>%
  group_by(day_of_week) %>%
  summarise(mean_sales = mean(TOTAL_SALES),
            total_sales = sum(TOTAL_SALES),
            sd_sales = sd(TOTAL_SALES)) %>%
  ungroup()

# Reorder the days of the week
levels_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
weekly_sales$day_of_week <- factor(weekly_sales$day_of_week, levels = levels_order)



# Plot the mean sales by day of the week
ggplot(weekly_sales, aes(x = day_of_week, y = mean_sales)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = mean_sales - sd_sales, ymax = mean_sales + sd_sales), width = 0.2) +
  labs(title = "Average Weekly Sales of meats by Day of the Week",
       x = "Day of the Week",
       y = "Average Sales") +
  theme_minimal()




#Plot influence of Promotions on meats Sales
ggplot(df_meats, aes(x = onpromotion, y = TOTAL_SALES, fill = onpromotion)) +
  geom_boxplot() +
  labs(title = "Influence of promotions on daily sales of meats",
       subtitle = "Ecuador (2013-2017)",
       x = "Promotions",
       y = "Daily sales")

#trend analysis

# Calculate a simple moving average (SMA) for a 30-day window
df_meats <- df_meats %>%
  mutate(SMA_30 = zoo::rollmean(TOTAL_SALES, k = 30, fill = NA))

# Plot the moving average
ggplot(df_meats, aes(x = date)) +
  geom_line(aes(y = TOTAL_SALES), color = "blue", alpha = 0.5) +
  geom_line(aes(y = SMA_30), color = "red") +
  labs(title = "30-Day Moving Average of Total Sales",
       x = "Date",
       y = "Total Sales")



# Fit a linear trend model
linear_model <- lm(TOTAL_SALES ~ date, data = df_meats)

# Add the trend line to the plot
ggplot(df_meats, aes(x = date, y = TOTAL_SALES)) +
  geom_line(color = "blue", alpha = 0.5) +
  geom_abline(intercept = coef(linear_model)[1], slope = coef(linear_model)[2], color = "red") +
  labs(title = "Linear Trend in Total Sales",
       x = "Date",
       y = "Total Sales")


# Plot with Loess smoothing
ggplot(df_meats, aes(x = date, y = TOTAL_SALES)) +
  geom_line(color = "blue", alpha = 0.5) +
  geom_smooth(method = "loess", color = "red") +
  labs(title = "Loess Smoothing of Total Sales",
       x = "Date",
       y = "Total Sales")



# Load necessary library
library(TTR)

# Calculate a 30-day Exponential Moving Average (EMA)
df_meats <- df_meats %>%
  mutate(EMA_30 = EMA(TOTAL_SALES, n = 30))

# Plot the EMA
ggplot(df_meats, aes(x = date)) +
  geom_line(aes(y = TOTAL_SALES), color = "blue", alpha = 0.5) +
  geom_line(aes(y = EMA_30), color = "red") +
  labs(title = "30-Day Exponential Moving Average of Total Sales",
       x = "Date",
       y = "Total Sales")

#anomaly detection
library("anomalize")
data_anomalized_meats <- df_meats %>%
  time_decompose(TOTAL_SALES, method = "stl", frequency = 365, trend = "auto", merge = TRUE) %>%
  anomalize(remainder, method = "gesd", max_anoms = 0.005) %>%
  time_recompose()

data_anomalized_meats %>%
  plot_anomalies(time_recomposed = TRUE, alpha_dots = 1) +
  labs(title = "Anomaly Detection in Meat Sales", x = "Date", y = "Total Sales") +
  theme(plot.title = element_text(hjust = 0.5))

decomp_stl_meats_additive <- stl(ts_meats, s.window = "periodic", t.window = 365)
trend_component_meats <- decomp_stl_meats_additive$time.series[, "trend"]
seasonal_component_meats <- decomp_stl_meats_additive$time.series[, "seasonal"]
anomaly_points_meat <- which(data_anomalized_meats$anomaly == "Yes")

anomaly_points_meat <- c(278,363,643,1008,1373)

# Replace anomalies with the sum of trend and seasonal components
data_anomalized_meats$TOTAL_SALES_replaced <- data_anomalized_meats$TOTAL_SALES

for (i in anomaly_points_meat) {
  data_anomalized_meats$TOTAL_SALES_replaced[i] <- trend_component_meats[i] + 
    seasonal_component_meats[i]
}

ts_meats_cleaned <- ts(data_anomalized_meats$TOTAL_SALES_replaced, frequency = 365, start = c(2013, 1))
autoplot(ts_meats_cleaned)



#Sationarity Test
#Stationariy Tests
adf.test(ts_meats )
kpss.test(ts_meats )
adf.test(ts_meats)
kpss.test(ts_meats)
pp.test(ts_meats)
pp.test(ts_meats )

ts_meats_diff<- diff(ts_meats)
plot(ts_meats)
plot(ts_meats_diff)


install.packages("strucchange")
library(strucchange)

# Create or load your time series data
set.seed(123)

# Detect breakpoints in the time series
breakpoints_result <- breakpoints(ts_meats ~ 1)

# Summarize the breakpoints
summary(breakpoints_result)

# Plot the time series with breakpoints
plot(time_series_data, main = "Time Series with Breakpoints")
lines(breakpoints_result, col = "red")







# Print the p-values matrix
print(p_values)

# Convert the matrix to a data frame for ggplot
p_values_melted <- melt(p_values, na.rm = TRUE)

# Plot the heatmap
ggplot(data = p_values_melted, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "red", high = "green", na.value = "grey50") +
  labs(title = "Granger Causality Test p-values Heatmap",
       x = "Cause",
       y = "Effect",
       fill = "p-value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


DView(combined_ts)

#var----
# Re-evaluate lag order selection using VARselect

columns <- c("ts_frozen_foods_robust", "ts_liquor_wine_beer_robust", "ts_meats_robust", "ts_poultry_robust", "ts_prepared_foods_robust")

for (col1 in columns) {
  for (col2 in columns) {
    if (col1 != col2) {
      formula1 <- as.formula(paste(col1, "~", col2))
      formula2 <- as.formula(paste(col2, "~", col1))
      test_result1 <- grangertest(formula1, order = 31, data = combined_ts)
      test_result2 <- grangertest(formula2, order = 31, data = combined_ts)
      print(paste("Granger causality test:", col1, "->", col2))
      print(test_result1)
      print(paste("Granger causality test:", col2, "->", col1))
      print(test_result2)
    }
  }
}

#Preparing data ----

#handling anomalies
#frozen foods
data_anomalized_frozen_foods <- df_frozen_foods %>%
  time_decompose(TOTAL_SALES, method = "stl", frequency = 365, trend = "auto", merge = TRUE) %>%
  anomalize(remainder, method = "gesd", max_anoms = 0.005) %>%
  time_recompose()

log10_ts_frozen_foods <- log10(ts_frozen_foods)
decomp_stl_log10_frozen_foods <- stl(log10_ts_frozen_foods, s.window = "periodic", t.window = 365)
log10_trend_component_frozen_foods <- decomp_stl_log10_frozen_foods$time.series[, "trend"]
log10_seasonal_component_frozen_foods <- decomp_stl_log10_frozen_foods$time.series[, "seasonal"]
anomaly_points_frozen_foods <- which(data_anomalized_frozen_foods$anomaly == "Yes")
data_anomalized_frozen_foods$TOTAL_SALES_replaced <- data_anomalized_frozen_foods$TOTAL_SALES

for (i in anomaly_points_frozen_foods) {
  log10_trend_season_sum_frozen_foods <- log10_trend_component_frozen_foods[i] + 
    log10_seasonal_component_frozen_foods[i]
  data_anomalized_frozen_foods$TOTAL_SALES_replaced[i] <- 10^log10_trend_season_sum_frozen_foods
}

ts_frozen_foods_cleaned<-ts(data_anomalized_frozen_foods$TOTAL_SALES_replaced, 
                            frequency = 365, start=c(2013,1)) 

#liquor wine beer
data_anomalized_lwr <- df_liquor_wine_beer %>%
  time_decompose(TOTAL_SALES, method = "stl", frequency = 365, trend = "auto", merge = TRUE) %>%
  anomalize(remainder, method = "gesd", max_anoms = 0.005) %>%
  time_recompose()

decomp_stl_liquor_wine_beer_additive <- stl(ts_liquor_wine_beer, s.window = "periodic", 
                                            t.window=365)
trend_component_lwr <- decomp_stl_liquor_wine_beer_additive$time.series[, "trend"]
seasonal_component_lwr <- decomp_stl_liquor_wine_beer_additive$time.series[, "seasonal"]
anomaly_points_lwr <- which(data_anomalized_lwr$anomaly == "Yes")
anomaly_points_lwr

data_anomalized_lwr$TOTAL_SALES_replaced <- data_anomalized_lwr$TOTAL_SALES

for (i in anomaly_points_lwr) {
  data_anomalized_lwr$TOTAL_SALES_replaced[i] <- trend_component_lwr[i] + seasonal_component_lwr[i]
}

ts_liquor_wine_beer_cleaned<-ts(data_anomalized_lwr$TOTAL_SALES_replaced, frequency = 365, start=c(2013,1)) 

#meats

data_anomalized_meats <- df_meats %>%
  time_decompose(TOTAL_SALES, method = "stl", frequency = 365, trend = "auto", merge = TRUE) %>%
  anomalize(remainder, method = "gesd", max_anoms = 0.005) %>%
  time_recompose()

data_anomalized_meats %>%
  plot_anomalies(time_recomposed = TRUE, alpha_dots = 1) +
  labs(title = "Anomaly Detection in Meat Sales", x = "Date", y = "Total Sales") +
  theme(plot.title = element_text(hjust = 0.5))

decomp_stl_meats_additive <- stl(ts_meats, s.window = "periodic", t.window = 365)
trend_component_meats <- decomp_stl_meats_additive$time.series[, "trend"]
seasonal_component_meats <- decomp_stl_meats_additive$time.series[, "seasonal"]
anomaly_points_meat <- which(data_anomalized_meats$anomaly == "Yes")

anomaly_points_meat <- c(278,363,643,1008,1373)

# Replace anomalies with the sum of trend and seasonal components
data_anomalized_meats$TOTAL_SALES_replaced <- data_anomalized_meats$TOTAL_SALES

for (i in anomaly_points_meat) {
  data_anomalized_meats$TOTAL_SALES_replaced[i] <- trend_component_meats[i] + 
    seasonal_component_meats[i]
}

ts_meats_cleaned <- ts(data_anomalized_meats$TOTAL_SALES_replaced, frequency = 365, start = c(2013, 1))
autoplot(ts_meats_cleaned)


#poultry
data_anomalized_poultry <- df_poultry %>%
  time_decompose(TOTAL_SALES, method = "stl", frequency = 365, trend = "auto", merge = TRUE) %>%
  anomalize(remainder, method = "iqr", max_anoms = 0.005) %>%
  time_recompose()

data_anomalized_poultry <- data_anomalized_poultry %>%
  mutate(anomaly = ifelse(anomaly == "Yes" & TOTAL_SALES < 500, "No", anomaly))


decomp_stl_poultry_additive <- stl(ts_poultry, s.window = "periodic", 
                                   t.window=365)
trend_component_poultry <- decomp_stl_poultry_additive$time.series[, "trend"]
seasonal_component_poultry <- decomp_stl_poultry_additive$time.series[, "seasonal"]
anomaly_points_poultry <- which(data_anomalized_poultry$anomaly == "Yes")

data_anomalized_poultry$TOTAL_SALES_replaced <- data_anomalized_poultry$TOTAL_SALES

for (i in anomaly_points_poultry) {
  data_anomalized_poultry$TOTAL_SALES_replaced[i] <- trend_component_poultry[i] + 
    seasonal_component_poultry[i]
}

ts_poultry_cleaned<-ts(data_anomalized_poultry$TOTAL_SALES_replaced, frequency = 365, start=c(2013,1)) 
autoplot(ts_poultry_cleaned)

#prepared foods
data_anomalized_food <- df_prepared_foods %>%
  time_decompose(TOTAL_SALES, method = "stl", frequency = 365, trend = "auto", merge = TRUE) %>%
  anomalize(remainder, method = "gesd", max_anoms = 0.005) %>%
  time_recompose()

ts_prepared_foods <- ts(df_prepared_foods$TOTAL_SALES, frequency = 365)
decomp_stl_prepared_foods_additive <- stl(ts_prepared_foods, s.window = "periodic", t.window = 365)

trend_component_prepared <- decomp_stl_prepared_foods_additive$time.series[, "trend"]
seasonal_component_prepared <- decomp_stl_prepared_foods_additive$time.series[, "seasonal"]

anomaly_points_prepared <- c(642, 1214, 1548)

data_anomalized_food$TOTAL_SALES_replaced <- data_anomalized_food$TOTAL_SALES

for (i in anomaly_points_prepared) {
  data_anomalized_food$TOTAL_SALES_replaced[i] <- trend_component_prepared[i] + seasonal_component_prepared[i]
}

ts_prepared_foods_cleaned<-ts(data_anomalized_food$TOTAL_SALES_replaced, frequency = 365, start=c(2013,1)) 
autoplot(ts_prepared_foods_cleaned)


#creating a multivariate time series
multivariate_ts <- cbind(
  ts_frozen_foods_cleaned,
  ts_liquor_wine_beer_cleaned,
  ts_meats_cleaned,
  ts_poultry_cleaned,
  ts_prepared_foods_cleaned
)


#VAR-1 Differencing ----
plot(multivariate_ts)

ndiffs<-ndiffs(multivariate_ts)
multivariate_ts_diffed <- diff(multivariate_ts, differences= ndiffs)
plot(multivariate_ts_diffed)


#selecting the best model using the aic, bic, hqic criterion criterion, 

lag_selection <- VARselect(multivariate_ts_diffed, lag.max = 30, type = "const")
lag_selection$selection
results <- data.frame(lag_order = integer(), p_value = numeric(), AIC = numeric(), BIC = numeric())

for (lag in 1:30) {
  fit_var <- VAR(multivariate_ts_diffed, p = lag, type = "const")
  Serial_test <- serial.test(fit_var, lags.pt = lag+3, type = "PT.asymptotic")
  p_value <- Serial_test$serial$p.value
  aic_value <- AIC(fit_var)
  bic_value <- BIC(fit_var)
  results <- rbind(results, data.frame(lag_order = lag, p_value = p_value, AIC = aic_value, BIC = bic_value))
}
results
fit_var <- VAR(multivariate_ts_diffed, p = 15, type = "const")

#AIC AND BIC
AIC(fit_var)
BIC(fit_var)

#serial test should be greater than 0.05
Serial1 <- serial.test(fit_var, lags.pt = 18, type = "PT.asymptotic")
Serial1

#heteroscedasticity
arch1 <- arch.test(fit_var, lags.multi = 18, multivariate.only = TRUE)
arch1

#normality test
Norm1 <- normality.test(fit_var, multivariate.only = TRUE)
Norm1

#stability test
Stability1 <- stability(fit_var, type = "OLS-CUSUM")
plot(Stability1)

# Forecast the VAR model---
var_forecast <- predict(fit_var, n.ahead = 14)



#extracting fitted values - from the 16th values 
fitted_vales <- fitted(fit_var)
ts_frozen_foods_fitted<- fitted_vales[,1]
fitted_vales
#extracting predictions
frozen_foods_forecast<- var_forecast$fcst$ts_frozen_foods_cleaned
liquor_wine_beer_forecast <- var_forecast$fcst$ts_liquor_wine_beer_cleaned
meats_forecast <- var_forecast$fcst$ts_meats_cleaned
poultry_forecast <- var_forecast$fcst$ts_poultry_cleaned
prepared_foods_forecast <- var_forecast$fcst$ts_prepared_foods_cleaned

#extracting point forecasts

frozen_foods_point_forecast <- frozen_foods_forecast[, "fcst"]
liquor_wine_beer_point_forecast <- liquor_wine_beer_forecast[, "fcst"]
meats_point_forecast <- meats_forecast[, "fcst"]
poultry_point_forecast <- poultry_forecast[, "fcst"]
prepared_foods_point_forecast <- prepared_foods_forecast[, "fcst"]

#converting back to original scale

#undifferencing

last_value_frozen_foods_ts <- tail(ts_frozen_foods_cleaned, 1)
last_value_liquor_wine_beer_ts <- tail(ts_liquor_wine_beer_cleaned, 1)
last_value_meats_ts <- tail(ts_meats_cleaned, 1)
last_value_poultry_ts <- tail(ts_poultry_cleaned, 1)
last_value_prepared_foods_ts <- tail(ts_prepared_foods_cleaned, 1)

undifferenced_frozen_foods_point_forecast <- cumsum(c(last_value_frozen_foods_ts, 
                                                      frozen_foods_point_forecast))
undifferenced_liquor_wine_beer_point_forecast <- cumsum(c(last_value_liquor_wine_beer_ts, liquor_wine_beer_point_forecast))
undifferenced_meats_point_forecast <- cumsum(c(last_value_meats_ts, meats_point_forecast))
undifferenced_poultry_point_forecast <- cumsum(c(last_value_poultry_ts, poultry_point_forecast))
undifferenced_prepared_foods_point_forecast <- cumsum(c(last_value_prepared_foods_ts, prepared_foods_point_forecast))



#Plotting Forecasts
library(vars)
library(tseries)
library(forecast)
library(lubridate)

undifferenced_frozen_foods_point_forecast

# Functions for normalization and inversion
# Extract forecasted values and unnormalize them
# Plot the original time series and add forecasts
#Frozen Foods
date_seq <- seq.Date(from = as.Date("2017-08-16"), by = "day", length.out = 15)
df_forecast_frozen_foods <- data.frame(
  date = date_seq,
  TOTAL_SALES = undifferenced_frozen_foods_point_forecast,
  TOTAL_PROMOTIONS = NA,
  onpromotion = "NO",
  isin = "my_forecast"
)

upper<-undifferenced_frozen_foods_point_forecast+2*sd(undifferenced_frozen_foods_point_forecast)
lower<-undifferenced_frozen_foods_point_forecast-2*sd(undifferenced_frozen_foods_point_forecast)

df_upper<- data.frame(
  date = date_seq,
  TOTAL_SALES = upper,
  TOTAL_PROMOTIONS = NA,
  onpromotion = "NO",
  isin = "upper bound"
)

df_lower<- data.frame(
  date = date_seq,
  TOTAL_SALES = lower,
  TOTAL_PROMOTIONS = NA,
  onpromotion = "NO",
  isin = "lower bound"
)

view(df_frozen_foods)

df_frozen_foods <- df_frozen_foods %>% mutate(isin = "original_data")
df_comb_forecast_ff <- rbind(tail(df_frozen_foods, 200), df_forecast_frozen_foods, df_upper,df_lower)

ggplot(df_comb_forecast_ff, aes(x = date , y = TOTAL_SALES, color = isin)) + geom_line() + ggtitle(" Time Series Of Frozen Foods")

#Prepared Foods
date_seq <- seq.Date(from = as.Date("2017-08-16"), by = "day", length.out = 15)
df_forecast_prepared_foods <- data.frame(
  date = date_seq,
  TOTAL_SALES = undifferenced_prepared_foods_point_forecast,
  TOTAL_PROMOTIONS = NA,
  onpromotion = "NO",
  isin = "my_forecast"
)


upper<-undifferenced_prepared_foods_point_forecast+2*sd(undifferenced_prepared_foods_point_forecast)
lower<-undifferenced_prepared_foods_point_forecast-2*sd(undifferenced_prepared_foods_point_forecast)

df_upper<- data.frame(
  date = date_seq,
  TOTAL_SALES = upper,
  TOTAL_PROMOTIONS = NA,
  onpromotion = "NO",
  isin = "upper bound"
)

df_lower<- data.frame(
  date = date_seq,
  TOTAL_SALES = lower,
  TOTAL_PROMOTIONS = NA,
  onpromotion = "NO",
  isin = "lower bound"
)


df_prepared_foods <- df_prepared_foods %>% mutate(isin = "original_data")
df_comb_forecast_pf <- rbind(tail(df_prepared_foods, 200), df_forecast_prepared_foods, df_upper, df_lower)

ggplot(df_comb_forecast_pf, aes(x = date , y = TOTAL_SALES, color = isin)) + geom_line() + ggtitle(" Time Series Of Prepared Foods")

#Liquor, Wine, Beer
date_seq <- seq.Date(from = as.Date("2017-08-16"), by = "day", length.out = 15)
df_forecast_liquor_wine_beer <- data.frame(
  date = date_seq,
  TOTAL_SALES = undifferenced_liquor_wine_beer_point_forecast,
  TOTAL_PROMOTIONS = NA,
  onpromotion = "NO",
  isin = "my_forecast"
)

upper<-undifferenced_liquor_wine_beer_point_forecast+2*sd(undifferenced_liquor_wine_beer_point_forecast)
lower<-undifferenced_liquor_wine_beer_point_forecast-2*sd(undifferenced_liquor_wine_beer_point_forecast)

df_upper<- data.frame(
  date = date_seq,
  TOTAL_SALES = upper,
  TOTAL_PROMOTIONS = NA,
  onpromotion = "NO",
  isin = "upper bound"
)

df_lower<- data.frame(
  date = date_seq,
  TOTAL_SALES = lower,
  TOTAL_PROMOTIONS = NA,
  onpromotion = "NO",
  isin = "lower bound"
)

df_liquor_wine_beer <- df_liquor_wine_beer %>% mutate(isin = "original_data")
df_comb_forecast_lwr <- rbind(tail(df_liquor_wine_beer, 200), df_forecast_liquor_wine_beer, df_upper, df_lower)

ggplot(df_comb_forecast_lwr, aes(x = date , y = TOTAL_SALES, color = isin)) + geom_line() + ggtitle(" Time Series Of Liquor, Wine and Beer")

#Poultry
date_seq <- seq.Date(from = as.Date("2017-08-16"), by = "day", length.out = 15)
df_forecast_poultry <- data.frame(
  date = date_seq,
  TOTAL_SALES = undifferenced_poultry_point_forecast,
  TOTAL_PROMOTIONS = NA,
  onpromotion = "NO",
  isin = "my_forecast"
)

upper<-undifferenced_poultry_point_forecast+2*sd(undifferenced_poultry_point_forecast)
lower<-undifferenced_poultry_point_forecast-2*sd(undifferenced_poultry_point_forecast)

df_upper<- data.frame(
  date = date_seq,
  TOTAL_SALES = upper,
  TOTAL_PROMOTIONS = NA,
  onpromotion = "NO",
  isin = "upper bound"
)

df_lower<- data.frame(
  date = date_seq,
  TOTAL_SALES = lower,
  TOTAL_PROMOTIONS = NA,
  onpromotion = "NO",
  isin = "lower bound"
)


df_poultry <- df_poultry %>% mutate(isin = "original_data")
df_comb_forecast_p <- rbind(tail(df_poultry, 200), df_forecast_poultry, df_upper, df_lower)

ggplot(df_comb_forecast_p, aes(x = date , y = TOTAL_SALES, color = isin)) + geom_line() + ggtitle(" Time Series Of Poultry")

#Meats
date_seq <- seq.Date(from = as.Date("2017-08-16"), by = "day", length.out = 15)
df_forecast_meats <- data.frame(
  date = date_seq,
  TOTAL_SALES = undifferenced_meats_point_forecast,
  TOTAL_PROMOTIONS = NA,
  onpromotion = "NO",
  isin = "my_forecast"
)

upper<-undifferenced_meats_point_forecast+2*sd(undifferenced_meats_point_forecast)
lower<-undifferenced_meats_point_forecast-2*sd(undifferenced_meats_point_forecast)

df_upper<- data.frame(
  date = date_seq,
  TOTAL_SALES = upper,
  TOTAL_PROMOTIONS = NA,
  onpromotion = "NO",
  isin = "upper bound"
)

df_lower<- data.frame(
  date = date_seq,
  TOTAL_SALES = lower,
  TOTAL_PROMOTIONS = NA,
  onpromotion = "NO",
  isin = "lower bound"
)


df_meats <- df_meats %>% mutate(isin = "original_data")
df_comb_forecast_m <- rbind(tail(df_meats, 200), df_forecast_meats, df_upper, df_lower)

ggplot(df_comb_forecast_m, aes(x = date , y = TOTAL_SALES, color = isin)) + geom_line() + ggtitle(" Time series of Meats")






#Var-2 CubeRoot + Scaling + Differencing ----

#transformation
ts_meats_transformed <- ts_meats_cleaned^(1/3)
ts_poultry_transformed<- ts_poultry_cleaned^(1/3)
ts_frozen_foods_transformed <- ts_frozen_foods_cleaned^(1/3)
ts_liquor_wine_beer_transformed<- ts_liquor_wine_beer_cleaned^(1/3)
ts_prepared_foods_transformed <- ts_prepared_foods_cleaned^(1/3)


#scaling
normalize_min_max <- function(x) {
  min_x <- min(x, na.rm = TRUE)
  max_x <- max(x, na.rm = TRUE)
  scaled_x <- (x - min_x) / (max_x - min_x)
  list(scaled = scaled_x, min = min_x, max = max_x)
}

invert_min_max_scaling <- function(scaled_data, min, max) {
  original_data <- (scaled_data * (max - min)) + min
  return(original_data)
}


normalize_and_store <- function(ts_data) {
  result <- normalize_min_max(ts_data)
  list(
    scaled = result$scaled,
    min = result$min,
    max = result$max
  )
}


frozen_foods_normalized <- normalize_and_store(ts_frozen_foods_transformed)
ts_frozen_foods_min_max <- frozen_foods_normalized$scaled

liquor_wine_beer_normalized <- normalize_and_store(ts_liquor_wine_beer_transformed)
ts_liquor_wine_beer_min_max <- liquor_wine_beer_normalized$scaled

meats_normalized <- normalize_and_store(ts_meats_transformed)
ts_meats_min_max <- meats_normalized$scaled

poultry_normalized <- normalize_and_store(ts_poultry_transformed)
ts_poultry_min_max <- poultry_normalized$scaled

prepared_foods_normalized <- normalize_and_store(ts_prepared_foods_transformed)
ts_prepared_foods_min_max <- prepared_foods_normalized$scaled


#creating a multivariate time series
multivariate_ts <- cbind(
  ts_frozen_foods_min_max,
  ts_liquor_wine_beer_min_max,
  ts_meats_min_max,
  ts_poultry_min_max,
  ts_prepared_foods_min_max
)



plot(multivariate_ts)
ndiff <- ndiffs(multivariate_ts)
multivariate_ts_diffed <- diff(multivariate_ts, ndiff)


#aic and bic values 
lag_selection <- VARselect(multivariate_ts_diffed, lag.max = 30, type = "none")
print(lag_selection$selection)

results <- data.frame(lag_order = integer(), p_value = numeric(), AIC = numeric(), BIC = numeric())

# Loop through lag orders from 1 to 30
for (lag in 1:30) {
  # Fit VAR model with the current lag order
  fit_var <- VAR(multivariate_ts_diffed, p = lag, type = "none")
  
  # Perform serial correlation test
  Serial_test <- serial.test(fit_var, lags.pt = lag+3, type = "PT.asymptotic")
  
  # Extract p-value
  p_value <- Serial_test$serial$p.value
  
  # Extract AIC and BIC
  aic_value <- AIC(fit_var)
  bic_value <- BIC(fit_var)
  
  # Store the results
  results <- rbind(results, data.frame(lag_order = lag, p_value = p_value, AIC = aic_value, BIC = bic_value))
}
results
fit_var <- VAR(multivariate_ts_diffed, p = 14, type = "none")
plot(multivariate_ts_diffed)
#aic and bic values 
AIC(fit_var)
BIC(fit_var)

#serial test should be greater than 0.05
Serial1 <- serial.test(fit_var, lags.pt =17, type = "PT.asymptotic")
Serial1

#heteroscedasticity
arch1 <- arch.test(fit_var, lags.multi = 17, multivariate.only = TRUE)
arch1

#normality test
Norm1 <- normality.test(fit_var, multivariate.only = TRUE)
Norm1

#stability test
Stability1 <- stability(fit_var, type = "OLS-CUSUM")
plot(Stability1)

# Forecast the VAR model---
var_forecast <- predict(fit_var, n.ahead = 14)

#extracting predictions
frozen_foods_forecast<- var_forecast$fcst$ts_frozen_foods_min_max
liquor_wine_beer_forecast <- var_forecast$fcst$ts_liquor_wine_beer_min_max
meats_forecast <- var_forecast$fcst$ts_meats_min_max
poultry_forecast <- var_forecast$fcst$ts_poultry_min_max
prepared_foods_forecast <- var_forecast$fcst$ts_prepared_foods_min_max

#extracting point forecasts

frozen_foods_point_forecast <- frozen_foods_forecast[, "fcst"]
liquor_wine_beer_point_forecast <- liquor_wine_beer_forecast[, "fcst"]
meats_point_forecast <- meats_forecast[, "fcst"]
poultry_point_forecast <- poultry_forecast[, "fcst"]
prepared_foods_point_forecast <- prepared_foods_forecast[, "fcst"]

#converting back to original scale

#undifferencing

last_value_frozen_foods_ts_normalized <- tail(ts_frozen_foods_min_max, 1)
last_value_liquor_wine_beer_ts_normalized <- tail(ts_liquor_wine_beer_min_max, 1)
last_value_meats_ts_normalized <- tail(ts_meats_min_max, 1)
last_value_poultry_ts_normalized <- tail(ts_poultry_min_max, 1)
last_value_prepared_foods_ts_normalized <- tail(ts_prepared_foods_min_max, 1)

undifferenced_frozen_foods_point_forecast <- cumsum(c(last_value_frozen_foods_ts_normalized, 
                                                      frozen_foods_point_forecast))
undifferenced_liquor_wine_beer_point_forecast <- cumsum(c(last_value_liquor_wine_beer_ts_normalized,
                                                          liquor_wine_beer_point_forecast))
undifferenced_meats_point_forecast <- cumsum(c(last_value_meats_ts_normalized, meats_point_forecast))
undifferenced_poultry_point_forecast <- cumsum(c(last_value_poultry_ts_normalized, poultry_point_forecast))
undifferenced_prepared_foods_point_forecast <- cumsum(c(last_value_prepared_foods_ts_normalized, prepared_foods_point_forecast))

#unnormalizing

unnormalized_forecast_frozen_foods <- invert_min_max_scaling(undifferenced_frozen_foods_point_forecast, frozen_foods_normalized$min, frozen_foods_normalized$max)
unnormalized_forecast_liquor_wine_beer <- invert_min_max_scaling(undifferenced_liquor_wine_beer_point_forecast, liquor_wine_beer_normalized$min, liquor_wine_beer_normalized$max)
unnormalized_forecast_meats <- invert_min_max_scaling(undifferenced_meats_point_forecast, meats_normalized$min, meats_normalized$max)
unnormalized_forecast_poultry <- invert_min_max_scaling(undifferenced_poultry_point_forecast, poultry_normalized$min, poultry_normalized$max)
unnormalized_forecast_prepared_foods <- invert_min_max_scaling(undifferenced_prepared_foods_point_forecast, prepared_foods_normalized$min, prepared_foods_normalized$max)

#raising to the third power
forecast_frozen_foods <- unnormalized_forecast_frozen_foods^3
forecast_liquor_wine_beer <- unnormalized_forecast_liquor_wine_beer^3
forecast_meats <- unnormalized_forecast_meats^3
forecast_poultry <- unnormalized_forecast_poultry^3
forecast_prepared_foods <- unnormalized_forecast_prepared_foods^3

# Plot the original time series and add forecasts
#Frozen Foods
date_seq <- seq.Date(from = as.Date("2017-08-16"), by = "day", length.out = 15)
df_forecast_frozen_foods <- data.frame(
  date = date_seq,
  TOTAL_SALES = forecast_frozen_foods,
  TOTAL_PROMOTIONS = NA,
  onpromotion = "NO",
  isin = "my_forecast"
)

upper<-forecast_frozen_foods+2*sd(forecast_frozen_foods)
lower<-forecast_frozen_foods-2*sd(forecast_frozen_foods)

df_upper<- data.frame(
  date = date_seq,
  TOTAL_SALES = upper,
  TOTAL_PROMOTIONS = NA,
  onpromotion = "NO",
  isin = "upper bound"
)

df_lower<- data.frame(
  date = date_seq,
  TOTAL_SALES = lower,
  TOTAL_PROMOTIONS = NA,
  onpromotion = "NO",
  isin = "lower bound"
)

view(df_frozen_foods)

df_frozen_foods <- df_frozen_foods %>% mutate(isin = "original_data")
df_comb_forecast_ff <- rbind(tail(df_frozen_foods, 200), df_forecast_frozen_foods, df_upper,df_lower)

ggplot(df_comb_forecast_ff, aes(x = date , y = TOTAL_SALES, color = isin)) + geom_line() + ggtitle(" Time Series Of Frozen Foods")

#Prepared Foods
date_seq <- seq.Date(from = as.Date("2017-08-16"), by = "day", length.out = 15)
df_forecast_prepared_foods <- data.frame(
  date = date_seq,
  TOTAL_SALES = forecast_prepared_foods,
  TOTAL_PROMOTIONS = NA,
  onpromotion = "NO",
  isin = "my_forecast"
)


upper<-forecast_prepared_foods+2*sd(forecast_prepared_foods)
lower<-forecast_prepared_foods-2*sd(forecast_prepared_foods)

df_upper<- data.frame(
  date = date_seq,
  TOTAL_SALES = upper,
  TOTAL_PROMOTIONS = NA,
  onpromotion = "NO",
  isin = "upper bound"
)

df_lower<- data.frame(
  date = date_seq,
  TOTAL_SALES = lower,
  TOTAL_PROMOTIONS = NA,
  onpromotion = "NO",
  isin = "lower bound"
)


df_prepared_foods <- df_prepared_foods %>% mutate(isin = "original_data")
df_comb_forecast_pf <- rbind(tail(df_prepared_foods, 200), df_forecast_prepared_foods, df_upper, df_lower)

ggplot(df_comb_forecast_pf, aes(x = date , y = TOTAL_SALES, color = isin)) + geom_line() + ggtitle(" Time Series Of Prepared Foods")

#Liquor, Wine, Beer
date_seq <- seq.Date(from = as.Date("2017-08-16"), by = "day", length.out = 15)
df_forecast_liquor_wine_beer <- data.frame(
  date = date_seq,
  TOTAL_SALES = forecast_liquor_wine_beer,
  TOTAL_PROMOTIONS = NA,
  onpromotion = "NO",
  isin = "my_forecast"
)

upper<-forecast_liquor_wine_beer+2*sd(forecast_liquor_wine_beer)
lower<-forecast_liquor_wine_beer-2*sd(forecast_liquor_wine_beer)

df_upper<- data.frame(
  date = date_seq,
  TOTAL_SALES = upper,
  TOTAL_PROMOTIONS = NA,
  onpromotion = "NO",
  isin = "upper bound"
)

df_lower<- data.frame(
  date = date_seq,
  TOTAL_SALES = lower,
  TOTAL_PROMOTIONS = NA,
  onpromotion = "NO",
  isin = "lower bound"
)

df_liquor_wine_beer <- df_liquor_wine_beer %>% mutate(isin = "original_data")
df_comb_forecast_lwr <- rbind(tail(df_liquor_wine_beer, 200), df_forecast_liquor_wine_beer, df_upper, df_lower)

ggplot(df_comb_forecast_lwr, aes(x = date , y = TOTAL_SALES, color = isin)) + geom_line() + ggtitle(" Time Series Of Liquor, Wine and Beer")

#Poultry
date_seq <- seq.Date(from = as.Date("2017-08-16"), by = "day", length.out = 15)
df_forecast_poultry <- data.frame(
  date = date_seq,
  TOTAL_SALES = forecast_poultry,
  TOTAL_PROMOTIONS = NA,
  onpromotion = "NO",
  isin = "my_forecast"
)

upper<-forecast_poultry+2*sd(forecast_poultry)
lower<-forecast_poultry-2*sd(forecast_poultry)

df_upper<- data.frame(
  date = date_seq,
  TOTAL_SALES = upper,
  TOTAL_PROMOTIONS = NA,
  onpromotion = "NO",
  isin = "upper bound"
)

df_lower<- data.frame(
  date = date_seq,
  TOTAL_SALES = lower,
  TOTAL_PROMOTIONS = NA,
  onpromotion = "NO",
  isin = "lower bound"
)


df_poultry <- df_poultry %>% mutate(isin = "original_data")
df_comb_forecast_p <- rbind(tail(df_poultry, 200), df_forecast_poultry, df_upper, df_lower)

ggplot(df_comb_forecast_p, aes(x = date , y = TOTAL_SALES, color = isin)) + geom_line() + ggtitle(" Time Series Of Poultry")

#Meats
date_seq <- seq.Date(from = as.Date("2017-08-16"), by = "day", length.out = 15)
df_forecast_meats <- data.frame(
  date = date_seq,
  TOTAL_SALES = forecast_meats,
  TOTAL_PROMOTIONS = NA,
  onpromotion = "NO",
  isin = "my_forecast"
)

upper<-forecast_meats+2*sd(forecast_meats)
lower<-forecast_meats-2*sd(forecast_meats)

df_upper<- data.frame(
  date = date_seq,
  TOTAL_SALES = upper,
  TOTAL_PROMOTIONS = NA,
  onpromotion = "NO",
  isin = "upper bound"
)

df_lower<- data.frame(
  date = date_seq,
  TOTAL_SALES = lower,
  TOTAL_PROMOTIONS = NA,
  onpromotion = "NO",
  isin = "lower bound"
)


df_meats <- df_meats %>% mutate(isin = "original_data")
df_comb_forecast_m <- rbind(tail(df_meats, 200), df_forecast_meats, df_upper, df_lower)

ggplot(df_comb_forecast_m, aes(x = date , y = TOTAL_SALES, color = isin)) + geom_line() + ggtitle(" Time series of Meats")






#Var-3 Smoothing + scaling +no structural Breaks ----
#smoothing
ts_frozen_foods_smoothed <- SMA(ts_frozen_foods_cleaned, n=5)
ts_liquor_wine_beer_smoothed <- SMA(ts_liquor_wine_beer_cleaned, n=5)
ts_meats_smoothed <- SMA(ts_meats_cleaned, n=5)
ts_poultry_smoothed <- SMA(ts_poultry_cleaned, n=5)
ts_prepared_foods_smoothed <- SMA(ts_prepared_foods_cleaned, n=5)

#converting back to time series
ts_frozen_foods_smoothed <- ts(ts_frozen_foods_smoothed[5:length(ts_frozen_foods_smoothed)],
                               start=c(2013,5),
                               frequency=365)
ts_liquor_wine_beer_smoothed <- ts(ts_liquor_wine_beer_smoothed[5:length(ts_liquor_wine_beer_smoothed)],
                                   start=c(2013,5),
                                   frequency=365)
ts_meats_smoothed <- ts(ts_meats_smoothed[5:length(ts_meats_smoothed)],
                        start=c(2013,5),
                        frequency=365)
ts_poultry_smoothed <- ts(ts_poultry_smoothed[5:length(ts_poultry_smoothed)],
                          start=c(2013,5),
                          frequency=365)
ts_prepared_foods_smoothed <- ts(ts_prepared_foods_smoothed[5:length(ts_prepared_foods_smoothed)],
                                 start=c(2013,5),
                                 frequency=365)

#scaling
normalize_min_max <- function(x) {
  min_x <- min(x, na.rm = TRUE)
  max_x <- max(x, na.rm = TRUE)
  scaled_x <- (x - min_x) / (max_x - min_x)
  list(scaled = scaled_x, min = min_x, max = max_x)
}

invert_min_max_scaling <- function(scaled_data, min, max) {
  original_data <- (scaled_data * (max - min)) + min
  return(original_data)
}


normalize_and_store <- function(ts_data) {
  result <- normalize_min_max(ts_data)
  list(
    scaled = result$scaled,
    min = result$min,
    max = result$max
  )
}


frozen_foods_normalized <- normalize_and_store(ts_frozen_foods_smoothed)
ts_frozen_foods_min_max <- frozen_foods_normalized$scaled

liquor_wine_beer_normalized <- normalize_and_store(ts_liquor_wine_beer_smoothed)
ts_liquor_wine_beer_min_max <- liquor_wine_beer_normalized$scaled

meats_normalized <- normalize_and_store(ts_meats_smoothed)
ts_meats_min_max <- meats_normalized$scaled

poultry_normalized <- normalize_and_store(ts_poultry_smoothed)
ts_poultry_min_max <- poultry_normalized$scaled

prepared_foods_normalized <- normalize_and_store(ts_prepared_foods_smoothed)
ts_prepared_foods_min_max <- prepared_foods_normalized$scaled

#filtering out structural breaks
ts_frozen_foods_filtered <- ts_frozen_foods_min_max[1200:length(ts_frozen_foods_min_max)]

ts_liquor_wine_beer_filtered <- ts_liquor_wine_beer_min_max[1200:length(ts_liquor_wine_beer_min_max)]

ts_meats_filtered <- ts_meats_min_max[1200:length(ts_meats_min_max)]

ts_poultry_filtered <- ts_poultry_min_max[1200:length(ts_poultry_min_max)]

ts_prepared_foods_filtered <- ts_prepared_foods_min_max[1200:length(ts_prepared_foods_min_max)]

ts_frozen_foods_filtered <- ts(ts_frozen_foods_filtered, start = c(2016, 108), frequency = 365)

ts_liquor_wine_beer_filtered <- ts(ts_liquor_wine_beer_filtered, start = c(2016, 108), frequency = 365)

ts_meats_filtered <- ts(ts_meats_filtered, start = c(2016, 108), frequency = 365)

ts_poultry_filtered <- ts(ts_poultry_filtered, start = c(2016, 108), frequency = 365)

ts_prepared_foods_filtered <- ts(ts_prepared_foods_filtered, start = c(2016, 108), frequency = 365)
#creating a multivariate time series
multivariate_ts <- cbind(
  ts_frozen_foods_filtered,
  ts_liquor_wine_beer_filtered,
  ts_meats_filtered,
  ts_poultry_filtered,
  ts_prepared_foods_filtered
)

# Plot the multivariate time series
plot.ts(multivariate_ts)
ndiff <- ndiffs(multivariate_ts)
multivariate_ts_diffed <- diff(multivariate_ts, ndiff)

#both the aicc and the bic criterion suggest a model of order 5

lag_selection <- VARselect(multivariate_ts_diffed, lag.max = 30, type = "const")
print(lag_selection$selection)


results <- data.frame(lag_order = integer(), p_value = numeric(), AIC = numeric(), BIC = numeric())

# Loop through lag orders from 1 to 30
for (lag in 1:30) {
  # Fit VAR model with the current lag order
  fit_var <- VAR(multivariate_ts_diffed, p = lag, type = "const")
  
  # Perform serial correlation test
  Serial_test <- serial.test(fit_var, lags.pt = lag+3, type = "PT.asymptotic")
  
  # Extract p-value
  p_value <- Serial_test$serial$p.value
  
  # Extract AIC and BIC
  aic_value <- AIC(fit_var)
  bic_value <- BIC(fit_var)
  
  # Store the results
  results <- rbind(results, data.frame(lag_order = lag, p_value = p_value, AIC = aic_value, BIC = bic_value))
}
results
fit_var <- VAR(multivariate_ts_diffed, p = 25, type = "const")

#aic and bic
AIC(fit_var)
BIC(fit_var)
#serial test should be greater than 0.05
Serial1 <- serial.test(fit_var, lags.pt = 28, type = "PT.asymptotic")
Serial1

#heteroscedasticity
arch1 <- arch.test(fit_var, lags.multi = 28, multivariate.only = TRUE)
arch1

#normality test
Norm1 <- normality.test(fit_var, multivariate.only = TRUE)
Norm1

#stability test
Stability1 <- stability(fit_var, type = "OLS-CUSUM")
plot(Stability1)

# Forecast the VAR model---
var_forecast <- predict(fit_var, n.ahead = 14)

#extracting predictions
frozen_foods_forecast<- var_forecast$fcst$ts_frozen_foods_filtered
liquor_wine_beer_forecast <- var_forecast$fcst$ts_liquor_wine_beer_filtered
meats_forecast <- var_forecast$fcst$ts_meats_filtered
poultry_forecast <- var_forecast$fcst$ts_poultry_filtered
prepared_foods_forecast <- var_forecast$fcst$ts_prepared_foods_filtered

#extracting point forecasts

frozen_foods_point_forecast <- frozen_foods_forecast[, "fcst"]
liquor_wine_beer_point_forecast <- liquor_wine_beer_forecast[, "fcst"]
meats_point_forecast <- meats_forecast[, "fcst"]
poultry_point_forecast <- poultry_forecast[, "fcst"]
prepared_foods_point_forecast <- prepared_foods_forecast[, "fcst"]

#converting back to original scale

#undifferencing

last_value_frozen_foods_ts_normalized <- tail(ts_frozen_foods_min_max, 1)
last_value_liquor_wine_beer_ts_normalized <- tail(ts_liquor_wine_beer_min_max, 1)
last_value_meats_ts_normalized <- tail(ts_meats_min_max, 1)
last_value_poultry_ts_normalized <- tail(ts_poultry_min_max, 1)
last_value_prepared_foods_ts_normalized <- tail(ts_prepared_foods_min_max, 1)

undifferenced_frozen_foods_point_forecast <- cumsum(c(last_value_frozen_foods_ts_normalized, 
                                                      frozen_foods_point_forecast))
undifferenced_liquor_wine_beer_point_forecast <- cumsum(c(last_value_liquor_wine_beer_ts_normalized,
                                                          liquor_wine_beer_point_forecast))
undifferenced_meats_point_forecast <- cumsum(c(last_value_meats_ts_normalized, meats_point_forecast))
undifferenced_poultry_point_forecast <- cumsum(c(last_value_poultry_ts_normalized, poultry_point_forecast))
undifferenced_prepared_foods_point_forecast <- cumsum(c(last_value_prepared_foods_ts_normalized, prepared_foods_point_forecast))

#unnormalizing
forecast_frozen_foods <- invert_min_max_scaling(undifferenced_frozen_foods_point_forecast, frozen_foods_normalized$min, frozen_foods_normalized$max)
forecast_liquor_wine_beer <- invert_min_max_scaling(undifferenced_liquor_wine_beer_point_forecast, liquor_wine_beer_normalized$min, liquor_wine_beer_normalized$max)
forecast_meats <- invert_min_max_scaling(undifferenced_meats_point_forecast, meats_normalized$min, meats_normalized$max)
forecast_poultry <- invert_min_max_scaling(undifferenced_poultry_point_forecast, poultry_normalized$min, poultry_normalized$max)
forecast_prepared_foods <- invert_min_max_scaling(undifferenced_prepared_foods_point_forecast, prepared_foods_normalized$min, prepared_foods_normalized$max)

# Plot the original time series and add forecasts
#Frozen Foods
date_seq <- seq.Date(from = as.Date("2017-08-16"), by = "day", length.out = 15)
df_forecast_frozen_foods <- data.frame(
  date = date_seq,
  TOTAL_SALES = forecast_frozen_foods,
  TOTAL_PROMOTIONS = NA,
  onpromotion = "NO",
  isin = "my_forecast"
)

upper<-forecast_frozen_foods+2*sd(forecast_frozen_foods)
lower<-forecast_frozen_foods-2*sd(forecast_frozen_foods)

df_upper<- data.frame(
  date = date_seq,
  TOTAL_SALES = upper,
  TOTAL_PROMOTIONS = NA,
  onpromotion = "NO",
  isin = "upper bound"
)

df_lower<- data.frame(
  date = date_seq,
  TOTAL_SALES = lower,
  TOTAL_PROMOTIONS = NA,
  onpromotion = "NO",
  isin = "lower bound"
)

view(df_frozen_foods)

df_frozen_foods <- df_frozen_foods %>% mutate(isin = "original_data")
df_comb_forecast_ff <- rbind(tail(df_frozen_foods, 200), df_forecast_frozen_foods, df_upper,df_lower)

ggplot(df_comb_forecast_ff, aes(x = date , y = TOTAL_SALES, color = isin)) + geom_line() + ggtitle(" Time Series Of Frozen Foods")

#Prepared Foods
date_seq <- seq.Date(from = as.Date("2017-08-16"), by = "day", length.out = 15)
df_forecast_prepared_foods <- data.frame(
  date = date_seq,
  TOTAL_SALES = forecast_prepared_foods,
  TOTAL_PROMOTIONS = NA,
  onpromotion = "NO",
  isin = "my_forecast"
)


upper<-forecast_prepared_foods+2*sd(forecast_prepared_foods)
lower<-forecast_prepared_foods-2*sd(forecast_prepared_foods)

df_upper<- data.frame(
  date = date_seq,
  TOTAL_SALES = upper,
  TOTAL_PROMOTIONS = NA,
  onpromotion = "NO",
  isin = "upper bound"
)

df_lower<- data.frame(
  date = date_seq,
  TOTAL_SALES = lower,
  TOTAL_PROMOTIONS = NA,
  onpromotion = "NO",
  isin = "lower bound"
)


df_prepared_foods <- df_prepared_foods %>% mutate(isin = "original_data")
df_comb_forecast_pf <- rbind(tail(df_prepared_foods, 200), df_forecast_prepared_foods, df_upper, df_lower)

ggplot(df_comb_forecast_pf, aes(x = date , y = TOTAL_SALES, color = isin)) + geom_line() + ggtitle(" Time Series Of Prepared Foods")

#Liquor, Wine, Beer
date_seq <- seq.Date(from = as.Date("2017-08-16"), by = "day", length.out = 15)
df_forecast_liquor_wine_beer <- data.frame(
  date = date_seq,
  TOTAL_SALES = forecast_liquor_wine_beer,
  TOTAL_PROMOTIONS = NA,
  onpromotion = "NO",
  isin = "my_forecast"
)

upper<-forecast_liquor_wine_beer+2*sd(forecast_liquor_wine_beer)
lower<-forecast_liquor_wine_beer-2*sd(forecast_liquor_wine_beer)

df_upper<- data.frame(
  date = date_seq,
  TOTAL_SALES = upper,
  TOTAL_PROMOTIONS = NA,
  onpromotion = "NO",
  isin = "upper bound"
)

df_lower<- data.frame(
  date = date_seq,
  TOTAL_SALES = lower,
  TOTAL_PROMOTIONS = NA,
  onpromotion = "NO",
  isin = "lower bound"
)

df_liquor_wine_beer <- df_liquor_wine_beer %>% mutate(isin = "original_data")
df_comb_forecast_lwr <- rbind(tail(df_liquor_wine_beer, 200), df_forecast_liquor_wine_beer, df_upper, df_lower)

ggplot(df_comb_forecast_lwr, aes(x = date , y = TOTAL_SALES, color = isin)) + geom_line() + ggtitle(" Time Series Of Liquor, Wine and Beer")

#Poultry
date_seq <- seq.Date(from = as.Date("2017-08-16"), by = "day", length.out = 15)
df_forecast_poultry <- data.frame(
  date = date_seq,
  TOTAL_SALES = forecast_poultry,
  TOTAL_PROMOTIONS = NA,
  onpromotion = "NO",
  isin = "my_forecast"
)

upper<-forecast_poultry+2*sd(forecast_poultry)
lower<-forecast_poultry-2*sd(forecast_poultry)

df_upper<- data.frame(
  date = date_seq,
  TOTAL_SALES = upper,
  TOTAL_PROMOTIONS = NA,
  onpromotion = "NO",
  isin = "upper bound"
)

df_lower<- data.frame(
  date = date_seq,
  TOTAL_SALES = lower,
  TOTAL_PROMOTIONS = NA,
  onpromotion = "NO",
  isin = "lower bound"
)


df_poultry <- df_poultry %>% mutate(isin = "original_data")
df_comb_forecast_p <- rbind(tail(df_poultry, 200), df_forecast_poultry, df_upper, df_lower)

ggplot(df_comb_forecast_p, aes(x = date , y = TOTAL_SALES, color = isin)) + geom_line() + ggtitle(" Time Series Of Poultry")

#Meats
date_seq <- seq.Date(from = as.Date("2017-08-16"), by = "day", length.out = 15)
df_forecast_meats <- data.frame(
  date = date_seq,
  TOTAL_SALES = forecast_meats,
  TOTAL_PROMOTIONS = NA,
  onpromotion = "NO",
  isin = "my_forecast"
)

upper<-forecast_meats+2*sd(forecast_meats)
lower<-forecast_meats-2*sd(forecast_meats)

df_upper<- data.frame(
  date = date_seq,
  TOTAL_SALES = upper,
  TOTAL_PROMOTIONS = NA,
  onpromotion = "NO",
  isin = "upper bound"
)

df_lower<- data.frame(
  date = date_seq,
  TOTAL_SALES = lower,
  TOTAL_PROMOTIONS = NA,
  onpromotion = "NO",
  isin = "lower bound"
)


df_meats <- df_meats %>% mutate(isin = "original_data")
df_comb_forecast_m <- rbind(tail(df_meats, 200), df_forecast_meats, df_upper, df_lower)

ggplot(df_comb_forecast_m, aes(x = date , y = TOTAL_SALES, color = isin)) + geom_line() + ggtitle(" Time series of Meats")






#Passing to a CSV file ----
forecast_frozen_foods <- unnormalized_forecast_frozen_foods^3
forecast_liquor_wine_beer <- unnormalized_forecast_liquor_wine_beer^3
forecast_meats <- unnormalized_forecast_meats^3
forecast_poultry <- unnormalized_forecast_poultry^3
forecast_prepared_foods <- unnormalized_forecast_prepared_foods^3

df_forecast_total <- df_forecast_frozen_foods
df_forecast_total <- df_forecast_frozen_foods %>% rename("Frozen Foods" = "TOTAL_SALES")
df_forecast_total$`Liquor Wine Beer`<-df_forecast_liquor_wine_beer$TOTAL_SALES
df_forecast_total$`Meats`<-df_forecast_meats$TOTAL_SALES
df_forecast_total$`Poultry`<-df_forecast_poultry$TOTAL_SALES
df_forecast_total$`Prepared Foods`<-df_forecast_prepared_foods$TOTAL_SALES

write.csv(df_forecast_total, row.names = FALSE, file = "PredictedValuesVAR.csv")

