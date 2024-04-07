## load packages
library(neonUtilities)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lubridate)
library(caTools)
library(lmtest)
library(Metrics)

#### Load and prepare data ####
## Load mosquito data using loadByProduct()
mosAll <- loadByProduct(dpID="DP1.10043.001", 
                        startdate = "2016-01", 
                        enddate = "2023-12",
                        package="basic", check.size=F)

# Extract and separate out tables in global environment
list2env(mosAll, .GlobalEnv)

# Extract year and add column
mos_expertTaxonomistIDProcessed$year <- year(as.Date(mos_expertTaxonomistIDProcessed$setDate))

## Load weather data GUAN
weatherGUAN <- read.csv("C:\\Users\\Fartrell Cluggins\\Downloads\\open-meteo-17.96N66.91W146m.csv")
weatherUNDE <- read.csv("C:\\Users\\Fartrell Cluggins\\Downloads\\open-meteo-46.22N89.57W536mUNDE.csv")
# select variables for GUAN
weatherGUAN <- weatherGUAN %>%
  mutate(temp = temperature_2m_mean..Â.F., precip = precipitation_sum..mm.) %>%
  select(time, temp, precip)
# select variables for UNDE
weatherUNDE <- weatherUNDE %>%
  mutate(temp = temperature_2m_mean..Â.F., precip = precipitation_sum..mm.) %>%
  select(time, temp, precip)

# Total individuals per site per collectDate
sumAllcollectDate <- mos_expertTaxonomistIDProcessed %>%
  group_by(siteID, collectDate, year) %>%
  filter(targetTaxaPresent != "N")

# Total individuals per site per setDate
sumAllsetDate <- mos_expertTaxonomistIDProcessed %>%
  group_by(siteID, setDate, year) %>%
  filter(targetTaxaPresent != "N")


#### Prepare GUAN for model ####
# Filter out for GUAN using setDate
sumGUANsetDate <- sumAllsetDate %>%
  filter(siteID == 'GUAN') %>%
  group_by(setDate) %>%
  summarise(totalIndividuals = sum(individualCount, na.rm=TRUE)) %>%
  mutate(setDate = as.Date(setDate, format = "%Y-%m-%d")) %>%
  group_by(setDate) %>%
  summarise(totalByDate = sum(totalIndividuals, na.rm=TRUE))

# Filter out for GUAN using collectDate  
sumGUANcollectDate <- sumAllcollectDate %>%
  filter(siteID == 'GUAN') %>%
  group_by(collectDate) %>%
  summarise(totalIndividuals = sum(individualCount, na.rm=TRUE)) %>%
  mutate(collectDate = as.Date(collectDate, format = "%Y-%m-%d")) %>%
  group_by(collectDate) %>%
  summarise(totalByDate = sum(totalIndividuals, na.rm=TRUE))


sum2019setDate <- sumAllsetDate %>%
  #filter(siteID == "DSNY")
  filter(year == 2019)

# Ensure dates are in the Date format
sumAllDate$collectDate <- as.Date(sumAllDate$collectDate)
weatherGUAN$time <- as.Date(weatherGUAN$time)
sumGUANsetDate$setDate <- as.Date(sumGUANsetDate$setDate)
sumGUANcollectDate$collectDate <- as.Date(sumGUANcollectDate$collectDate)
sumUNDEcollectDate$collectDate <- as.Date(sumUNDEcollectDate$collectDate)
weatherUNDE$time <- as.Date(weatherUNDE$time)
# Check to make sure proper format
str(sumGUANsetDate)
str(weatherGUAN)

## Merge the data frames
dataGUAN <- merge(sumGUANsetDate, weatherGUAN, by.x = "setDate", by.y = "time") %>%
  select(setDate, totalByDate, temp, precip)

dataGUANcollect <- merge(sumGUANcollectDate, weatherGUAN, by.x = "collectDate", by.y = "time") %>%
  select(collectDate, totalByDate, temp, precip)

### Run function to calculate 5 day temp averages ###
calculate_5day_avg_temp_precip <- function(set_date, weather_data) {
  # Define the start and end of the 5-day period preceding the collect date
  period_end <- set_date - 1 # Exclude the collect date itself
  period_start <- set_date - 5 # Start of the 5-day period
  
  # Filter weather data for the 5-day period
  filtered_data <- weatherGUAN %>%
    filter(time >= period_start & time <= period_end)
  
  # Calculate average temperature
  avg_temp <- mean(filtered_data$temp, na.rm = TRUE)
  
  # Calculate average precipitation
  avg_precip <- mean(filtered_data$precip, na.rm = TRUE)
  
  # Return both averages
  return(list(avgTemp = avg_temp, avgPrecip = avg_precip))
}

# Apply the function to each collectDate in the mosquito count to calculate the averages
results <- lapply(sumGUANcollectDate$collectDate, calculate_5day_avg_temp_precip, weather_data = weatherGUAN)

# The results are in a list format, separate them into two vectors
avgTemps <- sapply(results, function(x) x$avgTemp)
avgPrecips <- sapply(results, function(x) x$avgPrecip)

# Add these vectors as columns in the mos dataset
sumGUANcollectDate$avgTemp_5daysBefore <- avgTemps
sumGUANcollectDate$avgPrecip_5daysBefore <- avgPrecips

# View the updated mos dataset
head(sumGUANcollectDate)

#### Prepare UNDE for model ####
# Filter out for GUAN using collectDate  
sumUNDEcollectDate <- sumAllcollectDate %>%
  filter(siteID == 'UNDE') %>%
  group_by(collectDate) %>%
  summarise(totalIndividuals = sum(individualCount, na.rm=TRUE)) %>%
  mutate(collectDate = as.Date(collectDate, format = "%Y-%m-%d")) %>%
  group_by(collectDate) %>%
  summarise(totalByDate = sum(totalIndividuals, na.rm=TRUE))

# Merge weather and count data frames
dataUNDEcollect <- merge(sumUNDEcollectDate, weatherUNDE, by.x = "collectDate", by.y = "time") %>%
  select(collectDate, totalByDate, temp, precip)

### Run function to calculate 5 day temp averages ###
calculate_5day_avg_temp_precip <- function(set_date, weather_data) {
  # Define the start and end of the 5-day period preceding the collect date
  period_end <- set_date - 1 # Exclude the collect date itself
  period_start <- set_date - 5 # Start of the 5-day period
  
  # Filter weather data for the 5-day period
  filtered_data <- weatherUNDE %>%
    filter(time >= period_start & time <= period_end)
  
  # Calculate average temperature
  avg_temp <- mean(filtered_data$temp, na.rm = TRUE)
  
  # Calculate average precipitation
  avg_precip <- mean(filtered_data$precip, na.rm = TRUE)
  
  # Return both averages
  return(list(avgTemp = avg_temp, avgPrecip = avg_precip))
}

# Apply the function to each collectDate in the mosquito count dataset to calculate the averages
results <- lapply(sumUNDEcollectDate$collectDate, calculate_5day_avg_temp_precip, weather_data = weatherUNDE)

# The results are in a list format, separate them into two vectors
avgTemps <- sapply(results, function(x) x$avgTemp)
avgPrecips <- sapply(results, function(x) x$avgPrecip)

# Add these vectors as columns in the mos dataset
sumUNDEcollectDate$avgTemp_5daysBefore <- avgTemps
sumUNDEcollectDate$avgPrecip_5daysBefore <- avgPrecips

# View the updated mos dataset
head(sumUNDEcollectDate)

#### Descriptive Statistics #####
plot(dataGUAN$temp, dataGUAN$totalByDate)
plot(dataUNDEcollect$temp, dataUNDEcollect$totalByDate)
plot(dataGUAN$precip, dataGUAN$totalByDate)
plot(dataGUANcollect$collectDate, dataGUANcollect$totalByDate, main="SumCollectDate over Time", xlab="Date", ylab="SumCollectDate")
plot(dataUNDEcollect$collectDate, dataUNDEcollect$totalByDate, main="SumCollectDate over Time", xlab="Date", ylab="SumCollectDate")

## Total individuals per site per year
summary <- mos_expertTaxonomistIDProcessed %>%
  group_by(siteID, year) %>%
  summarise(totalIndividuals = sum(individualCount, na.rm=TRUE))

# Plot total individuals per site per year
ggplot(summary, aes(x = factor(year), y = totalIndividuals, fill = factor(siteID))) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(x = "Year", y = "Total Individuals", fill = "Site ID") +
  ggtitle("Total Individuals per Site per Year")

 
## Summarize total individuals per year across all sites
total_individuals_per_year <- mos_expertTaxonomistIDProcessed %>%  
  group_by(year) %>%  
  summarise(totalIndividuals = sum (individualCount, na.rm = TRUE))

# Plot total individuals per year
ggplot(total_individuals_per_year, aes(x = year, y = totalIndividuals)) +
  geom_line(group=1, colour="blue") +
  geom_point(size=2, colour="red") +
  theme_minimal() +
  labs(title = "Total Number of Individuals Per Year",
       x = "Year",
       y = "Total Individuals")

# Plot the number of mosquitoes caught per collection day at NIWO
ggplot(sumAllDate, aes(x=collectDate, y=totalByDate)) +
  geom_line(color="darkcyan") +
  #scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1)) +
  ggtitle("Number of Mosquitoes Caught Per Collection Day (All Sites)") +
  labs(x="Collect Date", y="Number of Mosquitoes")


#### Fit a simple model ####
## GUAN daily mean for set date
modelGUAN <- lm(totalByDate ~ temp, data = dataGUAN)
summary(modelGUAN)
## GUAN 5 day average prior to collect date
modelGUANcollect <- lm(totalByDate ~ avgTemp_5daysBefore + avgPrecip_5daysBefore, data = sumGUANcollectDate)
summary(modelGUAN)
## UNDE 5 day average prior to collect date
modelUNDEcollect <- lm(totalByDate ~ avgTemp_5daysBefore + avgPrecip_5daysBefore, data = sumUNDEcollectDate)
summary(modelUNDEcollect)

# Calculate Pearson correlation coefficient
pearson_correlation <- cor(dataGUAN$totalByDate, dataGUAN$temp, method = "pearson")
print(paste("Pearson correlation coefficient: ", pearson_correlation))

##### UNDE - Build the model with training and testing sets ####
# Check for missing values in key columns
summary(sumUNDEcollectDate[c("totalByDate", "avgTemp_5daysBefore", "avgPrecip_5daysBefore")])

# remove rows with missing values
sumUNDEcollectDate <- na.omit(sumUNDEcollectDate)

# Split data into testing and training sets
set.seed(123) # For reproducibility
split <- sample.split(sumUNDEcollectDate$totalByDate, SplitRatio = 0.7)

train_dataU <- subset(sumUNDEcollectDate, split == TRUE)
test_dataU <- subset(sumUNDEcollectDate, split == FALSE)

# Build the model
modelUNDE <- lm(totalByDate ~ avgTemp_5daysBefore + avgPrecip_5daysBefore, data = train_dataU)
summary(modelUNDE)

# Confirm tests
bptest(modelUNDE) # Breusch-Pagan test for heteroskedasticity
# Check for normality and constant variance
plot(modelUNDE$residuals)
hist(modelUNDE$residuals)

# Run predictions
predictions <- predict(modelUNDE, newdata = test_dataU)
# Calculate root mean square error
rmse <- rmse(test_dataU$totalByDate, predictions)
print(paste("RMSE:", rmse))


# Calculate residuals and fitted values
residuals <- modelUNDE$residuals
fitted_values <- modelUNDE$fitted.values

# Create the plot
ggplot() +
  geom_point(aes(x = fitted_values, y = residuals), color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Fitted Values", y = "Residuals", title = "Residuals vs. Fitted Values") +
  theme_minimal()


##### GUAN - Build the model with training and testing sets ####
# Check for missing values in key columns
summary(sumGUANcollectDate[c("totalByDate", "avgTemp_5daysBefore", "avgPrecip_5daysBefore")])

# remove rows with missing values
sumGUANcollectDate <- na.omit(sumGUANcollectDate)

# Split data into testing and training sets
set.seed(123) # For reproducibility
split <- sample.split(sumGUANcollectDate$totalByDate, SplitRatio = 0.7)

train_dataG <- subset(sumGUANcollectDate, split == TRUE)
test_dataG <- subset(sumGUANcollectDate, split == FALSE)

# Build the model
modelGUAN <- lm(totalByDate ~ avgTemp_5daysBefore + avgPrecip_5daysBefore, data = train_dataG)
summary(modelGUAN)

# Confirm tests
bptest(modelGUAN) # Breusch-Pagan test for heteroskedasticity
# Check for normality and constant variance
plot(modelGUAN$residuals)
hist(modelGUAN$residuals)

# Run predictions
predictionsU <- predict(modelGUAN, newdata = test_dataG)
# Calculate root mean square error
rmse <- rmse(test_data$totalByDate, predictions)
print(paste("RMSE:", rmse))


# Create the time series plot
ggplot(dataAll, aes(x = collectDate)) +
  geom_line(aes(y = totalByDate, color = "Actual")) +
  geom_line(aes(y = predicted, color = "Predicted")) +
  labs(x = "Date", y = "Value", title = "Actual and Predicted values over time", color = "Legend")

#### Test for avg temp counts #####
# GUAN
temp_summary <- dataGUAN %>%
  group_by(temp) %>%
  summarise(total_mosquitoes = sum(totalByDate)) %>%
  arrange(desc(total_mosquitoes))

ggplot(temp_summary, aes(x = temp, y = total_mosquitoes)) +
  geom_line() + # or geom_point() to plot individual points
  labs(title = "Total Mosquito Counts by Average Temperature",
       x = "Average Temperature",
       y = "Total Mosquito Counts") +
  theme_minimal()

ggplot(dataGUAN, aes(x = temp, y = totalByDate)) +
  geom_point() +  # Adds the scatter plot
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  
  labs(x = "Temperature", y = "Mosquito Counts", title = "Relationship Between Temperature and Mosquito Counts") +
  theme_minimal()

# Perform correlation test between temp and number of mosquitoes
cor.test(dataGUAN$temp, dataGUAN$totalByDate)

# Distribution of Mosquito Counts by Temp GUAN
dataGUAN$rounded_temp <- round(dataGUAN$temp)
ggplot(dataGUAN, aes(x = factor(rounded_temp), y = totalByDate)) +
  geom_boxplot() +
  labs(x = "Temperature", y = "Mosquito Counts", title = "Distribution of Mosquito Counts by Temperature") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# UNDE
temp_summary <- dataUNDEcollect %>%
  group_by(temp) %>%
  summarise(total_mosquitoes = sum(totalByDate)) %>%
  arrange(desc(total_mosquitoes))

ggplot(temp_summary, aes(x = temp, y = total_mosquitoes)) +
  geom_line() + # or geom_point() to plot individual points
  labs(title = "Total Mosquito Counts by Average Temperature",
       x = "Average Temperature",
       y = "Total Mosquito Counts") +
  theme_minimal()


ggplot(dataUNDEcollect, aes(x = temp, y = totalByDate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  
  labs(x = "Temperature", y = "Mosquito Counts", title = "Relationship Between Temperature and Mosquito Counts") +
  theme_minimal()

# Perform correlation test between temp and number of mosquitoes
cor.test(dataUNDEcollect$temp, dataUNDEcollect$totalByDate)


# Distribution of Mosquito Counts by Temp UNDE
dataUNDEcollect$rounded_temp <- round(dataUNDEcollect$temp)
ggplot(dataUNDEcollect, aes(x = factor(rounded_temp), y = totalByDate)) +
  geom_boxplot() +
  labs(x = "Temperature", y = "Mosquito Counts", title = "Distribution of Mosquito Counts by Temperature") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Get the predicted values from the model
data$predicted <- predict(model, data)

# Create the scatter plot. each point represents an observation, with the predicted value on the x-axis and the actual value on the y-axis. 
#The red dashed line represents perfect prediction (actual = predicted)
ggplot(data, aes(x = predicted, y = totalByDate)) +
  geom_point() +
  labs(x = "Predicted", y = "Actual", title = "Actual vs Predicted values") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed")

