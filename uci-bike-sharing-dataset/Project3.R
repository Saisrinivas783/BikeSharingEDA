# File Importing
bike_df <- read.csv("hour.csv")
head(bike_df, 5)

# Renaming Columns
names(bike_df) <- c('Instant','Date','Season','Year','Month','Hour','Holiday','Weekday','Workingday','Weather_condition','Temperature','Feelslike_temp','Humidity','Windspeed','Casual','Registered','Total_count')
head(bike_df)

# Convert categorical columns to factors
categorical_col <- c('Season', 'Year', 'Month', 'Hour', 'Holiday', 'Weekday', 'Workingday', 'Weather_condition')
bike_df[, categorical_col] <- lapply(bike_df[, categorical_col], factor)

# Check for null values
null_check <- colSums(is.na(bike_df))
print(null_check)

library(ggplot2)
library(dplyr)


# Hour Of the Day v/s Count
hourly_total_counts <- bike_df %>%
  group_by(Hour) %>%
  summarise(total_rental_count = sum(Total_count) / 1000)

ggplot(hourly_total_counts, aes(x = factor(Hour), y = total_rental_count)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(
    title = "Total Bike Rentals by Hour of the Day",
    x = "Hour of the Day",
    y = "Total Rental Count (Thousands)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



