bike_df<-read.csv("hour.csv")
head(bike_df,5)
names(bike_df) <- c('Instant','Date','Season','Year','Month','Hour','Holiday','Weekday','Workingday','Weather_condition','Temperature','Feelslike_temp','Humidity','Windspeed','Casual','Registered','Total_count')
head(bike_df)
categorical_col = c('Season', 'Year', 'Month', 'Hour','Holiday', 'Weekday','Workingday', 'Weather_condition')
bike_df[,categorical_col] = lapply(bike_df[,categorical_col], factor)
str(bike_df)
library(dplyr)
# Grouping the bike data by weekday and calculating the average rental count
bike_sharing_df_grouped_by_weekday <- bike_df %>%
  group_by(Weekday) %>%
  summarise(average_bike_rental_count = mean(Total_count))

# Print the grouped data
print(bike_sharing_df_grouped_by_weekday)

# Extracting the average bike rental counts vector
average_bike_rental_counts_vector <- bike_sharing_df_grouped_by_weekday$average_bike_rental_count
library(ggplot2)
ggplot(data = bike_sharing_df_grouped_by_weekday, aes(x = Weekday, y = average_bike_rental_count)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(
    title = "Average bike rental counts by day of the week",
    x = "Day of the week",
    y = "Average bike rental count"
  ) +
  theme_minimal()
# Weather Conditions


# Then, proceed with aggregating and plotting
weather_counts <- aggregate(bike_df$Total_count, by=list(bike_df$Weather_condition), FUN=sum)
print(weather_counts)

library(ggplot2)

ggplot(weather_counts, aes(x=Group.1, y=x)) +
  geom_bar(stat="identity") +
  labs(title="Bike Rental Counts by Weather Condition",
       x="Weather Condition",
       y="Total Rental Count")
#Month WIse
monthly_average_counts <- bike_df %>%
  group_by(Year, Month) %>%
  summarise(
    average_casual = mean(Casual),
    average_registered = mean(Registered)
  )

print(monthly_average_counts)

library(ggplot2)

monthly_average_counts$Month <- factor(monthly_average_counts$Month)

ggplot(monthly_average_counts, aes(x = Month, y = average_casual, group = Year, color = as.factor(Year))) +
  geom_line() +
  labs(
    title = "Monthly Average Casual Bike Rentals (Year-wise)",
    x = "Month",
    y = "Average Casual Rentals"
  ) +
  scale_x_discrete(labels = month.abb) +
  theme_minimal()

ggplot(monthly_average_counts, aes(x = Month, y = average_registered, group = Year, color = as.factor(Year))) +
  geom_line() +
  labs(
    title = "Monthly Average Registered Bike Rentals (Year-wise)",
    x = "Month",
    y = "Average Registered Rentals"
  ) +
  scale_x_discrete(labels = month.abb) +
  theme_minimal()
## 3

holiday_data <- subset(bike_df, Holiday == 1)
regular_data <- subset(bike_df, Holiday == 0)
t_test_result <- t.test(holiday_data$Total_count, regular_data$Total_count)
print(t_test_result)
ggplot(bike_df, aes(x = factor(Holiday), y = Total_count)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Bike Rentals on Holidays vs. Regular Days",
    x = "Day Type",
    y = "Total Rental Count"
  ) +
  theme_minimal()

# Hours 
hourly_average_counts <- bike_df %>%
  group_by(Hour) %>%
  summarise(average_rental_count = mean(Total_count))

# Create a bar plot to visualize the average bike rentals by hour
ggplot(hourly_average_counts, aes(x = factor(Hour), y = average_rental_count)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(
    title = "Average Bike Rentals by Hour of the Day",
    x = "Hour of the Day",
    y = "Average Rental Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

