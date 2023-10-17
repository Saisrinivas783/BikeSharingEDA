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
str(bike_df)

#libraries used
library(ggplot2)
library(dplyr)
library(scales)
library(gridExtra)
library(grid)


bar_yearly_totals <- bike_df %>%
  group_by(Year) %>%
  summarize(Casual_Total = sum(Casual), Registered_Total = sum(Registered))

# Create a year-wise distribution plot for Casual users with unique year-based colors
casual_plot <- ggplot(bar_yearly_totals, aes(x = factor(Year), y = Casual_Total, fill = factor(Year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Year-wise Distribution of Casual Users", x = "Year", y = "Casual Count", fill = "Year")

# Customize the y-axis labels to display in standard numeric notation
casual_plot <- casual_plot + scale_y_continuous(labels = comma)

# Create a year-wise distribution plot for Registered users with unique year-based colors
registered_plot <- ggplot(bar_yearly_totals, aes(x = factor(Year), y = Registered_Total, fill = factor(Year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Year-wise Distribution of Registered Users", x = "Year", y = "Registered Count", fill = "Year")

# Customize the y-axis labels to display in standard numeric notation
registered_plot <- registered_plot + scale_y_continuous(labels = comma)

# Display the two plots side by side
grid.arrange(casual_plot, registered_plot, ncol = 2)
#==========================================================
filtered_data <- bike_df %>% filter(Year == 0)
filtered_data1 <- bike_df %>% filter(Year == 1)


# Calculate the total Casual and Registered counts for the filtered data
yearly_totals <- filtered_data %>%
  group_by(Year) %>%
  summarize(Casual_Total = sum(Casual), Registered_Total = sum(Registered))
yearly_totals1 <- filtered_data1 %>%
  group_by(Year) %>%
  summarize(Casual_Total1 = sum(Casual), Registered_Total1 = sum(Registered))

# Calculate the percentages for Casual and Registered users
yearly_totals$Casual_Percentage <- (yearly_totals$Casual_Total / (yearly_totals$Casual_Total + yearly_totals$Registered_Total)) * 100
yearly_totals$Registered_Percentage <- (yearly_totals$Registered_Total / (yearly_totals$Casual_Total + yearly_totals$Registered_Total)) * 100

yearly_totals1$Casual_Percentage1 <- (yearly_totals1$Casual_Total1 / (yearly_totals1$Casual_Total1 + yearly_totals1$Registered_Total1)) * 100
yearly_totals1$Registered_Percentage1 <- (yearly_totals1$Registered_Total1 / (yearly_totals1$Casual_Total1 + yearly_totals1$Registered_Total1)) * 100
# Colors for the pie chart slices
pie_colors <- c("lightblue", "lightgreen")

# Create a pie chart for Year 0
labels <- c(
  paste("Casual: ", scales::percent(yearly_totals$Casual_Percentage / 100), sep = ""),
  paste("Registered: ", scales::percent(yearly_totals$Registered_Percentage / 100), sep = "")
)
pie(c(yearly_totals$Casual_Percentage, yearly_totals$Registered_Percentage), labels = labels, col = pie_colors, main = "Year 0: Casual and Registered Users")

labels <- c(
  paste("Casual: ", scales::percent(yearly_totals1$Casual_Percentage1 / 100), sep = ""),
  paste("Registered: ", scales::percent(yearly_totals1$Registered_Percentage1 / 100), sep = "")
)
pie(c(yearly_totals1$Casual_Percentage1, yearly_totals1$Registered_Percentage1), labels = labels, col = pie_colors, main = "Year 1: Casual and Registered Users")



#==========================================================
# Hour Of the Day v/s Count

hourly_total_counts <- bike_df %>%
  group_by(Hour) %>%
  summarise(total_rental_count = sum(Total_count) / 1000)

hourly_casual_counts <- bike_df %>%
  group_by(Hour) %>%
  summarise(casual_count = sum(Casual) / 1000)
hourly_registered_counts <- bike_df %>%
  group_by(Hour) %>%
  summarise(registered_count = sum(Registered) / 1000)
grid.arrange(
  ggplot(hourly_casual_counts, aes(x = factor(Hour), y = casual_count)) +
    geom_bar(stat = "identity", fill = "lightgreen") +
    labs(
      title = "Casual Bike Rentals by Hourly",
      x = "Hour of the Day",
      y = "Casual Count (Thousands)"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)),
  
  ggplot(hourly_registered_counts, aes(x = factor(Hour), y = registered_count)) +
    geom_bar(stat = "identity", fill = "orange") +
    labs(
      title = "Registered Bike Rentals by Hourly",
      x = "Hour of the Day",
      y = "Registered Count (Thousands)"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)),
  ncol=2
)

ggplot(hourly_total_counts, aes(x = factor(Hour), y = total_rental_count)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(
    title = "Total Bike Rentals by Hour of the Day",
    x = "Hour of the Day",
    y = "Total Rental Count (Thousands)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Day of the Week v/s Total Casual User Count
casual_by_weekday <- bike_df %>%
  group_by(Weekday) %>%
  summarise(total_casual_count = sum(Casual/1000)) 
#===================================================

# Day of the Week v/s Total Casual User Count
casual_by_weekday <- bike_df %>%
  group_by(Weekday) %>%
  summarise(total_casual_count = sum(Casual/1000)) 

# Day of the Week v/s Total Registered User Count
registered_by_weekday <- bike_df %>%
  group_by(Weekday) %>%
  summarise(total_registered_count = sum(Registered/1000))

# Define the order of weekdays for proper sorting in the bar chart
weekday_order <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

# Create the bar charts
grid.arrange(
  ggplot(casual_by_weekday, aes(x = factor(Weekday, levels = 0:6), y = total_casual_count)) +
    geom_bar(stat = "identity", fill = "orange") +
    labs(
      title = "Casual User Bike  Counts by Day of the Week",
      x = "Day of the Week",
      y = "Total Casual User Count(In Thousands)"
    ) +
    scale_x_discrete(labels = weekday_order) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)),
  
  ggplot(registered_by_weekday, aes(x = factor(Weekday, levels = 0:6), y = total_registered_count)) +
    geom_bar(stat = "identity", fill = "lightgreen") +
    labs(
      title = "Registered User Bike  Counts by Day of the Week",
      x = "Day of the Week",
      y = "Total Registered User Count(In Thousands)"
    ) +
    scale_x_discrete(labels = weekday_order) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)), ncol=2)


#====================================================
#Month-wise
grid.arrange(
  ggplot(bike_df, aes(x = factor(Month, labels = month.abb), y = Casual)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    labs(
      x = "Month",
      y = "Casual Count",
      title = "Month-wise Casual User Bike Sharing "
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)),
  
  ggplot(bike_df, aes(x = factor(Month, labels = month.abb), y = Registered/1000)) +
    geom_bar(stat = "identity", fill = "orange") +
    labs(
      x = "Month",
      y = "Registered Count(in thousands)",
      title = "Month-wise Registered User Bike Sharing "
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)),
  ncol=2
)
#====================================================
#Season-Wise


bike_df$Season <- factor(bike_df$Season, levels = c(1, 2, 3, 4), labels = c("spring", "summer", "fall", "winter"))

# Create the bar plot
ggplot(bike_df, aes(x = Season, y = Total_count/1000, fill = Season)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    x = "Season",
    y = "Count(In thousands)",
    title = "Counts of Bike Rentals by Season"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("spring" = "brown", "summer" = "red", "fall" = "orange", "winter" = "skyblue"))

#================================================================

# Weather

weather_names <- c("Clear, Few clouds", "Mist + Cloudy", "Light Snow, Light Rain", "Heavy Rain + Ice Pallets")

# Create a bar plot with weather condition names
ggplot(bike_df, aes(x = factor(Weather_condition, labels = weather_names), y = Total_count, fill = factor(Season))) +
  geom_col() +
  theme_bw() +
  labs(
    x = 'Weather Condition',
    y = 'Total Count',
    title = 'Weather Condition Distribution of Counts'
  )
#=============================================================
