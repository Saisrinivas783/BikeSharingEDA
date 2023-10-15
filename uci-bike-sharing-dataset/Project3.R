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

#Casual VS Registered Users
# Create a year-wise distribution plot for Casual users with unique year-based colors
casual_plot <- ggplot(bar_yearly_totals, aes(x = factor(Year), y = Casual_Total, fill = factor(Year))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("orange", "purple")) +  # Manually specifying the colors
  labs(title = "Year-wise Distribution of Casual Users", x = "Year", y = "Casual Count", fill = "Year")

# Customize the y-axis labels to display in standard numeric notation
casual_plot <- casual_plot + scale_y_continuous(labels = comma)

# Create a year-wise distribution plot for Registered users with unique year-based colors
registered_plot <- ggplot(bar_yearly_totals, aes(x = factor(Year), y = Registered_Total, fill = factor(Year))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("orange", "purple")) +  # Manually specifying the colors
  labs(title = "Year-wise Distribution of Registered Users", x = "Year", y = "Registered Count", fill = "Year")

# Customize the y-axis labels to display in standard numeric notation
registered_plot <- registered_plot + scale_y_continuous(labels = comma)

# Display the two plots side by side
grid.arrange(casual_plot, registered_plot, ncol = 2)
#==========================================================
filtered_data <- bike_df %>% filter(Year == 0)
filtered_data1 <- bike_df %>% filter(Year == 1)



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




