library(readxl) 
userP_userI_this <- read_excel("C:/Users/milly/Downloads/userP_userI_this.xls") 
View(userP_userI_this) 
head(userP_userI_this)
names(userP_userI_this)
summary(userP_userI_this)
plot(userP_userI_this)
dev.off()
plot(userP_userI_this$averageHeartRate)  #categorical variable
plot( userPerf_userInfo3$avg(totalDistance),userPerf_userInfo3$timeOfDay) # error, go to lines 18-28 to see how it got fixed
plot(userPerf_userInfo3$timeOfDay)

# K means
install.packages("factoextra")

# 1 works
library(factoextra)

  # 1st test plot :

# Check for missing or infinite values
missing_values <- sum(is.na(userPerf_userInfo3$timeOfDay) | !is.finite(userPerf_userInfo3$timeOfDay))

if (missing_values > 0) {
  # Handle missing or infinite values (remove or impute)
  userPerf_userInfo3 <- userPerf_userInfo3[complete.cases(userPerf_userInfo3$timeOfDay), ]
}

# Proceed with plotting
barplot(table(userPerf_userInfo3$timeOfDay), main = " Plot 1", xlab = "X-Axis Label", ylab = "Y-Axis Label")

#3 works

# Install and load the dplyr package
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}

# Load the dplyr package
library(dplyr)

#install.packages("ggplot2")
library(ggplot2)


# Create a bar plot using ggplot2
print(ggplot(userPerf_userInfo3, aes(x = timeOfDay)) +
        geom_bar(fill = "lightblue") +
        labs(title = "Bar Plot of Time of Day", x = "Time of Day", y = "Count"))


# 4

library(readxl)
userPerf_userInfo3 <- read_excel("C:/Users/milly/Downloads/userPerf_userInfo3.xlsx")
View(userPerf_userInfo3)

# Assuming 'userPerf_userInfo3' is your data frame
# Assuming 'averageSpeed' is the column containing average speed
# Assuming 'averageHeartRate' is the column containing average heart rate

library(ggplot2)


# Create a scatter plot
ggplot(userPerf_userInfo3, aes(x = averageSpeed, y = averageHeartRate)) +
  geom_point(color = "blue", size = 3) +
  labs(title = "Scatter Plot of Average Speed vs. Average Heart Rate",
       x = "Average Speed",
       y = "Average Heart Rate")


#5 

# Check if a value is finite
is_finite_result <- is.finite(userPerf_userInfo3$totalDistance)
print(is_finite_result)  # FALSE

# Check if a value is infinite
is_infinite_result <- is.infinite(userPerf_userInfo3$totalDistance)
print(is_infinite_result)  # TRUE

# Identify missing values in a specific column
missing_values <- is.na(userPerf_userInfo3$timeOfDay)
# Count missing values in each column
col_missing <- colSums(is.na(userPerf_userInfo3))

# Remove rows with missing values
your_data_no_na <- userPerf_userInfo3[complete.cases(userPerf_userInfo3), ]

---

# Install and load the Hmisc package if not already installed
if (!requireNamespace("Hmisc", quietly = TRUE)) {
  install.packages("Hmisc")
}
library(Hmisc)

# Assuming 'userPerf_userInfo3' is your data frame
# Assuming 'your_column' is the column you want to impute

# Impute missing values with mean
userPerf_userInfo3$averageSpeed <- impute(userPerf_userInfo3$averageSpeed, mean)
userPerf_userInfo3$totalDistance <- impute(userPerf_userInfo3$totalDistance, mean)


#6



# Install and load necessary packages if not already installed
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)



# Create a scatter plot
ggplot(userPerf_userInfo3, aes(x = timeOfDay, y = averageHeartRate)) +
  geom_point(color = "blue", size = 3) +
  labs(title = "Scatter Plot of Average Heart Rate by Time of Day",
       x = "Time of Day",
       y = "Average Heart Rate")

#7 works

# Assuming 'userPerf_userInfo3' is your data frame
# Assuming 'timeCol' is the column containing time information
# Assuming 'timeOfDay' is the column containing time of day (as a string)

# Install and load necessary packages if not already installed
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(dplyr)
library(ggplot2)



# Create a scatter plot
ggplot(userPerf_userInfo3, aes(x = timeCol, y = as.factor(timeOfDay), color = timeOfDay)) +
  geom_point(size = 3) +
  labs(title = "Scatter Plot of Time of Day",
       x = "Time",
       y = "Time of Day",
       color = "Time of Day") +
  theme_minimal()

#8 # Assuming 'userPerf_userInfo3' is your data frame
# Assuming 'timeOfDay' is the column containing time of day (as a string)
# Assuming 'totalDistance' is the column containing total distance

# Install and load necessary packages if not already installed
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(dplyr)
library(ggplot2)



# Create a scatter plot
ggplot(userPerf_userInfo3, aes(x = timeOfDay, y = session_ID, color = timeOfDay)) +
  geom_point(size = 3) +
  labs(title = "Scatter Plot of Total Distance by Time of Day",
       x = "Time of Day",
       y = "sessions",
       color = "Time of Day") +
  theme_minimal()

# 9 
# Assuming 'userPerf_userInfo3' is your data frame
# Assuming 'timeOfDay' is the column containing time of day (as a string)
# Assuming 'averageHeartRate' is the column containing average heart rate
# Assuming 'totalDistance' is the column containing total distance

# Install and load necessary packages if not already installed
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(dplyr)
library(ggplot2)



# Filter for morning sessions
morning_data <- userPerf_userInfo3 %>%
  filter(timeOfDay == "morning")

# Create a scatter plot
ggplot(morning_data, aes(x = averageHeartRate, y = totalDistance)) +
  geom_point(size = 3) +
  labs(title = "Scatter Plot of Average Heart Rate vs. Total Distance for Morning Sessions",
       x = "Average Heart Rate",
       y = "Total Distance") +
  theme_minimal()



    # correlation analysis

str(userPerf_userInfo3[, c("totalDistance", "sessionDuration", "averageHeartRate")])

userPerf_userInfo3$totalDistance <- as.numeric(userPerf_userInfo3$totalDistance)
userPerf_userInfo3$sessionDuration <- as.numeric(userPerf_userInfo3$sessionDuration)
userPerf_userInfo3$averageHeartRate <- as.numeric(userPerf_userInfo3$averageHeartRate)

cor(userPerf_userInfo3[, c("totalDistance", "sessionDuration", "averageHeartRate")])


  # time series analysis

session_ID_totDistance$f0_ <- as.numeric(session_ID_totDistance$f0_)

session_ID_totDistance$session_ID <- as.numeric(session_ID_totDistance$session_ID)


plot(session_ID_totDistance$session_ID, session_ID_totDistance$f0_, type = "p", 
     xlab = "Session ID", ylab = "Total Distance", ylim = c(0, 300))


  # add weekday name

install.packages("lubridate")
library(lubridate)

userPerf_userInfo3$weekday_name <- weekdays(userPerf_userInfo3$dayDateCol)

  # bar chart : weekdays and number of sessions

# install.packages("ggplot2")
library(ggplot2)

# Assuming your_dataset has columns weekday_name and session_ID
userPerf_userInfo3 <- userPerf_userInfo3[, c("weekday_name", "session_ID")]


# Calculate the count of sessions for each weekday
session_counts <- table(userPerf_userInfo3$weekday_name)

# Create a bar chart
barplot(session_counts, col = "skyblue", main = "Number of Sessions by Weekday",
        xlab = "Weekday", ylab = "Number of Sessions",  ylim = c(0, 150))


  # average total distance per weekday 

# Assuming your_dataset has columns weekday_name, session_ID, and totalDistance
userPerf_userInfo3 <- userPerf_userInfo3[, c("weekday_name", "totalDistance")]

userPerf_userInfo3$totalDistance <- as.numeric(userPerf_userInfo3$totalDistance)


# Calculate the average totalDistance for each weekday
average_distance <- aggregate(totalDistance ~ weekday_name, data = userPerf_userInfo3, FUN = mean)

# Create a bar chart
ggplot(average_distance, aes(x = weekday_name, y = totalDistance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Average Total Distance by Weekday",
       x = "Weekday",
       y = "Average Total Distance")


----

  # total distance (grouped by sessions) against weekday days
  
  
# Load the dplyr package
library(dplyr)

# Convert totalDistance to numeric (if not done already)
newest_joined$totalDistance <- as.numeric(newest_joined$totalDistance)

# Aggregate data to calculate the sum of totalDistance for each session_ID and weekday_name
summed_data <- newest_joined %>%
  group_by(weekdayName, session_ID) %>%
  summarise(totalDistance = sum(totalDistance, na.rm = TRUE))

# Filter data where totalDistance is less than 4000
filtered_data <- summed_data[summed_data$totalDistance < 44000, ]

# Create a scatter plot
ggplot(filtered_data, aes(x = weekdayName, y = totalDistance)) +
  geom_point(color = "blue") +
  labs(title = "Scatter Plot of Total Distance by Weekday (Total Distance < 4000)",
       x = "Weekday",
       y = "Total Distance")





---
  
  # K means clustering
  
  
# step 1 : libraries
  library(dplyr)
  library(ggplot2)

# load the dataset; take note of its name; mine is 'newest_joined'

library(readxl)
newest_joined <- read_excel("C:/Users/milly/Downloads/newest_joined.xlsx")
View(newest_joined)

# in this dataset the columns are grouped according to session_ID. So each total and average and so on are calculated for that particular session
# a session here being an uninterrupted exercise. An idle state of more than 5 min would finish current session. 

# see the structure of loaded dataset:
str(newest_joined)


# checking for missing values , do some scaling 

  # 1. select columns for clustering

selected_columns <- newest_joined[, c( "totalDistance", "averageSpeed",  
                                       "averageHeartRate", 
                                      "sessionDuration", "totalCalories" )]


  # 2. remove rows with missing values
selected_columns <- na.omit(selected_columns)

  # 3. scale numeric variables only 

# Convert selected variables to numeric
numeric_columns <- c(
  "totalDistance", "averageSpeed",  
  "averageHeartRate", 
  "sessionDuration", "totalCalories"
)

selected_columns[numeric_columns] <- lapply(selected_columns[numeric_columns], as.numeric)


scaled_data <- scale(selected_columns[numeric_columns])

  # 4. check the structure of the scaled data
str(scaled_data)

# check if there are any missing values; if TRUE then need to impute (below)
any(is.na(scaled_data))
# replace infinite values with a large numeric value or another meaningful value based on your data distribution
scaled_data[is.na(scaled_data)] <- mean(scaled_data, na.rm = TRUE)

# check if any infinite values are detected; if TRUE then need to impute (replace)
any(!is.finite(scaled_data))
# replace infinite values with the mean
scaled_data[!is.finite(scaled_data)] <- apply(scaled_data, 2, function(x) ifelse(is.finite(x), x, mean(x, na.rm = TRUE)))

#NEXT:

  # 5. chose the optimal number of clusters 'K'

# Elbow method to find the optimal number of clusters (K)
wss <- numeric(10)

for (i in 1:10) {
  kmeans_model <- kmeans(scaled_data, centers = i, nstart = 20)
  wss[i] <- sum(kmeans_model$withinss)
}

# Plot the elbow curve
plot(1:10, wss, type = "b", pch = 19, frame = FALSE, main = "Elbow Method", 
     xlab = "Number of Clusters (K)", ylab = "Within-cluster Sum of Squares (WSS)")



# try K = 6
k <- 4
kmeans_model <- kmeans(scaled_data, centers = k, nstart = 10)

# Add cluster assignment to the original dataset
newest_joined$cluster <- as.factor(kmeans_model$cluster)

# Check the cluster distribution
table(newest_joined$cluster)


# FINALLY 
# visualize the results


# Load the tidyr package
library(tidyr)
library(dplyr)

# Summary statistics for each cluster
summary_by_cluster <- newest_joined %>%
  group_by(cluster) %>%
  summarise(across(everything(), mean, na.rm = TRUE))

# Visualize cluster characteristics
plot_cluster_characteristics <- function(data, cluster_var) {
  plot_data <- gather(data, key = "variable", value = "value", -all_of(cluster_var))
  ggplot(plot_data, aes(x = .data[[cluster_var]], y = value, fill = as.factor(.data[[cluster_var]]))) +
    geom_boxplot() +
    facet_wrap(~variable, scales = "free_y", ncol = 2) +
    labs(title = "Cluster Characteristics", x = "Cluster", y = "Value")
}

# Run the function
plot_cluster_characteristics(summary_by_cluster, "cluster")



  # visualize 

# Install and load the factoextra package
install.packages("factoextra")
library(factoextra)

# try K = 6
k <- 3
kmeans_model <- kmeans(scaled_data, centers = k, nstart = 10)

# Add cluster assignment to the original dataset
newest_joined$cluster <- as.factor(kmeans_model$cluster)

# Check the cluster distribution
table(newest_joined$cluster)

# Visualize cluster characteristics using factoextra
fviz_cluster(kmeans_model, data = scaled_data, geom = "point",
             stand = FALSE, # Don't standardize variables
             ellipse.type = "norm", # Use normal ellipses
             main = "Cluster Plot"
)

# Run the function
plot_cluster_characteristics(summary_by_cluster, "cluster")


---
  

  # Install and load the caret package
  install.packages("caret")
library(caret)

near_zero_var_cols <- nearZeroVar(scaled_data, saveMetrics = TRUE)$nzv
print(near_zero_var_cols)
scaled_data <- scaled_data[, -near_zero_var_cols]
  
remaining_near_zero_var_cols <- nearZeroVar(scaled_data, saveMetrics = TRUE)$nzv
print(remaining_near_zero_var_cols)

pca_result <- prcomp(scaled_data, scale. = FALSE, center = FALSE)
  

---
  
  # Cluster exaample 2
  
library(ggplot2)
library(dplyr)

# Assuming 'newest_joined' is your data frame
df <- newest_joined %>%
  mutate(sessDur = sessionDuration,
         dayofweek = weekdayName)
set.seed(123)  # Setting seed for reproducibility
sampled_df <- df[sample(nrow(df), 100), ]

# Now, you can use ggplot to create your scatter plot
ggplot(sampled_df, aes(x = sessDur, y = dayofweek)) +
  geom_point()

---

  # a rundown of session Durations for each day and time of day (for the whole dataset). It can be later narrowed down to 
  # a single week, like when showing a summary for last week etc
    
library(ggplot2)
library(dplyr)

# Assuming 'newest_joined' is your data frame
df <- newest_joined %>%
  mutate(sessionDuration = as.numeric(sessionDuration),
         timeOfDay = timeOfDay) %>%
  group_by(session_ID) %>%
  summarise(total_sessDur = sum(sessionDuration, na.rm = TRUE)) %>%
  left_join(newest_joined, by = "session_ID")

# Now, you can use ggplot to create your scatter plot
ggplot(df, aes(x = weekdayName, y = timeOfDay, size = total_sessDur)) +
  geom_point() +
  scale_size_continuous(range = c(1, 5)) +  # Adjust the range for desired size scaling
  labs(title = "Scatter Plot with Session Duration",
       x = "Day of Week",
       y = "Time of Day",
       size = "Total Session Duration") +
  theme_minimal()


---
  
  # for most recent  week only
  
library(ggplot2)
library(dplyr)

# Assuming 'newest_joined' is your data frame
df <- newest_joined %>%
  mutate(sessionDuration = as.numeric(sessionDuration),
         timeOfDay = timeOfDay,
         dayDateCol = as.Date(dayDateCol)) %>%
  filter(dayDateCol >= max(dayDateCol) - 6) %>%  # Filter for the most recent week
  group_by(session_ID) %>%
  summarise(total_sessDur = sum(sessionDuration, na.rm = TRUE)) %>%
  left_join(newest_joined, by = "session_ID")

# Now, you can use ggplot to create your scatter plot
ggplot(df, aes(x = weekdayName, y = timeOfDay, size = total_sessDur)) +
  geom_point() +
  scale_size_continuous(range = c(1, 10)) +  # Adjust the range for desired size scaling
  labs(title = "Scatter Plot with Session Duration",
       x = "Day of Week",
       y = "Time of Day",
       size = "Total Session Duration") +
  theme_minimal()


  # all days grouped into weeks against the total session duration. The size of the dot represents the number 
  # of sessions that week


library(ggplot2)
library(dplyr)

#  'newest_joined' is my dataset
df <- newest_joined %>%
  mutate(sessionDuration = as.numeric(sessionDuration),
         dayDateCol = as.Date(dayDateCol)) %>%
  filter(sessionDuration < 40000) %>%  # Filter sessionDuration as there were some outliers
  group_by(session_ID) %>%  # so that each session in that week contributes to the size of the dot on the graph
  summarise(total_sessDur = sum(sessionDuration, na.rm = TRUE),
            sessions_per_week = n_distinct(format(dayDateCol, "%V"))) %>%
  left_join(newest_joined, by = "session_ID")

# group by 7-day intervals and calculate total session duration and number of sessions per week
df_weekly <- df %>%
  mutate(week_interval = cut(dayDateCol, breaks = "1 week", labels = FALSE)) %>%
  group_by(week_interval) %>%
  summarise(total_sessDur_weekly = sum(total_sessDur, na.rm = TRUE),
            total_sessions_per_week = sum(sessions_per_week, na.rm = TRUE))

# use ggplot to create your scatter plot with session duration against weeks
ggplot(df_weekly, aes(x = as.factor(week_interval), y = total_sessDur_weekly, size = total_sessions_per_week)) +
  geom_point() +
  scale_size_continuous(range = c(1, 5)) +  # Adjust the range for desired size scaling
  labs(title = "Scatter Plot with Session Duration Against Weeks",
       x = "Week",
       y = "Total Session Duration",
       size = "Number of Sessions per Week") +
  theme_minimal() +
  scale_y_continuous(labels = function(x) paste0(floor(x / 60), "h"))



  # 
# install : https://cran.rstudio.com/bin/windows/Rtools/rtools43/rtools.html
# then :
install.packages("stats")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggfortify")

library(stats)
library(dplyr)
library(ggplot2)
library(ggfortify)

View(newest_joined)

# store all numeric (not char, not string in a separate data object called my_data)


                  # OPTION 1 ------------------------------------------------------------------------------------------
                 # my_data grouped by session_id

                  

                  # define wssplot function, where we grouped dataset by session_ID
                  wssplot <- function(data, nc = 15, seed = 1234) {
                    wss <- (nrow(data) - 1) * sum(apply(data[, -1], 2, var))
                    for (i in 2:nc) {
                      set.seed(seed)
                      wss[i] <- sum(kmeans(data[, -1], centers = i)$withinss)
                    }
                    plot(1:nc, wss, type = "b", xlab = "Number of clusters", ylab = "Within groups sum of squares")
                  }
                  
                  # prepare  data
                  my_data <- newest_joined %>%
                    select(1, 2, 4, 17, 6, 8,5) %>%   # column 17 is sessionDurationSeconds in seconds; column 5 is sessionDuration in decimal minutes
                    filter(totalDistance < 40000 & sessionDuration < 700) %>%
                    mutate_all(as.numeric)
                  
                  # Group by session_ID and calculate summary statistics
                  grouped_data <- my_data %>%
                    group_by(session_ID) %>%
                    summarise(
                      totDistance = sum(totalDistance),
                      avgSpeed = mean(averageSpeed),
                      avgHeartRate = mean(averageHeartRate),
                      sessDur = sum(sessionDurationSeconds),
                      totCalories = sum(totalCalories)
                    )
                  
                  #  wssplot to choose the optimum number of clusters
                  wssplot(grouped_data)
                  
                  # exclude session_ID for kmeans clustering
                  KM <- kmeans(grouped_data[, -2], 4) # exclude session_ID and sessionDuration here as we needed it for grouping only (last 2 columns)
                                                      # from line 611
                  # visualize the results
                  autoplot(KM, data = grouped_data, frame = TRUE)
                  
                  # view the cluster centers
                  KM$centers

                  # OPTION 1 END -------------------------------------------------------------------------------

                  
 # OPTION 2 ------- my_data as it is, not grouped by session_ID. Here a single session could have have multiple sub sessions 
                  # where idle time in between was less than 5 min. OPTION 1 would be then more realistic 
my_data <- newest_joined %>%
select(1, 2, 4, 5, 6) %>%
filter(totalDistance < 40000) %>%    # the outliers
filter(sessionDuration < 40000) %>%  # the outliers
mutate_all(as.numeric)   # have to be numeric for wss plot
                  
                  
                  
# chose the optimum number of clusters: use WSS Plot 
    # step 1 add the wssplot function
wssplot2 <- function(data, nc=15, seed = 1234)
{
  wss <- (nrow(data) -1)* sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers = i) $withinss)}
  plot(1:nc, wss, type="b", xlab= "Number of clusters", ylab= "within groups sum of squares")
  
}

    # step 2 - chose optimum number of clusters for the my_data - use wssplot function
wssplot2(my_data)

# my optimum is 6 or 8 could also do

# K- means Cluster

KM = kmeans(my_data, 6)

  # Evaluating cluster analysis
  # 1.  cluster plot - method 1
autoplot(KM,data = my_data, frame = TRUE)

  # 2.  cluster centres - method 2 

KM$centers # (cluster number from line 619)
# the centers of those clusters are not overlapping, although the averageSpeeds and averageHeartRates are indicative of 
# a distinct persons physical ability (similar average heart rate to average speed)

# clusters are distinct in nature




# ----------------------------------------
# before exporting:
# Function to convert decimal minutes to HH:MM:SS format

newest_joined <- subset(newest_joined, select = -sessionDurationFormatted2)  # Method 2


# Convert decimal minutes to seconds
newest_joined$sessionDurationSeconds <- round(newest_joined$sessionDuration * 60)

# Function to convert seconds to HH:MM:SS format
convert_seconds_to_hms <- function(seconds) {
  hours <- floor(seconds / 3600)
  minutes <- floor((seconds %% 3600) / 60)
  seconds <- seconds %% 60
  
  # Format the result as HH:MM:SS
  sprintf("%02d:%02d:%02d", hours, minutes, seconds)
}

# Apply the conversion function to the 'sessionDurationSeconds' column
newest_joined$sessionDurationFormatted3 <- sapply(newest_joined$sessionDurationSeconds, convert_seconds_to_hms)

# Print the result
print(newest_joined)

# ---------------------------------------

# export the results

library(dplyr)

# create a new data frame with all columns from 'newest_joined'
new_data_frame <- newest_joined %>% select(everything())

# Display the structure of the new data frame
#str(new_data_frame)


# from that data frame export to csv file

getwd() # current working directory that the csv file would be exported to
setwd("C:/Users/milly/OneDrive/Desktop")   # change working directory to Desktop
write.csv(new_data_frame, "performance_data_final.csv")


# DO THE SAME TO CLUSTERED RESULTS:

library(dplyr)

# create a new data frame with all columns from 'newest_joined'
new_data_frame2 <- my_data %>% select(everything())

# Display the structure of the new data frame
#str(new_data_frame2)


# from that data frame export to csv file

getwd() # current working directory that the csv file would be exported to
setwd("C:/Users/milly/OneDrive/Desktop")   # change working directory to Desktop
write.csv(new_data_frame, "clustered_data.csv")



--------

library(dplyr)

# k-means by grouped session_id
distinct_session_count <- new_data_frame %>% 
  filter(sessionDuration < 40000 & totalDistance < 40000) %>%
  summarise(distinct_sessions = n_distinct(session_ID))

# Print the result
print(distinct_session_count)


-----
  
# tableau 1
  

library(ggplot2)
library(dplyr)

# Assuming 'newest_joined' is your data frame
df33 <- newest_joined %>%
  mutate(sessionDuration = as.numeric(sessionDurationSeconds),
         timeOfDay = timeOfDay,
         dayDateCol = as.Date(dayDateCol)) %>%
  filter(dayDateCol >= max(dayDateCol) - 6) %>%  # Filter for the most recent week
  group_by(session_ID, weekdayName, timeOfDay) %>%
  summarise(total_sessDur = sum(sessionDurationSeconds, na.rm = TRUE)) %>%
  right_join(expand.grid(session_ID = unique(newest_joined$session_ID),
                         weekdayName = unique(newest_joined$weekdayName),
                         timeOfDay = unique(newest_joined$timeOfDay)),
             by = c("session_ID", "weekdayName", "timeOfDay")) %>%
  left_join(newest_joined, by = c("session_ID", "weekdayName", "timeOfDay")) %>%
  filter(sessionDurationSeconds < 2000000)  # Add filter condition

# Now, you can use ggplot to create your scatter plot
ggplot(df, aes(x = weekdayName, y = timeOfDay, size = total_sessDur)) +
  geom_point() +
  scale_size_continuous(range = c(1, 10)) +  # Adjust the range for desired size scaling
  labs(title = "Scatter Plot with Session Duration",
       x = "Day of Week",
       y = "Time of Day",
       size = "Total Session Duration") +
  theme_minimal()


# create a new data frame with all columns from 'newest_joined'
new_data_frame <- df33 %>% select(everything())

# Display the structure of the new data frame
#str(new_data_frame)


# from that data frame export to csv file

getwd() # current working directory that the csv file would be exported to
setwd("C:/Users/milly/OneDrive/Desktop")   # change working directory to Desktop
write.csv(new_data_frame, "first_dashboard.csv")
  

  
  






















