# Installing and loading the packages:

install.packages("tidyverse")
library(tidyverse)
install.packages("lubridate")
library(lubridate)
install.packages("ggplot2")
library(ggplot2)
install.packages("showtext")
library(showtext)
library(scales)

# Reading the data sets "divvy-tripdata":

df2108 <- read.csv("raw_data/202108-divvy-tripdata.csv")
df2109 <- read.csv("raw_data/202109-divvy-tripdata.csv")
df2110 <- read.csv("raw_data/202110-divvy-tripdata.csv")
df2111 <- read.csv("raw_data/202111-divvy-tripdata.csv")
df2112 <- read.csv("raw_data/202112-divvy-tripdata.csv")
df2201 <- read.csv("raw_data/202201-divvy-tripdata.csv")
df2202 <- read.csv("raw_data/202202-divvy-tripdata.csv")
df2203 <- read.csv("raw_data/202203-divvy-tripdata.csv")
df2204 <- read.csv("raw_data/202204-divvy-tripdata.csv")
df2205 <- read.csv("raw_data/202205-divvy-tripdata.csv")
df2206 <- read.csv("raw_data/202206-divvy-tripdata.csv")
df2207 <- read.csv("raw_data/202207-divvy-tripdata.csv")


# Binding all the tables

df <- rbind(df2108, df2109, df2110, df2111, df2112, df2201, df2202, df2203,
            df2204, df2205, df2206, df2207)


# PREPARE:

# Changing all blank values to missing values (NA):

df[df == ""] <- NA

# Checking for columns with missing values:

for (i in 1:13){
  n_row <- nrow(subset(df, is.na(df[i])))
  if (n_row != 0){
    message(colnames(df[i]), " ", n_row)
  }
}


# Checking for duplicated rows:

length(unique(df$ride_id))

# Checking for misspelling in rideable_type and member_casual columns:

unique(df$rideable_type)
unique(df$member_casual)


# PROCESS:

# Removing rows with missing values in end_lat or end_lng:

df <- subset(df, !is.na(df$end_lat) & !is.na(df$end_lng))

# Removing rows with classic_bike in rideable_type column and
# missing values in four station columns:

df_class_mis_st <- subset(df, is.na(df$start_station_name) & df$rideable_type == "classic_bike"
                     | is.na(df$start_station_id) & df$rideable_type == "classic_bike"
                     | is.na(df$end_station_name) & df$rideable_type == "classic_bike"
                     | is.na(df$end_station_id) & df$rideable_type == "classic_bike")

df <- anti_join(df, df_class_mis_st, by="ride_id")

# Creating data.frame with customer types:
df_customer_type <- data.frame(customer_type = df$member_casual)

# Creating data.frame with rideable types:
df_rideable_type <- data.frame(rideable_type = df$rideable_type)

# Creating data.frame with time data:
df_temp <- data.frame(started_at = df$started_at, ended_at = df$ended_at)

# Separating the time information in Year, Month, Day, Weekday, Time_started, Time_ended and Duration columns:
df_temp$year <- as.numeric(format(as.Date(df_temp$started_at), format = "%Y"))
df_temp$month <- as.numeric(format(as.Date(df_temp$started_at), format = "%m"))
df_temp$day <- as.numeric(format(as.Date(df_temp$started_at), format = "%d"))
df_temp$weekday <- wday(df_temp$started_at, label=TRUE, abbr=FALSE, locale = "en")
df_temp$time_started <- format(as.POSIXct(df_temp$started_at), format = "%H:%M:%S")
df_temp$time_ended <- format(as.POSIXct(df_temp$ended_at), format = "%H:%M:%S")
df_temp$duration <- as.numeric(difftime(as.POSIXct(df_temp$ended_at), 
                                        as.POSIXct(df_temp$started_at), units="mins"))

df_time <- data.frame(year = df_temp$year, month = df_temp$month, day = df_temp$day, 
                      weekday = df_temp$weekday, time_started = df_temp$time_started,
                      time_ended = df_temp$time_ended, duration_mins = round(df_temp$duration))

# Creating data.frame with station's information:
df_station <- data.frame(start_station_name = df$start_station_name, start_station_id
                         = df$start_station_id, end_station_name =df$end_station_name,
                         end_station_id = df$end_station_id)


# ANALYZE:

# Comparing customer type with number of rides:

customer_vs_no_rides <- data.frame(matrix(ncol = 2, nrow = 2))
col_names <- c("customer_type", "percentage_of_rides")
colnames(customer_vs_no_rides) <- col_names
customer_types <- unique(df_customer_type)

for(i in 1:2){
  customer_vs_no_rides[i, 1] <- customer_types[i, 1]
  customer_vs_no_rides[i, 2] <- round(nrow(filter(df_customer_type,df_customer_type[1]
                                            == customer_types[i, 1])) / nrow(df_customer_type)
                                      * 100, digits = 2)
  
}

# Comparing customer types with number of rides by month:

df_customer_month <- data.frame(customer_type = df_customer_type$customer_type, month = df_time$month)

month_vs_customer <- data.frame(matrix(ncol = 4, nrow = 12))
col_names <- c("months", "casual", "member", "total_rides")
colnames(month_vs_customer) <- col_names
months <- month.name[unique(df_time$month)]

for (i in 1:5) {
  temp <- nrow(filter(df_customer_month, df_customer_month$month == i + 7))
  month_vs_customer[i, 1] <- months[i]
  month_vs_customer[i, 2] <- nrow(filter(df_customer_month, df_customer_month$customer_type 
                                         == "casual", df_customer_month$month == i + 7))
  month_vs_customer[i, 3] <- nrow(filter(df_customer_month, df_customer_month$customer_type 
                                         == "member", df_customer_month$month == i + 7))
  month_vs_customer[i, 4] <- temp
}

for (i in 6:12) {
  temp <- nrow(filter(df_customer_month, df_customer_month$month == i - 5))
  month_vs_customer[i, 1] <- months[i]
  month_vs_customer[i, 2] <- nrow(filter(df_customer_month, df_customer_month$customer_type 
                                                   == "casual", df_customer_month$month == i - 5))
  month_vs_customer[i, 3] <- nrow(filter(df_customer_month, df_customer_month$customer_type 
                                               == "member", df_customer_month$month == i - 5))
  month_vs_customer[i, 4] <- temp
}


# Comparing customer type with number of rides by weekday:

df_customer_weekday <- data.frame(customer_type = df_customer_type$customer_type, weekday = as.numeric(df_time$weekday))

weekday_vs_customer <- data.frame(matrix(ncol = 4, nrow = 7))
col_names <- c("weekday", "casual", "member", "total_rides")
row_names <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
colnames(weekday_vs_customer) <- col_names
weekdays <- as.numeric(unique(df_time$weekday))

for (i in 1:7){
  temp <- nrow(filter(df_customer_weekday, df_customer_weekday$weekday == i))
  weekday_vs_customer[i, 1] <- row_names[i]
  weekday_vs_customer[i, 2] <- nrow(filter(df_customer_weekday, df_customer_weekday$customer_type 
                                                 == "casual", df_customer_weekday$weekday == i))
  weekday_vs_customer[i, 3] <- nrow(filter(df_customer_weekday, df_customer_weekday$customer_type 
                                                 == "member", df_customer_weekday$weekday == i))
  weekday_vs_customer[i, 4] <- temp
}


# Comparing customer type with number of rides by duration of the ride:

df_customer_duration <- data.frame(customer_type = df_customer_type$customer_type, duration = df_time$duration)

customer_vs_duration <- data.frame(matrix(ncol = 4, nrow = 2))
col_names <- c("customer_type", "all_rides", "below_30_min", "above_30_min")
colnames(customer_vs_duration) <- col_names
customers <- unique(df_customer_duration$customer_type)

for (i in 1:2){
  customer_vs_duration[i, 1] <- customers[i]
  customer_vs_duration[i, 2] <- round(mean(subset(df_customer_duration$duration,
                                                  df_customer_duration$customer_type
                                                  == customers[i])))
  customer_vs_duration[i, 3] <- round(mean(subset(df_customer_duration$duration,
                                                  df_customer_duration$customer_type
                                                  == customers[i] & df_customer_duration$duration <= 30)))
  customer_vs_duration[i, 4] <- round(mean(subset(df_customer_duration$duration,
                                                  df_customer_duration$customer_type
                                                  == customers[i] & df_customer_duration$duration > 30)))
}


# Comparing customer type with percentage of the rides below and above 30 minutes:

customer_vs_percentage_duration <- data.frame(matrix(ncol = 3, nrow = 2))
col_names <- c("customer_type", "below_30_min", "above_30_min")
colnames(customer_vs_percentage_duration) <- col_names

for (i in 1:2){
  customer_vs_percentage_duration[i, 1] <- customers[i]
  customer_vs_percentage_duration[i, 2] <- round( nrow(subset(df_customer_duration, 
                                                              df_customer_duration$customer_type == customers[i]
                                                              & df_customer_duration$duration <= 30))
                                                  / nrow(subset(df_customer_duration,
                                                                df_customer_duration$customer_type == customers[i]))
                                                  * 100, digits=2)
  
  customer_vs_percentage_duration[i, 3] <- round(nrow(subset(df_customer_duration,
                                                             df_customer_duration$customer_type == customers[i]
                                                             & df_customer_duration$duration > 30))
                                                 / nrow(subset(df_customer_duration,
                                                               df_customer_duration$customer_type == customers[i]))
                                                 * 100, digits = 2)
}

# Comparing customer type with number of rides by rideable type:

df_customer_rideable <- data.frame(customer_type = df_customer_type$customer_type,
                                   rideable_type = df_rideable_type$rideable_type)

customer_vs_rideable <- data.frame(matrix(ncol = 4, nrow = 2))
col_names <- c("customer_type", "electric_bike", "classic_bike", "docked_bike")
colnames(customer_vs_rideable) <- col_names
customers <- unique(df_customer_rideable$customer_type)
bikes <- unique(df_customer_rideable$rideable_type)

for (i in 1:2){
  temp <- nrow(filter(df_customer_rideable, df_customer_rideable$customer_type == customers[i]))
  customer_vs_rideable[i, 1] <- customers[i]
  for (j in 2:4){
    customer_vs_rideable[i, j] <- round(nrow(filter(df_customer_rideable, 
                                              df_customer_rideable$customer_type
                                              == customers[i] & df_customer_rideable$rideable_type
                                              == bikes[j-1])) / temp * 100, digits = 2)
  }
}


# Comparing customer type with number of rides by stations:

df_customer_station <- data.frame(customer_type = df_customer_type$customer_type,
                                   start_station_name = df_station$start_station_name,
                                  end_station_name = df_station$end_station_name)

start_stations <- unique(df_customer_station$start_station_name)
end_stations <- unique(df_customer_station$end_station_name)
stations <- unique(c(start_stations, end_stations))
customers <- unique(df_customer_station$customer_type)

customer_vs_station <- data.frame(matrix(ncol = 3, nrow = length(stations)))
col_names <- c("stations", "member", "casual")
colnames(customer_vs_station) <- col_names

for (i in 1:length(stations)) {
  if (is.na(stations[i])) {
    customer_vs_station[i, 1] <- NA
    for (j in 1:2){
      customer_vs_station[i, j + 1] <- nrow(subset(df_customer_station, df_customer_station$customer_type == customers[j] &
                                                     is.na(df_customer_station$start_station_name))) + nrow(
                                                       subset(df_customer_station, df_customer_station$customer_type
                                                              == customers[j] & is.na(df_customer_station$end_station_name)))
    }
  }
  else {
    customer_vs_station[i, 1] <- stations[i]
    for (k in 1:2){
      customer_vs_station[i, k + 1] <- nrow(subset(df_customer_station, df_customer_station$customer_type == customers[k] &
                                                     df_customer_station$start_station_name == stations[i])) + nrow(
                                                       subset(df_customer_station, df_customer_station$customer_type ==
                                                                customers[k] & df_customer_station$end_station_name
                                                              == stations[i]))
    }
  }
}

casual_vs_top_station <- data.frame(stations = customer_vs_station$stations, visits = customer_vs_station$casual)
casual_vs_top_station <- casual_vs_top_station[order(casual_vs_top_station$visits, decreasing = TRUE), ]
casual_vs_top_station <- casual_vs_top_station[c(2:21), ]

member_vs_top_station <- data.frame(stations = customer_vs_station$stations, visits = customer_vs_station$member)
member_vs_top_station <- member_vs_top_station[order(member_vs_top_station$visits, decreasing = TRUE), ]
member_vs_top_station <- member_vs_top_station[c(2:21), ]

# Comparing casual customer with number of rides below and above 30 min by stations:
df_customer_station_duration <- data.frame(customer_type = df_customer_type$customer_type,
                                  start_station_name = df_station$start_station_name,
                                  end_station_name = df_station$end_station_name, duration =
                                    df_time$duration_mins)

casual_station_vs_duration <- data.frame(matrix(ncol = 3, nrow = length(stations)))
col_names <- c("stations", "below_30_min", "above_30_min")
colnames(casual_station_vs_duration) <- col_names

for (i in 1:length(stations)) {
  if (is.na(stations[i])) {
    casual_station_vs_duration[i, 1] <- NA
    casual_station_vs_duration[i, 2] <- nrow(subset(df_customer_station_duration, df_customer_station_duration$customer_type == "casual" & is.na(df_customer_station_duration$start_station_name) & df_customer_station_duration$duration <= 30)) + nrow(subset(df_customer_station_duration, df_customer_station_duration$customer_type == "casual" & is.na(df_customer_station_duration$end_station_name) & df_customer_station_duration$duration <= 30))
    casual_station_vs_duration[i, 3] <- nrow(subset(df_customer_station_duration, df_customer_station_duration$customer_type == "casual" & is.na(df_customer_station_duration$start_station_name) & df_customer_station_duration$duration > 30)) + nrow(subset(df_customer_station_duration, df_customer_station_duration$customer_type == "casual" & is.na(df_customer_station_duration$end_station_name) & df_customer_station_duration$duration > 30))
  }
  else {
    casual_station_vs_duration[i, 1] <- stations[i]
    casual_station_vs_duration[i, 2] <- nrow(subset(df_customer_station_duration, df_customer_station_duration$customer_type == "casual" & df_customer_station_duration$start_station_name == stations[i] & df_customer_station_duration$duration <= 30)) + nrow(subset(df_customer_station_duration, df_customer_station_duration$customer_type == "casual" & df_customer_station_duration$end_station_name == stations[i] & df_customer_station_duration$duration <= 30))
    casual_station_vs_duration[i, 3] <- nrow(subset(df_customer_station_duration, df_customer_station_duration$customer_type == "casual" & df_customer_station_duration$start_station_name == stations[i] & df_customer_station_duration$duration > 30)) + nrow(subset(df_customer_station_duration, df_customer_station_duration$customer_type == "casual" & df_customer_station_duration$end_station_name == stations[i] & df_customer_station_duration$duration > 30))
  }
}

casual_vs_top_station_below <- data.frame(stations = casual_station_vs_duration$stations, below_30_min = casual_station_vs_duration$below_30_min)
casual_vs_top_station_below <- casual_vs_top_station_below[order(casual_vs_top_station_below$below_30_min, decreasing = TRUE), ]
casual_vs_top_station_below <- casual_vs_top_station_below[c(2:21), ]

casual_vs_top_station_above <- data.frame(stations = casual_station_vs_duration$stations, above_30_min = casual_station_vs_duration$above_30_min)
casual_vs_top_station_above <- casual_vs_top_station_above[order(casual_vs_top_station_above$above_30_min, decreasing = TRUE), ]
casual_vs_top_station_above <- casual_vs_top_station_above[c(2:21), ]



# Comparing members with number of rides below and above 30 min by stations:
member_station_vs_duration <- data.frame(matrix(ncol = 3, nrow = length(stations)))
col_names <- c("stations", "below_30_min", "above_30_min")
colnames(member_station_vs_duration) <- col_names

for (i in 1:length(stations)) {
  if (is.na(stations[i])) {
    member_station_vs_duration[i, 1] <- NA
    member_station_vs_duration[i, 2] <- nrow(subset(df_customer_station_duration, df_customer_station_duration$customer_type == "member" & is.na(df_customer_station_duration$start_station_name) & df_customer_station_duration$duration <= 30)) + nrow(subset(df_customer_station_duration, df_customer_station_duration$customer_type == "member" & is.na(df_customer_station_duration$end_station_name) & df_customer_station_duration$duration <= 30))
    member_station_vs_duration[i, 3] <- nrow(subset(df_customer_station_duration, df_customer_station_duration$customer_type == "member" & is.na(df_customer_station_duration$start_station_name) & df_customer_station_duration$duration > 30)) + nrow(subset(df_customer_station_duration, df_customer_station_duration$customer_type == "member" & is.na(df_customer_station_duration$end_station_name) & df_customer_station_duration$duration > 30))
  }
  else {
    member_station_vs_duration[i, 1] <- stations[i]
    member_station_vs_duration[i, 2] <- nrow(subset(df_customer_station_duration, df_customer_station_duration$customer_type == "member" & df_customer_station_duration$start_station_name == stations[i] & df_customer_station_duration$duration <= 30)) + nrow(subset(df_customer_station_duration, df_customer_station_duration$customer_type == "member" & df_customer_station_duration$end_station_name == stations[i] & df_customer_station_duration$duration <= 30))
    member_station_vs_duration[i, 3] <- nrow(subset(df_customer_station_duration, df_customer_station_duration$customer_type == "member" & df_customer_station_duration$start_station_name == stations[i] & df_customer_station_duration$duration > 30)) + nrow(subset(df_customer_station_duration, df_customer_station_duration$customer_type == "member" & df_customer_station_duration$end_station_name == stations[i] & df_customer_station_duration$duration > 30))
  }
}

member_vs_top_station_below <- data.frame(stations = member_station_vs_duration$stations, below_30_min = member_station_vs_duration$below_30_min)
member_vs_top_station_below <- member_vs_top_station_below[order(member_vs_top_station_below$below_30_min, decreasing = TRUE), ]
member_vs_top_station_below <- member_vs_top_station_below[c(2:21), ]

member_vs_top_station_above <- data.frame(stations = member_station_vs_duration$stations, above_30_min = member_station_vs_duration$above_30_min)
member_vs_top_station_above <- member_vs_top_station_above[order(member_vs_top_station_above$above_30_min, decreasing = TRUE), ]
member_vs_top_station_above <- member_vs_top_station_above[c(2:21), ]


# SHARE:

# Loading font:

font_add_google("Montserrat")
showtext_auto()

# Creating graph  to compare customer type with number of rides:
ggplot(customer_vs_no_rides, aes(customer_type, percentage_of_rides, fill = customer_type)) + 
  geom_col(position = "dodge") + scale_fill_manual(values = c("casual" = "#3CB4E5",
                                                              "member" = "#0C0B31")) +
  geom_text(aes(label=percentage_of_rides), vjust=-0.5, color="#2D2A26",
            family = "Montserrat", fontface = "bold", position = position_dodge(0.9),
            size=3.5) +
  ggtitle("Customer vs. number of rides") + xlab("Customer Type") +
  ylab("Percentage of Total") +
  theme(legend.position="none", 
    plot.title = element_text(family = "Montserrat", face = "bold", 
                              size = 10, colour = "#2D2A26"),
    axis.title.x = element_text(family = "Montserrat", size = 8,
                                face = "bold", colour = "#2D2A26"),
    axis.text.x = element_text(family = "Montserrat", size = 8,
                               face = "bold", colour = "#2D2A26"),
    axis.title.y = element_text(family = "Montserrat", size = 8,
                                face = "bold", colour = "#2D2A26"),
    axis.text.y = element_text(family = "Montserrat", size = 8,
                               face = "bold", colour = "#2D2A26"),
  )


# Creating graph  to compare customer types with number of rides by month:
month_vs_customer$months <- factor(month_vs_customer$months, levels = month_vs_customer$months)

ggplot(month_vs_customer) + 
  geom_bar(aes(x= months, y=total_rides, fill = total_rides), stat = "identity", show.legend = FALSE) +
  scale_fill_gradient2(low="white", high="blue") +
  geom_line(aes(x=months, y=casual, colour = "Casual"), size = 2, group=1) +
  geom_line(aes(x=months, y=member, colour = "Member"), size = 2, group=1) +
  scale_colour_manual("", values = c("Casual"="#3CB4E5", "Member"="#0C0B31")) +
  labs(title= "Customer Type vs. Month rides", subtitle = "From August 2021 to July 2022", x="Months",y="Total Rides") +
  scale_y_continuous(labels = comma) +
  theme(plot.title = element_text(family = "Montserrat", size = 10,
                                  face = "bold", colour = "#2D2A26"),
        plot.subtitle = element_text(family = "Montserrat", size = 8,
                                  face = "bold", colour = "#2D2A26"),
        axis.title.x = element_text(family = "Montserrat", size = 8,
                                    face = "bold", colour = "#2D2A26"),
        axis.text.x = element_text(family = "Montserrat", size = 8,
                                    face = "bold", colour = "#2D2A26", angle = 45),
        axis.title.y = element_text(family = "Montserrat", size = 8,
                                    face = "bold", colour = "#2D2A26"),
        axis.text.y = element_text(family = "Montserrat", size = 8,
                                   face = "bold", colour = "#2D2A26"),
        legend.text = element_text(family = "Montserrat", size = 8,
                                   face = "bold", colour = "#2D2A26"))
  
  
# Creating graph  to compare customer type with number of rides by weekday:
weekday_vs_customer$weekday <- factor(weekday_vs_customer$weekday, levels = weekday_vs_customer$weekday)

ggplot(weekday_vs_customer) + 
  geom_bar(aes(x= weekday, y=total_rides, fill = total_rides), stat = "identity", show.legend = FALSE) +
  scale_fill_gradient2(low="white", high="blue") +
  geom_line(aes(x=weekday, y=casual, colour = "Casual"), size = 2, group=1) +
  geom_line(aes(x=weekday, y=member, colour = "Member"), size = 2, group=1) +
  scale_colour_manual("", values = c("Casual"="#3CB4E5", "Member"="#0C0B31")) +
  labs(title= "Customer Type vs. Weekday rides", x="Months",y="Total Rides") +
  scale_y_continuous(labels = comma) +
  theme(plot.title = element_text(family = "Montserrat", size = 10,
                                  face = "bold", colour = "#2D2A26"),
        axis.title.x = element_text(family = "Montserrat", size = 8,
                                    face = "bold", colour = "#2D2A26"),
        axis.text.x = element_text(family = "Montserrat", size = 8,
                                   face = "bold", colour = "#2D2A26", angle = 45),
        axis.title.y = element_text(family = "Montserrat", size = 8,
                                    face = "bold", colour = "#2D2A26"),
        axis.text.y = element_text(family = "Montserrat", size = 8,
                                   face = "bold", colour = "#2D2A26"),
        legend.text = element_text(family = "Montserrat", size = 8,
                                   face = "bold", colour = "#2D2A26"))


# Creating graph  to compare customer type with number of rides by duration of the ride:
new_customer_vs_duration <- customer_vs_duration %>%
  gather(key = averages, value = value, all_rides:above_30_min)

ggplot(new_customer_vs_duration, aes(x= fct_relevel(averages, "above_30_min", after = 2),
                                     y=value, fill=customer_type)) + 
  geom_bar(position = "dodge", stat = "identity") + 
  geom_text(aes(label=value), vjust=-0.3, color="#2D2A26", family = "Montserrat",
            fontface = "bold", position = position_dodge(0.9), size=3.5) +
  scale_fill_manual("Customer Types", values = c("casual" = "#3CB4E5", "member" = "#0C0B31")) +
  ggtitle("Customer Type vs. Average ride duration") + xlab("Averages") + 
  ylab("Minutes") + theme(plot.title = element_text(family = "Montserrat", face = "bold", 
                                                    size = 10, colour = "#2D2A26"),
                          axis.title.x = element_text(family = "Montserrat", face = "bold",
                                                    size = 8, colour = "#2D2A26"),
                          axis.text.x = element_text(family = "Montserrat", size = 8,
                                                     face = "bold", colour = "#2D2A26"),
                          axis.title.y = element_text(family = "Montserrat", face = "bold",
                                                      size = 8, colour = "#2D2A26"),
                          axis.text.y = element_text(family = "Montserrat", size = 8,
                                                     face = "bold", colour = "#2D2A26"),
                          legend.title = element_text(family = "Montserrat", face = "bold",
                                                      size = 8, colour = "#2D2A26"),
                          legend.text = element_text(family = "Montserrat", size = 8,
                                                     face = "bold", colour = "#2D2A26"))
  
# Creating graph  to compare customer type with number of rides by duration of the ride:
new_customer_vs_percentage_duration <- customer_vs_percentage_duration %>%
  gather(key = percentages, value = value, below_30_min:above_30_min)

ggplot(new_customer_vs_percentage_duration, aes(x= fct_relevel(percentages, "above_30_min", after = 2),
                                     y=value, fill=customer_type)) + 
  geom_bar(position = "dodge", stat = "identity") + 
  geom_text(aes(label=value), vjust=-0.3, color="#2D2A26", family = "Montserrat",
            fontface = "bold", position = position_dodge(0.9), size=3.5) +
  scale_fill_manual("Customer Types", values = c("casual" = "#3CB4E5", "member" = "#0C0B31")) +
  ggtitle("Customer Type vs. Percentage of rides below and above 30 minutes") + xlab("Duration of the ride") + 
  ylab("Percentages") + theme(plot.title = element_text(family = "Montserrat", face = "bold", 
                                                    size = 10, colour = "#2D2A26"),
                          axis.title.x = element_text(family = "Montserrat", face = "bold",
                                                      size = 8, colour = "#2D2A26"),
                          axis.text.x = element_text(family = "Montserrat", size = 8,
                                                     face = "bold", colour = "#2D2A26"),
                          axis.title.y = element_text(family = "Montserrat", face = "bold",
                                                      size = 8, colour = "#2D2A26"),
                          axis.text.y = element_text(family = "Montserrat", size = 8,
                                                     face = "bold", colour = "#2D2A26"),
                          legend.title = element_text(family = "Montserrat", face = "bold",
                                                      size = 8, colour = "#2D2A26"),
                          legend.text = element_text(family = "Montserrat", size = 8,
                                                     face = "bold", colour = "#2D2A26"))


# Creating graph  to compare customer type with number of rides by rideable type:
customer_vs_rideable <- data.frame(customer_type = customer_vs_rideable$customer_type,
                                   electric_bike = customer_vs_rideable$electric_bike,
                                   classic_bike = customer_vs_rideable$classic_bike)

new_customer_vs_rideable <- customer_vs_rideable %>% 
  gather(key = rideable_type, value = value, electric_bike:classic_bike)

ggplot(new_customer_vs_rideable, aes(x= rideable_type, y=value, fill=customer_type)) + 
  geom_bar(position = "dodge", stat = "identity") + 
  geom_text(aes(label=value), vjust=-0.3, color="#2D2A26", family = "Montserrat",
            fontface = "bold", position = position_dodge(0.9), size=3.5) +
  scale_fill_manual("Customer Types", values = c("casual" = "#3CB4E5", "member" = "#0C0B31")) +
  ggtitle("Customer Type vs. Rideable Type") + xlab("Rideable Type") + 
  ylab("Percentage of rides") + theme(plot.title = element_text(family = "Montserrat", face = "bold", 
                                                    size = 10, colour = "#2D2A26"),
                          axis.title.x = element_text(family = "Montserrat", face = "bold",
                                                      size = 8, colour = "#2D2A26"),
                          axis.text.x = element_text(family = "Montserrat", face = "bold",
                                                      size = 8, colour = "#2D2A26"),
                          axis.title.y = element_text(family = "Montserrat", face = "bold",
                                                      size = 8, colour = "#2D2A26"),
                          axis.text.y = element_text(family = "Montserrat", face = "bold",
                                                     size = 8, colour = "#2D2A26"),
                          legend.title = element_text(family = "Montserrat", face = "bold",
                                                      size = 8, colour = "#2D2A26"),
                          legend.text = element_text(family = "Montserrat", size = 8,
                                                     face = "bold", colour = "#2D2A26"))


# Creating graph  to show most used stations by casual customers:
ggplot(casual_vs_top_station, aes(x = visits, y = reorder(stations, +visits), fill = visits)) + 
  geom_bar(stat = "identity") + scale_fill_gradient2(high="#0C0B31") + scale_x_continuous(labels = comma) +
  geom_text(aes(label=visits), hjust = 1.1, color="white", family = "Montserrat",
            fontface = "bold",) + 
  ggtitle("Most used stations by casual customers") + xlab("Number of Visits") + ylab("") + 
  theme(legend.position = "none",
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        plot.title = element_text(family = "Montserrat", face = "bold", 
                                  size = 10, colour = "#2D2A26"),
        axis.title.x = element_text(family = "Montserrat", face = "bold",
                                    size = 8, colour = "#2D2A26"),
        axis.text.y = element_text(family = "Montserrat", face = "bold",
                           size = 8, colour = "#2D2A26"))

# Creating graph  to show most used stations by casual customers with rides' duration below 30 minutes:
ggplot(casual_vs_top_station_below, aes(x = below_30_min, y = reorder(stations, +below_30_min), fill = below_30_min)) + 
  geom_bar(stat = "identity") + scale_fill_gradient2(high="#0C0B31") + scale_x_continuous(labels = comma) +
  geom_text(aes(label=below_30_min), hjust = 1.1, color="white", family = "Montserrat",
            fontface = "bold",) + 
  ggtitle("Most used stations by casual customers with rides' duration below 30 minutes") + xlab("Number of Visits") + ylab("") + 
  theme(legend.position = "none",
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        plot.title = element_text(family = "Montserrat", face = "bold", 
                                  size = 10, colour = "#2D2A26"),
        axis.title.x = element_text(family = "Montserrat", face = "bold",
                                    size = 8, colour = "#2D2A26"),
        axis.text.y = element_text(family = "Montserrat", face = "bold",
                                   size = 8, colour = "#2D2A26"))

# Creating graph  to show most used stations by casual customers with rides' duration above 30 minutes:
ggplot(casual_vs_top_station_above, aes(x = above_30_min, y = reorder(stations, +above_30_min), fill = above_30_min)) + 
  geom_bar(stat = "identity") + scale_fill_gradient2(high="#0C0B31") + scale_x_continuous(labels = comma) +
  geom_text(aes(label=above_30_min), hjust = 1.1, color="white", family = "Montserrat",
            fontface = "bold",) + 
  ggtitle("Most used stations by casual customers with rides' duration above 30 minutes") + xlab("Number of Visits") + ylab("") + 
  theme(legend.position = "none",
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        plot.title = element_text(family = "Montserrat", face = "bold", 
                                  size = 10, colour = "#2D2A26"),
        axis.title.x = element_text(family = "Montserrat", face = "bold",
                                    size = 8, colour = "#2D2A26"),
        axis.text.y = element_text(family = "Montserrat", face = "bold",
                                   size = 8, colour = "#2D2A26"))

# Creating graph  to show most used stations by members:
ggplot(member_vs_top_station, aes(x = visits, y = reorder(stations, +visits), fill = visits)) + 
  geom_bar(stat = "identity") + scale_fill_gradient2(high="#0C0B31") + scale_x_continuous(labels = comma) +
  geom_text(aes(label=visits), hjust = 1.1, color="white", family = "Montserrat",
            fontface = "bold",) + 
  ggtitle("Most used stations by annual members") + xlab("Number of Visits") + ylab("") + 
  theme(legend.position = "none",
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        plot.title = element_text(family = "Montserrat", face = "bold", 
                                  size = 10, colour = "#2D2A26"),
        axis.title.x = element_text(family = "Montserrat", face = "bold",
                                    size = 8, colour = "#2D2A26"),
        axis.text.y = element_text(family = "Montserrat", face = "bold",
                                   size = 8, colour = "#2D2A26"))

# Creating graph  to show most used stations by members with rides' duration below 30 minutes:
ggplot(member_vs_top_station_below, aes(x = below_30_min, y = reorder(stations, +below_30_min), fill = below_30_min)) + 
  geom_bar(stat = "identity") + scale_fill_gradient2(high="#0C0B31") + scale_x_continuous(labels = comma) +
  geom_text(aes(label=below_30_min), hjust = 1.1, color="white", family = "Montserrat",
            fontface = "bold",) + 
  ggtitle("Most used stations by members with rides' duration below 30 minutes") + xlab("Number of Visits") + ylab("") + 
  theme(legend.position = "none",
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        plot.title = element_text(family = "Montserrat", face = "bold", 
                                  size = 10, colour = "#2D2A26"),
        axis.title.x = element_text(family = "Montserrat", face = "bold",
                                    size = 8, colour = "#2D2A26"),
        axis.text.y = element_text(family = "Montserrat", face = "bold",
                                   size = 8, colour = "#2D2A26"))

# Creating graph  to show most used stations by members with rides' duration above 30 minutes:
ggplot(member_vs_top_station_above, aes(x = above_30_min, y = reorder(stations, +above_30_min), fill = above_30_min)) + 
  geom_bar(stat = "identity") + scale_fill_gradient2(high="#0C0B31") + scale_x_continuous(labels = comma) +
  geom_text(aes(label=above_30_min), hjust = 1.1, color="white", family = "Montserrat",
            fontface = "bold",) + 
  ggtitle("Most used stations by members with rides' duration above 30 minutes") + xlab("Number of Visits") + ylab("") + 
  theme(legend.position = "none",
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        plot.title = element_text(family = "Montserrat", face = "bold", 
                                  size = 10, colour = "#2D2A26"),
        axis.title.x = element_text(family = "Montserrat", face = "bold",
                                    size = 8, colour = "#2D2A26"),
        axis.text.y = element_text(family = "Montserrat", face = "bold",
                                   size = 8, colour = "#2D2A26"))
