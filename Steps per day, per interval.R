#extracts data from csv
rawdata <- read.csv("activity.csv")

#Group by date and then summarize by total steps taken
library(dplyr)
stepsPerDay <- group_by(rawdataNoNA, date) %>% 
               summarise("stepsDay" = sum(steps, na.rm = TRUE))

#graphs histogram of steps per day
hist(stepsPerDay$stepsDay, 
     breaks = 50, 
     xlab = "Steps per Day", 
     main = "Histogram: Frequency of Steps per Day")


#calculates and then prints the Mean and Median of steps per day over timeframe
stepsMean <- summarise(stepsPerDay, "Mean Steps per Day" = mean(stepsDay), 
                      "Median Steps per Day" = median(stepsDay))
print(stepsMean)

   
#Make a time series plot
##5-min interval (x-axis) vs avg # steps taken across all days (y-axis)

stepsPerInterval <- group_by(rawdata, interval) %>% 
                     summarise("stepsInterval" = mean(steps, na.rm = TRUE))

plot(data = stepsPerInterval, 
     stepsInterval ~ interval, 
     type = "l")

stepsPerInterval[stepsPerInterval$stepsInterval == max(stepsPerInterval$stepsInterval), ]

