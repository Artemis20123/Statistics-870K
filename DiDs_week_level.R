
library(dplyr)

### data pre=analysis:select the data at week level

weekcode = read.csv("C:/Users/Lijh/Desktop/statistics/project/data/week_code.csv")
newwfdata = read.csv("C:/Users/Lijh/Desktop/statistics/project/data/csv/new_wf.csv")

newwfdata$judge = 0


i = 1
j = 1
m = 1

## i,m = row numbers in the two datasets
## j = column numbers in week code dataset
for (m in 1:20130) {
  if (is.na(newwfdata$city_code[m])) {
    next
  }
  for (i in 1:95) {
    if (newwfdata$city_code[m] == weekcode$City_code[i]) {
  
      for (j in 2:12) {

        if (newwfdata$daynum[m] == weekcode[i, j]) {

          newwfdata$judge[m] = 1
        }
        
        
      }
    }
  }
}

# seperate the entire daily dataset into two parts
weekdata <- subset(newwfdata, judge != 0)
exclusivedata = subset(newwfdata, judge != 1)

# add a new column in the week dataset indicating the number of weeks
weekdata$week = 0
g = 1
k = 1
l = 1

for (g in 1:834) {
  for (k in 1:95) {
    if (weekdata$city_code[g] == weekcode$City_code[k]) {
      if (weekdata$daynum[g] == weekcode[k, 2]) {
        weekdata$week[g] = 0
      }
      if (weekdata$daynum[g] == weekcode[k, 3]) {
        weekdata$week[g] = -1
      }
      if (weekdata$daynum[g] == weekcode[k, 4]) {
        weekdata$week[g] = -2
      }
      if (weekdata$daynum[g] == weekcode[k, 5]) {
        weekdata$week[g] = -3
      }
      if (weekdata$daynum[g] == weekcode[k, 6]) {
        weekdata$week[g] = -4
      }
      if (weekdata$daynum[g] == weekcode[k, 7]) {
        weekdata$week[g] = -5
      }
      if (weekdata$daynum[g] == weekcode[k, 8]) {
        weekdata$week[g] = 1
      }
      if (weekdata$daynum[g] == weekcode[k, 9]) {
        weekdata$week[g] = 2
      }
      if (weekdata$daynum[g] == weekcode[k, 10]) {
        weekdata$week[g] = 3
      }
      if (weekdata$daynum[g] == weekcode[k, 11]) {
        weekdata$week[g] = 4
      }
      if (weekdata$daynum[g] == weekcode[k, 12]) {
        weekdata$week[g] = 5
      }
    }  
  }
}

write.table(
  weekdata,
  "weekdata.csv",
  row.names = FALSE,
  col.names = TRUE,
  sep = ","
)


### the regression part
## the weekdata is treated in the Excel, a new column has been added named 'lead_lag_dummy'
weekdata = read.csv("C:/Users/Lijh/Desktop/statistics/project/data/csv/weekdata.csv")

temp_2 = weekdata$temp * weekdata$temp

didreg_week = lm(
  aqi ~ treat + temp + temp_2 + prec
  + snow + as.factor(city_code) + as.factor(week),
  data = weekdata
)
summary(didreg_week)

# comparison between the selected days and the left days (robust check)
didreg_weekday = lm(
  aqi ~ treat + temp + temp_2 + prec
  + snow + as.factor(city_code) + as.factor(daynum),
  data = weekdata
)
summary(didreg_weekday)
# this regression's result is very different from using the entire dataset and the exclusive data
# the reason may be the relatively small size of dataset (only 836 recods, while others are 10^6 order)

temp_2 = exclusivedata$temp * exclusivedata$temp
didreg_weekleft = lm(
  aqi ~ treat + temp + temp_2 + prec
  + snow + as.factor(city_code) + as.factor(daynum),
  data = exclusivedata
)
summary(didreg_weekleft)