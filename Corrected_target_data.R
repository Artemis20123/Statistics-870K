newwfdata = read.csv("C:/Users/Lijh/Desktop/statistics/project/data/csv/new_wf.csv")
weekcode = read.csv("C:/Users/Lijh/Desktop/statistics/project/data/week_code.csv")

### data pre-analysis:add the fields of lead & lag dummies
newwfdata$period_sum = 0

## baseline value: if the city is in control group, values of these two fields will be zero.
newwfdata$lock_date = 0
newwfdata$lock_sum = 0

i = 1
m = 1

## i,m = row numbers in the two datasets

for (m in 1:20130) {
  if (is.na(newwfdata$city_code[m])) {
    next
  }
  newwfdata$period_sum = newwfdata$daynum - 8400
}

## only 95 cities of 330 have been locked down
for (m in 1:20130) {
  if (is.na(newwfdata$city_code[m])) {
    next
  }
  for (i in 1:95) {
    if (newwfdata$city_code[m] == weekcode$City_code[i]) {
      newwfdata$lock_date[m] = weekcode[i, 2] - 8400
      
    }
  }
}

## calculate the relative order of the days
for (m in 1:20130) {
  for (i in 1:95) {
    if (newwfdata$city_code[m] == weekcode$City_code[i]) {
      newwfdata$lock_sum[m] = newwfdata$period_sum[m] - newwfdata$lock_date[m]
      
    }
  }
}

## calculate all the lead and lag dummies of each weeks
newwfdata$w4_lead = 0
newwfdata$w3_lead = 0
newwfdata$w2_lead = 0
newwfdata$w1_lead = 0
newwfdata$w0 = 0
newwfdata$w1 = 0
newwfdata$w2 = 0
newwfdata$w3 = 0
newwfdata$w4 = 0

for (m in 1:20130) {
  for (i in 1:95) {
    if (newwfdata$city_code[m] == weekcode$City_code[i]) {
      if (newwfdata$lock_sum[m] < -21) {
        newwfdata$w4_lead[m] = 1
      }
      if (-21 <= newwfdata$lock_sum[m] &&
          newwfdata$lock_sum[m] <= -15) {
        newwfdata$w3_lead[m] = 1
      }
      if (-14 <= newwfdata$lock_sum[m] &&
          newwfdata$lock_sum[m] <= -8) {
        newwfdata$w2_lead[m] = 1
      }
      if (-7 <= newwfdata$lock_sum[m] &&
          newwfdata$lock_sum[m] <= -1) {
        newwfdata$w1_lead[m] = 1
      }
      if (0 <= newwfdata$lock_sum[m] &&
          newwfdata$lock_sum[m] <= 6) {
        newwfdata$w0[m] = 1
      }
      if (7 <= newwfdata$lock_sum[m] &&
          newwfdata$lock_sum[m] <= 13) {
        newwfdata$w1[m] = 1
      }
      if (14 <= newwfdata$lock_sum[m] &&
          newwfdata$lock_sum[m] <= 20) {
        newwfdata$w2[m] = 1
      }
      if (21 <= newwfdata$lock_sum[m] &&
          newwfdata$lock_sum[m] <= 27) {
        newwfdata$w3[m] = 1
      }
      if (28 <= newwfdata$lock_sum[m]) {
        newwfdata$w4[m] = 1
      }
    }
  }
}

write.table(
  newwfdata,
  "corrected_weekly_data.csv",
  row.names = FALSE,
  col.names = TRUE,
  sep = ","
)
