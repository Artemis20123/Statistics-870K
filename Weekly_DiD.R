# new baseline (treated cities)
library(haven)
wf_stata_generated <- read_dta("C:/Users/Lijh/Desktop/statistics/project/data/wf_stata_generated.dta")

stata_treated <- subset(wf_stata_generated, daynum >= 8401 & daynum <=8461 & t_asign == 1)


# the author's method (delete treat, entire dataset, delete Lead_D7 manually)
temp_2 = wf_stata_generated$temp * wf_stata_generated$temp
# temp_2 = stata_treated$temp * stata_treated$temp

didreg_week = lm(
  aqi ~ temp + temp_2 + prec + snow
  + Lead_D28 + Lead_D21 + Lead_D14 + D0 + D7 + D14 + D21 + D28
  + as.factor(city_code) + as.factor(daynum),
  data = wf_stata_generated
)
summary(didreg_week)


library(coefplot)
coefplot(
  didreg_week,
  predictors = c(
    "D0",
    "D7",
    "D14",
    "D21",
    "D28",
    "Lead_D14",
    "Lead_D21",
    "Lead_D28"
  )
)

# corrected finding (delete treat, lockdown cities only, delete w1_lead)
week_data <- read.csv("C:/Users/Lijh/Desktop/statistics/project/data/csv/corrected_weekly_data.csv")
week_treated <- subset(week_data, daynum >= 8401 & daynum <=8461 & t_asign == 1)

temp_2 = week_treated$temp * week_treated$temp
#temp_2 = week_data$temp * week_data$temp

didreg_week1 = lm(
  aqi ~ temp + temp_2 + prec + snow
  + w4_lead + w3_lead + w2_lead +w1_lead+ w0 + w1 + w2 + w3 + w4
  + as.factor(city_code) + as.factor(daynum),
  data = week_treated
)
summary(didreg_week1)

# the multicollinearity has been proved
library(coefplot)
coefplot(
  didreg_week1,
  predictors = c(
    "w4_lead",
    "w3_lead",
    "w2_lead",
    "w0",
    "w1",
    "w2",
    "w3",
    "w4"
  )
)