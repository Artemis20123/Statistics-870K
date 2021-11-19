# read wf.csv and make subset of wf.csv to select 60 days from 20200101 to 20200301
wf <- read.csv("/Users/tilly/Desktop/wf.csv")
newdata <- subset(wf, daynum >= 8401 & daynum <=8461)

# province code
# created and edited in the Excel
# also adjusted the error in the translation of city
# saved as "province_code_reference.csv"
province_data = read.csv("/Users/tilly/Desktop/province_code_reference.csv")

# city & province_code & province
for (m in 1:20130){
  if (is.na(newdata$city_code[m])){
    next
  }
  for (n in 1:330){
    if (newdata$city_code[m]==province_data$city_code[n]){
      newdata$city[m] = province_data$city[n]
      newdata$province_code[m] = province_data$province_code[n]
      newdata$province[m] = province_data$province[n]
    }
  }
}
write.csv(newdata, file = ('wf_city_province.csv'))

# province fixed effect
wfprovince = read.csv("/Users/tilly/Desktop/wf_city_province.csv")

# as.factor(province_code)
didreg_unit_fe = lm(aqi ~ treat + city_code + daynum + as.factor(province_code),
                    data = wfprovince)
summary(didreg_unit_fe)

# as.factor(province_code) + as.factor(daynum)
didreg_two_way = lm(aqi ~ treat + city_code + as.factor(province_code) +
                      as.factor(daynum), data = wfprovince)
summary(didreg_two_way)