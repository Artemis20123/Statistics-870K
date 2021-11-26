## code 1
# read the original data
wf <- read.csv("/Users/tilly/Desktop/wf.csv")

# take a subset from 20200101 to 20200301
newdata <- subset(wf, daynum >= 8401 & daynum <=8461)
province_data = read.csv("/Users/tilly/Desktop/province_code_reference.csv")

# for loop to match wf and province_code_reference
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

# derive the treat coefficient of each province
# here take Jiangsu as an example
wfprovince<- read.csv("/Users/tilly/Desktop/wf_city_province.csv")
prov_320 <- subset(wfprovince, province_code == 320)
didreg_320 <- lm(aqi ~ treat + as.factor(daynum) + as.factor(city_code),
                 data = prov_320)
summary(didreg_320)

## code 2
# read treat_coef
library(readxl)
treat_coef <- read_excel("Desktop/treat_coef.xlsx")
View(treat_coef)

# summarize na values
summary(is.na(treat_coef$treat_beta))

# remove na
treat_coef_no_na <- na.omit(treat_coef)

# draw the histogram
hist(treat_coef_no_na$Treat_beta, 
     main = 'Histogram of the treat coefficient at provincial level',
     xlab = 'Treat coefficient')