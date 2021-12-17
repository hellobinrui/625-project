library(dplyr)
library(tidyverse)

# load data
weather = read.csv("histWeather.csv", header = TRUE)
forecast = read.table("forecast.dat", header = FALSE)
loc = read.csv("locations.csv", header = TRUE)

# process forecast data
forecast = as.data.frame(forecast)
colnames(forecast) = c("city_code", "predicted_date", "value", "category", "date")
forecast = forecast[forecast$category=="ProbPrecip",]

# probability to 0/1
fun <- function(x){
  if (x[3] < 50) return(0)
  else if (x[3] >= 50) return(1)
}
nvalue = apply(forecast, 1, fun)
forecast2 = forecast
forecast2$value = nvalue

# weather: season
mon = months(as.Date(weather$Date))
season = ifelse(mon %in% c("April", "May", "March"), "Spring", 
                ifelse(mon %in% c("July","August", "June"), "Summer",
                       ifelse(mon %in% c("September", "October", "Novermber"), "Autumn",
                              "Winter")))
weather$season = season
# forecast: season
mon = months(as.Date(forecast2$date))
season = ifelse(mon %in% c("April", "May", "March"), "Spring", 
                ifelse(mon %in% c("July","August", "June"), "Summer",
                       ifelse(mon %in% c("September", "October", "Novermber"), "Autumn",
                              "Winter")))
forecast2$season = season

# weather: city
f4 = function(s){
  return(loc[loc$AirPtCd==s, ]$city)
}
f4 = Vectorize(f4)
weather$city = f4(weather$AirPtCd)
weather$city = as.factor(weather$city)
# weather: state
f = function(s){
  return(loc[loc$AirPtCd == s, ]$state)
}
f = Vectorize(f)
weather$state = f(weather$AirPtCd)
weather$state = as.factor(weather$state)
# forecast: city
f2 = function(s){
  return(loc[s, ]$city)
}
f2 = Vectorize(f2)
forecast2$city = f2(forecast2$city_code)
forecast2$city = as.factor(forecast2$city)
# forecast: state
f3 = function(s){
  return(loc[s, ]$state)
}
f3 = Vectorize(f3)
forecast2$state = f3(forecast2$city_code)
forecast2$state = as.factor(forecast2$state)

weather$PrecipitationIn[which(weather$PrecipitationIn == "T")] = 1
weather$PrecipitationIn = as.numeric(weather$PrecipitationIn)
weather$PrecipitationIn[which(weather$PrecipitationIn==0)] = 0
weather$PrecipitationIn[which(weather$PrecipitationIn!=0)] = 1

#save(weather, file="weather.RData")
#save(forecast2, file="forecast.RData")


#-------compare accuracy---
mat0 <- weather[,c("Date", "PrecipitationIn", "city", "state", "season")]
mat1 <- forecast2[,c("city_code", "predicted_date", "date", "value", "city", "state", "season")]

mat0 <- filter(mat0, !is.na(mat0[,2]))

write.csv(mat0, file="true weather.csv")
write.csv(mat1, file="predicted weather.csv")

## only today-today :some double results and not identical. Only take 1.
mat1 <- mat1[which(mat1$predicted_date == mat1$date),]

factor_city <- unique(mat1$city)
for (i in length(factor_city)) {
  mat_test_1 <- mat1[mat1$city == factor_city[i], ]
  
}

mat2 <- distinct(mat1)

mat2 = mat2 %>% 
  group_by(date, city) %>%
  mutate(new_value = max(value)) %>%
  select(date, new_value, city, state, season) %>%
  distinct()

colnames(mat0)[1] = "date"

mat3 = mat2 %>%
  inner_join(mat0[,1:4], by = c("date", "city", "state"))



mat3[,2] <- as.vector(mat3[,2])
mean(mat3[,2] != mat3[,6])

#by city
v1 <- mat3 %>%
  group_by(city) %>%
  summarise(city_mis = mean(new_value != PrecipitationIn))

#by state
v2 <- mat3 %>%
  group_by(state) %>%
  summarise(city_mis = mean(new_value != PrecipitationIn))

mean(mat3[,2] != mat3[,6])
#0.2156
write.csv(v1, file="bycity.csv")
write.csv(v2, file="bystate.csv")
