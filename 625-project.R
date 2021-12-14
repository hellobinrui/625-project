weather = read.csv("histWeather.csv", header = TRUE)
forecast = read.table("forecast.dat", header = FALSE)
loc = read.csv("locations.csv", header = TRUE)
library(dplyr)
library(tidyverse)
library(bigmemory)
library(sparklyr)
f = function(s){
  return(loc[loc$AirPtCd == s, ]$state)
}
f = Vectorize(f)
forecast = as.data.frame(forecast)
colnames(forecast) = c("city_code", "predicted_date", "value", "category", "date")
forecast = forecast %>% mutate(AirPtCd = f(city_code))
#merged_data = forecast %>% inner_join(weather, by = "AirPtCd")
for(i in 1:113){
  city = forecast[forecast$city_code == i, ]
  wt = weather[weather$AirPtCd == f(i), ]
  merged_data = city %>% inner_join(wt, by = "AirPtCd")
  
}
d = forecast[1:18,]
distinct(d) %>% pivot_wider(names_from = category, values_from = value)

d = weather
d$PrecipitationIn[which(d$PrecipitationIn == "T")] = 1
d$PrecipitationIn = as.numeric(d$PrecipitationIn)
d$PrecipitationIn[which(d$PrecipitationIn==0)] = 0
d$PrecipitationIn[which(d$PrecipitationIn!=0)] = 1
d = d %>% dplyr::filter(!is.na(d$PrecipitationIn))
d = d%>% select(-c("Events"))
mon = months(as.Date(d$Date))
season = ifelse(mon %in% c("April", "May", "March"), "Spring", 
                ifelse(mon %in% c("July","August", "June"), "Summer",
                ifelse(mon %in% c("September", "October", "Novermber"), "Autumn",
                "Winter")))

# impute the data
library(mice)

var_unchange = c("prep")
data_unchange=subset(d,select=var_unchange)
data_to_change=d %>% select(-var_unchange)
tempData=mice(data_to_change,m=5,meth="pmm",seed=500)
data5=complete(tempData,5)
d=bind_cols(data_unchange,data5)
# begin with the data saved before
data$season = season
write.csv(data, "625-data.csv")
data = read.csv("625-data.csv")
data$PrecipitationIn = as.factor(data$PrecipitationIn)
data = data[,-1]
colnames(data)[1] = "prep"
data$WindDirDegrees = d$WindDirDegrees
data$state = f(data$AirPtCd)
data$state = as.factor(data$state)
data$prep = as.factor(data$prep)
data$AirPtCd = as.factor(data$AirPtCd)
data$season = as.factor(data$season)
#EDA
library(ggplot2)
gather(data[,1:10], key = 'variable', value = 'value', -c('prep'))%>% ggplot() +
  geom_boxplot(aes(x = prep, y = value))+
  facet_wrap(.~variable, scales= 'free_y')+
  xlab("Precipitation")+theme_bw()

gather(data[,c(1, 11:19)], key = 'variable', value = 'value', -c('prep'))%>% ggplot() +
  geom_boxplot(aes(x = prep, y = value))+
  facet_wrap(.~variable, scales= 'free_y')+
  xlab("Precipitation")+theme_bw()
# train_test split 
data = data[,-21] # omit c("AirPtCd")]
set.seed(2021)
n = dim(data)[1]
train_id = sample(seq(1, n, 1), floor(n*0.7))
test = data[-train_id, ]
train = data[train_id, ]
# LR

lr_fit = glm(prep~.-state, data = train, family = binomial(link = "logit"))
summary(lr_fit)
lr_prob = predict(lr_fit, test, type = "response")
cut_off = 0.5
test_pred = ifelse(lr_prob<cut_off, 0, 1)
error_table=table(test_pred, test$prep)
fn = error_table[2,1]/sum(error_table[, 1])
fp = error_table[1,2]/sum(error_table[, 2])
ber<-(fn+fp)/2  # 0.1624639
sum(test_pred != test$prep) / dim(test)[1] #0.1496409
pr <- prediction(test_pred, test$prep)
auc = performance(pr, "auc") 
auc@y.values  #0.8375361

library(ROCR)
library(Metrics)
library(rpart)
library(rpart.plot)
library(rattle)

# CART
tree11 = rpart(prep~., train[,-23], parms = list(split = "gini"), method="class",cp=0.013)
plotcp(tree11) 
fancyRpartPlot(tree11)
test.pred=predict(tree11, test,type="class")
error_table=table(test.pred, test$prep)
fn = error_table[2,1]/sum(error_table[, 1])
fp = error_table[1,2]/sum(error_table[, 2])
ber<-(fn+fp)/2  # 0.2010455
sum(test.pred != test$prep) / dim(test)[1] #0.1763999
test.pred<-ifelse(test.pred==0, 0, 1)
pr <- prediction(test.pred, test$prep)
auc = performance(pr, "auc") 
auc@y.values  #0.7989545

# deal with unbalanced label
prop = table(train$prep) / dim(train)[1]
w = rep(1/prop[1], dim(train)[1])
w[which(train$prep== 1)] = 1/prop[2]
#CART with weights
tree11.balance = rpart(prep~.-state, train, parms = list(split = "gini"), method="class",cp=0.0085,weights = w)
plotcp(tree11.balance)
fancyRpartPlot(tree11.balance)
test.pred=predict(tree11.balance, test,type="class") 
error_table = table(test.pred, test$prep)
fn = error_table[2,1]/sum(error_table[, 1])
fp = error_table[1,2]/sum(error_table[, 2])
(ber<-(fn+fp)/2) #0.1895323
sum(test.pred != test$prep) / dim(test)[1] #0.1892811
test.pred<-ifelse(test.pred==0, 0, 1)
pr <- prediction(test.pred, test$prep)
auc = performance(pr, "auc") 
auc@y.values  # 0.8104677


#random forest
library(randomForest)

rf_prep = randomForest(prep ~., data = train[,-23], mtry = floor(sqrt(21)),
                       importance = TRUE)
importance(rf_prep)[1:15,]
varImpPlot(rf_prep,n.var=10)
test$state = as.factor(test$state)
test$prep = as.factor(test$prep)
test.pred=predict(rf_prep, newdata =test) 
error_table = table(test.pred, test$prep)
fn = error_table[2,1]/sum(error_table[, 1])
fp = error_table[1,2]/sum(error_table[, 2])
(ber<-(fn+fp)/2) #0.1495826
sum(test.pred != test$prep) / dim(test)[1] #0.1385744
test.pred<-ifelse(test.pred==0, 0, 1)
pr <- prediction(test.pred, test$prep)
auc = performance(pr, "auc") 
auc@y.values   #0.8504174
# Adaboost

library(gbm)
# train-validation split
train$prep = as.numeric(train$prep) -1
nd = dim(train)[1]
train_id1 = sample(seq(1, nd, 1), floor(nd*0.8))
validation = train[-train_id1, ]
train1 = train[train_id1, ]
ada_prep = gbm(prep~.-state, data = train1, 
               distribution = "adaboost", n.trees = 500,
               cv.folds=5,interaction.depth = 3, shrinkage = 0.07)
ada_pred_response = predict(ada_prep, newdata = validation, n.trees = 500, type = "response")
cut_off = 0.5
ada_pred = ifelse(ada_pred_response>0.5,1,0) 
mean(ada_pred!=validation$prep) # 0.1394381
error_table = table(ada_pred, validation$prep)
fn = error_table[2,1]/sum(error_table[, 1])
fp = error_table[1,2]/sum(error_table[, 2])
ber<-(fn+fp)/2  #0.1456426
pr <- prediction(ada_pred_response, validation$prep)
auc = performance(pr, "auc")
auc@y.values # 0.9329032
perf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(perf)
abline(a = 0, b = 1)
true_pos = error_table[2, 2] / sum(error_table[, 2])
false_pos = error_table[2, 1] / sum(error_table[, 1])
points(c(false_pos), c(true_pos), col = "red")

# additive model
library(nlme)
library(splines)
library(mgcv)
library(gam)

gam_prep = mgcv::gam(prep ~ s(Max_TemperatureF)+ factor(AirPtCd)+ CloudCover + s(WindDirDegrees), data = train1, family = binomial
                 )
#+ s(Mean_TemperatureF)+ s(Min_TemperatureF)+
#s(Max_Dew_PointF)+Min_DewpointF+ s(Max_Humidity) + s(Mean_Humidity)+
 # s(Min_Humidity)+ s(Max_Sea_Level_PressureIn) + s(Mean_Sea_Level_PressureIn) +
  #s(Mean_VisibilityMiles) + s(Min_VisibilityMiles) + s(Max_Wind_SpeedMPH) +
#  s(Mean_Wind_SpeedMPH) + s(Max_Gust_SpeedMPH) 