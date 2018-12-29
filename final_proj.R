#####################################################################################################
## Dithya Sridharan - 48
## Krina Joshi - 
## Freya Genesis Dmello - 
## Final Project - Analysis of Apple App Store
## Last updated - November 27, 2018
#####################################################################################################

#####################################################################################################
##Install packages and libraries
#####################################################################################################

install.packages('bit64')
library(bit64)
installed.packages("dplyr")
library(dplyr)
install.packages("randomForest")
library(randomForest)
install.packages("plotrix")
library(plotrix)
install.packages("ggcorrplot")
library(ggcorrplot)
install.packages("gridExtra")
library("gridExtra")
install.packages("ggplot2")
library(ggplot2)
install.packages("DAAG")
library(DAAG)
install.packages("lattice")
library(lattice)
install.packages("MASS")
library(MASS)
install.packages("devtools")
library(devtools)
installed.packages("easyGgplot2")
library(easyGgplot2)
install.packages("tidyverse")
library(tidyverse)
install.packages("e1071")
library(e1071)
install.packages("gbm")
library(gbm)
install.packages("xgboost")
library(xgboost)

#################################################################################################
##Reading the data
#################################################################################################

app_data <- data.table::fread("E:\\AppleStore.csv")


##################################################################################################
##Exploratory Data Analysis
##################################################################################################

###################################################################################################
##Checking for missing values
####################################################################################################

app_data[!complete.cases(app_data),] #The data has rows with missing values
is.na(app_data) #The data set has rows with missing values
new_app_data = na.omit(app_data)
##Check for missing values in new app data
new_app_data[!complete.cases(new_app_data),] #0 rows with missing values

###################################################################################################
##Variable Transformations
###################################################################################################

#################################################################################################
##Finding apps which are best in business - calculate revenue = total rating count * price
#################################################################################################

new_app_data <- mutate(new_app_data, 'revenue' = new_app_data$rating_count_tot * new_app_data$price)

##################################################################################################
##Finding favorites - calculate favourite rank = total rating count * user rating
##################################################################################################

new_app_data <- mutate(new_app_data, 'fav_rank' = new_app_data$rating_count_tot * new_app_data$user_rating)

###################################################################################################
##Transforming cont_rating
###################################################################################################

u <- unique(new_app_data$cont_rating)
new_app_data$cont_rating=gsub("4+",4,new_app_data$cont_rating)
new_app_data$cont_rating=gsub("12+",12,new_app_data$cont_rating)
new_app_data$cont_rating=gsub("17+",17,new_app_data$cont_rating)
new_app_data$cont_rating=gsub("9+",9,new_app_data$cont_rating)


####################################################################################################
##Visualization
####################################################################################################

####################################################################################################
##Jitter plots 
####################################################################################################


##Assign rows that have best business(revenue>median of revenue) as 1 and 0 otherwise
new_app_data <- mutate(new_app_data,'revenue_above_med' = ifelse(new_app_data$revenue>median(new_app_data$revenue),1,0))
p1 <- ggplot(data = new_app_data, mapping = aes(x = revenue_above_med, y = fav_rank)) + geom_jitter(aes(color = revenue_above_med))
p2 <- ggplot(data = new_app_data, mapping = aes(x = revenue_above_med, y = user_rating)) + geom_jitter(aes(color = revenue_above_med))
grid.arrange(p1,p2,nrow=1)
p3 <- ggplot(data = new_app_data, mapping = aes(x = revenue_above_med, y = lang.num)) + geom_jitter(aes(color = revenue_above_med))
prim <- as.factor(new_app_data$prime_genre)
p4 <- p1 <- ggplot(data = new_app_data, mapping = aes(x = revenue_above_med, y = prim)) + geom_jitter(aes(color = revenue_above_med))
ggplot(new_app_data, aes(x=price, y=prim)) +
  geom_point(size=2, shape=23)
grid.arrange(p3,p4,nrow=1)
##p1 - In this graph, those that do good business(revenue>median) might not necessarily have a favourite rank.This is because some of the top ranked apps are free 
##p2 - In this graph we can see that apps that make revenue and apps that do not have similar user ratings. So user rating does not necessarily depend on price of the app specified
##p3 - In this graph we can see that apps that revenue made does not depend on the number of languages supported
##p4 - In this graph, we can see that apps that made high revenue fall under education genre

#########################################################################################################
##Identifying top 10 apps based on ratings count(we assume if app is rated it must have been downloaded)
#########################################################################################################

top_10 <- new_app_data[order(-new_app_data$rating_count_tot),]
top_10_apps <- top_10[1:10,]
##Interesting thing to note - None of the top 10 downloaded apps have any revenue or price associated

#########################################################################################################
##Bar Plot
#####################################################################################################
p<-ggplot(data=top_10_apps, aes(y=rating_count_tot, x=track_name, fill = track_name, xlab = "App Name")) +
  geom_bar(stat="identity")
p
#The top 10 downloaded apps are - Angry birds,Bible,Candy Crush Saga, Clash of Titans, FacBook. Insta, Pandora, Pinterest, Spotify, Temple Run
#Facebook is the top most downloaded app

######################################################################################################
##Pie Chart to see how top apps and primary genre are related
###################################################################################################

install.packages("plotrix")
library(plotrix)

slices <- c(2,1,4,2,1)
pct <- round(slices/sum(slices)*100)
#slices <- top10$rating_count_tot
lbls <- c("Social Networking","Photo & Video","Games","Music","Reference")
lbls <- paste(lbls,pct)
lbls <- paste(lbls,"%",sep="")
pie3D(slices,labels=lbls,explode=0.1,col=rainbow(length(lbls)),
      main="PIE CHART OF TOP TEN RATINGS AGAINST PRIMARY GENRE ")

##Top 10 apps against genre tells us that Games apps are the most popular followed by social networking apps

#####################################################################################################
##Bar Plot to see how favourite apps and supporting devices relate to top 10 apps
#####################################################################################################

size <- as.factor(top_10_apps$size_bytes)
ggplot(data=top_10_apps, aes(x=track_name, y=size, fill = track_name)) +
  geom_bar(stat="identity")
#Facebook has the most size of download but is still the most downloaded app. Bible comes second. 
#Most of the gaming apps have a heavy size to download but are still most popularly downloaded apps
ggplot(data=top_10_apps, aes(x=track_name, y=user_rating, fill = track_name)) +
  geom_bar(stat="identity")
#Can be seen that most of the top 10 apps have similar user ratings(4 and above)
#Despite Facebook being the most downloaded app it has the least user rating compared to other top 10 apps


#############################################################################################################
##Correlation plot for feature engineering
############################################################################################################

nums <- unlist(lapply(top_10_apps, is.numeric))
top_10_num <- top_10_apps[, nums]
top_10_num <- top_10_num[, !(colnames(top_10_num) %in% c("id","vpp_lic","game_enab","rating_above_med","size_bytes","track_name","revenue","price","revenue_above_med"))]
corr <- round(cor(top_10_num), 1)
ggcorrplot(corr)
#This graph gives the correlation of feature of top 10 apps. We can see that the folowing features are correlated to user ratings:
#No of screen shots available
#No of supporting devuces
#Ratings of current version of app
#Total count of current version downloaded
#We see that no of languages app supports does not relate to user rating
#Also, all the top apps downloaded are free

##############################################################################################################
##Let us check top 5 apps which carry a price
##############################################################################################################

top_5_price <- new_app_data[order(-new_app_data$price),]
top_5_price <- top_10_price[1:5,]
ggplot(data=top_5_price, aes(y=price, x=track_name, fill = track_name, xlab = "App Name")) +
  geom_bar(stat="identity")

##############################################################################################################
##Let us check top 10 apps which have good content rating
##############################################################################################################
contr <- as.factor(new_app_data$cont_rating)
top_10_cont <- new_app_data[order(-contr),]
top_10_cont <- top_10_cont[1:10,]
ggplot(data=top_10_cont, aes(y=cont_rating, x=track_name, fill = track_name, xlab = "App Name")) +
  geom_bar(stat="identity")

################################################################################################################
##Prediction
################################################################################################################

##############################################
# Feature Selection by Xgboost
##########################################
#objective = "binary:logistic": we will train a binary classification model ;
#max_depth = 2: the trees won't be deep, because our case is very simple ;
#nthread = 2: the number of cpu threads we are going to use;
#nrounds = 2: there will be two passes on the data, the second one will enhance the model by further reducing the difference between ground truth and prediction.
#verbose = 2: will print information after every rounds


newappdata = na.omit(app_data)
newappdata[!complete.cases(newappdata),] #checking na values if present

newappdata <- mutate(newappdata, resp = ifelse(newappdata$user_rating > median(newappdata$user_rating),1,0)) #resp column which has 0/1 response
head(newappdata)
as.numeric(newappdata$ver)
as.numeric(newappdata$prime_genre)
bstDense1 <- xgboost(data = as.matrix(newappdata$id), label = newappdata$resp ,max_depth = 2, eta = 1, nthread = 2, nrounds = 1, objective = "binary:logistic")
#bstDense2 <- xgboost(data = as.matrix(newappdata$track_name), label = newappdata$resp ,max_depth = 2, eta = 1, nthread = 2, nrounds = 1, objective = "binary:logistic")
bstDense3 <- xgboost(data = as.matrix(newappdata$size_bytes), label = newappdata$resp ,max_depth = 2, eta = 1, nthread = 2, nrounds = 1, objective = "binary:logistic")
bstDense4 <- xgboost(data = as.matrix(newappdata$price), label = newappdata$resp ,max_depth = 2, eta = 1, nthread = 2, nrounds = 1, objective = "binary:logistic")
bstDense5 <- xgboost(data = as.matrix(newappdata$rating_count_tot), label = newappdata$resp ,max_depth = 2, eta = 1, nthread = 2, nrounds = 1, objective = "binary:logistic")
bstDense6 <- xgboost(data = as.matrix(newappdata$rating_count_ver), label = newappdata$resp ,max_depth = 2, eta = 1, nthread = 2, nrounds = 1, objective = "binary:logistic")
bstDense7 <- xgboost(data = as.matrix(newappdata$user_rating), label = newappdata$resp ,max_depth = 2, eta = 1, nthread = 2, nrounds = 1, objective = "binary:logistic")
bstDense8 <- xgboost(data = as.matrix(newappdata$user_rating_ver), label = newappdata$resp ,max_depth = 2, eta = 1, nthread = 2, nrounds = 1, objective = "binary:logistic")
#bstDense9 <- xgboost(data = as.matrix(newappdata$ver), label = newappdata$resp ,max_depth = 2, eta = 1, nthread = 2, nrounds = 1, objective = "binary:logistic")
bstDense10 <- xgboost(data = as.matrix(as.numeric(newappdata$cont_rating)), label = newappdata$resp ,max_depth = 2, eta = 1, nthread = 2, nrounds = 1, objective = "binary:logistic")
bstDense11 <- xgboost(data = as.matrix(as.numeric(newappdata$prime_genre)), label = newappdata$resp ,max_depth = 2, eta = 1, nthread = 2, nrounds = 1, objective = "binary:logistic")
bstDense12 <- xgboost(data = as.matrix(newappdata$sup_devices.num), label = newappdata$resp ,max_depth = 2, eta = 1, nthread = 2, nrounds = 1, objective = "binary:logistic")
bstDense13 <- xgboost(data = as.matrix(newappdata$ipadSc_urls.num), label = newappdata$resp ,max_depth = 2, eta = 1, nthread = 2, nrounds = 1, objective = "binary:logistic")
bstDense14 <- xgboost(data = as.matrix(newappdata$lang.num), label = newappdata$resp ,max_depth = 2, eta = 1, nthread = 2, nrounds = 1, objective = "binary:logistic")
bstDense15 <- xgboost(data = as.matrix(newappdata$vpp_lic), label = newappdata$resp ,max_depth = 2, eta = 1, nthread = 2, nrounds = 1, objective = "binary:logistic")


#error_attr<-c(bstDense1,bstDense3,bstDense4,bstDense5,bstDense6,bstDense7,bstDense8,bstDense10,bstDense11,bstDense12,bstDense13,bstDense14,bstDense15)

#ERROR RATES

#size_bytes = 0.438377
#price = 0.438377
#rating_count_tot = 0.325552
#rating_count_ver = 0.340142
#user_rating_ver = 0.190079 xxxx
#cont_rating = 0.435459
#prime_genre = 0.438377 
#sup_devices.num = 0.437960
#ipadSc_urls.num = 0.437127 
#lang.num = 0.395443 
#vpp_lic = 0.438377 XXXX




############################################
## randomforest
##############################################
#log1 = glm(resp ~ .-id - track_name - vpp_lic,data = newappdata, family = binomial)
install.packages("randomForest")
library(randomForest)
fit_rf = randomForest(user_rating~. -track_name -ver -id, data=newappdata)
#rating_count_tot rating_count_ver user_rating_ver prime_genre ipadSc_urls.num lang.num
importance(fit_rf)
varImpPlot(fit_rf)
#rflinreg <- lm(user_rating ~., data=newappdata)
pred_rf <- predict(fit_rf, newappdata)
mean((pred_rf - newappdata$user_rating)^2)
avg <- mean(newappdata$user_rating)
r2 <- 1 - mean((pred_rf - newappdata$user_rating)^2) / mean((avg - newappdata$user_rating)^2)
r2 #0.989 
#good model



#########################################################
# New dataset of important features
########################################################
data2<-newappdata[,c("user_rating","rating_count_tot","user_rating_ver","rating_count_ver","prime_genre","size_bytes","lang.num","ipadSc_urls.num","sup_devices.num","price")]

head(data2)
dim(data2) #7197 10

########
#Training and test data
######
set.seed(1234)
C = sample(1:nrow(data2), 0.67*nrow(data2))
y_train = data2$user_rating[C]
y_test = data2$user_rating[-C]
x_train = data2[C,]
x_test = data2[-C,]

################################################

###### Bagging  ###########################

################################################
set.seed(12345678)
arr = c(3,9)
i=1
error.store <- c()
for (i in 1:2) {
  
  bag_fit<-randomForest(user_rating~., data = x_train , n.tree = 10000, mtry = arr[i])
  varImpPlot(bag_fit)
  importance(bag_fit)
  y_hat3<-predict(bag_fit, newdata = x_test , type = "response")
  y_hatnum1<-as.numeric(y_hat3) 
  
  misclass_bag<-sum(abs(y_test-y_hatnum1))/length(y_hatnum1)
  error.store <- c(error.store,misclass_bag )
  
}
error.store
#0.2927597 0.2884741

##################################################3
## SVM 
######################################################
#C: It is the regularization parameter,
#kernel: It specifies the kernel type to be used in the algorithm. 
#gamma: It is the kernel coefficient
y_train <- as.factor(y_train)
svm_model <- svm(user_rating~., data = x_train)

svm_tune <- tune(svm, train.x=x_train, train.y=y_train, kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))

svm_model_after_tune <- svm(user_rating ~ ., data=data2, kernel="radial", cost=svm_tune$best.parameters[1,1], gamma=svm_tune$best.parameters[1,2])

pred <- predict(svm_model_after_tune,x_test)
pred <- data.frame(pred)


#######################################################
## Boosting
#####################################################
#### adding resp (0/1)
#data3 is with 0/1 resp
data3<-newappdata[,c("resp","rating_count_tot","user_rating_ver","rating_count_ver","prime_genre","size_bytes","lang.num","ipadSc_urls.num","sup_devices.num","price")]

head(data3)
dim(data3) #7197 10

########
#Training and test data
######
set.seed(12134)
k = sample(1:nrow(data3), 0.67*nrow(data3))
y_train1 = data3$resp[k]
y_test1 = data3$resp[-k]
x_train1 = data3[k,]
x_test1 = data3[-k,]
head(x_train1)
boost.train<-x_train1
boost.test<-x_test1
shrink<-c(.1,.4,.6,.8)
max_iter <-1000
store_error<-c()
for(i in 1:length(shrink)){
  boost.fit<-gbm(resp~., data = boost.train, n.trees = max_iter,shrinkage = shrink[i], interaction.depth = 3, distribution = "adaboost")
  temp<-c()
  for(j in 1:max_iter){
    y_hat<-predict(boost.fit, newdata = boost.test, n.trees = j, type = "response")
    misclass_boost<-sum(abs(y_test1-y_hat))/length(y_hat)
    temp<-c(temp,misclass_boost)
  }
  store_error<-cbind(store_error,temp)
  
}
store_error

head(store_error)
colnames(store_error)<-paste("shrinkage", shrink, sep = ":")
plot(store_error[,1], main ="error profiles", ylab="error",xlab="boosting iterations" , ylim=c(.07,.5))
lines(store_error[,2],col="red")
lines(store_error[,3],col="blue")
lines(store_error[,4],col="green")





