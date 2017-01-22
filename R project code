install.packages("psych")		#Perform descriptive statistics
install.packages("ggplot2")		#For advanced figures, plots, charts
install.packages("Hmisc")		#Provides correlation matrix with significance values
install.packages("RODBC")		#Provides an ODBC interface for R
install.packages("sqldf")
library("psych")
getwd()

workingdirectory = "E:\\R Project\\kc_house_data\\"
setwd(workingdirectory)

getwd()

housedata = read.csv("kc_house_data.csv",header = T)

summary(housedata)
str(housedata)
names(housedata)
head(housedata , n=10)
nrow(housedata)
ncol(housedata)

#Converting categorical variables to factor

housedata$id = as.factor(housedata$id)
housedata$waterfront = as.factor(housedata$waterfront)
housedata$view = as.factor(housedata$view)
housedata$condition = as.factor(housedata$condition)
housedata$grade = as.factor(housedata$grade)
housedata$condition = as.factor(housedata$zipcode)
#housedata$yr_built = as.factor(housedata$yr_built)
#housedata$yr_renovated = as.factor(housedata$yr_renovated)

#### Use the strptime() function to convert the variables
#### Create a new date object based off of admitted_dt_tm
date = strptime(as.character(housedata$date),"%Y%m%dT000000")

#### Convert the new date object to a dataframe
appraisaldate = as.data.frame(date)

#### Add the new dataframe to the original dataframe
housedata = data.frame(housedata,appraisaldate)

colnames(housedata$date.1) = housedata$newdate;

#Renaming date.1 to newdate
names(housedata)[names(housedata) == 'date.1'] <- 'newdate'


# getting cities data
citiesdata = read.csv("cities_data.csv",header = T)
citiesdata$City = as.factor(citiesdata$City)
nrow(citiesdata)
ncol(citiesdata)

library(sqldf)

#joining cities data with the house sales data using zipcode as the common id.

HouseDataWithCities<-sqldf("SELECT hd.*, c.City from housedata hd 
                         LEFT JOIN citiesdata c
                         ON hd.zipcode=c.ZipCode")


nrow(HouseDataWithCities)
ncol(HouseDataWithCities)
sum(is.na(HouseDataWithCities))
head(HouseDataWithCities,10)
names(HouseDataWithCities)

### Year House Sold
HouseDataWithCities$YearSold<-as.numeric(substr(HouseDataWithCities$date, 1,4))

### Month House Sold
HouseDataWithCities$MonthSold<-as.numeric(substr(HouseDataWithCities$date,5,6))

### Years renovated
HouseDataWithCities$yrsRenovated<-ifelse((HouseDataWithCities$yr_renovated)>0,
                                       (HouseDataWithCities$yr_renovated - HouseDataWithCities$yr_built),0)

### Building Age
HouseDataWithCities$buildingAge<- (HouseDataWithCities$YearSold - HouseDataWithCities$yr_built)


#Data Cleaning

str(HouseDataWithCities)
summary(HouseDataWithCities)
names(HouseDataWithCities)
ncol(HouseDataWithCities)

#Latitude,Longitude and ZipCode variables are removed.
HouseDataWithCities$lat<- HouseDataWithCities$long<- HouseDataWithCities$zipcode<- NULL

#Removing more unnecessary variables
HouseDataWithCities$yr_built <- HouseDataWithCities$yr_renovated <- HouseDataWithCities$yr_renovated <-HouseDataWithCities$YearSold <- NULL

#No of bedrooms < 0 are removed
HouseDataWithCities<-HouseDataWithCities[which(HouseDataWithCities$bedrooms>0),]


#Numeric Data
numericdata<- HouseDataWithCities[,sapply(HouseDataWithCities, is.numeric)]
names(numericdata)
str(numericdata)
ncol(numericdata)

#factor data
factordata<- HouseDataWithCities[,sapply(HouseDataWithCities, is.factor)]
names(factordata)
str(factordata)
ncol(factordata)

#Finding correlation between input variables
corr = cor(numericdata[c(2:9)])
View(corr, "Correlation between input variables")

#Bathrooms, sqft_above, sqft_living15 is highly correlated with sqft_living. SO removing them.
#sqft_lot and sqft_lot15 are highly correlated. So removing one of them
HouseDataWithCities$bathrooms <- HouseDataWithCities$sqft_above <-HouseDataWithCities$sqft_living15 <- NULL
HouseDataWithCities$sqft_lot15 <- NULL

str(HouseDataWithCities)
install.packages("moments")
library("moments")
skewness(numericdata)

#Plotting histograms
hist(HouseDataWithCities$price)
hist(HouseDataWithCities$bedrooms)
hist(HouseDataWithCities$sqft_living)
hist(HouseDataWithCities$sqft_lot)
hist(HouseDataWithCities$floors)
hist(HouseDataWithCities$sqft_basement)
hist(HouseDataWithCities$MonthSold)
hist(HouseDataWithCities$yrsRenovated)
hist(HouseDataWithCities$buildingAge)

# We see that the variables price, sqft_lot, and yrsRenovated are highly skewed
#Applying log transformation to make it more normal.
attach(HouseDataWithCities)
hist(log(price))
hist(log(sqft_lot))
hist(log(yrsRenovated))

#Plot of factor data

plot(waterfront)
plot(view)
plot(condition)
plot(grade)

#Principal Component Analysis
PCA = princomp(numericdata,cor = FALSE)
plot(PCA,main = "Screeplot")

#PCA says that around 4 variables are important

#Factor Analysis
names(numericdata)
FA = factanal(~bedrooms+sqft_living+sqft_lot+floors+sqft_basement+MonthSold+yrsRenovated+buildingAge,factors=4,rotation="varimax", 
  scores="none",data=numericdata)

# 4 factors are sufficient

#DATA SPLITTING
#70% training and 30% testing

str(HouseDataWithCities)
## 70% of the sample size
smp_size <- floor(0.70 * nrow(HouseDataWithCities))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(HouseDataWithCities)), size = smp_size)

train <- HouseDataWithCities[train_ind, ]
test <- HouseDataWithCities[-train_ind, ]

is.na(HouseDataWithCities)
sapply(train, function(x) sum(is.na(x)))

names(HouseDataWithCities)
# multiple linear regression
regressionmodel <- lm( log(price) ~ bedrooms+sqft_living+log(sqft_lot)+floors+waterfront+view+grade+sqft_basement
+MonthSold+yrsRenovated+buildingAge , data = train)

summary(regressionmodel)

#Linearity
pairs(numericdata,panel=panel.smooth)

#finding vif
library(car)
vif(regressionmodel)

#sqft_living has a vif score > 5. Hence removing it.
regressionmodel2 <- lm( log(price) ~ bedrooms+log(sqft_lot)+floors+waterfront+grade+view+sqft_basement
+MonthSold+yrsRenovated+buildingAge , data = train)

summary(regressionmodel2)
vif(regressionmodel2)

#Checking for independence of residuals.
durbinWatsonTest(regressionmodel2)

#The result is not significant. Hence this assumption is not violated

#Linearity and Normality
plot(regressionmodel2)

#Residuals are normally distributed
str(test)

#Regression without log transforming

regressionmodel3 <- lm( price ~ bedrooms+ sqft_lot+floors+waterfront+grade+view+sqft_basement
+MonthSold+yrsRenovated+buildingAge , data = train)


#Predicting the test dataset price values
unique(test$grade)
unique(train$grade)
head(test,10)
test$sqft_lot <- log(test$sqft_lot)
predictions <- exp(predict(regressionmodel2, test))
predictions2 <- predict(regressionmodel3, test)

View(predictions)
View(predictions2)

#Calculating RMSE
install.packages("lubridate")
install.packages("hydroGOF")
library(lubridate)
library(hydroGOF)
library(rpart)

RMSE=rmse(predictions ,test$price)
RMSE2=rmse(predictions2 ,test$price)

# Log transformed model yields a lower RMSE. So log transformed model is better.

#Decision Tree
train2 = subset(train, select=c(price,bedrooms,sqft_lot,floors,waterfront,view,condition,grade,sqft_basement,MonthSold,yrsRenovated,buildingAge))
str(train2)
model <- rpart(price~., train2)
plot(model)
text(model)

pred <- predict(model, test) 
View(pred)
RMSE_tree=rmse(pred ,test$price)

#RMSE for decision tree is lesser than the Multiple regression model. Hence decision tree is the better model

