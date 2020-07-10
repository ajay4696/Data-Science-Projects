#clear Environment

rm(list = ls())

# set Working Directory 

setwd("F:/DATA SCIENCE/# Project/1")

# confirm working directory 

getwd()

#Load Data

df = read.csv('day.csv')

##################################### Exploratory Data Analysis 

class(df)
head(df)
dim(df)
names(df)
str(df)
summary(df)

#remove "instant" variable as its just index and "dteday" as we need to predict count on 
#seasonal basis not on date basis and also drop "casual" and "registered" as count is sum of this two

df = subset(df,select = -c(instant,dteday,casual, registered))
names(df)

############################ Rename the variables for better understanding

names(df)[2]="year"
names(df)[3]="month"
names(df)[7]="weather"
names(df)[8]="temperature"
names(df)[10]="humidity"
names(df)[12]="count"

names(df)

############################ Seperating categorical and numerical variables

#categorical variables 

cat_names=c('season','year','month','holiday','weekday','workingday','weather')

#numerical variables

cnames=c('temperature', 'atemp','humidity', 'windspeed','count')

############################### Data Pre-processing

#checking for missing values

sum(is.na(df))

#--> there's no missing value in the Data

df1=df
#df=df1

##############################  Outlier Analysis

library(ggplot2)

for (i in 1:length(cnames)) {
  assign(paste0("as",i), ggplot(aes_string(y = (cnames[i]), x = "count"), data = subset(df))+
           stat_boxplot(geom = "errorbar", width = 0.5)+
           geom_boxplot(outlier.colour = "red", fill = "green", outlier.shape = 18,
                        outlier.size = 2, notch = FALSE)+
           theme(legend.position = "bottom")+
           labs(y=cnames[i],x="Bike Count")+
           ggtitle(paste("Boxplot for Count of Bikes with", cnames[i])))
  
}

gridExtra::grid.arrange(as1,as2,as3,as4,as5,ncol=2)

#--> According to the outlier Analysis varibles "windspeed" and "humidity" shows outliers

# removing outliers by capping upper fence and lower fence values

for(i in cnames){
  print(i)
  #Quartiles
  Q1 = quantile(df[,i],0.25)
  Q3 = quantile(df[,i],0.75)
  
  #Inter quartile range 
  IQR = Q3-Q1
  
  # Upperfence and Lower fence values 
  UL = Q3 + (1.5*IQR(df[,i]))
  LL = Q1 - (1.5*IQR(df[,i]))
  
  # No of outliers and inliers in variables 
  No_outliers = length(df[df[,i] > UL,i])
  No_inliers = length(df[df[,i] < LL,i])
  
  # Capping with upper and inner fence values 
  df[df[,i] > UL,i] = UL
  df[df[,i] < LL,i] = LL
  
}

# plotting boxplots after removing outiers

for(i in 1:length(cnames))
{
  assign(paste0("zx",i),ggplot(aes_string(y=(cnames[i]),x = 'count'), data=subset(df))+
           stat_boxplot(geom = "errorbar",width = 0.5) +
           geom_boxplot(outlier.color = "red",fill="green",
                        outlier.shape = 18,outlier.size = 1,notch = FALSE)+
           theme(legend.position = "bottom")+
           labs(y = cnames[i],x='count')+
           ggtitle(paste("boxplot of count for",cnames[i])))
}

gridExtra::grid.arrange(zx1,zx2,zx3,zx4,zx5,ncol = 2)

#--> from this boxplots the variables are free from outliers

########################## understanding Data using visualization 

# checking impact of categorical variables on count

for(i in 1:length(cat_names))
{
  assign(paste0("b",i),ggplot(aes_string(y='count',x = (cat_names[i])),
                              data=subset(df))+
           geom_bar(stat = "identity",fill = "DarkSlateBlue") +
           # labs(title = "Scatter Plot of count vs", x = (cnames[i]), y = "count")+
           ggtitle(paste("Number of bikes rented with respect to",cat_names[i])))+
    theme(axis.text.x = element_text( color="black", size=8))+
    theme(plot.title = element_text(face = "bold"))
}

# using library(gridExtra)
gridExtra::grid.arrange(b1,b2,b3,b4,ncol = 2)
gridExtra::grid.arrange(b5,b6,b7,ncol = 2)

#--> From barplot we can observe below points 

#1.) season vs count : Summer, Fall and Winter has more count as compared to Spring season with 
#    almost between 4000 to 8000 bike count on daily basis 
#2.) year vs count : the count of bike rented increased in the year 2012 as compared to 2011 
#3.) month vs count : the count goes on increases gradually from march and reaches maximum up to
#    october and slightly decreases in november and december. 
#4.) holiday vs count : the count of bike rented is much high on holidays as compared to working day 
#5.) week day vs count : bike count is maximum on day 5 and 6 as per the weekdays 
#6.) working day vs count : the count is slightly increased on week ends as compared to working days 
#7.) weather vs count : the count of bike rented is maximum on days having clear weather with few or 
#    partly cloudy as compared to days with mist combined with clouds and least in bad weather

###############################################################################################
# Bikes rented with respect to Working day

ggplot(df, aes(x= reorder(weekday, -count), y = count))+
  geom_bar(stat = "identity", fill = "blue")+
  labs(title = "No. of Bikes rented vs weekdays", x = "days of week")+
  theme(panel.background = element_rect("pink"))+
  theme(plot.title = element_text(face = "bold"))
#--> maximum bike rented on day 5 and 4 and least on day 0 

# Bikes rented with respect to temp and humidity

ggplot(df,aes(temperature,count)) + 
  geom_point(aes(color=humidity),alpha=0.5) +
  labs(title = "Bikes rented with respect to variation in temperature and humidity", x = "temperature")+ theme_bw()
#--> maximum bike rented between temp 0.50 to 0.75 and humidity 0.50 to 0.80 


# Bikes rented with respect to temp and windspeed

ggplot(df, aes(x = temperature, y = count))+
  geom_point(aes(color=windspeed))+
  labs(title = "Bikes rented with respect to temperature and windspeed", x = "temperature")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  theme_bw()
#--> maximum bike rented temperature between 0.50 to 0.75 and windspeed below 0.2

# Bikes rented with respect to temp and season

ggplot(df, aes(x = temperature, y = count))+
  geom_point(aes(color=season))+
  labs(title = "Bikes rented with respect to temperature and season", x = "temperature")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  theme_bw()
#--> maximum bike rented between temperature 0.5 to 0.75 and for season 2 and 3

########################### Feature Selection
df1 = df
df = df1

# correlation analysis by plotting correlation plot

library(corrgram)
corrgram(df[,cnames],order = F,upper.panel = panel.pie,
         text.panel = panel.txt,main="Correlation plot for numeric variables")

#-->According to correlation analysis temp and atemp variables are highly correlated therefore we drop atemp variable 

# Anova analysis for categorical variable with target numeric variable

for(i in cat_names){
  print(i)
  Anova_result= summary(aov(formula = count~ df[,i],df))
  print(Anova_result)
}

#--> According to Anova analysis variables "holiday","weekday" and "workingday" have p value > 0.05
#therefore we drop them as well.

# Dimension reduction

df = subset(df,select = -c(atemp,holiday,weekday,workingday))


# our data after dimension reduction 

summary(df)

head(df)

# updating  continous and categorical variables after dimension reduction

# Continuous variable
cnames= c('temperature','humidity', 'windspeed', 'count')

# Categorical variables
cat_names = c('season', 'year', 'month','weather')

##################################### Feature Scaling

#checking distribution of each continuous variables

for(i in 1:length(cnames))
{
  assign(paste0("h",i),ggplot(aes_string(x=(cnames[i])),
                              data=subset(df))+
           geom_histogram(fill="darkslateblue",colour = "black")+geom_density()+
           scale_y_continuous(breaks =scales::pretty_breaks(15))+
           scale_x_continuous(breaks = scales::pretty_breaks(15))+
           theme_bw()+xlab(cnames[i])+ylab("Frequency")+
           ggtitle(paste("distribution of ",cnames[i])))
}

gridExtra::grid.arrange(h1,h2,h3,h4,ncol = 2)

#--> According to distribution plot all data is Normalized

# saving the pre_processed data 
write.csv(df, "bike_rental_data.csv", row.names = FALSE)

##################################### Model Development

# cleaning R Environment
library(DataCombine)
rmExcept("df")

# copy data
df=df1
df1=df

# Function for Error metrics to calculate performance of model
mape = function(y,y1){
  mean(abs((y-y1)/y))*100
}

# Function for r2 to calculate the goodness of fit of model
rsquare=function(y,y1){
  cor(y,y1)^2
}

# Function for RMSE value 
rmse = function(y,y1){
  difference = y - y1
  root_mean_square = sqrt(mean(difference^2))
  print(root_mean_square)
}

# calling Categorical varaibles 
cat_names= c("season","year","month","weather")

# creating dummy variables using dummies library
library(dummies)
df = dummy.data.frame(df,cat_names)

dim(df)
head(df)
#--> hence dummy data set is created 

# Dividing data into train and test sets

library(caTools)
set.seed(123)
sample = sample.split(df, SplitRatio = 0.80)
train1 = subset(df, sample == TRUE)
test1 = subset(df, sample == FALSE)

############################# Decision Tree for Regression 
# Model Development on train data 
library(rpart)

DT_model = rpart(count~., train1,method = "anova")
DT_model

# Prediction on train data
DT_train= predict(DT_model,train1[-25])

# Prediction on test data
DT_test= predict(DT_model,test1[-25])

# MAPE For train data
DT_MAPE_Train = mape(train1[,25],DT_train)#56.09
#--> DT_MAPE_Train = 56.09

# MAPE For train data test data
DT_MAPE_Test = mape(test1[,25],DT_test)
#--> DT_MAPE_Test = 21.96

# Rsquare  For train data
DT_r2_train = rsquare(train1[,25],DT_train)
#--> DT_r2_train = 0.7871

# Rsquare For test data       
DT_r2_test = rsquare(test1[,25],DT_test)
#--> DT_r2_test = 0.8041

# rmse For train data
DT_rmse_train = rmse(train1[,25],DT_train)
#--> DT_rmse_train = 889.295

# rmse For test data
DT_rmse_test = rmse(test1[,25],DT_test)
#--> DT_rmse_test = 877.962

##################################### Random Forest for Regression

# Model Development on Train data using randomForest library  
RF_model= randomForest(count~.,train1,ntree=100,method="anova")

RF_model = randomForest::randomForest(count~.,train1,ntree=100, method="anova")

# Prediction on train data
RF_train= predict(RF_model,train1[-25])

# Prediction on test data
RF_test = predict(RF_model,test1[-25])

# MAPE For train data
RF_MAPE_Train = mape(train1[,25],RF_train)
#--> RF_MAPE_Train = 26.04

# MAPE For test data
RF_MAPE_Test = mape(test1[,25],RF_test)
#--> RF_MAPE_Test = 15.27

# Rsquare  For train data
RF_r2_train=rsquare(train1[,25],RF_train)
#--> RF_r2_train = 0.9658

# Rsquare For test data       
RF_r2_test=rsquare(test1[,25],RF_test)
#--> RF_r2_test = 0.891

# rmse For train data
RF_rmse_train = rmse(train1[,25],RF_train)
#--> RF_rmse_train = 367.886

# rmse For test data
RF_rmse_test = rmse(test1[,25],RF_test)
#--> RF_rmse_test = 654.77

#################################### Linear Regression model

# Recalling numeric Variables to check variance inflation factor for Multicollinearity
cnames= c("temperature","humidity","windspeed")
numeric_data= df[,cnames]

# VIF test  using usdm library
library(usdm)
vifcor(numeric_data,th=0.6)
#--> value of VIF of all variables are almost 1 so there's no multicollinearity issue.

# Linear Regression model development
LR_model = lm(count ~.,data = train1)
summary(LR_model) 

# prediction on train data 
LR_train = predict(LR_model,train1[,-25])

# prediction on test data 
LR_test= predict(LR_model,test1[-25])

# MAPE For train data
LR_MAPE_Train = mape(train1[,25],LR_train)
#--> LR_MAPE_Train = 44.605

# MAPE For test data
LR_MAPE_Test = mape(test1[,25],LR_test)
#--> LR_MAPE_Test = 18.032

# Rsquare For train data
LR_r2_train = rsquare(train1[,25],LR_train)
#--> LR_r2_train = 0.84

# Rsquare For test data
LR_r2_test = rsquare(test1[,25],LR_test)
#--> LR_r2_test = 0.83

# rmse For train data
LR_rmse_train = rmse(train1[,25],LR_train)
#--> LR_rmse_train = 769.97

# rmse For test data
LR_rmse_test = rmse(test1[,25],LR_test)
#--> LR_rmse_test = 811.08

############################################ Results 

Model = c('Decision Tree for Regression', 'Random Forest', 'Linear Regression')

MAPE_Train = c(DT_MAPE_Train, RF_MAPE_Train, LR_MAPE_Train)

MAPE_Test = c(DT_MAPE_Test, RF_MAPE_Test, LR_MAPE_Test)

Rsquare_Train = c(DT_r2_train, RF_r2_train, LR_r2_train)

Rsquare_Test = c(DT_r2_test, RF_r2_test, LR_r2_test)

Rmse_Train = c(DT_rmse_train, RF_rmse_train, LR_rmse_train)

Rmse_Test = c(DT_rmse_test, RF_rmse_test, LR_rmse_test)

Final_results = data.frame(Model,MAPE_Train,MAPE_Test,Rsquare_Train,
                           Rsquare_Test,Rmse_Train,Rmse_Test)

Final_results
# Saving the Final Output
write.csv(Final_results, "Final_results.csv", row.names = FALSE)

# From above results Random Forest model have optimum values and hence best model.
