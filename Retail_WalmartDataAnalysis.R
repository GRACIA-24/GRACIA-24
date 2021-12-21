setwd("D:/Data/Data_Frames")
getwd()

### IMPORTING THE Walmart_Store_sales.csv file

library(readxl)

walmart_ss <- read.csv("Walmart_Store_sales.csv" , header = TRUE, sep = ',')

View(walmart_ss)
dim(walmart_ss)

names(walmart_ss)

sum(is.na(walmart_ss))

library(corrplot)

wlmt_corr <- walmart_ss %>%
  select ( Store , Weekly_Sales , Temperature , Fuel_Price, Holiday_Flag , CPI , Unemployment )

set.seed(123)

corr <- cor ( wlmt_corr ) 

corrplot ( corr, order = "hclust", outline = T, cl.pos = 'n', 
           rect.col = "black",  tl.col = "red", addCoef.col = "black", 
           number.digits = 2, number.cex = 0.60, tl.cex = 0.7, cl.cex = 1, 
           col = colorRampPalette (c ( "blue","white","red" )) (100) )


################################ ANALYSIS TASKS ################################ 

###Basic Statistics tasks

### Question 1: Which store has maximum sales

agg_sum <- aggregate(Weekly_Sales ~ Store , data = walmart_ss , FUN = sum)

colnames(agg_sum) <- c("Store" , "Total_sales")
head (agg_sum)

agg_sum[ which.max(agg_sum$Total_sales) , 'Store'] 

## THEREFORE, STORE 20 HAS THE MAXIMUM SALES.


### Question 2: Which store has maximum standard deviation i.e., the sales vary a lot. 

agg_sd <- aggregate(Weekly_Sales ~ Store , data = walmart_ss , FUN = sd)

colnames(agg_sd) <- c("Store" , "Standard_deviation")
head (agg_sd)

agg_sd[ which.max(agg_sd$Standard_deviation) , 'Store'] 

### THEREFORE, STORE 14 HAS THE MAXIMUM STANDARD DEVIATION.

## to find out the coefficient of mean to standard deviation = sd / mean

agg_mean <- aggregate(Weekly_Sales ~ Store , data = walmart_ss , FUN = mean)
colnames(agg_mean) <- c("Store" , "Mean")
head (agg_mean)

coff_sdm <- ( agg_sd[14,2] / agg_mean[14,2] )
coff_sdm

### Therefore the coefficient of mean to standard deviation is 0.1571367


### Question 3: Which store/s has good quarterly growth rate in Q3'2012

##converting and grouping dates into quarters

library (tidyverse)
library(plyr) 
library(dplyr)
library(zoo)

class(walmart_ss$Date)

walmart_ss$Date = as.Date ( walmart_ss$Date, format = "%d-%m-%Y" )
walmart_ss <-  arrange ( walmart_ss, Date)
  walmart_ss$qdate <- as.yearqtr (walmart_ss$Date)

data_qtr <- walmart_ss %>%
    group_by (qdate) %>%
  summarise( Weekly_Sales = mean( Weekly_Sales) )

##extracting the total sales of each store with the yearqtrs 
agg_qgr <- aggregate( walmart_ss$Weekly_Sales , by = list( walmart_ss $qdate , walmart_ss$Store) , FUN = sum)  

colnames(agg_qgr) <- c("Qdate" , "Store" , "Total_sales")

view (agg_qgr)

##calculating the quarterly growth rate for each store 
qgr_diff <- agg_qgr %>%
  group_by( Store )%>%
  mutate( growth_rate = ((Total_sales - lag( Total_sales))/ lag(Total_sales)) * 100)
  
head(qgr_diff)
  
##filtering the quarterly growth rates of the stores in 2012 Q3
qgr_fltr <- qgr_diff %>% filter( Qdate == "2012 Q3")
  
head(qgr_fltr)

##ordering the growth rate to find which store has good quarterly growth rate
qgr_srtd <- qgr_fltr [order(qgr_fltr$growth_rate, decreasing = T), ]
 
head(qgr_srtd)

### Therefore Store 7 has good quarterly growth rate in Q3'2012


### Question 4: Find out holidays which have higher sales than the mean sales in .
#     non-holiday season for all stores together

view(walmart_ss)

library(tidyr)
library(data.table)

walmart_ss$Date = as.Date ( walmart_ss$Date, format = "%Y-%m-%d" )

## creating a column with variables for Non_Holidays and Holidays

walmart_ss <- walmart_ss %>%                                                     ##column with variable for Non_holidays
  mutate( Non_Holidays =  case_when( Holiday_Flag == 0 ~ "Not_holiday" ))


##Super Bowl: 12-Feb-10, 11-Feb-11, 10-Feb-12, 8-Feb-13
##Labor Day: 10-Sep-10, 9-Sep-11, 7-Sep-12, 6-Sep-13
##Thanksgiving: 26-Nov-10, 25-Nov-11, 23-Nov-12, 29-Nov-13
##Christmas: 31-Dec-10, 30-Dec-11, 28-Dec-12, 27-Dec-13

holidays <- list (
  'Super Bowl' = c('12-Feb-10', '11-Feb-11', '10-Feb-12', '8-Feb-13'),
  'Labor Day' = c('10-Sep-10', '9-Sep-11', '7-Sep-12', '6-Sep-13'),
  'Thanksgiving' = c('26-Nov-10', '25-Nov-11', '23-Nov-12', '29-Nov-13'),
  'Christmas' = c('31-Dec-10', '30-Dec-11', '28-Dec-12', '27-Dec-13'))%>% 
lapply (as.Date, format='%d-%B-%y')

holidays <- melt (as.data.table(holidays), measure.vars=names(holidays)) %>%     ## column with variables for Holidays
  set_names(c("Holidays", "Date"))

walmart_ss <- merge (walmart_ss, holidays, by="Date", all.x=T )

## Finding the Mean sales in the holidays and non-holiday seasons 
# for all the stores together.

walmart_ss$Holiday <- coalesce( walmart_ss$Holidays , walmart_ss$Non_Holidays )
walmart_ss <- walmart_ss[ , -c( 10 , 11 )]

hs_mean <-  aggregate( Weekly_Sales ~ Holiday , data = walmart_ss , FUN = mean )
hs_mean <- hs_mean [order(hs_mean$Weekly_Sales, decreasing = T), ]
colnames(hs_mean) <- c("Holiday" , "Mean_sales")

hs_mean


### Therefore we clearly see that the holidays - Thanksgiving , Super Bowl and Labor Day
#   have mean sales higher than than the non-holiday season.


### Question 5: Provide a monthly and semester view of sales in units and give insights

view(walmart_ss)

library(lubridate)
library(reshape)
library(reshape2)

## Monthly view of Sales


walmart_ss$Month <- months ( walmart_ss$Date )

walmart_ss$Year <- year ( walmart_ss$Date )

monthly_sales <- cast ( walmart_ss , Month ~ Year , value = "Weekly_Sales", sum )

monthly_sales %>% arrange ( match ( monthly_sales$Month , month.name ))

## Semester view of Sales

walmart_ss$quarter <- quarters (walmart_ss$Date)

quarter_sales <- cast (walmart_ss , Year ~ quarter , value = "Weekly_Sales" , sum )

halfyearly_sales<- quarter_sales %>%
  mutate( H1 = Q1 + Q2 , H2 = Q3 + Q4 )

sem_sales <- walmart_ss %>%
  select ( Month , Year , Weekly_Sales ) %>%
  group_by ( Month , Year ) %>%
  summarise ( Total_Sales = sum ( Weekly_Sales ))

halfyearly_sales

### We see that, In the years 2010 and 2011 the sales is higher in H2.
#   Whereas, In the year 2012 the sales is higher in H1.

## Insights on Walmart Store sales 

library(ggplot2)
library(plotly)
library(corrplot)
library(RColorBrewer)
library(graphics)
library(MLmetrics)
library(caTools) 


# 1- Histogram of sales over the period 2010-2012

graph1.weekly<- ts ( walmart_ss$Weekly_Sales , start = 2010 , end = 2012 , frequency = 12 )

?ts

plot(graph1.weekly)

view(agg_mean)

graph2.mean_wkly <- ts (agg_mean$Store, agg_mean$Mean , frequency = 36 )

g <- decompose ( graph2.mean_wkly)

plot(g)

walmart_ss$Store <- as.factor ( walmart_ss$Store )

?ggplot

ggplot ( walmart_ss , aes ( Store, Weekly_Sales ))+
geom_point() +
  geom_point(data = walmart_ss, aes(y = Weekly_Sales), colour = 'blue', size = 3)


# 3. Histogram of walmart store sales 

?hist

hist ( walmart_ss$Weekly_Sales )

hist ( walmart_ss$Weekly_Sales , breaks = 20 , col = "blue" , border = "black")

# 4. Graph of sales with holidays

ggplot ( walmart_ss , aes ( x = Store , y =  Weekly_Sales )) +
  geom_point ( aes ( color = Holiday_Flag )) +
  geom_smooth() + 
  coord_cartesian()


############################## STATISTICAL MODEL ############################### 

### Statistical model to show monthly sales 

agg_mean

as.factor ( walmart_ss$Store )

ggplot ( agg_mean , aes ( x = Store , y  = Weekly_Sales )) +
  geom_point ( aes ( color = Store )) +
  geom_smooth()

arrange ( agg_mean , desc ( Weekly_Sales ))

ms_sales.d <- data.frame(sem_sales)
ms_sales.d

str ( ms_sales.d )

ggplot ( ms_sales.d , aes ( Total_Sales )) +
  geom_histogram ( aes ( y =..density..) , color = "black" , fill ="white" , bins = 20 ) +
  geom_density ( alpha = 0.2 , fill = "red" )

ggplot ( ms_sales.d , aes ( x = Month , y = Total_Sales )) +
  geom_point ( aes ( color = "orange" ))


##### LINEAR REGRESSION

# Splitting dataset into training set and test set
set.seed(123)                                                  # Seed initializes the randomness
sample = sample.split ( walmart_ss, SplitRatio = 0.7 )         # Returns a vector with T for 70% of data

trainingSet = subset ( walmart_ss , sample == T )
testSet = subset ( walmart_ss , sample == F )


# Create model for all columns considered

model = lm ( formula = Weekly_Sales ~ ., data = trainingSet)
summary(model)

y_pred_train = predict(model, newdata = trainingSet)

length(y_pred_train)
view (y_pred_train)

## Visualizing the training set results
ggplot() + 
  geom_point ( aes ( x = trainingSet$Weekly_Sales , y=y_pred_train )) +
  xlab ( 'Actual_WeeklySales' ) +
  ylab ( 'Predicted_WeeklySales' ) +
  ggtitle ( 'Comparison of WeeklySales Forecast' )

## Visualizing the test set results

y_pred_test = predict(model, newdata = testSet)

ggplot() + 
  geom_point (aes ( x = testSet$Weekly_Sales , y = y_pred_test )) +
  xlab( 'Actual_WeeklySales' ) +
  ylab( 'Predicted_WeeklySales' ) +
  ggtitle( 'Comparison of WeeklySales Forecast' )

### Parameters to validate the accuracy of the model and improvise.

MAPE(y_pred_test,trainingSet$Weekly_Sales)

RMSE(y_pred_test,trainingSet$Weekly_Sales)

# We see that, when all factors are considered, then the model is having highest accuracy for 
# prediction model at 70.5%.

#####  Linear one to one comparison

## Weekly Sales vs Store

model = lm ( formula = Weekly_Sales ~ Store, data = trainingSet )
summary(model)

y_pred_train = predict ( model , newdata = trainingSet )

length(y_pred_train)
view(y_pred_train)

## Visualizing the training set results
ggplot() + 
  geom_point ( aes ( x = trainingSet$Weekly_Sales , y = y_pred_train )) +
  xlab ( 'Actual_WeeklySales' ) +
  ylab ( 'predicted_WeeklySales' ) +
  ggtitle ( 'Comparison of WeeklySales Forecast' )

## Visualizing the test set results

y_pred_test = predict (model , newdata = testSet )

ggplot() + 
  geom_point ( aes ( x = testSet$Weekly_Sales , y = y_pred_test )) +
  xlab( 'Actual_WeeklySales' ) +
  ylab( 'Predicted_WeeklySales'  )+
  ggtitle( 'comparison of WeeklySales forcast' )

### Parameters to validate the accuracy of the model and improvise.

MAPE(y_pred_test,trainingSet$Weekly_Sales)

RMSE(y_pred_test,trainingSet$Weekly_Sales)

# based on Holiday_Flag vs Weekly Sales

model = lm ( formula = Weekly_Sales ~ Holiday_Flag, data = trainingSet)
summary(model)

trainingSet$Weekly_Sales_pred = predict (model , newdata = trainingSet )

length(trainingSet$Weekly_sales_pred)

plot ( trainingSet$Weekly_Sales , resid(model), 
     ylab = "Residuals", 
     xlab = "WeeklySales", 
     main = "Residual plot" ) 

## Visualizing the test set results

y_pred_test = predict ( model, newdata = testSet )

ggplot() +
  geom_point ( aes ( x = trainingSet$Holiday_Flag , y = trainingSet$Weekly_Sales ), colour = 'yellow') +
  geom_line ( aes ( x = trainingSet$Holiday_Flag , y = trainingSet$weekly_Sales_pred ), colour = 'blue') +
  xlab( 'Holiday_Flag' ) +
  ylab( 'Weekly_Sales' ) +
  ggtitle ( 'Holiday_Flag vs Weekly_Sales (Training set)' ) 

ggplot() + 
  geom_point ( aes ( x = testSet$Weekly_Sales , y = y_pred_test )) +
  xlab( 'Actual_WeeklySales' ) + 
  ylab( 'Predicted_WeeklySales' ) +
  ggtitle( 'Comparison of WeeklySales Forecast' )

plot ( trainingSet$Weekly_Sales, resid(model), 
     ylab = "Residuals", 
     xlab = "Sales",
     main = "Residual Plot")


## Parameters to validate the accuracy of the model and improvise.

MAPE(y_pred_test,trainingSet$Weekly_Sales)

RMSE(y_pred_test,trainingSet$Weekly_Sales)

plot ( trainingSet$Weekly_Sales, resid(model), 
     ylab = "Residuals", 
     xlab = "Sales", 
     main = "Residual plot")

### Select the model which gives best accuracy.

head ( walmart_ss[,2:9] )

str ( walmart_ss )

walmart_ss$Holiday_Flag <- as.factor ( walmart_ss$Holiday_Flag )

walmart_ss1 <- walmart_ss %>%
  select ( Store , Weekly_Sales , Temperature , Fuel_Price , CPI , Unemployment , Year )

set.seed(123)

corr <- cor ( walmart_ss1 )

View(corr)

corrplot ( corr,order = "hclust", outline = T, cl.pos = 'n', 
           rect.col = "black",  tl.col = "indianred4", addCoef.col = "black", 
           number.digits = 2, number.cex = 0.60, tl.cex = 0.7, cl.cex = 1, 
           col = colorRampPalette (c ( "green4","white","red" )) (100) )

### Factors affecting the weekly sales individually are Store,Holiday_flag,temp,CPI and Unemployment
#   but when their combined effect is considered on sales, then a better model in multiple regression 
#   gives an accuracy model of 70.5%.




























