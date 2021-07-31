#Load the dataset
Bigmart_train<-read.csv("C:/r studio/csvfiles/big-mart-sales-dataset/Train_UWu5bXk.csv",header = T)
Bigmart_test<-read.csv("C:/r studio/csvfiles/big-mart-sales-dataset/Test_u94Q5KV.csv", header = T)
head(Bigmart_train)
head(Bigmart_test)
str(Bigmart_train)
str(Bigmart_test)
#Checking Dimension of the train and test
dim(Bigmart_train)
dim(Bigmart_test)
#we have 5681 rows and 11 columns in test dataset. there sholud always be one less column
#in test dataset compare to train dataset

#checking if data has missing values
table(is.na(Bigmart_train))
#in train data we have 1463 missing values. its important to find and locate this missing values

#Lets check the variables in which missing value is there
colSums(is.na(Bigmart_train))
#we can see that Item_Weight has 1463 missing values

summary(Bigmart_train)

#Item_Fat_Content has mis-matched factor levels.
#Minimum value of item_visibility is 0. Practically, this is not possible. If an item occupies shelf space in a grocery store, it ought to have some visibility. We'll treat all 0's as missing values.
#Item_Weight has 1463 missing values (already explained above).
#Outlet_Size has a unmatched factor levels.

#These inference will help us in treating these variable more accurately.

#Graphical representation of variables
library(ggplot2)
ggplot(Bigmart_train, aes(x = Item_Visibility, y = Item_Outlet_Sales)) + geom_point(size = 2.5, color = "navy") + xlab("Item_Visibility") + ylab("Item_Outlet_Sales") + ggtitle(" Item Visibility Vs Item Outlet Sales")
#We can see that majority of sales has been obtained from products having
#visibility less than 0.2. This suggests that item_visibility < 2 must be an 
#important factor in determining sales. Let's plot few more interesting graphs 
#and explore such hidden stories.

ggplot(Bigmart_train, aes(Outlet_Identifier, Item_Outlet_Sales)) + geom_bar(stat = "identity", color = "purple") +theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black"))  + ggtitle("Outlets vs Total Sales") + theme_bw()
#Here, we infer that OUT027 has contributed to majority of sales followed by OUT35.
#OUT10 and OUT19 have probably the least footfall, thereby contributing to the 
#least outlet sales.

ggplot(Bigmart_train, aes(Item_Type, Item_Outlet_Sales)) + geom_bar( stat = "identity") +theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "navy")) + xlab("Item Type") + ylab("Item Outlet Sales")+ggtitle("Item Type vs Sales")
#From this graph, we can infer that Fruits and Vegetables contribute to the highest
#amount of outlet sales followed by snack foods and household products. 
#This information can also be represented using a box plot chart. 
#The benefit of using a box plot is, you get to see the outlier and mean deviation
#of corresponding levels of a variable (shown below).
ggplot(Bigmart_train, aes(Item_Type, Item_MRP)) +geom_boxplot() +ggtitle("Box Plot") + theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "red")) + xlab("Item Type") + ylab("Item MRP") + ggtitle("Item Type vs Item MRP")

#he black point you see, is an outlier. The mid line you see in the box,
#is the mean value of each item type.

#Now we will combine the dataset so that we dont need to seperate coding. 
# to combine the dataframes we must have same number pf columns which we dont have in this case
dim(Bigmart_train)
dim(Bigmart_test)
# here we have one less column in test data. so will add one column
# We can give this column any value. An intuitive approach would be to extract the mean
#value of sales from train data set and use it as placeholder for test variable
#Item _Outlet_ Sales. Anyways, let's make it simple for now. I've taken a value 1. 
#Now, we'll combine the data sets.

Bigmart_test$Item_Outlet_Sales <- 1   # here we are adding the item outlet sales
#column from train to test dataset
combi<- rbind(Bigmart_train,Bigmart_test)

#Impute missing value by median. I'm using median because it is known to be highly
#robust to outliers. Moreover, for this problem, our evaluation metric is RMSE 
#which is also highly affected by outliers. Hence, median is better in this case.

combi$Item_Weight[is.na(combi$Item_Weight)]<- median(combi$Item_Weight, na.rm = TRUE)
table(is.na(combi$Item_Weight))

str(combi)
# In our dataset only 3 variables we have continuous variable. and all other are categorical
#take an example of Item_visibility where we have more zero values and we consider them as missing and will do imputation

combi$Item_Visibility<- ifelse(combi$Item_Visibility==0, median(combi$Item_Visibility), combi$Item_Visibility)

#Now in categorical variables there are mismatched levels in variables that we need to be corrected
levels(combi$Outlet_Size)[1] <- "Other"
library(plyr)
combi$Item_Fat_Content <- revalue(combi$Item_Fat_Content,c("LF" = "Low Fat", "reg" = "Regular"))
combi$Item_Fat_Content <- revalue(combi$Item_Fat_Content, c("low fat" = "Low Fat"))
table(combi$Item_Fat_Content)

# Create a new column 2013 <-  year
combi$Year <- 2013 - combi$Outlet_Establishment_Year

#drop variables not required in modeling
 library(dplyr)
 combi <- select(combi, -c(Item_Identifier, Outlet_Identifier, Outlet_Establishment_Year))

#divide data set
 new_train <- combi[1:nrow(Bigmart_train),]
new_test <-  combi[-(1:nrow(Bigmart_test)),]

#Linear Regression
linear_model<- lm(Item_Outlet_Sales~., data = new_train)
summary(linear_model)

par(mfrow=c(2,2))
plot(linear_model)
# all plots have different story to tell
#but the most imp story is portrayed by Residual vs fitted
#Residual values are the difference between actual and predicted outcome values. 
#Fitted values are the predicted values. If you see carefully, you'll discover it as a 
#funnel shape graph (from right to left ). The shape of this graph suggests that our
#model is suffering from heteroskedasticity (unequal variance in error terms).
#Had there been constant variance, there would be no pattern visible in this graph.

#A common practice to tackle heteroskedasticity is by taking the log of response variable. 
#Let's do it and check if we can get further improvement.
linear_model<-lm(log(Item_Outlet_Sales)~., data = new_train)
summary(linear_model)
#Again check the plot for residual and fitted
# Check the RMSE
RMSE<- function(error) { sqrt(mean(error^2)) }
RMSE(linear_model$residuals)

