# Name of Group - Newport 07310
#Team Members - Ameya Ghag, Wen Qiang, Ruijie Wang
#Lending Club Loan Default Prediction project
#ANLY 502
#Professor - Dr. Faith Bradley

#pulling packages for R analysis
install.packages("gmodels")
install.packages("lubridate")
install.packages("plyr")
install.packages("ggplot2")
install.packages("caTools")
install.packages("e1071")
install.packages("ROCR")
install.packages("caret")
install.packages("ROSE")
install.packages("sqldf")
library(gmodels)
library(lubridate)
library(plyr)
library(ggplot2)
library(caTools)
library(e1071)
library(ROCR)
library(caret)
library(ROSE)
library(sqldf)

#In this step I am reading data from the 2016 Q4 data downloaded from Lending Club Website
#This data is being read into a data frame called main_data
# Link - https://www.lendingclub.com/info/download-data.action
#for above link you will need to select '2016 Q4' under Download Loan Data on right side, and then press Download
#It should be a 18.2MB file with around 104K rows

main_data = read.csv("/Users/ameyaghag/Desktop/Lending Club/LoanStats_2016Q4.csv")

#since I will be editing a lot of the columns, I am now create a working data frame below 

main_df <- main_data

#checking for values where loan status is available

main_df$loan_status <- as.character(main_df$loan_status)

#Discarding other values of loan status - and selecting only those where it is 1/0 or Fully Paid/Charged Off
#Charged OFf means it is a bad loan and has been handed over to collection agency for any recovery possible

sel_loan_status <- main_df$loan_status=="Fully Paid" | main_df$loan_status=="Charged Off"
main_df <- subset(main_df, sel_loan_status==TRUE) 

#Encoding loan_status 0 - Charged Off, 1 - Fully paid
main_df$loan_status <- ifelse(main_df$loan_status=="Fully Paid",1,0)
main_df$loan_status <- as.integer(main_df$loan_status)


#in this step i will be removing the % sign and converting from factor to numeric and lastly check for any NA values

str(main_df$int_rate) 
main_df$int_rate <- as.numeric(sub("%","",main_df$int_rate)) 
main_df$int_rate <- main_df$int_rate/100
is.numeric(main_df$int_rate) # TRUE
anyNA(main_df$int_rate)
summary(main_df$int_rate)

#again, here i am removing % and converting to numeric and dividing by 100 to make the scale small enough for a good plot

str(main_df$revol_util) 
main_df$revol_util <- as.numeric(sub("%","",main_df$revol_util)) 
main_df$revol_util <- main_df$revol_util / 100
is.numeric(main_df$revol_util) # TRUE
anyNA(main_df$revol_util) 
summary(main_df$revol_util)

#same as above, converting factor to numeric, and checking for NA missing values

main_df$installment <- as.character(main_df$installment) 
main_df$installment <- as.numeric(main_df$installment) 
is.numeric(main_df$installment) 
anyNA(main_df$installment) 


#same as above, converting factor to numeric, and checking for NA missing values

main_df$revol_bal <- as.character(main_df$revol_bal) 
main_df$revol_bal <- as.numeric(main_df$revol_bal) 
anyNA(main_df$revol_bal) 

#same as above, converting factor to numeric, and checking for NA missing valuesclass(main_df$loan_amnt) 

main_df$loan_amnt <- as.numeric(main_df$loan_amnt) 
is.numeric(main_df$loan_amnt) 
anyNA(main_df$loan_amnt) 

#converting annual income from factor to numeric

main_df$annual_inc <- as.character(main_df$annual_inc) 
main_df$annual_inc <- as.numeric(main_df$annual_inc) 
main_df <- main_df[!is.na(main_df$annual_inc), ]

#converting pub_rec from factor to numeric


main_df$pub_rec <- as.character(main_df$pub_rec)
main_df$pub_rec <- as.numeric(main_df$pub_rec)

#converting dti from factor to numeric

main_df$dti <- as.character(main_df$dti) 
main_df$dti <- as.numeric(main_df$dti) 

#converting open accounts from factor to numeric


main_df$open_acc <- as.character(main_df$open_acc) 
main_df$open_acc <- as.numeric(main_df$open_acc) 


#converting delinq_2yrs from factor to numeric

main_df$delinq_2yrs <- as.character(main_df$delinq_2yrs)
main_df$delinq_2yrs <- as.numeric(main_df$delinq_2yrs) 


#converting inq_last_6mths from factor to numeric

main_df$inq_last_6mths <- as.character(main_df$inq_last_6mths) 
main_df$inq_last_6mths <- as.numeric(main_df$inq_last_6mths) 

#after subsetting data for only loan_status=1/0, we remain with around 65K rows, down from 104K rows


#the dataset originally has around 150 columns
#most of these columns are not fully populated and after looking at data dictionary, only 20 of the variables make
#sense to use for exploratory analysis
#we intend to reduce this further during model building


#in this step, i am selecting only ~20 of the variables that i intend to use for exploratory analysis
#these are stored in selected_features, and then i will use subset to create final_df
#final_Df is the dataframe which we intend to use for exploration

selected_features <- 
c("member_id","loan_status","grade", "sub_grade","open_acc","pub_rec", "dti", 
"delinq_2yrs","inq_last_6mths", "emp_length", "annual_inc", "home_ownership",
"purpose", "addr_state","loan_amnt","int_rate", "installment", "issue_d", 
"revol_bal", "revol_util","total_acc","num_rev_accts")

final_df <- subset(main_df, select = selected_features)

# i will now plot a histogram of most of the variables to derive some information about the data


#plotting a histogram of loan amounts issued here

hist(final_df$loan_amnt, main="Histogram for Loan Amount ($)",xlab="Loan Amount", col="blue",las=1)
summary(final_df$loan_amnt)

#plotting a histogram of interest rate on loans issued here

hist(final_df$int_rate, main="Histogram for Interest Rate",xlab="Interest Rate", col="green",las=1)
summary(final_df$int_rate)

# we see some outliers interest rate, which we will clean up later

#plotting a histogram of monthly installment size issued here

hist(final_df$installment, main="Histogram for Installment Sizes ($)",xlab="Installments", col="purple",las=1)
summary(final_df$installment)

#plotting a histogram of total credit+banking accounts  here

hist(final_df$total_acc, main="Histogram for Total Accounts ever",xlab="# of Total Accts", col="blue",las=1)
summary(final_df$total_acc)

# again we see some outliers, that will be cleaned later

#plotting a histogram of open accounts (credit accounts may be closed, example - credit cards) 

hist(final_df$open_acc, main="Histogram for # of Open Accounts",xlab="# of Open Accts", col="blue",las=1)
summary(final_df$open_acc)

#again some outliers that can be cleaned up

#plotting a histogram of revolving accounts
#revolving accounts are those with an unpaid balance - these are not necessarily accounts in default 

hist(final_df$num_rev_accts, main="Histogram for # of Individuals with Revolving Accts",xlab="# of Revolving Accts", col="blue",las=1)
summary(final_df$num_rev_accts)

#again some outliers that can be cleaned up

#plotting a histogram of credit bureau inquiries in last 6 months here

hist(final_df$inq_last_6mths, main="Histogram for # of Inquiries in Last 6 Months per Individual",xlab="# of Revolving Accts", col="blue",las=1)
summary(final_df$inq_last_6mths)

#a lot of outliers that can be cleaned up


# Total Loans Charge Off vs Fully Paid, broken down by Loan Grade 

#sqldf is a sql integration that treats dataframes like a sql table 
#this method is easier to query dataframes, for generating counts

loan_grade_cnt <- sqldf('select loan_status, grade, count(member_id) as members from final_df group by 1,2')

#using ggplot to plot grades on x axis and # of members on y. loan status =1/0 will be shader used inside barplot
#aes stands for aesthetic mappings -  describe how variables in the data are mapped to geoms visual

plot_a <- ggplot(data = loan_grade_cnt, aes(x = grade, y = members, fill = loan_status)) + geom_bar(stat = "identity")

#now using labs to generate labels

plot_a<- plot_a + labs(y="# of Members", x="Loan Grades from A (top quality) to G (poor)")

#ggtitle is used for generating main title label for chart
      
plot_a <- plot_a + ggtitle("Total Loans Charge Off vs Fully Paid, broken down by Loan Grade")

plot_a

# Total Loans Charge Off vs Fully Paid, broken down by Loan Purpose 

loan_purpose_cnt <- sqldf('select loan_status, purpose, count(member_id) as members from final_df group by 1,2')

plot_b <- ggplot(data = loan_purpose_cnt, aes(x = purpose, y = members, fill = loan_status)) + geom_bar(stat = "identity")

plot_b<- plot_b + labs(y="# of Members", x="Loan Purpose")
      
plot_b <- plot_b + ggtitle("Total Loans Charge Off vs Fully Paid, broken down by Loan Purpose")

plot_b

#plotting a correlation matrix here to understand 

cor(final_df[, sapply(final_df, class) != "factor"]) #Checking multicollinearity

table(final_df$loan_status)

#this step also shows us that dti and revol_utl still have NA values that need to be removed
#will remove outliers in next section now

#for the below steps, we looked at outliers based on summary
#based on max and mean, we set some guesstimated cut-offs
#for example for income, it didnt make sense that some one with an income of $1M would take a very small loan
#or for example, any would have more than $500K in pending loans - those didnt fit our criteria
#removing outliers didnt really affect our dataset volume by more than a 1500 records overall

#now we remove the outliers from the data

#removing outliers from annual income
hist(final_df$annual_inc, main="Histogram checking for outliers in income",xlab="Annual Income ($)",  col="blue",las=1)
summary(final_df$annual_inc)

inc_outliers <- which(final_df$annual_inc > 1000000) 
final_df <- final_df[-inc_outliers,] 

hist(final_df$annual_inc, main="Histogram checking for outliers in income",xlab="Annual Income ($)",  col="blue",las=1)

#removing outliers from open accounts
summary(final_df$open_acc)
hist(final_df$open_acc)

open_acc_outliers <- which(final_df$open_acc > 35 | final_df$open_acc <0 ) 
final_df <- final_df[-open_acc_outliers,] 

#removing outliers from revolving balance

summary(final_df$revol_bal)
plot(final_df$revol_bal)

revol_bal_outlier <- which(final_df$revol_bal > 500000 | final_df$revol_bal <0 ) 
final_df <- final_df[-revol_bal_outlier,] 


#removing outliers from 2 year delinquency

summary(final_df$delinq_2yrs)
plot(final_df$delinq_2yrs)

del2yr_outlier <- which(final_df$delinq_2yrs > 15 | final_df$delinq_2yrs <0 ) 
final_df <- final_df[-del2yr_outlier,]


#Removing outliers for pub_rec
summary(final_df$pub_rec)
plot(final_df$pub_rec)

index.outliers3 <- which(final_df$pub_rec > 10 | final_df$pub_rec <0 ) 
final_df <- final_df[-index.outliers3,]

summary(final_df$dti)

#Removing NA values for dti
final_df <- final_df[!is.na(final_df$dti), ]

#Removing NA values for revol_util

summary(final_df$revol_util)
final_df <- final_df[!is.na(final_df$revol_util), ]
str(final_df)

#loan_status is still an integer, so converting this back to a factor
#this is actually a retro step we added
#while generating confusion matrix later, we had an error where the data type of loan_status and the prediction variable were not the same

final_df$loan_status=as.factor(final_df$loan_status)

str(final_df)
model_data<-final_df

#plotting for correlation matrix

cor(final_df[, sapply(final_df, class) != "factor"]) 

#due to memory issues and constraints on Macbook, we had to remove of the redundant features and use only the following for model building

model_data<-final_df

model_features <- 
c("loan_status", "sub_grade","dti", 
"delinq_2yrs","inq_last_6mths", "annual_inc", 
 "loan_amnt","int_rate", "installment", 
"revol_bal", "revol_util","num_rev_accts")

model_data <- subset(model_data, select = model_features)

anyNA(model_data) # No missing values
dim(model_data)

#Set the seed of R's random number generator, which is useful for creating simulations or random objects that can be
#reproduced. The random numbers are the same, and they would continue to be the same no matter how far out in the
#sequence we went

set.seed(123)

#splitting dataset into 75% for training and 25% for test

#sample_flag is a flag generated by sample.split function, which reserves 75% rows for training data and 25% for test
#training data is stored in training and test in test variables
sample_flag <- sample.split(model_data$loan_status, 0.75)
training <- subset(model_data, sample_flag==TRUE)
test <- subset(model_data, sample_flag==FALSE)

str(test$loan_status)

#model1 consists of the glm logistic regression model
#family=binomial specifies that it is a logistic regression model, since glm consists of multiple model types

model1 <- glm(loan_status ~ ., family = "binomial", data = training)
summary(model1)

#here we are predicting outcomes on test data
#model1 is the object containing the logistic regression model
#newdata is an optional variable, however we have reserved 25% test for testing and comparison
#Type=response since we are predicting using data from test dataset

predict_probablity <- predict(model1, newdata = test, type = "response")
summary(predict_probablity)



#Cut-off value = 0.6
cutoff_test1 <- ifelse(predict_probablity > 0.6, 1,0) #Setting cut-off to be at 0.5
table(test$loan_status,predict_probablity )


#Plotting the ROC-curve
curve_1 <- roc.curve(test$loan_status, cutoff_test1,col="red", main="The ROC-curve for Model with cut-off=0.5")
curve_1

#roc curve is very close to diagonal line, but area under curve is only 0.55

cutoff_test1=as.factor(cutoff_test1)

#generating a confusion matrix
conf_1<-confusionMatrix(test$loan_status,cutoff_test1)
conf_1

#at cutoff=0.6
#accuracy is 75% 
#Prediction     0     1
#         0   616  3110
#         1   773 11205

#based on this we are only seeing 616 loans as charged off or bad loans, while test set has 3726
#so the true p rate is only around 17%

table(test$loan_status)

#so now we tried to raise cut-off value to 0.8

#Cut-off value = 0.8
cutoff_test2 <- ifelse(predict_probablity > 0.8, 1,0) #Setting cut-off to be at 0.8
table(test$loan_status,predict_probablity )


#Plotting the ROC-curve
curve_2 <- roc.curve(test$loan_status, cutoff_test2,col="red", main="The ROC-curve for Model with cut-off=0.8")
curve_2

#curve is now leaning more towards left hand border, and area under curve is 0.62, which has increased

cutoff_test2=as.factor(cutoff_test2)

conf_2<-confusionMatrix(test$loan_status,cutoff_test2)
conf_2

#accuracy of confusion matrix says it is 54% which is lower
#but the TP = 2898, much higher true p rate of 78%

#          Reference
#Prediction    0    1
#         0 2898  828
#         1 6492 5486















