# Installing required packages
if(!require(readr)) install.packages("readr" , repos="http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr" , repos="http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2" , repos="http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret" , repos="http://cran.us.r-project.org")
if(!require(purrr)) install.packages("purrr" , repos="http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest" , repos="http://cran.us.r-project.org")
if(!require(pROC)) install.packages("pROC" , repos="http://cran.us.r-project.org")
if(!require(fastDummies)) install.packages("fastDummies" , repos="http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot" , repos="http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr" , repos="http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra" , repos="http://cran.us.r-project.org")

library(knitr)
library(kableExtra)
library(readr)
library(dplyr)
library(ggplot2)
library(caret)
library(purrr)
library(randomForest)
library(pROC)
library(fastDummies)
library(rpart.plot)
library(data.table)

################# Downloading the dataset #################

temp <- tempfile()
url <- "https://drive.google.com/open?id=1cvul7cKtTObr-Ja-UvwO46rZ8z2IqQUf"
download.file(url, "temp")
rawdata <- fread("PS_20174392719_1491204439457_log.csv", header=TRUE)
unlink(temp)

# A few rows of the dataset
head(rawdata)

# Number of total transactions
total_transact<- rawdata %>% nrow()
total_transact

# Check to see if there is any missing data
any(is.na(rawdata)) 

# Calculating the transaction balance in original and destination accounts, converting step to hour of the day
rawdata<- rawdata %>% mutate(trans_orig=newbalanceOrig-oldbalanceOrg,
                   trans_dest=newbalanceDest-oldbalanceDest,
                   hour=step%%24)

#Creating a subset of rawdata including all fraudulant transactions
fraud_data<- rawdata %>% filter(isFraud==1) 

#Calculating fraud transaction percentage
number_of_fraud_transact<- fraud_data %>% nrow()
fraud_percentage<- (number_of_fraud_transact/total_transact)*100
fraud_percentage

# Number of each type of transaction
number_transaction_per_type<- rawdata %>% 
  group_by(type) %>% 
  summarize(len=length(type))

number_transaction_per_type

# Plot of each type of transaction"
rawdata%>% 
  ggplot(aes(x=type, fill=factor(isFraud))) + 
  geom_bar() + 
  scale_y_log10() +
  scale_fill_manual(values = c("#0c2032", "blue")) + 
  xlab("Type of transaction") + 
  ylab('Number of transactions') + 
  ggtitle("Number of transactions per transaction type") +
  theme(plot.title = element_text(hjust = 0.5))


# Table showing the number of fradulent transaction by type
fraud_by_type<- fraud_data%>% 
  group_by(type) %>% 
  summarize(number_of_fraud_transact_by_type=n())
fraud_by_type

# Number of transactions flagged as fraud that were actually fraud
fraud_data %>% 
  nrow()
rawdata %>% 
  filter(isFlaggedFraud==1 & isFraud==1) %>% count()

# Plot of number of transactions versus amount for fraudulent activities
fraud_data %>% 
  ggplot(aes(x=amount)) + 
  geom_histogram(fill="blue", col="#0c2032") + 
  xlab("Amount ($)") + ylab('Number of transactions') + 
  ggtitle("Number of transactions versus amount for fraudulent activities") +
  theme(plot.title = element_text(hjust = 0.5))

# Boxplot showing that fraudulant transactions have a higher median compared to non-fradulent transactions
rawdata %>% 
  ggplot(aes(y=amount, fill=factor(isFraud))) + 
  xlab("isFraud") +
  ylab("Amount") + 
  geom_boxplot() + 
  scale_y_log10() + 
  scale_fill_manual(values=c("#0c2032", "blue")) +       
  theme(axis.text.x = element_blank()) +
  labs(fill = "isFraud") + 
  ggtitle("Boxplot showing transaction amounts for fraudulent vs non-fradulent activities") +
  theme(plot.title = element_text(hjust = 0.5))

################# IDENTIFING FRADULENT DESTINATION ACCOUNTS #################

# The number of transactions in destination accounts
fraud_data %>% 
  group_by(nameDest) %>% 
  summarize(n=n()) %>% 
  filter (n>1) %>% nrow()

fraud_data %>% 
  group_by(nameDest) %>% 
  summarize(n=n()) %>% 
  filter (n>2) %>% nrow()

# Plot showing two groups of fradulent transactions
fraud_data %>% 
  ggplot(aes(x=trans_orig, y=trans_dest, col=type)) + 
  geom_point() + 
  scale_color_manual(values=c("#0c2032", "blue")) +
  ggtitle("Balance difference for destination vs original accounts") +
  theme(plot.title = element_text(hjust = 0.5))


# The number fraudulant activity for which trans_orig is negative.
fraud_data%>% 
  filter(trans_orig<0) %>% 
  count()

# There is no fraudulant activity for which trans_orig is positive.
fraud_data %>% 
  filter(trans_orig>0) %>% 
  count()

#There are 57 instances where the trans_orig remained at zero, but the transaction is Fraud.
fraud_data %>% 
  filter(trans_orig==0) %>% 
  count() 

# fradulent activities with amount, difference in original and destination balance of zero
fraud_data %>% 
  filter(trans_orig==0 & amount==0) %>% 
  select(amount, trans_orig, trans_dest, isFraud) %>% 
  count()

# Fraudulent activities for which money has been taken out of the account, but not transfered to destination account.
fraud_data %>% filter(amount!=0) %>% 
  select(type, amount, trans_orig, trans_dest, isFraud) %>% 
  filter(trans_dest==0) %>% 
  count()

################# TIME EFFECT #################
# Plot of transaction type per hour
rawdata %>%
  ggplot(aes(x=hour, fill=factor(type))) + 
  geom_bar(stat="count") + 
  scale_y_log10() + 
  scale_fill_brewer(palette=c("Blues")) +
  ggtitle("Number of transactions over time") +
  theme(plot.title = element_text(hjust = 0.5))

# Calculating and plotting fraud percentage per hour of the day.
rawdata %>% 
  group_by(hour) %>% 
  summarize(n_fraud_hour=sum(isFraud==1), 
            n_nonfraud_hour=sum(isFraud==0), 
            percentage_fraud_hour=n_fraud_hour/n_nonfraud_hour*100) %>% 
  ggplot(aes(hour, percentage_fraud_hour)) + 
  geom_line(col="blue") + 
  geom_point() + 
  ggtitle("Plot of fraud percentage per hour of the day") +
  theme(plot.title = element_text(hjust = 0.5))

#################### CLEANING DATA #######################
# Keeping only transaction types associated with Fraud activities ('TRANSFER', 'CASH_OUT') 
# Dropping some columns
dat<- rawdata %>% select(type, amount, oldbalanceOrg, newbalanceOrig, oldbalanceDest, newbalanceDest, 
           trans_orig, trans_dest, hour, isFraud) %>% filter(type %in% c("CASH_OUT", "TRANSFER"))

# Converting dependent variable to factor to eliminate error
dat$isFraud<- as.factor(dat$isFraud) 

# Converting categorical variable "type" to numeric variable
dat <- dummy_columns(dat, select_columns = "type")
dat<- dat %>% select(type_CASH_OUT, type_TRANSFER, amount, oldbalanceOrg, newbalanceOrig, oldbalanceDest, newbalanceDest, 
               trans_orig, trans_dest, hour, isFraud)


# Sampling a random subset of data to reduce the size
dat<- sample_frac(dat,size=0.3, replace = FALSE)

#################### MODELING ###########################
set.seed(1)
index <- createDataPartition(dat$isFraud, times=1, p=0.2, list=FALSE)
training_set <- dat %>% slice (-index)
validation_set <- dat %>% slice (index)
######################################################### 
# Create a subset of training_set for cross-validation purposes
set.seed(1)
ind <- createDataPartition(training_set$isFraud, times=1, p=0.2, list=FALSE)
training_set_split <- training_set %>% slice (-ind)
test_set <- training_set %>% slice (ind)
######################################################### 

nrow(training_set_split)
nrow(training_set)
nrow(test_set)
nrow(validation_set)

#################### BASE MODEL # 1 - JUST GUESSING #################### 
set.seed(1)
y_hat_guessing <- sample(c("1", "0"), length(ind), replace = TRUE) %>% factor()

#The overall accuracy is the overall proportion that is predicted correctly.
cm_1 <- confusionMatrix(data = y_hat_guessing, test_set$isFraud)

Accuracy_1<- cm_1$overall["Accuracy"]  # Calculating overall accuracy
Accuracy_1

Guessing<- cm_1$byClass[c("Sensitivity","Specificity")] # Calculating Sensitivity and Specificity
Guessing 

#################### BASE MODEL # 2 - TIME EFFECT #################### 
# Fraudulent activities occur more often overnight. 
# Model predicting that all fradulent activities occur overnight.

y_hat_time <- ifelse(test_set$hour>=1 & test_set$hour<=7,  "1", "0")  %>% factor()

# Confusion Matrix
cm_2 <- confusionMatrix(data = y_hat_time, test_set$isFraud)

# Calculating overall accuracy
cm_2$overall["Accuracy"] 

# Calculating Sensitivity and Specificity
TimeEffect<-cm_2$byClass[c("Sensitivity","Specificity")]  

# Comapring Models
rbind(Guessing, TimeEffect)

#################### MODEL # 3 - RPART CLASSIFICATION MODEL ####################
fit_rpart <- train(isFraud ~ .,
                   method = "rpart",
                   tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
                   data = training_set_split)
ggplot(fit_rpart) +
  theme(plot.title = element_text(hjust = 0.5))

# Plotting the decision tree
rpart.plot(fit_rpart$finalModel, type=5, main="Decision Tree")

# Prediction
y_hat_rpart <- predict(fit_rpart, test_set)

# Confusion Matrix
cm_3 <- confusionMatrix(data = y_hat_rpart, test_set$isFraud)

# Calculating overall accuracy
cm_3$overall["Accuracy"]

# Calculating Sensitivity and Specificity
Rpart<-cm_3$byClass[c("Sensitivity","Specificity")]

# Comparing Models
rbind(Guessing, TimeEffect, Rpart)

#################### MODEL # 4 - RANDOM FOREST Model #################### 
set.seed(1)
fit_rf <- train(isFraud ~ .,  data = training_set_split,
                trControl = trainControl(method = "cv", number =5),
                importance=TRUE,
                method="rf", ntree=25, tuneGrid = data.frame(mtry = seq(1, 6, 1)))

# Best mtry tuning parameter
fit_rf$bestTune  

# Plot of accuracy vs. number of randomly selected variables
ggplot(fit_rf) + 
  ggtitle("Accuracy vs. number of randomly selected variables") +
  theme(plot.title = element_text(hjust = 0.5))

# Plot of important variables
ggplot(varImp(fit_rf)) + 
  ggtitle("Predictive importance of variables") +
  theme(plot.title = element_text(hjust = 0.5))

# Prediction
y_hat_rf<- predict(fit_rf, test_set)

# Confusion Matrix
cm_4 <- confusionMatrix(data = y_hat_rf, test_set$isFraud)

# Calculating model accuracy
cm_4$overall["Accuracy"]

# Calculating model sensitivity and specificity
RandomForest<-cm_4$byClass[c("Sensitivity","Specificity")]

# Comparing Models
rbind(Guessing, TimeEffect, Rpart, RandomForest)

#################### TESTING THE FINAL CODE ON VALIDATION SET ####################
set.seed(1)

# Prediction on validation set
y_hat_final<- predict(fit_rf, validation_set)

# Confusion Matrix
cm_final <- confusionMatrix(data = y_hat_final, validation_set$isFraud)
cm_final$overall["Accuracy"]
RandomForest_final<-cm_final$byClass[c("Sensitivity","Specificity")]

#Comparing Models
rbind(Guessing, TimeEffect, Rpart, RandomForest, RandomForest_final)

