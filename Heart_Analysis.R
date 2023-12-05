#------------------Heart Disease Analysis and Prediction
#Clear Workspace
rm(list=ls())

## Set the working directory to the project root folder
# This assumes that the script file is located in the root folder of the project
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#Datasets : 1 cleveland.data
#           2 hungarian.data
#           3 switzerland.data
#Dataset Variables
#Age: The person's age in years
#Sex: The person's sex (1 = male, 0 = female)
#CP: The chest pain experienced (Value 1: typical angina, Value 2: atypical angina, Value 3: non-anginal pain, Value 4: asymptomatic)
#Trestbps: The person's resting blood pressure (mm Hg on admission to the hospital)
#Chol: The person's cholesterol measurement in mg/dl
#FBS: The person's fasting blood sugar (> 120 mg/dl, 1 = true; 0 = false)
#RestECG: Resting electrocardiographic measurement (0 = normal, 1 = having ST-T wave abnormality, 2 = showing probable or definite left ventricular hypertrophy by Estes' criteria)
#Thalach: The person's maximum heart rate achieved
#Exang: Exercise induced angina (1 = yes; 0 = no)
#Oldpeak: ST depression induced by exercise relative to rest ('ST' relates to positions on the ECG plot. 
#Slope: the slope of the peak exercise ST segment (Value 1: upsloping, Value 2: flat, Value 3: downsloping)
#CA: The number of major vessels (0-3)
#Thal: A blood disorder called thalassemia (3 = normal; 6 = fixed defect; 7 = reversable defect)
#Num: Heart disease (0 = no, 1 = yes) This is response variable

#------Data Loading
data_clev1<- read.csv('processed.cleveland.csv', stringsAsFactors = FALSE, header = FALSE)
data_hung1<- read.csv('processed.hungarian.csv',stringsAsFactors = FALSE, header = FALSE)
data_swiss1<- read.csv('processed.switzerland.csv', stringsAsFactors = FALSE, header = FALSE)
#data_va1<- read.csv('processed.va.csv',stringsAsFactors = FALSE, header = FALSE)

#Combining the 3 data set by row and giving name to the columns
data_heart<- rbind(data_clev1,data_hung1,data_swiss1)
names(data_heart) <- c('Age', 'Sex', 'CP', 'Trestbps', 'Chol', 'FBS', 'RestECG',
                       'Thalach', 'Exang', 'Oldpeak', 'Slope', 'CA', 'Thal', 'Num')

#------Data Preprocessing and Cleaning
# Converting the value from Character to integer
selected <- c('Age', 'Sex', 'CP', 'Trestbps', 'Chol', 'FBS', 'RestECG',
              'Thalach', 'Exang', 'Slope', 'CA', 'Thal', 'Num')
data_heart[, selected] <- lapply(data_heart[, selected], function(x){as.integer(x)})
data_heart$Oldpeak <- as.double(data_heart$Oldpeak)

# Check if we have NA values
na_count <- sapply(data_heart, function(x) sum(is.na(x))) 
na_count
summary(data_heart) #get statistic view of our variables

# Preprocessing to replace NA values with Mean and Mode
#Function for caculating mode
Mode <- function(x) {
  ux <- na.omit(unique(x) )
  tab <- tabulate(match(x, ux)); ux[tab == max(tab) ]
}

a <- mean(data_heart$Trestbps,na.rm = TRUE)    
for(i in 1:NROW(data_heart))   
{ 
  if(is.na(data_heart$Trestbps[i])==TRUE)
  {       data_heart$Trestbps[i] <- as.integer(a)     }  
}  
a <- mean(data_heart$Chol,na.rm=TRUE)    
for(i in 1:NROW(data_heart))   
{
  if(is.na(data_heart$Chol[i])==TRUE) 
  {       data_heart$Chol[i] <- as.integer(a)     }   
}   
a <- Mode(data_heart$FBS)    
for(i in 1:NROW(data_heart))   
{
  if(is.na(data_heart$FBS[i])==TRUE) 
  {       data_heart$FBS[i] <- as.integer(a)     }   
} 
a <- Mode(data_heart$RestECG)    
for(i in 1:NROW(data_heart))
{
  if(is.na(data_heart$RestECG[i])==TRUE)
  {       data_heart$RestECG[i] <- as.integer(a)     }  
}  
a <- Mode(data_heart$Thalach)    
for(i in 1:NROW(data_heart))
  
{
  if(is.na(data_heart$Thalach[i])==TRUE)
  {       data_heart$Thalach[i] <- as.integer(a)     }   
}
a <- Mode(data_heart$Exang)   

for(i in 1:NROW(data_heart))
{ 
  if(is.na(data_heart$Exang[i])==TRUE)  
  {       data_heart$Exang[i] <- as.integer(a)     }   
}   
a <- mean(data_heart$Oldpeak,na.rm=TRUE)    
for(i in 1:NROW(data_heart))   
  
{
  if(is.na(data_heart$Oldpeak[i])==TRUE)
  {       data_heart$Oldpeak[i] <- round(a,digits=1)     }   
}  
a <- Mode(data_heart$Slope)    
for(i in 1:NROW(data_heart))
{ 
  if(is.na(data_heart$Slope[i])==TRUE)
  {       data_heart$Slope[i] <- as.integer(a)     }   
} 
a <- Mode(data_heart$CA)    

for(i in 1:NROW(data_heart))   
{
  if(is.na(data_heart$CA[i])==TRUE) 
  {       data_heart$CA[i] <- as.integer(a)     }   
}
a <- Mode(data_heart$Thal)    

for(i in 1:NROW(data_heart))   
{ 
  if(is.na(data_heart$Thal[i])==TRUE)
  {       data_heart$Thal[i] <- as.integer(a)     }  
}   # Preprocessing end

#Here we se no NA values present
na_count <- sapply(data_heart, function(x) sum(is.na(x)))  # Check if we have NA values
na_count
str(data_heart)

#Grouping variable Num into 2 categories : 'no heart disease' = 0 and 'displaying heart disease' = 1
data_heart$Num[data_heart$Num == 2] <- 1
data_heart$Num[data_heart$Num == 3] <- 1
data_heart$Num[data_heart$Num == 4] <- 1

#Coverting Categorical Variables into factor
selectedColumns <- c('Sex', 'CP', 'FBS', 'RestECG','Exang', 'Slope', 'CA', 'Thal', 'Num')
data_heart[, selectedColumns] <- lapply(data_heart[, selectedColumns], function(x){as.factor(x)})
# Get Statistic summary of our variables
summary(data_heart) 
str(data_heart)
#data_heart <- data_heart[data_heart$Chol!= 0,]
#------DATA VISUALIZATION
levels(data_heart$Sex) <- c("Female", "Male")
levels(data_heart$CP) <- c("Typical angina", "Atypical angina", "No angina", "Asymptomatic" )
levels(data_heart$FBS) <- c("No", "Yes")
levels(data_heart$RestECG) <- c("Normal", "Abnormalities", "Hypertrophy")
levels(data_heart$Exang) <- c("No", "Yes")
levels(data_heart$Slope) <- c("Upsloping", "Flat", "Downsloping")
levels(data_heart$Thal) <- c("Normal flow","Fixed defect", "Reversible defect")
levels(data_heart$Num) <- c("No", "Yes")

#Num#
#Whether a patient has a heart disease or not
#Value 0: No, Value 1: Yes
ggplot(data_heart, aes(Num, fill=Num)) + 
  geom_bar() +
  labs(x="Disease", y="Number of patients") +
  guides(fill=FALSE) #Distribution is equal so we can use accuracy for evaluation in the model

#Age#
ggplot(data_heart, aes(Age, fill=Num)) + 
  geom_histogram(binwidth=1) +
  labs(fill="Disease", x="Age", y="Number of patients") #We do see as age is increase more likely the person has presence of heart disease

#Sex
ggplot(data_heart, aes(Sex, fill=Num)) + 
  geom_bar() +
  labs(fill="Disease", x="Sex", y="Number of patients") #The dataset contains more number of men than females and according to dataset men are more likely to have heart disease

#CP Chest Pain Type
ggplot(data_heart, aes(CP, fill=Num)) +
  geom_bar() +
  labs(fill="Disease", x="Chest pain type", y="Number of patients") #It is difficult to decide if the person has heart disease or not base on these symptions

#Trestbps Resting blood pressure in millimeters of mercury (mm Hg) when the patient was admitted to the hospital.
ggplot(data_heart, aes(Trestbps, fill=Num)) +
  geom_histogram(binwidth=3) +
  labs(fill="Disease", x="Blood pressure (mm Hg)", y="Number of patients") #Very High Blood pressure indicate presence of heart disease

#Finding Correlations
#install.packages("polycor")
library(polycor)
hetcorr <- hetcor(data_heart)
hetcorr
hetcorr_corr <-hetcorr$correlations
hetcorr_corr
#install.packages("corrplot")
library(corrplot)
corrplot(hetcorr_corr, type = "upper", order = "original", 
         tl.col = "black", tl.srt = 90, method = "number")


#------ Prediction using Classification Models

#Spliting the data set in Train and Test data with 70-30 ratio
set.seed(125)
ind <-sample(2,nrow(data_heart), replace = T, prob = c(0.7, 0.3))
train <- data_heart[ind ==1,]
test <- data_heart[ind ==2,]

####Decision Tree
library(rpart)
tree_model <- rpart(Num ~ ., train, method="class", control=rpart.control(xval=10))
library(rpart.plot)
rpart.plot(tree_model, type = 1, extra = 4, main="Classification Tree for Presence of Heart Disease")
nrow(tree_model$frame)
p1 <- predict(tree_model, test, type = 'class') #predict the test data
library(caret)
cm1 <- confusionMatrix(p1, test$Num) 
cm1

(tab1 <- table(predicted = p1, Actual = test$Num))
# Error
(1 - sum(diag(tab1))/sum(tab1)) * 100
#Accuracy
(acc<-(sum(diag(tab1))/sum(tab1)) * 100) #81.82%

#post pruning by finding best cp
bestcp <- tree_model$cptable[which.min(tree_model$cptable[,"xerror"]),"CP"]
bestcp #0.01255
fit.post <- prune.rpart(tree_model, cp=bestcp)
nrow(fit.post$frame)
library(rpart.plot)
rpart.plot(fit.post, type = 1, extra = 4, main="Classification Tree for Presence of Heart Disease")

#confusion matrix for test data of fit.post 
p2 <- predict(fit.post, test, type = 'class')
cm2 <- confusionMatrix(p2, test$Num)
cm2
(tab2 <- table(predicted = p2, Actual = test$Num))
# Error
(1 - sum(diag(tab2))/sum(tab2)) * 100
#Accuracy
(acc<-(sum(diag(tab2))/sum(tab2)) * 100) #82.72%

# After pruning the accuracy of decision tree increases from 81.82% to 82.72%
#In decision tree the significat variable in decising the Heart Disease in this data set is CP, Chol, Oldpeak, CA after pruning

#####Logistic Regression
# use glm() (general linear model) with family = "binomial" to fit a logistic 
logit.reg <- glm(Num ~ Age+Sex+CP+Trestbps+Chol+FBS+RestECG+
                   Thalach+Exang+Oldpeak+Slope+CA+Thal, 
                 data = train, family = "binomial") 
summary(logit.reg)

# use predict() with type = "response" to compute predicted probabilities. 
logitPredict <- predict(logit.reg, test, type = "response")
# we choose 0.5 as the cutoff here for 1 vs. 0 classes
logitPredictClass <- ifelse(logitPredict > 0.5, 1, 0)

# evaluate classifier on test.df

(tab3 <- table(logitPredictClass, test$Num))
#Error
(1 - sum(diag(tab3))/sum(tab3)) * 100
#Accuracy
(acc<-(sum(diag(tab3))/sum(tab3)) * 100) #82.72%

#Dropping those variables which are not significant
logit.reg1 <- glm(Num ~ CP+Chol+Thalach+Exang+Oldpeak+Slope+CA+Thal, 
                  data = train, family = "binomial") 

summary(logit.reg1)

# use predict() with type = "response" to compute predicted probabilities. 
logitPredict <- predict(logit.reg1, test, type = "response")
# we choose 0.5 as the cutoff here for 1 vs. 0 classes
logitPredictClass <- ifelse(logitPredict > 0.5, 1, 0)

# evaluate classifier on test
(tab4 <- table(logitPredictClass, test$Num))
#Error
(1 - sum(diag(tab4))/sum(tab4)) * 100
#Accuracy
(acc<-(sum(diag(tab4))/sum(tab4)) * 100) #83.18%

#After puting only significant variable the accuracy is now 83.18%
#Important variable which increase or decreases the odds of having heart disease in this dataset are :CP, Chol, Thalach, Exang, Oldpeak,Slope, CA, Thal


####RandomForest
#install.packages("randomForest")
library(randomForest)
rf <- randomForest(
  Num ~ .,
  data=train
)
rf$importance #Higher the value higher the importance of that variable in model

rfpredict = predict(rf, test)

(tab6 <- table(rfpredict, test$Num))
#Error
(1 - sum(diag(tab6))/sum(tab6)) * 100
#Accuracy
(acc<-(sum(diag(tab6))/sum(tab6)) * 100) #83.18%

#High importance variable according to RandomForest in deciding the presence of heart disease for this dataset is CP, Chol, Thalach, Oldpeak, Exang

#Based on the three models, Logistic Regression and Randomforest performs better than decision tree model

