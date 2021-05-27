rm(list=ls())
getwd()

#----------------------------Reading & DataProcessing-----------------------------------

#################### Reading & Understanding the data #####################
concrete_data=read.csv(file.choose(),header = T,sep = ',')
dim(concrete_data)
str(concrete_data)
head(concrete_data)
tail(concrete_data)
#################### Exploratory Data Analysis##########################

#Summary Statistics

summary(concrete_data)

#Study of Missing Values

sum(is.na(concrete_data)) #No Missing values

# Type Conversion - Check if any of the columns need to be type-converted 
#Conversion is not needed

concrete_data1=concrete_data

#Scatter plots
par(mfrow=c(2,2))
plot(x=concrete_data1$Cement,
     y=concrete_data1$Concrete_compressive_strength,
     main = "Cement vs Concrete Strength",
     xlab = "Cement",
     ylab = "Concrete_compressive_strength",
     col = "blue",
)

plot(x=concrete_data1$Water,
     y=concrete_data1$Concrete_compressive_strength,
     main = "Water vs Concrete Strength",
     xlab = "Water",
     ylab = "Concrete_compressive_strength",
     col = "blue",
)

plot(x=concrete_data1$Coarse_Aggregate,
     y=concrete_data1$Concrete_compressive_strength,
     main = "Corase_Aggregate vs Concrete Strength",
     xlab = "Corase_Aggregate",
     ylab = "Concrete_compressive_strength",
     col = "blue",
)

plot(x=concrete_data1$Age_in_days,
     y=concrete_data1$Concrete_compressive_strength,
     main = "Age vs Concrete Strength",
     xlab = "Age_in_days",
     ylab = "Concrete_compressive_strength",
     col = "blue",
)
# 
#correlation Plots

library(corrplot)
concrete_data_corr=cor(concrete_data1,use = "complete.obs")
par(mfrow=c(1,1))
corrplot(concrete_data_corr,method = "number",col = "black",cl.pos = "n")
#cement is highly corelated with CCS and Water and Superplasticizer has
#high correlation
concrete_data_corr

#################### Data Pre-Processing ##########################
#Split the data into train and validation sets.

set.seed(123)
library(caret)
train_rows=createDataPartition(concrete_data1$Concrete_compressive_strength,p=0.7,list = F)
head(train_rows)
train_data=concrete_data1[train_rows,]
test_data=concrete_data1[-train_rows,]
dim(train_data)
dim(test_data)

#Data Standardisation
summary(concrete_data1)
std_obj=preProcess(train_data[,!colnames(concrete_data1) %in% c("Concrete_compressive_strength")],
                   method = c("center","scale")
)
train_data[,!colnames(concrete_data1) %in% c("Concrete_compressive_strength")] <-predict(std_obj,newdata = train_data[,!colnames(concrete_data1) %in% c("Concrete_compressive_strength")])

test_data[,!colnames(concrete_data1) %in% c("Concrete_compressive_strength")] <-predict(std_obj,newdata = test_data[,!colnames(concrete_data1) %in% c("Concrete_compressive_strength")]) 
summary(train_data)
summary(test_data)

#################### Model Building ##########################
# Basic Model build

Model_Basic=lm(formula = Concrete_compressive_strength~.,data = (train_data))
summary(Model_Basic)
par(mfrow=c(2,2))
plot(Model_Basic)

# Predict test Data

pred_basic<-predict(Model_Basic,test_data[,!colnames(concrete_data1) %in% c("Concrete_compressive_strength")])
head(pred_basic)

#Error verification on Train data
#Peformance Metrics
library(DMwR)
(a<-regr.eval(train_data$Concrete_compressive_strength,Model_Basic$fitted.values))
(b<-regr.eval(test_data$Concrete_compressive_strength,pred_basic))
a<-as.data.frame(a)
b<-as.data.frame(b)
a
b
#OBSERVATION of MODEL_BASIC
#Model_Basic is significant F-stat: 140.2 and its Adjusted R^2 - 0.607
#Coarse-Aggregate and Fine-Aggreagate are insignificant variables
# Residual plots shows Errors are non-linear ,hetroscadestic and non-normal
# Error Metric - Error of test is less than train. MSe is high

#Steps to improve Model performance 

# Influential Observation

cook_dis=cooks.distance(Model_Basic)
cook_dis
plot(cook_dis,ylab = "Cook's Distance")

4/nrow(train_data)
3*mean(cook_dis)

max=which.max(cook_dis)
max
cook_dis[max]
dim(train_data)
train_cook<-train_data[-max,]
dim(train_cook)

# build Model
Model_Basic2<-lm(formula = Concrete_compressive_strength~.,data = (train_cook))
summary(Model_Basic2)
par(mfrow=c(2,2))
plot(Model_Basic2)

# Predict test Data

pred_basic2<-predict(Model_Basic2,test_data[,!colnames(train_cook) %in% c("Concrete_compressive_strength")])
head(pred_basic2)

#Error verification on Train data
#Peformance Metrics

(a1<-regr.eval(train_cook$Concrete_compressive_strength,Model_Basic2$fitted.values))
(b1<-regr.eval(test_data$Concrete_compressive_strength,pred_basic2))

a1<-as.vector(a1)
a<-cbind(a,a1)
b1<-as.vector(b1)
b<-cbind(b,b1)
#OBSERVATION OF MODEL_BASIC2
# Model_basic has influential data point which was removed in this model as it
#violates thumb rule of cooks' distance
#Adjusted Rsquare improved

# Step_AIC

library(MASS)
Model_AIC<-stepAIC(Model_Basic2,direction = "both",trace = T)
summary(Model_AIC)
par(mfrow=c(2,2))
plot(Model_AIC)

# Predict test data

pred_aic<-predict(Model_AIC,test_data[,!colnames(test_data) %in% c("Concrete_compressive_strength","Coarse_Aggregate","Fine_Aggregate")])
head(pred_aic)

#Performance Metrics
(a2<-regr.eval(train_cook$Concrete_compressive_strength,Model_AIC$fitted.values))
(b2<-regr.eval(test_data$Concrete_compressive_strength,pred_aic))

a2<-as.vector(a2)
a<-cbind(a,a2)
b2<-as.vector(b2)
b<-cbind(b,b2)

#OBSERVATION ON MODEL_AIC
# StepAIC removes coarse_aggregate and Fine_Aggregate as they are insignificant variables
# Model is significant - F stat : 190.5
#Adujusted R^2 - 0.6122 #Not improved much
#Less variables is better than all variables
#In Residual plots errors are still violating assumptions
#Error Metrics MSE is higher than Model_Basic

# VIF

library(car)
vif(Model_Basic2)
vif(Model_AIC)
# Build a Model removing high VIF 
Model_VIF=lm(Concrete_compressive_strength~Cement+Blast_Furnace_Slag+Superplasticizer+Water+Age_in_days,data = (train_cook))
summary(Model_VIF)
plot(Model_VIF)

# Build a Model removing high VIF
Model_VIF1=lm(Concrete_compressive_strength~Cement+Blast_Furnace_Slag+Fly_Ash+Water+Age_in_days,data = (train_cook))
summary(Model_VIF1)
plot(Model_VIF1)
vif(Model_VIF1)
# Predict test Data

pred_vif1<-predict(Model_VIF1,test_data[,!colnames(train_cook) %in% c("Concrete_compressive_strength","Coarse_Aggregate","Fine_Aggregate","Superplasticizer")])
head(pred_vif1)

#Error verification on Train data
#Peformance Metrics

(a3<-regr.eval(train_cook$Concrete_compressive_strength,Model_VIF1$fitted.values))
(b3<-regr.eval(test_data$Concrete_compressive_strength,pred_vif1))
a3<-as.vector(a3)
b3<-as.vector(b3)
a<-cbind(a,a3)
b<-cbind(b,b3)

#OBSERVATION ON MODEL_VIF
# vif of model_basic has multicolinearity for variables
#vif of Model_AIC - AIC has removed multicolinearity among variables
#Model_vif first removed fly_ash and next removed superplasticizer and built models
#Amongs all till now models high f statistic recorded
# Adjusted r^2 - 0.6122

# Data Transformation

Model_Transform<-lm(sqrt(Concrete_compressive_strength)~.,data = (train_cook))
summary(Model_Transform)
par(mfrow=c(2,2))
plot(Model_Transform)

vif(Model_Transform)
plot(Model_Transform$fitted.values,Model_Transform$residuals)
abline(h=0,col="blue",lwd=2)

# Predict test Data

pred_Transform<-predict(Model_Transform,test_data[,!colnames(train_cook) %in% c("Concrete_compressive_strength")])
head(pred_Transform)

#Error verification on Train data
#Peformance Metrics

(a4<-regr.eval(sqrt(train_cook$Concrete_compressive_strength),Model_Transform$fitted.values))
(b4<-regr.eval(sqrt(test_data$Concrete_compressive_strength),pred_Transform))
a4<-as.vector(a4)
b4<-as.vector(b4)
a<-cbind(a,a4)
b<-cbind(b,b4)

#Build a Model
Model_Transform1=lm(sqrt(Concrete_compressive_strength)~Cement+Blast_Furnace_Slag+Fly_Ash+Water+Age_in_days,data = (train_cook))
summary(Model_Transform1)

#OBSERVTION on MODEL_TRANSFORM
#Adjusted R^2 - 0.5975
#F- statistic - 134.6
#square root Data Transformations are able to remove hetroscedasticity and 
#non normality of data(Residual plots)
#Errors are very small compared to other models

#Binning

concrete_data$Age_in_days
length(concrete_data$Age_in_days)
max(concrete_data$Age_in_days)
#Manual Binning process
bins<-cut(concrete_data$Age_in_days,breaks = seq(0,400,50),right = T)
bins
table(bins)

concrete_data_binned<-concrete_data
concrete_data_binned$Age_in_days<-NULL
concrete_data_binned['Age_bins']<-bins
str(concrete_data_binned)
plot(concrete_data_binned$Age_bins,concrete_data_binned$Concrete_compressive_strength)

#split data into train and test

set.seed(123)
library(caret)
train_rows=createDataPartition(concrete_data_binned$Concrete_compressive_strength,p=0.7,list = F)
head(train_rows)
train_data_binned=concrete_data_binned[train_rows,]
test_data_binned=concrete_data_binned[-train_rows,]
dim(train_data_binned)
dim(test_data_binned)

# data standardization

summary(concrete_data_binned)
std_obj=preProcess(train_data[,!colnames(concrete_data_binned) %in% c("Concrete_compressive_strength","Age_bins")],
                   method = c("center","scale")
)
train_data_binned[,!colnames(concrete_data_binned) %in% c("Concrete_compressive_strength","Age_bins")] <-predict(std_obj,newdata = train_data[,!colnames(concrete_data_binned) %in% c("Concrete_compressive_strength","Age_bins")])

test_data_binned[,!colnames(concrete_data_binned) %in% c("Concrete_compressive_strength","Age_bins")] <-predict(std_obj,
                                                                                                                newdata = test_data[,!colnames(concrete_data_binned) %in% c("Concrete_compressive_strength","Age_bins")]) 
summary(train_data_binned)
summary(test_data_binned)
head(train_data_binned)

#Build a model

Model_binned<-lm(Concrete_compressive_strength~.,data = (train_data_binned))
summary(Model_binned)
par(mfrow=c(2,2))
plot(Model_binned)
library(car)
vif(Model_binned)

# Predict test Data

pred_bin<-predict(Model_binned,test_data_binned[,!colnames(train_data_binned) %in% c("Concrete_compressive_strength")])
head(pred_bin)

#Error verification on Train data
#Peformance Metrics
library(DMwR)
(a5<-regr.eval(train_data_binned$Concrete_compressive_strength,Model_binned$fitted.values))
(b5<-regr.eval(test_data_binned$Concrete_compressive_strength,pred_bin))
a5<-as.vector(a5)
b5<-as.vector(b5)
a<-cbind(a,a5)
b<-cbind(b,b5)

#OBSERVATION ON MODEL_BINNED
#Binning is done on Age columns at step of 50 
#Adjusted R^2 is improved - 0.6668
#Model and variables are significant 
# Errors in residual plots follow NOrmality , linearity and Homoscadesicity
#MSE of test > MSE of train

#Feature Engineering
#Add Ratio feature 

train_fe<-train_cook
test_fe<-test_data
train_fe$Ratio=train_fe$Water/train_fe$Cement
test_fe$Ratio=test_fe$Water/test_fe$Cement
plot(train_fe$Ratio,train_fe$Concrete_compressive_strength,xlab = "Water to cement Ratio")

#Build a Model
Model_FE<-lm(Concrete_compressive_strength~.,data = (train_fe))
summary(Model_FE)
par(mfrow=c(2,2))
plot(Model_FE)
#OBSERVATION ON MODEL_FE
# Water/cement Ratio is added to the Model_fe 
#Adjusted R2 is 0.615 
#Model is significant 128

#step AIC
Model_FE1<-stepAIC(Model_FE,direction = "both",trace = T)
summary(Model_FE1)
vif(Model_FE)
vif(Model_FE1)

Model_FE2=lm(Concrete_compressive_strength~Cement+Blast_Furnace_Slag+Fly_Ash+Water+Age_in_days+Ratio,data = (train_fe))
summary(Model_FE2)
plot(Model_FE2)
#OBSERVATION ON MODEL_FE2
#StepAIC is done removed coarse and fine Aggregate
#VIF is checked for both for multicollinearity check
#Adjusted Rsquare not changed much 0.612

########################Combination Ratio Feature and Binning Concept################
#Manual Binning process
bins<-cut(concrete_data$Age_in_days,breaks = seq(0,400,25),right = T)
bins
table(bins)

concrete_data_binned<-concrete_data
concrete_data_binned$Age_in_days<-NULL
concrete_data_binned['Age_bins']<-bins
str(concrete_data_binned)
plot(concrete_data_binned$Age_bins,concrete_data_binned$Concrete_compressive_strength)

#split data into train and test

set.seed(123)
library(caret)
train_rows=createDataPartition(concrete_data_binned$Concrete_compressive_strength,p=0.7,list = F)
head(train_rows)
train_data_binned=concrete_data_binned[train_rows,]
test_data_binned=concrete_data_binned[-train_rows,]
dim(train_data_binned)
dim(test_data_binned)

# data standardization

summary(concrete_data_binned)
std_obj=preProcess(train_data[,!colnames(concrete_data_binned) %in% c("Concrete_compressive_strength","Age_bins")],
                   method = c("center","scale")
)
train_data_binned[,!colnames(concrete_data_binned) %in% c("Concrete_compressive_strength","Age_bins")] <-predict(std_obj,newdata = train_data[,!colnames(concrete_data_binned) %in% c("Concrete_compressive_strength","Age_bins")])

test_data_binned[,!colnames(concrete_data_binned) %in% c("Concrete_compressive_strength","Age_bins")] <-predict(std_obj,
                                                                                                                newdata = test_data[,!colnames(concrete_data_binned) %in% c("Concrete_compressive_strength","Age_bins")]) 
summary(train_data_binned)
summary(test_data_binned)
head(train_data_binned)
train_data_binned$Ratio=train_data_binned$Water/train_data_binned$Cement
test_data_binned$Ratio=test_data_binned$Water/test_data_binned$Cement

#Build a model

Model_binned_FE<-lm(Concrete_compressive_strength~.,data = (train_data_binned))
summary(Model_binned_FE)
par(mfrow=c(2,2))
plot(Model_binned_FE)
library(car)
vif(Model_binned_FE)

# Predict test Data

pred_bin_fe<-predict(Model_binned_FE,test_data_binned[,!colnames(train_data_binned) %in% c("Concrete_compressive_strength")])
head(pred_bin_fe)

#Error verification on Train data
#Peformance Metrics
library(DMwR)
(a6<-regr.eval(train_data_binned$Concrete_compressive_strength,Model_binned_FE$fitted.values))
(b6<-regr.eval(test_data_binned$Concrete_compressive_strength,pred_bin_fe))
a6<-as.vector(a6)
b6<-as.vector(b6)
a<-cbind(a,a6)
b<-cbind(b,b6)

#OBSERVATION ON MODEL_binned_FE
#Binning is done at step 25
#Adjusted R^2 got improved- 0.802
#Model is significant 
#Multicolinearity still exists
#MSE of train < test and MAPE of test< train
#Less MSE is recorded in this model

#Checking Influential Observations
cook1<-cooks.distance(Model_binned_FE)
plot(cook1)
4/nrow(train_data_binned)
3*mean(cook1)
m<-which.max(cook1)
cook1[m]
train_data_binned<-train_data_binned[-m,]

#Removed influential observation building model again
Model_binned_FE<-lm(Concrete_compressive_strength~.,data = (train_data_binned))
summary(Model_binned_FE)
par(mfrow=c(2,2))
plot(Model_binned_FE)
#OBSERVATION ON MODEL_FE_bin
#Adjusted Rsquare improved 0.803

#StepAIC Model built

Model_binned_FE_AIC<-stepAIC(Model_binned_FE,direction = "both",trace = T)
summary(Model_binned_FE_AIC)
plot(Model_binned_FE_AIC)

# Predict test Data

pred_bin_fe_aic<-predict(Model_binned_FE_AIC,test_data_binned[,!colnames(train_data_binned) %in% c("Concrete_compressive_strength")])
head(pred_bin_fe_aic)

#Error verification on Train data
#Peformance Metrics
library(DMwR)
(a7<-regr.eval(train_data_binned$Concrete_compressive_strength,Model_binned_FE_AIC$fitted.values))
(b7<-regr.eval(test_data_binned$Concrete_compressive_strength,pred_bin_fe_aic))
a<-cbind(a,a7)
b<-cbind(b,b7)
#OBSERVATION ON MODEL_BIN_FE_AIC
#Adjusted rsquare is not changed much 0.803

#VIF check
vif(Model_binned_FE)
vif(Model_binned_FE_AIC)

Model_bin_fe_vif<-lm(Concrete_compressive_strength~Cement+Blast_Furnace_Slag+Fly_Ash+Water+Age_bins+Ratio,data = (train_data_binned))
summary(Model_bin_fe_vif)
vif(Model_bin_fe_vif)
plot(Model_bin_fe_vif)

# Predict test Data

pred_bin_fe_vif<-predict(Model_bin_fe_vif,test_data_binned[,!colnames(train_data_binned) %in% c("Concrete_compressive_strength","Coarse_Aggregate","Fine_Aggregate","Superplasticizer")])
head(pred_bin_fe_aic)

#Error verification on Train data
#Peformance Metrics
library(DMwR)
(a8<-regr.eval(train_data_binned$Concrete_compressive_strength,Model_bin_fe_vif$fitted.values))
(b8<-regr.eval(test_data_binned$Concrete_compressive_strength,pred_bin_fe_vif))
a<-cbind(a,a8)
b<-cbind(b,b8)

#OBSERVATION ON FINAL MODEL which is MODEL_FE_BIN_VIF

#VIF of MODEL_BIN_FE and MODEL_BIN_FE_AIC are high 
#Multicolinearity exists for both of them
#Experimented removing multiple variables ,now final model is able to explian 
# with fewer variables
#VIF of final model is less for variables
#Multicolinearity is removed
#Adjusted Rsquare not changed much - 0.802
#Model is significant - f stat value - 244.1
#MSE is better when compared to other models

##################Peformance Metric Table###################################
#Error Table for train data
colnames(a)<-c("Model_Basic","Model_Basic2","Model_AIC","Model_VIF1","Model_Transform","Model_Binned","Model_Bin_FE","Model_bin_FE_AIC","Model_bin_FE_VIF")
colnames(b)<-c("Model_Basic","Model_Basic2","Model_AIC","Model_VIF1","Model_Transform","Model_Binned","Model_Bin_FE","Model_bin_FE_AIC","Model_bin_FE_VIF")
cat("Error Metrics of different Models on train data")
a
cat("Error Metrics of different Models on test data")
b

################################ Write Predicted CCS new file#############
New_test_data<-cbind(test_data,pred_bin_fe_vif)
colnames(New_test_data)[10]<-"Predicted CCS"
#write.csv(New_test_data,file = "Test_Predicted_concretedata.csv",sep = ",")
