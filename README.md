# Concrete-Strength-Prediction
Prediction of Concrete Compressive Strength using Regression techniques in R language

## Problem Statement
Concrete is the most important material in civil engineering. The concrete 
compressive strength (CCS) is a function of age and ingredients. These ingredients include cement, blast furnace slag, fly ash, water, superplasticizer, 
coarse aggregate, and fine aggregate. In this use case, we will understand and 
find the relation between CCS and the independent features using statistical 
models and use the same for prediction. 

### 1. Reading & Data Preprocessing </br>
 a. Read the data </br>
 b. Perform Exploratory Analysis and identify of any correlation exists between variables </br>
 c. Data Preprocessing - Check and Handle Missing Values if any </br>
 d. Type Conversion - Check if any of the columns need to be type-converted </br>
 e. Data Standardisation - You can either opt to perform Standardisation or not </br>
### 2. Split the data into train and validation sets. (Set the seed at 123) </br>
### 3. Model Building </br>
 a. Model_Basic: Linear Regression Model on all attributes ; analyse model performance </br>
### 4. Steps to improve Model performance </br>
 a. Build additional models: </br>
  i.Model_AIC: Model with StepAIC (direction = ‘both’) </br>
  ii. Model_VIF : Multiple models with selected features based on VIF measures </br>
  iii. Model_Binned: Explore binning some features and run the best model among the above models. </br>
  iv. Model based on Data Transformation(Model_Transformation): With target variable transformed [sqrt] </br>
  v. Model based on Feature Engineering (Model_FE): Explore feature engineering by creating basic mathematical interaction between existing features from the domain understanding.</br> 
 b. Tabulate the train and validation results for all the above models </br>
 c. Analyse model performances and finalise the model </br>
 
