library(readxl)
library(rms)
library(boot)
library(dplyr)
library(caTools)
library(stargazer)
library(randomForest)
library(zoom)
library(tree)
library(rpart)
library(rpart.plot)
library(readxl)
library(psych)
library(car)
library(ggplot2)
library(ggfortify)
library(NbClust)
library(factoextra)
library(MASS)
library(lmtest)
library(leaps)
library(olsrr)

TRAINING <- read_excel("~/Dropbox/Zach/McGill/McHacks V/TRAINING.xlsx")
TEST <- read_excel("~/Dropbox/Zach/McGill/McHacks V/TEST.xlsx")

#GET RID OF NA VALUES
TRAINING = na.omit(TRAINING)

#IF TIME PERMITS HAVE MAKE A TRAIN AND TEST INSIDE THE TRAINNING PART AND RUN A K-FOLD TEST OR LOOCV TEST TO CHECK FOR OVERFITTING

#IDENTIFY AND REMOVE OUTLIERS

reg_outliers1 = lm(TRAINING$FLAG ~ TRAINING$Q201601_MILES)
outlierTest(reg_outliers1)
TRAINING = TRAINING[-c(4685), ]

reg_outliers2 = lm(TRAINING$FLAG ~ TRAINING$Q201601_SPONSORS)
outlierTest(reg_outliers2)
TRAINING = TRAINING[-c(100), ]

reg_outliers3 = lm(TRAINING$FLAG ~ TRAINING$Q201602_MILES)
outlierTest(reg_outliers3)
TRAINING = TRAINING[-c(1954), ]

reg_outliers4 = lm(TRAINING$FLAG ~ TRAINING$Q201602_SPONSORS)
outlierTest(reg_outliers4)
TRAINING = TRAINING[-c(20614), ]

reg_outliers5 = lm(TRAINING$FLAG ~ TRAINING$Q201603_MILES)
outlierTest(reg_outliers5)
TRAINING = TRAINING[-c(4095), ]

reg_outliers6 = lm(TRAINING$FLAG ~ TRAINING$Q201603_SPONSORS)
outlierTest(reg_outliers6)
TRAINING = TRAINING[-c(4509), ]

reg_outliers7 = lm(TRAINING$FLAG ~ TRAINING$Q201604_MILES)
outlierTest(reg_outliers7)
TRAINING = TRAINING[-c(418), ]

reg_outliers8 = lm(TRAINING$FLAG ~ TRAINING$Q201604_SPONSORS)
outlierTest(reg_outliers8)
TRAINING = TRAINING[-c(3250), ]

reg_outliers9 = lm(TRAINING$FLAG ~ TRAINING$MILES201601GROCERY)
outlierTest(reg_outliers9)
TRAINING = TRAINING[-c(27), ]

reg_outliers10 = lm(TRAINING$FLAG ~ TRAINING$MILES201602GROCERY)
outlierTest(reg_outliers10)
TRAINING = TRAINING[-c(31026), ]

reg_outliers11 = lm(TRAINING$FLAG ~ TRAINING$MILES201603GROCERY)
outlierTest(reg_outliers11)
TRAINING = TRAINING[-c(31023), ]

reg_outliers12 = lm(TRAINING$FLAG ~ TRAINING$MILES201604GROCERY)
outlierTest(reg_outliers12)
TRAINING = TRAINING[-c(1150), ]

reg_outliers13 = lm(TRAINING$FLAG ~ TRAINING$MILES201601CREDIT_CARD)
outlierTest(reg_outliers13)
TRAINING = TRAINING[-c(29), ]

reg_outliers14 = lm(TRAINING$FLAG ~ TRAINING$MILES201602CREDIT_CARD)
outlierTest(reg_outliers14)
TRAINING = TRAINING[-c(24232), ]

reg_outliers15 = lm(TRAINING$FLAG ~ TRAINING$MILES201603CREDIT_CARD)
outlierTest(reg_outliers15)
TRAINING = TRAINING[-c(4089), ]

reg_outliers16 = lm(TRAINING$FLAG ~ TRAINING$MILES201604CREDIT_CARD)
outlierTest(reg_outliers16)
TRAINING = TRAINING[-c(5128), ]

reg_outliers17 = lm(TRAINING$FLAG ~ TRAINING$MILES201601OTHER)
outlierTest(reg_outliers17)
TRAINING = TRAINING[-c(28590,9052), ]

reg_outliers18 = lm(TRAINING$FLAG ~ TRAINING$MILES201602OTHER)
outlierTest(reg_outliers18)
TRAINING = TRAINING[-c(9052), ]

reg_outliers19 = lm(TRAINING$FLAG ~ TRAINING$MILES201603OTHER)
outlierTest(reg_outliers19)
TRAINING = TRAINING[-c(29512), ]

reg_outliers20 = lm(TRAINING$FLAG ~ TRAINING$MILES201604OTHER)
outlierTest(reg_outliers20)
TRAINING = TRAINING[-c(2238), ]

reg_outliers21 = lm(TRAINING$FLAG ~ TRAINING$PROGRAM_TIER)
outlierTest(reg_outliers21)
TRAINING = TRAINING[-c(11), ]

reg_outliers22 = lm(TRAINING$FLAG ~ TRAINING$PROVINCE)
outlierTest(reg_outliers22)
TRAINING = TRAINING[-c(9181), ]

reg_outliers23 = lm(TRAINING$FLAG ~ TRAINING$EMAIL_OPTIN)
outlierTest(reg_outliers23)
TRAINING = TRAINING[-c(6), ]

reg_outliers24 = lm(TRAINING$FLAG ~ TRAINING$DEC_2016_ACCOUNT_BALANCE)
outlierTest(reg_outliers24)
TRAINING = TRAINING[-c(10067), ]

#SPLIT DATA
split = sample.split(TRAINING, SplitRatio = 0.2)
train = subset(TRAINING, split==TRUE)
test = subset(TRAINING, split==FALSE)

#SEPARATE DATA
Train_quant = train[c(10, 2:9, 11:22, 26)]
Train_qual = train[c(10, 24,23,25)]

#No time to run multicolinearity test before, if so woudld have elimated corelated variables

#CHOSE VARIBALES
stepBS <- regsubsets(x=FLAG ~. , data=Train_quant, nbest = 2)
sum.stepBS <- summary(stepBS)
names(sum.stepBS) # examine the names of values in the output
sum.stepBS$which  # examine the matrix containing which variables are in each model
sum.stepBS$rsq    # examine the r-squared for each model
sum.stepBS$cp     # examine Mallow's cp for each model: cp is the statistic to compare models with different parameters. 
# smaller cp means the model is more precise 
cbind(data.frame(sum.stepBS$which),sum.stepBS$rsq,sum.stepBS$cp)
  #Pick model 18
    #INCLUDE: Q201601_MILES + Q201601_SPONSORS + Q201602_MILES + Q201604_MILES + Q201604_SPONSORS + MILES201601GROCERY + MILES201603GROCERY MILES201604GROCERY MILES201601CREDIT_CARD

#QUANTITATIVE VARIABALES
 model1 = glm(FLAG  ~ Q201601_MILES + Q201601_SPONSORS + Q201602_MILES + 
                Q201604_MILES + Q201604_SPONSORS + MILES201601GROCERY + 
                MILES201603GROCERY + MILES201604GROCERY + MILES201601CREDIT_CARD, data = Train_quant, family = "binomial")

  #COLINEARITY OF MODEL 1
  model1 = glm(FLAG  ~ Q201601_MILES + Q201601_SPONSORS + Q201602_MILES + 
                Q201604_MILES + Q201604_SPONSORS + MILES201601GROCERY + 
                MILES201603GROCERY + MILES201604GROCERY + MILES201601CREDIT_CARD, data = Train_quant, family = "binomial")
  model1_test = lm(FLAG  ~ Q201601_MILES + Q201601_SPONSORS + Q201602_MILES + 
                 Q201604_MILES + Q201604_SPONSORS + MILES201601GROCERY + 
                 MILES201603GROCERY + MILES201604GROCERY + MILES201601CREDIT_CARD, data = Train_quant)
  ols_coll_diag(model1_test)
  
    #Remove where the VIF is higher than 4, Q201601_MILES, Q201602_MILES, MILES201603GROCERY, MILES201601CREDIT_CARD
  model2 = glm(FLAG  ~ Q201601_SPONSORS + Q201604_MILES + Q201604_SPONSORS + MILES201601GROCERY + 
                 + MILES201604GROCERY, data = Train_quant, family = "binomial")
  model2_test = lm(FLAG  ~ Q201601_MILES + Q201601_SPONSORS + 
                     Q201604_MILES + Q201604_SPONSORS + MILES201604GROCERY + 
                     MILES201603OTHER, data = Train_quant)
  ols_coll_diag(model2_test)
  
  summary(model2)
  
  #Check with a random forest that the factors are relevant
  forest=randomForest(FLAG~. , ntree=50, data=Train_quant, importance=TRUE)
  importance(forest)
  varImpPlot(forest)

#QUALITATIVE VARIBALES
  
  #Overfitted tree
  cat_var_reg_tree_O = rpart(FLAG ~ . , control=rpart.control(cp=0.0000000000000001), data = Train_qual)
  #Determine best CP
  cp_value = cat_var_reg_tree_O$cptable[which.min(cat_var_reg_tree_O$cptable[,"xerror"]),"CP"]
  #Create model with categorial variables for tree to consider
  cat_var_reg_tree = rpart(FLAG ~ ., control=rpart.control(cp=cp_value), data = Train_qual)
  rpart.plot(cat_var_reg_tree)
  summary(cat_var_reg_tree)
    #important qualitative vraibels are: PROVINCE

#FINAL MODEL 1
  
  final = glm(FLAG  ~ Q201601_SPONSORS + Q201604_MILES + Q201604_SPONSORS + MILES201601GROCERY + 
                         + MILES201604GROCERY + PROVINCE, data = train, family = "binomial")
  summary(final)  
  
#TEST THE MODEL - K-Fold test
  
  Kfitlin = glm(FLAG  ~ Q201601_SPONSORS + Q201604_MILES + Q201604_SPONSORS + MILES201601GROCERY + 
                + MILES201604GROCERY + PROVINCE, data = test, family = "binomial")
  cv.error_kf=rep(0,50) 
  for (i in 1:50)
  {
    cv.error_kf[i]=cv.glm(test, Kfitlin, K=10)$delta[1]
  }
  mean(cv.error_kf) #(0.1534637)
  
  #USE THE MODEL TO PREDICT ON TEST
  results <- round(predict(final,newdata=TEST,type='response'))
  TEST$FLAG_P<- results
  final_results = TEST[c(1,26)]
  write.csv(final_results, "~/Dropbox/Zach/McGill/McHacks V/zachary_amar_loyatyonechallenge_answers.txt")
    