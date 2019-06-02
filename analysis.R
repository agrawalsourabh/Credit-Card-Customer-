# installing packages


# load packages
library(plyr)
library(caret) # for sampling and confusion matrix, for CV
library(e1071) # for using classification algo - naiveBayes
library(randomForest) # for using classification algo - randomForest
library(class) # for using classification algo - KNN

# importing data set
our.data = read.csv("data/UCI_Credit_Card.csv")
str(our.data)

# Renameing column PAY_0
colnames(our.data)[colnames(our.data) == "PAY_0"] = "PAY_1"

# Convert default payment next month to factor
our.data$default.payment.next.month = as.factor(our.data$default.payment.next.month)
table(our.data$default.payment.next.month)

# Rename default.payment.next.month as DEFAULT
colnames(our.data)[colnames((our.data)) == "default.payment.next.month"] = "DEFAULT"

# Convert SEX to factor
our.data$SEX = as.factor(our.data$SEX)
table(our.data$SEX)

# Add one feature GENDER
our.data$GENDER = ifelse(our.data$SEX == 1, "Male", "Female")

# Education
# Convert EDUCATION to factor

# Merging 0, 5 and 6 to 4(others)
our.data$EDUCATION = ifelse(our.data$EDUCATION == 0 |our.data$EDUCATION == 5 | our.data$EDUCATION == 6,
       4, our.data$EDUCATION)

unique(our.data$EDUCATION)
our.data$EDUCATION = as.factor(our.data$EDUCATION)
table(our.data$EDUCATION)

# Find out Default %  in all education type
l = levels(our.data$EDUCATION)
default_count_perc = calDefaultPerc(x = our.data, colInd = 4, data.level = l )

# Marriage
unique(our.data$MARRIAGE)

# Merging 0 to 3(others)
our.data$MARRIAGE = ifelse(our.data$MARRIAGE == 3, 0, our.data$MARRIAGE)

# Convert to factors
our.data$MARRIAGE = as.factor(our.data$MARRIAGE)

table(our.data$MARRIAGE)


# Adding one feature marital status
addMaritalStatus = function(x, colInd, newColInd){
  for (i in 1: nrow(x)) {
    if(x[i, colInd] == 1){
      x[i, newColInd] = "Married"
    }
    else if (x[i, colInd] == 2) {
      x[i, newColInd] = "Not Married"
    }
    else{
      x[i, newColInd] = "Others"
    }
                                                                                                                                                                                              
  }
  return(x)
}
our.data$MARITALSTATUS = NA
our.data = addMaritalStatus(our.data, which(colnames(our.data) == "MARRIAGE"), 
                            which(colnames(our.data) == "MARITALSTATUS"))
our.data$MARITALSTATUS = as.factor(our.data$MARITALSTATUS)

# Explore Marital status vs default
marital_default_per_df = calDefaultPerc(x = our.data, 
                                           colInd = which(colnames(our.data) == "MARITALSTATUS"), 
                                           data.level = levels(our.data$MARITALSTATUS))

# PAY_1 is the highest positive correlated variable
unique(our.data$PAY_1)

# change this variable in to factors
our.data$PAY_1 = as.factor(our.data$PAY_1)

# Explore PAY_1 level vs Default
pay_1_default_perc.df = calDefaultPerc(x = our.data, 
                                       colInd = which(colnames(our.data) == "PAY_1"), 
                                       data.level = levels(our.data$PAY_1))

# Add one more feature remaining amount
# our.data$REMAINING_AMT = our.data[, ] - our.data[, 13:18]
check = calDiffBillAndPayPer(x = our.data, bill_amt_ind = c(13:18), pay_amt_ind = c(19:24))
our.data$REMAINING_AMT_PER = check

# check the correlation between DEFAULT and REMAINING_AMT_PER
cor(our.data$REMAINING_AMT_PER, as.numeric(our.data$DEFAULT))

# add one more feature REMAINING_AMT_PER_RANGE
# 0-10 -> 0
# 11-20 -> 1
# 21-30 -> 2
# 31-40 -> 3
# 41-50 -> 4
# 51-60 -> 5
# 61-70 -> 6
# 71-80 -> 7
# 81-90 -> 8
# 91-100 -> 9

check = getRemPerRange(x = our.data, 
                       colInd = which(colnames(our.data) == 'REMAINING_AMT_PER'))
our.data$REMAINING_AMT_PER_RANGE = check

# change it to factors
our.data$REMAINING_AMT_PER_RANGE = as.factor(our.data$REMAINING_AMT_PER_RANGE)

# Explore REMAINING_AMT_PER_RANGE vs default
RemainingAmtPerc_per_df = calDefaultPerc(x = our.data, 
                                        colInd = which(colnames(our.data) == "REMAINING_AMT_PER_RANGE"), 
                                        data.level = levels(our.data$REMAINING_AMT_PER_RANGE))

# -----------------------------------------------------------------------------------
# ------------------------------WORKPLACE BACKUP-------------------------------------
# -----------------------------------------------------------------------------------

# LIMIT_BAL
summary(our.data$LIMIT_BAL)
limit_bal_seg = getLimitBalRange(x = our.data, 
                                 colInd = which(colnames(our.data) == 'LIMIT_BAL'))
# Add one more feature LIMIT_BAL_SEG
our.data$LIMIT_BAL_SEG = limit_bal_seg

# change it into factor
our.data$LIMIT_BAL_SEG = as.factor(our.data$LIMIT_BAL_SEG)
# 
# # Find defaulters % per LIMIT_BAL_SEG
# limit_bal_seg_perc_def.df = calDefaultPerc(x = our.data, 
#                                            colInd = which(colnames(our.data) == 'LIMIT_BAL_SEG'), 
#                                            data.level = levels(our.data$LIMIT_BAL_SEG))


# -----------------------------------------------------------------------------------
# ------------------------------WORKPLACE BACKUP-------------------------------------
# -----------------------------------------------------------------------------------

# taking significant columns
remove_feature = c(1, 26, 27, 29, 30)
our.modified.data = our.data[, -remove_feature]

# Sampling the data into test set and training set
index = createDataPartition(y = our.modified.data$DEFAULT, times = 1, 
                            p = 0.8, list = F)
trd = our.modified.data[index, ]
tsd = our.modified.data[-index, ]

prop.table(table(our.modified.data$DEFAULT))
prop.table(table(trd$DEFAULT))
prop.table(table(tsd$DEFAULT))

# ------------------------------------------------------------------------------------
#       FITTING A CLASSIFIER USING NAIVEBAYES
# ------------------------------------------------------------------------------------
classifier.nb = naiveBayes(formula = DEFAULT ~., 
                           data = trd)

summary(classifier.nb)

# ------------------------------------------------------------------------------------
#       FITTING A CLASSIFIER USING RANDOMFOREST
# ------------------------------------------------------------------------------------
classifier.rf = randomForest(formula = DEFAULT ~., 
                           data = trd, ntree = 10)

summary(classifier.rf)

# ------------------------------------------------------------------------------------
#       FITTING A CLASSIFIER USING KNN
# ------------------------------------------------------------------------------------
our.predict.knn = knn(train = trd, test = tsd, k = 5, cl = trd$DEFAULT)
# CONFUSION MATRIX
confusionMatrix(our.predict.knn, tsd$DEFAULT)

#summary(classifier.rf)

# -------------------------------------------------------------------------------------
#     PREDICTING VALUES - NAIVEBAYES
# -------------------------------------------------------------------------------------

our.predict.nb = predict(classifier.nb, newdata = tsd, type = "class")
length(our.predict.nb)

# CONFUSION MATRIX
nB_cm = confusionMatrix(our.predict.nb, tsd$DEFAULT)

# -------------------------------------------------------------------------------------
#     PREDICTING VALUES - RANDOMFOREST
# -------------------------------------------------------------------------------------

our.predict.rf = predict(classifier.rf, newdata = tsd, type = "class")
length(our.predict.rf)

# CONFUSION MATRIX
rf_cm = confusionMatrix(our.predict.rf, tsd$DEFAULT)

# -----------------------------------------------------------------------------------
# ------------------------------WORKPLACE BACKUP-------------------------------------
# -----------------------------------------------------------------------------------

# create a data frame for model and their respective accuracy for 80:20 data
model_accuracy_80 = data.frame(Model.Name = c("Naive Bayes", "Random Forest", "KNN"), 
                               Accuracy = c(nB_cm$overall[1], rf_cm$overall[1], knn_cm$overall[1]))
model_accuracy_80$index = c(1, 2, 3)


# Applying k-fold cross validation - Naive Bayes
fold = createFolds(y = trd$DEFAULT, k = 10)

cv = lapply(fold, function(x){
  trd_fold = trd[-x, ]
  tsd_fold = trd[x, ]
  
  classifier.nb_f = naiveBayes(formula = DEFAULT ~., 
                             data = trd_fold)
  our.predict.nb_f = predict(classifier.nb_f, newdata = tsd_fold, type = "class")
  
  nb_cm_f = confusionMatrix(our.predict.nb_f, tsd_fold$DEFAULT)
  accuracy = nb_cm_f$overall[1]
  
  return(accuracy)
})

# Applying k-fold cross validation - Random Forest
fold = createFolds(y = trd$DEFAULT, k = 10)

cv_rf = lapply(fold, function(x){
  trd_fold = trd[-x, ]
  tsd_fold = trd[x, ]
  
  classifier.rf_f = randomForest(formula = DEFAULT ~., 
                               data = trd_fold, ntree = 20)
  our.predict.rf_f = predict(classifier.rf_f, newdata = tsd_fold, type = "class")
  
  rf_cm_f = confusionMatrix(our.predict.rf_f, tsd_fold$DEFAULT)
  accuracy = rf_cm_f$overall[1]
  
  return(accuracy)
})

# Applying k-fold cross validation - KNN
fold = createFolds(y = trd$DEFAULT, k = 10)

cv_knn = lapply(fold, function(x){
  trd_fold = trd[-x, ]
  tsd_fold = trd[x, ]
  
  our.predict.knn.f = knn(train = trd_fold, test = tsd_fold, k = 5, cl = trd_fold$DEFAULT)
  # CONFUSION MATRIX
  knn_cm_f = confusionMatrix(our.predict.knn.f, tsd_fold$DEFAULT)
  accuracy = knn_cm_f$overall[1]
  
  return(accuracy)
})

# model_accuracy data frame for k-fold cv
model_accuracy_80_cv = data.frame(Model.Name = c("Naive Bayes", "Random Forest", "KNN"), 
                               Accuracy = c(mean(as.numeric(cv)), mean(as.numeric(cv_rf)), 
                                            mean(as.numeric(cv_knn))))

# -----------------------------------------------------------------------------------
# ------------------------------WORKPLACE BACKUP-------------------------------------
# -----------------------------------------------------------------------------------


