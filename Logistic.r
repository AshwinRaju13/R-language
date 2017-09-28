# diabetes 

db_data= read.csv("c:/Users/Akesh/Desktop/tutorial/DATA/diabetes.csv")
head(db_data)

# separating the data into testing and training data.
library("caret")
db_part= createDataPartition(db_data$Outcome, p= 1/6, list= F)
db_train= db_data[-db_part, ]
db_test= db_data[db_part,]

# fiiting the model for the partition data
attach(db_data)
db_logic= glm(Outcome~.-SkinThickness-Insulin-Age, data = db_train, family = 'binomial')
summary(db_logic)

#prediction

db_pred= predict(db_logic, db_test, type = 'response',dispersion = 0.6)
db_pred

# creating confusing matrix
# For creating confuse matrix table we have to use actual testdata and prediction data which we predict using testdata.
db_conf= table(Actual= db_test$Outcome, predictive= db_pred>0.5)
db_conf
# in the confusion matrix table , it disclose that (2nd row /1st col) the actualvalue result says no. of person having diabetes and the predictive results no. of persons doesn't have sugar. 
#This makes us confuse, thus we have reduce that as much as possible so that we could save some more peoples life.

db_conf_1= table(Actual= db_test$Outcome, predictive= db_pred>0.3)
db_conf_1

# ROC curve
# True positive rate
# false positive rate
library('ROCR')
db_rocr1= prediction(db_pred, db_test$Outcome)
db_rocr2= performance(db_rocr1, 'tpr','fpr')
library('gplots')
plot(db_rocr2, colorize= TRUE, print.cutoffs.at= seq(0.1,by= 0.1))
plot(db_rocr2, colorize=TRUE, print.cutoffs.at=seq(0.1, by=0.1))
