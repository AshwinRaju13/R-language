# Stepwise method.

stp= read.csv(file.choose())
View(stp)

stp= stp[-1]
#in the data we have loan id as a variable which is not required for our model. so, it is excluded from the dataframe.

# another issue which we have in our data is NA values. It would give error while computing our model.
# in that case we use the MISSFOREST to replace all the NA values from the data.

attach(stp)
library("missForest")
stp_mis= missForest(stp, ntree = 300, mtry = 3)
dim(stp)
write.csv(stp_mis$ximp, file = "stp_mis1")
stp_mf= read.csv("stp_mis1", header = T)
View(stp_mf)

# After replacing the NA values still we have categorical variables in our data. we recode the variables into numaic
#thus, it will  be helpfull while we fitting the module for the data.

stp_mf$Gender<-ifelse(stp_mf$Gender=="Male", 1, 0)
stp_mf$Married<-ifelse(stp_mf$Married=="Yes", 1, 0)
stp_mf$Education<-ifelse(stp_mf$Education=="Graduate", 1, 0)
stp_mf$Self_Employed<-ifelse(stp_mf$Self_Employed=="Yes", 1, 0)
stp_mf$Property_Area_rural<-ifelse(stp_mf$Property_Area=="Rural",1,0)
stp_mf$Property_Area_urban<-ifelse(stp_mf$Property_Area=="Urban",1,0)
stp_mf$Loan_Status= ifelse(stp_mf$Loan_Status== "Y",1,0)
View(stp_mf)
stp_mf= stp_mf[-1]


# fitting the modle

stp_lm= lm(LoanAmount~Gender+Married+Education+Self_Employed+ApplicantIncome+Loan_Amount_Term
           +Credit_History+Loan_Status+Property_Area_Rural+Property_Area_Urban,data = stp_mf)
stp_lm
summary(stp_lm)
#  from the coefficients table we got some 'significant values' and the total variation explained 
# by the independent variable on the dependent variales is 37% (i.e) R- squared of the modle. Also, p-value
#is less than 0.05 which concludes that there is significant

#performing FORWARD method

#Forward selection: which involves starting with no variables in the model, testing the addition of each variable
#using a chosen model fit criterion, adding the variable (if any) whose inclusion gives the most statistically
#significant improvement of the fit, and repeating this process until none improves the model to a statistically
#significant extent.

stp_frd= step(stp_lm, direction = "forward")
summary(stp_frd)
# from the summary table married, eduction, application income and loan amount variable are signifiacant
# and r- square is 36%.

#performing backward

stp_bck= step(stp_lm, direction = 'backward')
summary(stp_bck)

# In this model after eleminating all the insignificant variable, the variable married, education, application, loan amount and loan status
# are significant.

#Performing stepwise.

stp_wise= step(stp_lm, direction = "both")
summary(stp_wise)

# In this model after eleminating all the insignificant variable, the variable married, education, application, loan amount and loan status
# are significant.

#conclusion: when we compare the r- square for both the regression and stepwise model, we get 37%
# in regression model which is best fit. and we have taken random independent variable as our dependent variable(for learning purpose)
