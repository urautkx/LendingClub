
loansvm<-read.csv("LoanStats_2017-18.csv", stringsAsFactors = F)

loansvm2<-loansvm[loansvm$loan_status%in%c("Fully Paid","Charged Off"),]

View(loansvm2)

str(loansvm)

loanFinal2<-loansvm[,c("loan_status","annual_inc","loan_amnt",
                       "purpose","term", "home_ownership", "dti",
                       "inq_last_6mths", "delinq_2yrs","revol_bal",
                       "revol_util", "total_acc", "emp_length", "open_acc",
                       "int_rate","initial_list_status",
                       "tot_cur_bal","grade")]

head(loanFinal2)
str(loanFinal2)
summary(loanFinal2)

# Variable pre-processing
loanFinal2$loan_status<-ifelse(loanFinal2$loan_status=="Fully Paid",1,0)
loanFinal2$loan_status<-as.factor(loanFinal2$loan_status)

library(tidyr)
loanFinal2<-separate(loanFinal2, "revol_util", into=c("revol_util"), sep = "%", remove = T, convert=T)
#This introduces NAs as values in revol_util columns where revol_util was blank originally
#revol_util measures the percentage of total revolving credit limit that is being utilized
#This field has a relationship with revol_bal. We notice that for mostrecords (except one)
#where revol_util is NA, revol_bal is zero. We will subsitute NA with zeros for this field
loanFinal2$revol_util[which(is.na(loanFinal2$revol_util))]<-0


#Let's convert emp_length into numeric
#We will eliminate the word "year", we wil replace 10+ with 15 and "< 10" with 0.5
el<-gsub("10\\+ years","15",loanFinal2$emp_length,ignore.case = T)
el<-gsub("< 1 year","0.5",el,ignore.case = T)
el<-gsub("1 year","1",el,ignore.case = T)
el<-gsub("n/a",NA,el,ignore.case = T)
el<-as.numeric(gsub("years","",el,ignore.case = T))
loanFinal2$emp_length<-el
rm(el)
#There are lot of "n/a" values which give NA on numeric conversion. 
#We will assign 0 to emp_length where it is NA assuming these people are 
#either unemployed or self-employed
loanFinal2$emp_length[which(is.na(loanFinal2$emp_length))]<-0

#Column term is currently a string field. Can be easily converted into numeric.
loanFinal2$term<-as.numeric(gsub(" months","",loanFinal2$term,ignore.case = T))

#Column int_rate is currently a string field as well. We will convert it into numeric.
loanFinal2$int_rate<-as.numeric(gsub("%","",loanFinal2$int_rate,ignore.case = T))



# Let's conver categorical variables into factor type
#2 level factors
loanFinal2$term<-as.factor(loanFinal2$term)
loanFinal2$initial_list_status<-as.factor(loanFinal2$initial_list_status)
#>2 level factors
#loanFinal2$grade<-as.factor(loanFinal2$grade)
loanFinal2$home_ownership<-as.factor(loanFinal2$home_ownership)
loanFinal2$purpose<-as.factor(loanFinal2$purpose)
#For grade we are going to hot encode it as 1,2,3,4,5,6,7 for 
#for grade A,B,C,D,E,F,G respectively
loanFinal2$grade[loanFinal2$grade=='A']<-1
loanFinal2$grade[loanFinal2$grade=='B']<-2
loanFinal2$grade[loanFinal2$grade=='C']<-3
loanFinal2$grade[loanFinal2$grade=='D']<-4
loanFinal2$grade[loanFinal2$grade=='E']<-5
loanFinal2$grade[loanFinal2$grade=='F']<-6
loanFinal2$grade[loanFinal2$grade=='G']<-7
loanFinal2$grade<-as.factor(loanFinal2$grade)

#convert categorical variables purpose and grade into dummy numeric variables.

dummy1<-model.matrix(~purpose-1,data=loanFinal2)
loanFinal2<-cbind(loanFinal2,dummy1[,-1])

#dummy2<-model.matrix(~grade-1,data=loanFinal2)
#loanFinal2<-cbind(loanFinal2,dummy2[,-1])


dummy3<-model.matrix(~home_ownership,data=loanFinal2)
loanFinal2<-cbind(loanFinal2,dummy3[,-1])



#Since we have dummy variables for factor variables, 
#we can remove the original factor variables
loanFinal3<-loanFinal2[,-which(names(loanFinal2)%in% c("purpose","home_ownership","emp_length"))]

str(loanFinal3)

summary(loanFinal3)

#dti has some NA's let's replace NA's with zeros
loanFinal3$dti[which(is.na(loanFinal3$dti))]<-0
#annual income is highly skewed with extreme high values. 
#loanFinal3$annual_inc[loanFinal3$annual_inc>150000]<-150000

summary(loanFinal3)


#scale numeric variables

loanFinal3$annual_inc<-scale(loanFinal3$annual_inc)

loanFinal3$loan_amnt<-scale(loanFinal3$loan_amnt)

loanFinal3$dti<-scale(loanFinal3$dti)

loanFinal3$inq_last_6mths<-scale(loanFinal3$inq_last_6mths)

loanFinal3$delinq_2yrs<-scale(loanFinal3$delinq_2yrs)

loanFinal3$revol_bal<-scale(loanFinal3$revol_bal)

loanFinal3$revol_util<-scale(loanFinal3$revol_util)

loanFinal3$total_acc<-scale(loanFinal3$total_acc)

#loanFinal3$emp_length<-scale(loanFinal3$emp_length)

loanFinal3$open_acc<-scale(loanFinal3$open_acc)

loanFinal3$int_rate<-scale(loanFinal3$int_rate)

loanFinal3$tot_cur_bal<-scale(loanFinal3$tot_cur_bal)

summary(loanFinal3)

table(loanFinal3$loan_status)

set.seed(1)

train.indices = sample(1:nrow(loanFinal3), 0.6*nrow(loanFinal3))
train = loanFinal3[train.indices, ]
nottrain = loanFinal3[-train.indices, ]

nottrain.indices = sample(1:nrow(nottrain), 0.5*nrow(nottrain))
validate = nottrain[nottrain.indices,]
test = nottrain[-nottrain.indices,]

#-------------------------------------------------------------------------
#Constructing Models - SVMs
library(kernlab)
library(caret)

#Using Linear SVM Kernel 
ksvm_linear <- ksvm(loan_status~ ., data = train, 
                    scale = FALSE, kernel = "vanilladot",
                    na.action=na.omit)

ksvm_linear
#parameter : cost C = 1
#training error : 33% 

validate_predict<-predict(ksvm_linear,validate[,-1])

conf_ksvmlinear<-confusionMatrix(validate_predict,validate$loan_status)

conf_ksvmlinear
# Accruacy 66.5% 
# Senstivity 20%
# Sepcificity 94%
# Positive class for calculating Sensitivity and Specificity 
# is factor level = 0 for loan_status i.e. loan that are charged off

#Therefore, our goal would be improve True Positives and reduce False negatives
#i.e. goal is to improve Sensitivity

#-------------------------------------------------------------

#Using Polynomial SVM Kernel 
ksvm_poly<- ksvm(loan_status~ ., data = train, 
                    scale = FALSE, kernel = "polydot",
                    na.action=na.omit)

ksvm_poly
#Parameters c = 1, degree =1, scale =1 , offset = 1
#training error = 33%

validate_predict<-predict(ksvm_poly,validate[,-1])

conf_ksvmpoly<-confusionMatrix(validate_predict,validate$loan_status)

conf_ksvmpoly
# Accruacy 66.5% 
# Senstivity 20%
# Sepcificity 94%
# Positive class for calculating Sensitivity and Specificity 
# is factor level = 0 for loan_status i.e. loan that are charged off

#--------------------------------------------------------------------------

#Using RBF SVM Kernel 
ksvm_rbf<- ksvm(loan_status~ ., data = train, 
                 scale = FALSE, kernel = "rbfdot",
                 na.action=na.omit)

ksvm_rbf
#Parameters c = 1, degree =1, scale =1 , offset = 1
#sigma = 0.025
#training error = 28.7%

validate_predict<-predict(ksvm_rbf,validate[,-1])

conf_ksvmrbf<-confusionMatrix(validate_predict,validate$loan_status)

conf_ksvmrbf
# Accruacy 68.53%
# Senstivity 34.1%
# Sepcificity 89.4%
# Positive class for calculating Sensitivity and Specificity 
# is factor level = 0 for loan_status i.e. loan that are charged off

#----------------------------------------------------------------------------

library(randomForest)
rf_model1<-randomForest(loan_status~.,data=train,importance=T,
                        ntree=700,
                        mtry=3,
                        do.trace=F,
                        proximity=F)

rf_model1
#OOB estimate of error rate : 30%

varImpPlot(rf_model1)

importance(rf_model1)

validate_predict<-predict(rf_model1,validate[,-1])

conf_rf<-confusionMatrix(validate_predict,validate$loan_status)

conf_rf
# Accuracy 70.6%
# Sensitivity 38.4%
# Specificity 91.6%

#--------------------------------------------------------------------------
#Logistics Regression

logistic_model<-glm(loan_status~., data=train,family="binomial")
logistic_model
#AIC : 13290

logistic_model_2<-step(logistic_model,direction="both")
#Step function removed revol_bal, term and purposehouse

summary(logistic_model_2)
#AIC : 13286
library(car)

vif(logistic_model_2)

#remove grade

logistic_model_3<-glm(formula = loan_status ~ annual_inc + loan_amnt + dti + inq_last_6mths + 
        delinq_2yrs + revol_util + total_acc + open_acc + int_rate + 
        initial_list_status + tot_cur_bal + purposecredit_card + 
        purposedebt_consolidation + purposehome_improvement + purposemajor_purchase + 
        purposemedical + purposemoving + purposeother + purposerenewable_energy + 
        purposesmall_business + purposevacation + purposewedding + 
        home_ownershipMORTGAGE + home_ownershipOWN, family = "binomial", 
    data = train)

summary(logistic_model_3)
#AIC : 13439

vif(logistic_model_3)

# AIC jumped quite high, we can not eliminate grade
# Let's add back grade and try eliminating purposecredit_card


logistic_model_4<-glm(formula = loan_status ~ grade + annual_inc + loan_amnt + dti + inq_last_6mths + 
                          delinq_2yrs + revol_util + total_acc + open_acc + int_rate + 
                          initial_list_status + tot_cur_bal + 
                          purposedebt_consolidation + purposehome_improvement + purposemajor_purchase + 
                          purposemedical + purposemoving + purposeother + purposerenewable_energy + 
                          purposesmall_business + purposevacation + purposewedding + 
                          home_ownershipMORTGAGE + home_ownershipOWN, family = "binomial", 
                      data = train)

summary(logistic_model_4)
#AIC : 13287 it dropped to previous levels

vif(logistic_model_4)

#VIF is still high for grade and int_rate. Let's try dropping int_rate

logistic_model_5<-glm(formula = loan_status ~ grade + annual_inc + loan_amnt + dti + inq_last_6mths + 
                          delinq_2yrs + revol_util + total_acc + open_acc  + 
                          initial_list_status + tot_cur_bal + 
                          purposedebt_consolidation + purposehome_improvement + purposemajor_purchase + 
                          purposemedical + purposemoving + purposeother + purposerenewable_energy + 
                          purposesmall_business + purposevacation + purposewedding + 
                          home_ownershipMORTGAGE + home_ownershipOWN, family = "binomial", 
                      data = train)

summary(logistic_model_5)
#AIC : 13298 it increased but increase is not too bad

vif(logistic_model_5)
#VIFs are good.
#Let's eliminate purposemoving and purposewedding since their p-values are high

logistic_model_6<-glm(formula = loan_status ~ grade + annual_inc + loan_amnt + dti + inq_last_6mths + 
                          delinq_2yrs + revol_util + total_acc + open_acc  + 
                          initial_list_status + tot_cur_bal + 
                          purposedebt_consolidation + purposehome_improvement + purposemajor_purchase + 
                          purposemedical + purposeother + purposerenewable_energy + 
                          purposesmall_business + purposevacation  + 
                          home_ownershipMORTGAGE + home_ownershipOWN, family = "binomial", 
                      data = train)

summary(logistic_model_6)
#AIC : 13299 it increased by one point

vif(logistic_model_6)
#VIFs still good. Let's remove initial_list_statusw

logistic_model_7<-glm(formula = loan_status ~ grade + annual_inc + loan_amnt + dti + inq_last_6mths + 
                          delinq_2yrs + revol_util + total_acc + open_acc  + 
                          tot_cur_bal + 
                          purposedebt_consolidation + purposehome_improvement + purposemajor_purchase + 
                          purposemedical + purposeother + purposerenewable_energy + 
                          purposesmall_business + purposevacation  + 
                          home_ownershipMORTGAGE + home_ownershipOWN, family = "binomial", 
                      data = train)

summary(logistic_model_7)
#AIC : 13300 it increased by one point

vif(logistic_model_7)
#VIFs still good. Let's remove purposevacation and purposeother

logistic_model_8<-glm(formula = loan_status ~ grade + annual_inc + loan_amnt + dti + inq_last_6mths + 
                          delinq_2yrs + revol_util + total_acc + open_acc  + 
                          tot_cur_bal + 
                          purposedebt_consolidation + purposehome_improvement + purposemajor_purchase + 
                          purposemedical + purposerenewable_energy + 
                          purposesmall_business  + 
                          home_ownershipMORTGAGE + home_ownershipOWN, family = "binomial", 
                      data = train)

summary(logistic_model_8)
#AIC : 13306 it increased slightly

vif(logistic_model_8)
#Let's remove dti for its high p-value

logistic_model_9<-glm(formula = loan_status ~ grade + annual_inc + loan_amnt + inq_last_6mths + 
                          delinq_2yrs + revol_util + total_acc + open_acc  + 
                          tot_cur_bal + 
                          purposedebt_consolidation + purposehome_improvement + purposemajor_purchase + 
                          purposemedical + purposerenewable_energy + 
                          purposesmall_business  + 
                          home_ownershipMORTGAGE + home_ownershipOWN, family = "binomial", 
                      data = train)

summary(logistic_model_9)
#AIC : 13306 it increased slightly

vif(logistic_model_9)
#Let's eliminate home_ownershipOWN

logistic_model_10<-glm(formula = loan_status ~ grade + annual_inc + loan_amnt + inq_last_6mths + 
                          delinq_2yrs + revol_util + total_acc + open_acc  + 
                          tot_cur_bal + 
                          purposedebt_consolidation + purposehome_improvement + purposemajor_purchase + 
                          purposemedical + purposerenewable_energy + 
                          purposesmall_business  + 
                          home_ownershipMORTGAGE, family = "binomial", 
                      data = train)

summary(logistic_model_10)
#AIC : 13312 it increased slightly

vif(logistic_model_10)
#Let's eliminiate purposerenewable_energy 


logistic_model_11<-glm(formula = loan_status ~ grade + annual_inc + loan_amnt + inq_last_6mths + 
                           delinq_2yrs + revol_util + total_acc + open_acc  + 
                           tot_cur_bal + 
                           purposedebt_consolidation + purposehome_improvement + purposemajor_purchase + 
                           purposemedical + 
                           purposesmall_business  + 
                           home_ownershipMORTGAGE, family = "binomial", 
                       data = train)

summary(logistic_model_11)
#AIC : 13312 it increased slightly

vif(logistic_model_11)

final_model<-logistic_model_11

train_pred_values<-predict(final_model,train[,-1],type="response")

train_pred_factors <- factor(ifelse(train_pred_values >= 0.85, 1 , 0))
train_actual_factors<-factor(train[,1])

confusionMatrix(train_pred_factors,train_actual_factors)

#For training data set
#Positive class :0 ("Charge Off" - loan_status)
#Sensitivity at .85 cut-off is ~96%


validate_pred_values<-predict(final_model,validate[,-1],type="response")

str(test_pred_values)

validate_actual_factors<- factor(validate[,1])

validate_pred_factors <- factor(ifelse(validate_pred_values >= 0.85, 1 , 0))

confusionMatrix(validate_pred_factors,validate_actual_factors)
#For training data set
#Positive class : 0
#Sensitivity at .85 cutoff is ~96%

validate$predictions<-validate_pred_factors

head(validate)

table(validate$grade,validate$predictions)

table(validate$grade,validate$loan_status)

#------------------------------------------------------------------------------------

#Reduced set of dimensions that are identified as important by Logistics Regression are:

#grade
#annual_inc
#loan_amnt
#inq_last_6mths
#delinq_2yrs
#revol_util
#total_acc
#open_acc
#tot_cur_bal
#purpose
#homeownership

#Reduced set of dimensions that are identified as important by Random Forest are:

#int_rate 
#revol_util
#tot_cur_bal
#dti
#grade
#revol_bal
#annual_inc
#loan_amnt
#total_acc
#open_acc
#inq_last_6mths
#delinq_2yrs

#Based on the above two list we can say that the most important factors that are essential
#in classifying charged off loans from paid loans are:
# 1. int_rate   : higher the int_rate, higher the probl of charge off
# 2. grade      : higher the grade, higher the prob of charge off
# 3. revol_util : higher revolving credit utilization, higher the prob of charge off
# 4. tot_cur_bal (+): higher the total cur bal, lower the probl of charge off
# 5. annual_inc : higher the income, lower the prob of charge off
# 6. loan_amnt  : higher the loan amnt, higher the prob of charge off
# 7. total_acc   (+): More the number of total acc, lower the probl of charge off
# 8. open_acc   : More the number of open_acc, higher the probl of charge off
# 9. inq_last_6mths : More the number of inquiries in last 6 months, higher the probl of charge off
# 10. delinq_2yrs : More the number of delinquencies, higher the probl of charge off. 
# 11. purposesmall_business : This purpose has higher prob of charge off.

#-----------------------------------------------------------------------------------

library(glmnet)

x=data.matrix(train[,-1])
y=train$loan_status

validateX=data.matrix(validate[,-1])
validate_actual_factors=validate$loan_status


#fit1<-glmnet(x,y,family="binomial",alpha=1)
#print(fit1)
#plot(fit1,label=T)


cvfit<-cv.glmnet(x,y,family="binomial", alpha=1)
plot(cvfit)

minLambdaLasso<-cvfit$lambda.min
minLambdaLasso

#cvfit$lambda.1se

#Binomial deviance is least at lambda = .007 i.e. log(lambda)=-5
#Binomial Deviance at log(lambda)=-4.5 is only slight more than 
#its minimum value. But the selected factors are 14 at log(lambda)=-4.5
#Lets select this value for lambda. Lambda = 0.011

lasso.mod <- glmnet(x,y,family="binomial",alpha=1,lambda = 0.011)

plot(lasso.mod,label=T)
print(lasso.mod)

lasso.pred<- predict(lasso.mod,s=0.011,newx=validateX,type="class")
validate_pred_factors<-as.factor(lasso.pred)

str(validate_pred_factors)

library(caret)

confusionMatrix(validate_pred_factors,validate_actual_factors)

#Senstivity:
#Selectivity:

predict(lasso.mod,s=.011,type="coefficients")

#These 14 variables are retained by lasso logistics regression:
#loan_amnt
#dti
#inq_last_6mths
#delinq_2yrs
#revol_util
#total_acc
#int_rate
#tot_cur_bal
#grade
#purposecredit_car
#purposehouse
#purposesmall_business
#home_ownershipMOrtgage
#home_ownershipRent
