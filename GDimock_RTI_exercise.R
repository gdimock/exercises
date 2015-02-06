#SQL Query from SQLite 

#CREATE TABLE  records_flat as 
#SELECT a.id,a.over_50k,a.age,a.education_num,a.capital_gain,a.capital_loss,a.hours_week , 
#b.name as country,
#c.name as education_level,
#d.name as marital_status,
#e.name as occupation, 
#f.name as race, 
#g.name as relationship, 
#h.name as sex, 
#i.name as workclass
# FROM records a left join countries b on (a.country_id = b.id)
               # left join education_levels c on (a.education_level_id = c.id)
               # left join marital_statuses d on (a.marital_status_id = d.id)
               # left join occupations e on (a.occupation_id = e.id)
               # left join races f on (a.race_id = f.id)
               # left join relationships g on (a.relationship_id = g.id)
               # left join sexes h on (a.sex_id = h.id)
               # left join workclasses i on (a.workclass_id = i.id)
##############################################################################

#Libraries needed for analysis
install.packages("gmodels")
install.packages("vcd")
install.packages("epiR")
install.packages("NSM3")
install.packages("rms")
install.packages("nmle")
install.packages("pROC")

library(foreign)
library(gmodels)
library(rms)
library(nmle)
library(pROC)
library(epiR)
library(vcd)
library(NSM3)

#Reading in records csv dataset
fileloc = "C:/Users/Greg/Desktop/Documents/RTI_Excercise/"
filename = "records_flat.csv"
records = read.csv(file = paste(fileloc,filename,sep=""),header= TRUE,row.names = 1)

#Attach records dataset for easier reference 
attach(records)
###########################################################################################
#Viewing Target Variable and summary information
table(over_50k)
#Event (1) for over_50k is not rare (about 24% occurance), so oversampling when splitting the
#is not necessary 
summary(records)

#Detecting Potential Associations with Target(over_50k)
target.marital <- table(over_50k,marital_status)
assocstats(target.marital)
#Association exists between marriage and target (medium strength)

target.country <-table(over_50k,country)
tassocstats(target.country)
#Association exists between country and target (very weak strength)
#Will have trouble with quasi-complete seperation because of number of levels 

target.sex <- table(over_50k,sex)
assocstats(target.sex)
#Association exists between sex and target (weak strength) 

target.race <- table(over_50k,race)
assocstats(target.race)
#Association exists between race and target (weak strength) 

target.education <- table(over_50k,education_level)
assocstats(target.education)
#Association exists between education level and target (medium strength)
#preschool and over_50k has one occurance (will have trouble with quasi complete seperation on training/testing/validaiton data) that would need to be worked around 

target.occupation <- table(over_50k,occupation)
assocstats(target.occupation)
#ASsociation exists between occupation and target (medium strength)
#may have trouble with qausi complete seperation on a training dataset (entry in table contingency table with values of 3 and 5 )

#Variable relationship is not in data dictionary provided, but is in the dataset
target.relationship <-table(over_50k,relationship)
assocstats(target.relationship)
#Association exists between relationship and target (medium strength)

target.workclass <- table(over_50k,workclass)
assocstats(target.workclass)
#Association exists between workclass and target (weak strength)
#problem with quasi complete seperation of data

#Chi squared associations seem to exist with all categorical variables, to varying degrees of strength 
###################################################################
#Next test for possible cases of confoudning 
mantelhaen.test(table(over_50k,sex,race))
mantelhaen.test(table(over_50k,sex,occupation))
#would like to continue in this 

###################################################################################
#Check for potential interactions: 
#Age and occupation
CrossTable(over_50k,occupation,age)
age.occ.interact <- epi.2by2(table(over_50k,occupation,age),homogeneity = "breslow.day")
summary(acc.occ.interact)$OR.homog
#this code is currently not working, but it is worth looking into further 

#########################################################################
##Split data for model building: 
##70% Trian, 20% Validation, 10% Test 
records$ranuni <- runif(nrow(records),0,1)
records_train <- records[records$ranuni < 0.70,1:14,]#With SAMPLE Size of nearly 35,000 set conservative p-value of 0.005
records_validate <- records[0.70 <= records$ranuni & records$ranuni < 0.90,1:14]
records_test <- records[records$ranuni >= 0.90,1:14]

#checking box tidwell assumption for continous variables
#Replacing capital gain and loss 0 values with small value to avoid log(0)
#records_train$capital_gain[records_train$capital_gain == 0] <- 0.01
#records_train$capital_loss[records_train$capital_loss == 0] <- 0.01
detach(records)
attach(records_train)

records_train <- data.frame(records_train, a.logs = age*log(age), ed.logs = education_num*log(education_num), h.logs = hours_week * log(hours_week))

detach(records_train)
attach(records_train)

BT.Check <- glm(over_50k ~ age + a.logs + education_num + ed.logs + hours_week + h.logs, family = binomial(logit))
summary(BT.Check)
#Neither education_num, age, hours_week pass the Box Tidwell assumption that the continous variables are linearly related to the logit
#Exponentials for each continous variable are recalculated -  however, none of the coefficients are large enough to make the adjusted value much greater than 1 so for interpretability leave exponents at 1
#Also education_num is not significant here. Could use education level instead to see if it is signifanct - since a relationship exists in the contignecy table from above

target.occupation2 <- table(over_50k,occupation)
#occupation does not have quasi complete seperation in the training data

target.education2 <- table(over_50k,education_level)
#education_level does not have quasi complete seperation in training data 

target.workclass2 <- table(over_50k,workclass)
#To use this variable in modeling building must fix quasi complete seperation with never-worked and over_50k


##Begin with a base model: logit.model_a 
logit.model_a <- glm(over_50k ~ age + hours_week + sex + race + occupation + education_level)
#all variables are significant except hours_week
summary(logit.model_a) 
logit.model_a.ROC <- roc(logit.model_a$y, logit.model_a$fitted)
print(logit.model_a.ROC) #AUC = 0.8246

##logit.model_b : removing insignificant hours_week
logit.model_b <- glm(over_50k ~ age + sex + race + occupation + education_level)
summary(logit.model_b)
logit.model_b.ROC <- roc(logit.model_b$y,logit.model_b$fitted)
print(logit.model_b.ROC) #AUC = 0.8168

##logit.model_c: replacing education_level with education_num (continous version)
logit.model_c <- glm(over_50k ~ age + sex + race + occupation + education_num)
summary(logit.model_c)
logit.model_c.ROC <- roc(logit.model_c$y,logit.model_c$fitted)
print(logit.model_c.ROC) #AUC = 0.8174 (slightly better than with education_level) 

##logit.model_d : adding marital_status (using education_num)
logit.model_d <- glm(over_50k ~ age + sex + race + occupation + education_num + marital_status)
summary(logit.model_d)
#race variable levels have become less significant, consider collapsing into white non-white?
logit.model_d.ROC <- roc(logit.model_d$y,logit.model_d$fitted)
print(logit.model_d.ROC) #AUC = 0.874 (big jump in AUC from c)

##logit.model_e: using model_d with education_level instead of education_num
logit.model_e <- glm(over_50k ~ age + sex + race + occupation + education_level + marital_status)
summary(logit.model_e)
#With a significance level of 0.002 race appears to not be significant (would have to actually test typeIII p-values to be sure)
logit.model_e.ROC <- roc(logit.model_e$y,logit.model_e$fitted)
print(logit.model_e.ROC) #AUC = 0.8738 

##logit.model_f : testing interaction with age and occupation
logit.model_f <- glm(over_50k ~ age + sex + race + occupation + education_num + marital_status + age*occupation)
summary(logit.model_f) #one singularity existed, age*occupation seemed to be significant 
#race variable levels have become less significant, consider collapsing into white non-white?
logit.model_f.ROC <- roc(logit.model_f$y,logit.model_f$fitted)
print(logit.model_f.ROC) #AUC = 0.8754 (not much different than model_e)

##logt.model_g: dropping race from model_d
logit.model_g <- glm(over_50k ~ age + sex + occupation + education_num + marital_status)
summary(logit.model_g)
logit.model_g.ROC <- roc(logit.model_g$y,logit.model_g$fitted)
print(logit.model_g.ROC) #AUC = 0.8738 (not much change from model_d)

##logit.model_full--> all variables except country, education_level, workclass 
logit.model_full <- glm(over_50k ~ age + sex + race + occupation + education_num + marital_status + hours_week + capital_gain + capital_loss)
summary(logit.model_full)#races are insignificant, 
logit.model_full.ROC <- roc(logit.model_full$y,logit.model_full$fitted)
print(logit.model_full.ROC) #AUC = 0.8911


logit.model_backwards <- stepAIC(logit.model_full, direction="backward")

#################################################################################################
## compare ROC curves 
roc.test(logit.model_full.ROC,logit.model_b.ROC) #different
roc.test(logit.model_b.ROC,logit.model_d.ROC) #different
roc.test(logit.model_e.ROC,logit.model_d.ROC) #not different 
roc.test(logit.model_g.ROC,logit.model_d.ROC) #not different 
roc.test(logit.model_full.ROC,logit.model_e.ROC) #different 
roc.test(logit.model_full.ROC,logit.model_g.ROC) #different 

##USE on validation data: Bring in full model, model_g and model_d  
detach(records_train)
attach(records_validate)

logit.model_dv <- glm(over_50k ~ age + sex + race + occupation + education_num + marital_status)
summary(logit.model_dv)
logit.model_dv.ROC <- roc(logit.model_dv$y,logit.model_dv$fitted)
print(logit.model_dv.ROC) #AUC = 0.8748 

logit.model_gv <- glm(over_50k ~ age + sex + occupation + education_num + marital_status)
summary(logit.model_gv)
logit.model_gv.ROC <- roc(logit.model_gv$y,logit.model_gv$fitted)
print(logit.model_gv.ROC) #AUC = 0.8746 (increased from training data)

logit.model_fullv <- glm(over_50k ~ age + sex + race + occupation + education_num + marital_status + hours_week + capital_gain + capital_loss)
summary(logit.model_fullv)#races are insignificant 
logit.model_fullv.ROC <- roc(logit.model_fullv$y,logit.model_fullv$fitted)
print(logit.model_fullv.ROC) #AUC = 0.8906

#Take out race 
logit.model_fullv2 <- glm(over_50k ~ age + sex + occupation + education_num + marital_status + hours_week + capital_gain + capital_loss)
summary(logit.model_fullv2)#races are insignificant 
logit.model_fullv2.ROC <- roc(logit.model_fullv2$y,logit.model_fullv2$fitted)
print(logit.model_fullv2.ROC) #AUC = 0.8905


roc.test(logit.model_gv.ROC,logit.model_dv.ROC) #not different 
roc.test(logit.model_fullv.ROC,logit.model_fullv2.ROC) #not different
roc.test(logit.model_fullv2.ROC,logit.model_gv.ROC)#different
roc.test(logit.model_fullv2.ROC,logit.model_dv.ROC)#different

#model_fullv2 iis best statistically with the least parameters (but would still need to check assumptions on capital_gain and capital_loss variables  )

##USE original dataset: 
detach(records_validate)
attach(records)

logit.model_FINAL <- glm(over_50k ~ age + sex + occupation + education_num + marital_status + hours_week + capital_gain + capital_loss)
summary(logit.model_FINAL)#races are insignificant 
logit.model_FINAL.ROC <- roc(logit.model_FINAL$y,logit.model_FINAL$fitted)
print(logit.model_FINAL.ROC) #AUC = 0.8916

#Final odds ratios: 
OR <- exp(coef(logit.model_FINAL)[-1])
OR

##GRAPHIC for marriage effect on over_50k

counts <- table(over_50k,marital_status)
barplot(counts, main="Making over 50K by Mariage Types", cex.names = 0.5,
        , ylab = "Count of those with Salary over 50K",col=c("darkblue","red"),
        legend.text = c("No","Yes"), beside=TRUE,las=2)

labs <- paste(names(table(marital_status)), "cylinders")
text(cex=1, x=x-.25, y=-1.25, labs, xpd=TRUE, srt=45)#SQL Query from SQLite 

#CREATE TABLE  records_flat as 
#SELECT a.id,a.over_50k,a.age,a.education_num,a.capital_gain,a.capital_loss,a.hours_week , 
#b.name as country,
#c.name as education_level,
#d.name as marital_status,
#e.name as occupation, 
#f.name as race, 
#g.name as relationship, 
#h.name as sex, 
#i.name as workclass
# FROM records a left join countries b on (a.country_id = b.id)
               # left join education_levels c on (a.education_level_id = c.id)
               # left join marital_statuses d on (a.marital_status_id = d.id)
               # left join occupations e on (a.occupation_id = e.id)
               # left join races f on (a.race_id = f.id)
               # left join relationships g on (a.relationship_id = g.id)
               # left join sexes h on (a.sex_id = h.id)
               # left join workclasses i on (a.workclass_id = i.id)
##############################################################################

#Libraries needed for analysis
install.packages("gmodels")
install.packages("vcd")
install.packages("epiR")
install.packages("NSM3")
install.packages("rms")
install.packages("nmle")
install.packages("pROC")

library(foreign)
library(gmodels)
library(rms)
library(nmle)
library(pROC)
library(epiR)
library(vcd)
library(NSM3)

#Reading in records csv dataset
fileloc = "C:/Users/Greg/Desktop/Documents/RTI_Excercise/"
filename = "records_flat.csv"
records = read.csv(file = paste(fileloc,filename,sep=""),header= TRUE,row.names = 1)

#Attach records dataset for easier reference 
attach(records)
###########################################################################################
#Viewing Target Variable and summary information
table(over_50k)
#Event (1) for over_50k is not rare (about 24% occurance), so oversampling when splitting the
#is not necessary 
summary(records)

#Detecting Potential Associations with Target(over_50k)
target.marital <- table(over_50k,marital_status)
assocstats(target.marital)
#Association exists between marriage and target (medium strength)

target.country <-table(over_50k,country)
tassocstats(target.country)
#Association exists between country and target (very weak strength)
#Will have trouble with quasi-complete seperation because of number of levels 

target.sex <- table(over_50k,sex)
assocstats(target.sex)
#Association exists between sex and target (weak strength) 

target.race <- table(over_50k,race)
assocstats(target.race)
#Association exists between race and target (weak strength) 

target.education <- table(over_50k,education_level)
assocstats(target.education)
#Association exists between education level and target (medium strength)
#preschool and over_50k has one occurance (will have trouble with quasi complete seperation on training/testing/validaiton data) that would need to be worked around 

target.occupation <- table(over_50k,occupation)
assocstats(target.occupation)
#ASsociation exists between occupation and target (medium strength)
#may have trouble with qausi complete seperation on a training dataset (entry in table contingency table with values of 3 and 5 )

#Variable relationship is not in data dictionary provided, but is in the dataset
target.relationship <-table(over_50k,relationship)
assocstats(target.relationship)
#Association exists between relationship and target (medium strength)

target.workclass <- table(over_50k,workclass)
assocstats(target.workclass)
#Association exists between workclass and target (weak strength)
#problem with quasi complete seperation of data

#Chi squared associations seem to exist with all categorical variables, to varying degrees of strength 
###################################################################
#Next test for possible cases of confoudning 
mantelhaen.test(table(over_50k,sex,race))
mantelhaen.test(table(over_50k,sex,occupation))
#would like to continue in this 

###################################################################################
#Check for potential interactions: 
#Age and occupation
CrossTable(over_50k,occupation,age)
age.occ.interact <- epi.2by2(table(over_50k,occupation,age),homogeneity = "breslow.day")
summary(acc.occ.interact)$OR.homog
#this code is currently not working, but it is worth looking into further 

#########################################################################
##Split data for model building: 
##70% Trian, 20% Validation, 10% Test 
records$ranuni <- runif(nrow(records),0,1)
records_train <- records[records$ranuni < 0.70,1:14,]#With SAMPLE Size of nearly 35,000 set conservative p-value of 0.005
records_validate <- records[0.70 <= records$ranuni & records$ranuni < 0.90,1:14]
records_test <- records[records$ranuni >= 0.90,1:14]

#checking box tidwell assumption for continous variables
#Replacing capital gain and loss 0 values with small value to avoid log(0)
#records_train$capital_gain[records_train$capital_gain == 0] <- 0.01
#records_train$capital_loss[records_train$capital_loss == 0] <- 0.01
detach(records)
attach(records_train)

records_train <- data.frame(records_train, a.logs = age*log(age), ed.logs = education_num*log(education_num), h.logs = hours_week * log(hours_week))

detach(records_train)
attach(records_train)

BT.Check <- glm(over_50k ~ age + a.logs + education_num + ed.logs + hours_week + h.logs, family = binomial(logit))
summary(BT.Check)
#Neither education_num, age, hours_week pass the Box Tidwell assumption that the continous variables are linearly related to the logit
#Exponentials for each continous variable are recalculated -  however, none of the coefficients are large enough to make the adjusted value much greater than 1 so for interpretability leave exponents at 1
#Also education_num is not significant here. Could use education level instead to see if it is signifanct - since a relationship exists in the contignecy table from above

target.occupation2 <- table(over_50k,occupation)
#occupation does not have quasi complete seperation in the training data

target.education2 <- table(over_50k,education_level)
#education_level does not have quasi complete seperation in training data 

target.workclass2 <- table(over_50k,workclass)
#To use this variable in modeling building must fix quasi complete seperation with never-worked and over_50k


##Begin with a base model: logit.model_a 
logit.model_a <- glm(over_50k ~ age + hours_week + sex + race + occupation + education_level)
#all variables are significant except hours_week
summary(logit.model_a) 
logit.model_a.ROC <- roc(logit.model_a$y, logit.model_a$fitted)
print(logit.model_a.ROC) #AUC = 0.8246

##logit.model_b : removing insignificant hours_week
logit.model_b <- glm(over_50k ~ age + sex + race + occupation + education_level)
summary(logit.model_b)
logit.model_b.ROC <- roc(logit.model_b$y,logit.model_b$fitted)
print(logit.model_b.ROC) #AUC = 0.8168

##logit.model_c: replacing education_level with education_num (continous version)
logit.model_c <- glm(over_50k ~ age + sex + race + occupation + education_num)
summary(logit.model_c)
logit.model_c.ROC <- roc(logit.model_c$y,logit.model_c$fitted)
print(logit.model_c.ROC) #AUC = 0.8174 (slightly better than with education_level) 

##logit.model_d : adding marital_status (using education_num)
logit.model_d <- glm(over_50k ~ age + sex + race + occupation + education_num + marital_status)
summary(logit.model_d)
#race variable levels have become less significant, consider collapsing into white non-white?
logit.model_d.ROC <- roc(logit.model_d$y,logit.model_d$fitted)
print(logit.model_d.ROC) #AUC = 0.874 (big jump in AUC from c)

##logit.model_e: using model_d with education_level instead of education_num
logit.model_e <- glm(over_50k ~ age + sex + race + occupation + education_level + marital_status)
summary(logit.model_e)
#With a significance level of 0.002 race appears to not be significant (would have to actually test typeIII p-values to be sure)
logit.model_e.ROC <- roc(logit.model_e$y,logit.model_e$fitted)
print(logit.model_e.ROC) #AUC = 0.8738 

##logit.model_f : testing interaction with age and occupation
logit.model_f <- glm(over_50k ~ age + sex + race + occupation + education_num + marital_status + age*occupation)
summary(logit.model_f) #one singularity existed, age*occupation seemed to be significant 
#race variable levels have become less significant, consider collapsing into white non-white?
logit.model_f.ROC <- roc(logit.model_f$y,logit.model_f$fitted)
print(logit.model_f.ROC) #AUC = 0.8754 (not much different than model_e)

##logt.model_g: dropping race from model_d
logit.model_g <- glm(over_50k ~ age + sex + occupation + education_num + marital_status)
summary(logit.model_g)
logit.model_g.ROC <- roc(logit.model_g$y,logit.model_g$fitted)
print(logit.model_g.ROC) #AUC = 0.8738 (not much change from model_d)

##logit.model_full--> all variables except country, education_level, workclass 
logit.model_full <- glm(over_50k ~ age + sex + race + occupation + education_num + marital_status + hours_week + capital_gain + capital_loss)
summary(logit.model_full)#races are insignificant, 
logit.model_full.ROC <- roc(logit.model_full$y,logit.model_full$fitted)
print(logit.model_full.ROC) #AUC = 0.8911


logit.model_backwards <- stepAIC(logit.model_full, direction="backward")

#################################################################################################
## compare ROC curves 
roc.test(logit.model_full.ROC,logit.model_b.ROC) #different
roc.test(logit.model_b.ROC,logit.model_d.ROC) #different
roc.test(logit.model_e.ROC,logit.model_d.ROC) #not different 
roc.test(logit.model_g.ROC,logit.model_d.ROC) #not different 
roc.test(logit.model_full.ROC,logit.model_e.ROC) #different 
roc.test(logit.model_full.ROC,logit.model_g.ROC) #different 

##USE on validation data: Bring in full model, model_g and model_d  
detach(records_train)
attach(records_validate)

logit.model_dv <- glm(over_50k ~ age + sex + race + occupation + education_num + marital_status)
summary(logit.model_dv)
logit.model_dv.ROC <- roc(logit.model_dv$y,logit.model_dv$fitted)
print(logit.model_dv.ROC) #AUC = 0.8748 

logit.model_gv <- glm(over_50k ~ age + sex + occupation + education_num + marital_status)
summary(logit.model_gv)
logit.model_gv.ROC <- roc(logit.model_gv$y,logit.model_gv$fitted)
print(logit.model_gv.ROC) #AUC = 0.8746 (increased from training data)

logit.model_fullv <- glm(over_50k ~ age + sex + race + occupation + education_num + marital_status + hours_week + capital_gain + capital_loss)
summary(logit.model_fullv)#races are insignificant 
logit.model_fullv.ROC <- roc(logit.model_fullv$y,logit.model_fullv$fitted)
print(logit.model_fullv.ROC) #AUC = 0.8906

#Take out race 
logit.model_fullv2 <- glm(over_50k ~ age + sex + occupation + education_num + marital_status + hours_week + capital_gain + capital_loss)
summary(logit.model_fullv2)#races are insignificant 
logit.model_fullv2.ROC <- roc(logit.model_fullv2$y,logit.model_fullv2$fitted)
print(logit.model_fullv2.ROC) #AUC = 0.8905


roc.test(logit.model_gv.ROC,logit.model_dv.ROC) #not different 
roc.test(logit.model_fullv.ROC,logit.model_fullv2.ROC) #not different
roc.test(logit.model_fullv2.ROC,logit.model_gv.ROC)#different
roc.test(logit.model_fullv2.ROC,logit.model_dv.ROC)#different

#model_fullv2 iis best statistically with the least parameters (but would still need to check assumptions on capital_gain and capital_loss variables  )

##USE original dataset: 
detach(records_validate)
attach(records)

logit.model_FINAL <- glm(over_50k ~ age + sex + occupation + education_num + marital_status + hours_week + capital_gain + capital_loss)
summary(logit.model_FINAL)#races are insignificant 
logit.model_FINAL.ROC <- roc(logit.model_FINAL$y,logit.model_FINAL$fitted)
print(logit.model_FINAL.ROC) #AUC = 0.8916

#Final odds ratios: 
OR <- exp(coef(logit.model_FINAL)[-1])
OR

##GRAPHIC for marriage effect on over_50k

counts <- table(over_50k,marital_status)
barplot(counts, main="Making over 50K by Mariage Types", cex.names = 0.5,
        , ylab = "Count of those with Salary over 50K",col=c("darkblue","red"),
        legend.text = c("No","Yes"), beside=TRUE,las=2)

labs <- paste(names(table(marital_status)), "cylinders")
text(cex=1, x=x-.25, y=-1.25, labs, xpd=TRUE, srt=45)
