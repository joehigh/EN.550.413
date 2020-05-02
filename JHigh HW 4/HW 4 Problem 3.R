#Problem 3
library("faraway")
data(pima)
pregnant<-pima.df$pregnant
glucose<-pima.df$glucose
diastolic<-pima.df$diastolic
triceps<-pima.df$triceps
insulin<-pima.df$insulin
bmi<-pima.df$bmi
diabetes<-pima.df$diabetes
age<-pima.df$age
test<-pima.df$test

#Part 1
summary(pima)
#Looking at the summary, we see that there are several predictor variables that take on the value 0.
#For the variables glucose, diastolic, triceps, insulin, and bmi, 0 is not a realistic value.
#Indeed, if a human being had a blood glucose concentration of 0, they wouldn't be alive. Your
#body absolutely requires glucose to produce ATP to do things like live. Similarly, for insulin.
#Insulin is what facilitates glucose uptake into every eukaryotic cell, and although a diabetes 
#test is testing for low insulin levels, it certainly won't be 0 for any living human being. 
#The diastolic variable must represent blood pressure. If someone had a blood pressure of 0,
#they wouldn't be alive to test for diabetes. A blood pressure value of 0 means that there is no
#blood flow and thus no transport of nutrients (such as glucose!) into cells.
#The tricep variable represents the tricep skinfold thickness (in millimeters, a measure of length).
#While it is true that length can take on the value 0, the tricep skinfold thickness cannot
#for a living person. As long as there is skin on a person's body, that value will be greater
#than 0. It is also a common (proportional) measure for subcutaneous fat, which also cannot be 0.
#bmi = body mass index = mass(or weight) to height ratio - bmi = 0 means that, that person 
#has a mass of 0 units, or weighs 0 units. Clearly, this is not possible. 
#These values are probably data entry errors. The values for these variables may not have 
#been known at the time measurements were taken, so they were errorneously recorded as 0.

#We will have to remove these observations from consideration if we want to continue working
#with all of the predictor variables.

#Part 2
pima.df<-pima[!(pima$glucose==0 | pima$diastolic==0 | pima$triceps==0 | pima$insulin==0 | pima$bmi==0),]
logit.full<-glm(test ~ ., family = binomial(link = logit), data = pima.df)
summary(logit.full)
#For each level i of predictor variable, the number of trials, m_i = 1 (i.e. "small"). Thus, we
#have a sparse distribution of data and therfore the model deviance will not be a good approximation
#for goodness-of-fit measure. We will have to use other means to measure the fit of the data,
#such as the Hosmer-Lemeshow test or a variant of this test.

#Part 3
summary(logit.full)
cor(pima.df)
#The marginal critical z value for the diastolic predictor is -.120, giving a p-value of 0.90446.
#Under the null hypothesis that the coefficient for diastolic = 0, we accept the null at all
#significance levels. That is, diastolic is not significant in the full logistic regression model.
#Moreover, referring to the correlation matrix for the variables in the model, we see that 
#corr(diastolic, test) = 0.1926733 (>0) and are therefore weakly correlated. Thus, we can 
#conclude that diastolic is not statistically significant.

#The parameters (coefficients; excluding the intercept term) measure the degree of 
#association between the probability of the event occuring and the value of its predictor 
#variable. Because this is a logistic regression model, the degree of association depends on
#the location of the predictor value. However, we can still infer whether or not the probability  
#of the event is increasing or decreasing depending on the sign of the coefficient.
#Hence, because the coefficient of diastolic is -0.001420 < 0, we can conclude that the higher
#a given woman's diastolic blood pressure, the lower the probability that given woman will 
#test positive for diabetes. Thus, the answer to the question is: According to the full model,
#women who test positive for diabetes do not, in general, have high diastolic blood pressure.

#The answers to these questions are seemingly contradictory because on one hand we have shown
#that the diastolic and test are positively correlated; on the otherhand, we also see that the
#coefficient for diastolic is negative, and hence a negative relationship between diastolic
#and test. Superficially, this appears to be a contradiction. However, it is most likely the 
#case that at least one of the other predictor variables in the model are highly correlated 
#(and hence statistically significant) with test and correlated with diastolic. It is clear 
#from the correlation matrix that at very large values of diastolic, it will start to have a
#positive effect on test. In terms of a Real Analysis course: There exists some large N such that
#when n > N, diastolic_n begins to have positive relationship on test (If diastolic were a seq).

#Part 4
logit.mod2<-glm(test ~ diastolic + bmi + age, family = binomial(link = logit), data = pima.df)
#Because the m_i are all small (all = 1), the deviance, nor the scaled the deviance, can be used
#to compare the two models since the partial deviance will not be chi-squared distributed.
#We can, however, use other means of model comparison. I will compare the AIC values for each to
#start.
summary(logit.mod2)
summary(logit.full)
#The AIC value for the full model = 362.02
#The AIC value for the reduced model = 430.52
# ==> AIC{diastolic, bmi, age} > AIC_full
#Thus, according to AIC criterion, the full model is a better fit than the reduced model with 
#{diastolic, bmi, age} predictors.

#Part 5
#diastolic variable is the diastolic blood pressure of a patient 
#bmi is the body mass index of a patient
#age is the patient's age

#There are 2 ways in which we can interpret the coefficients. We can interpret them by their
#effect on the probabilities and by their effect on the odds. 
#In particular, the coefficients for diastolic, bmi, and age are all positive. Thus, when
#any one of the predictors is increased by one unit, the probability of testing positive for
#diabetes increases but not necessarily by a proportional amount since it's a logistic function
#and not linear.
#We can also interpret these coefficients in terms of the odds of testing positive for diabetes.
#For each unit increase in diastolic level, the odds of testing positive increases by a factor
#of exp(0.003715) = 1.0037. In other words, the odds of testing positive increases by 0.37% for
#every unit increase in diastolic level.
#For each unit increase in bmi level, the odds of testing positive increases by a factor
#of exp(0.088388) = 1.09241. In other words, the odds of testing positive increases by 9.24% for
#every unit increase in bmi level.
#For each unit increase in age, the odds of testing positive increases by a factor
#of exp(0.074799) = 1.07767. In other words, the odds of testing positive increases by 7.48% for
#every unit increase in age.
#Note: The interpretation of the odds for each predictor variable assumes  that the other
#predictor variables are held constant.

#Part 6
#Installed pROC packages
diastolic.0<-diastolic-mean(diastolic)
age.0<-age-mean(age)
bmi.0<-bmi-mean(bmi)
fit<-glm(test ~ diastolic.0 + age.0 + bmi.0, family = binomial(link=logit), data = pima.df)
summary(fit)

true.positive<-sum((pima.df$fitted)>0.5)*(pima.df$test)
false.positive<-sum((pima.df$fitted>0.5)*(1-pima.df$test))
true.negative<-sum((pima.df$fitted<=0.5)*(1-pima.df$test))
false.negative<-sum((pima.df$fitted<=0.5)*(pima.df$test))
TPR<-true.positive/(true.positive + false.negative)
FPR<-false.positive/(true.negative+false.positive)
pima.df$fitted
fitted(pima.df)
