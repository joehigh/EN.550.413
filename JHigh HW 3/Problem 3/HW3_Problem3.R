#Problem 3
#part(a)
library("foreign")
iq.data<-read.dta("kidiq.dta")
## part a
library("foreign")
iq.data <- read.dta("kidiq.dta")
r <- lm(kid_score ~ mom_iq + mom_work + mom_age,iq.data)
summary(r)

#part (b)
#The F-statistic: 36.84 on 3, dof=430. This suggests that none of the coefficients are linearly
#associated with the response, i.e. unable to predict the response. 
#Additionally, the low Multiple and Adjusted R-squared values suggest that there is a weak 
#correlation between the predictor variables mom.iq, mom_work, mom_age and the response.
#However, the marginal p-value for mom.iq, 2e-16, is statistically significant at all levels
#of significance.
#This hints at the multicollinearity phenonemnon.

# part (c)
r2 <- lm(kid_score ~ mom_iq,iq.data)
summary(r2)

#To test the hypothesis, we that coefficients for mom_work and mom_age are 0. 

anova(r2,r)

#The ANOVA Table gives a p-value = 0.387, so we accept the alternative hypothesis that at least 
#one of the predictor variables, mom_work or mom_age, from the first model has predicitive power,
#i.e. is statistically significant.

#If we assume a simple linear model between mom_age and kid_score we can make inferences 
#regarding the influence of mother's age on the childs test scores, i.e. how the predictor 
#variable mom_age influences the response variable, kid_score.

#The coefficient  for mom_age = 0.35. If we compare any two children whose mothers' age at birth
#differed by 1 year, it can be predicted that there will be an approximately 0.35 increase in 
#the test score.
#This suggests that children born from older mothers do better on these exams, so from this
#analysis, it is tempting to advise mothers to have children at very old ages. However, this is 
#obviously not a great recommendation because there are other factors to consider, such as fertility
#and birth defects at older ages. Therefore, this recommendation assumes that children born 
#from older mothers do better on tests, no matter how old. It has already been stated that this is
#not necessarily the case. 
#If we furthmore assume that mom_age is the (or one of) variable with predictive power 
#(one of the variables the hypothesis test picked up on), the recommendation is valid.

#part (d)
r3 <- lm(kid_score ~ .,iq.data)
summary(r3)

#With the predictor variable mom_hs added, the coefficients for mom_work and mom_age changed quite
#a bit, while mom_iq remained relatively stable. The F-statistic is around the same value, though 
#a bit smaller: 29.38 on 4, dof=429. Therefore, we have the same results as in part (a), where only
#mom_iq has a significant p-value. 
#Although mom_age coefficient decreased a bit, it remains positive, so our recommendation for 
#mothers to have children at an older age must remain with the same assumptions from part (b).

#part (e)
par(mfrow = c(3, 2))
plot(r3, which = c(1:6))
summary(influence.measures(r3))
#The Residual plot against the fitted values suggest a constant variance. There doesn't appear to be
#any systemic departure dependent on the fitted values. Thus, showing signs of constant variance.

#The Quantile-Quantile plot appears to be approximately linear with standardized residuals
#against the theoretical quantiles; however, the lower and upper tails of the QQ-Plot are 
#a little skewed suggesting partial drift from normality. Overall, though, the residuals are 
#approximately normally distributed.

#In the Residual plot, it appears that there are at least 6 outliers: 3 on each side of the line about y=0.
#about y=0. Because it appears that the error distribution is approximately normal, it is possible 
#that these potential outliers are in fact outliers of the data set. 

#The points 7, 87, 111, 152, 213, and 286, have asteriks next to them in the dffit column
#drawing attention to their high influence. Thus, these are potential influential points.
#The hat values, or leverage scores, in the hat column with higher values, those with a 
#leverage score of 0.03 likely have an impact on the data. Here, 0.03 is a high hat value since
#our sample size is relatively large, n = 434. 

#part (f)
r4 <- lm(formula = kid_score ~ . + mom_hs:mom_age, data = iq.data)
summary(r4)
#The resulting model from augmenting the data with an interaction between mom_age and mom_hs is
#kid_score = 48.32 - 28.78*mom_hs + 0.54*mom_iq + 0.13*mom_work - 0.99*mom_age +1.57*mom_hs:mom_age

#The model from part (d) was:
#kid_score = 20.82 + 5.56*mom_hs +0.56*mom_iq + 0.13*mom_work + 0.22*mom_age

#There are both differences and similarities between the models, which are actually very interesting.
#First, the intercepts for each model has changed quite significantly. While they are both positive,
#there is still a significant increased from model d to model f. 
#Even more interesting, adding the interaction between both mom_hs and mom_age into the model caused
#a significant decrease in their coefficients while the other variables remained exactly constant!
#This suggests that for children whose mother went to highschool, there is a positive relationship
#between mother's age at birth and the child's test score. The resulting model also suggests that
#there is a negative relationship between mother's age of child's birth and the child's test
#score. The addition of the interaction had no effect on the coefficient for mom_work and very 
#little effect on mom_iq which suggests that the influence of mom_work and mom_iq are independent
#of the interaction between mom_hs and mom_age.

anova(r3,r4)
#Our p-value from resulting from the F-statistic is 0.04856 where we are analyzing on the 
#alpha = 0.05 significance level. Because the p-value < alpha, we reject the null hypothesis 
#that the reduced model (in part (d)) is correct. In other words, we accept the alternative
#hypothesis that our full model, which includes the interaction variable mom_hs*mom_age, is 
#correct. In particular, at least one of our new coefficients is non-zero. Because there is only
#one additional coefficient, that coefficient is significant. Hence, the coefficient for 
#the interaction is statistically significant.