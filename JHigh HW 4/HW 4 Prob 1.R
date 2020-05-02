#Problem 1
library(faraway)
library(leaps)
library(MASS)
data("prostate")

#Part 1
#The following is the best subset of the predictor variables for predicting the response, lpsa,
#in the prostate data. The regsubsets function will be used for this (from the leaps package).
bs<-regsubsets(lpsa ~ ., data = prostate)
(rs <-summary(bs))
#This table shows the best subset of predictor variables for each corresponding number of
#predictor variables.
#In parts (a)-(d), we will use this best subset table for 4 different Variable Selection procedures.

#(a): Backward Elimination
#Set our threshold alpha, alpha.out = 0.1
g<-lm(formula = lpsa ~ ., data = prostate)
summary(g)
#Since the p-value for gleason is the greatest p-value greater than alpha.out = 0.1, we remove
#gleason predictor variable first.

g2<-update(g, . ~ . - gleason)
summary(g2)
#the marginal p-value of the lcp predictor variable is the greatest p-value greater than alpha.out.
#Thus, we remove the lcp variable:

g3<-update(g2, . ~ . - lcp)
summary(g3)
#We now remove pgg45 variable:

g4<-update(g3, . ~ . - pgg45)
summary(g4)
#Remove age predictor variable next since its marginal p-value > alpha.out = 0.1

g5<-update(g4, . ~ . - age)
summary(g5)
#The marginal p-value for lbph > alpha.out = 0.1. We remove lbph predictor variable:

g6<-update(g5, . ~ . -lbph)
summary(g6)
#The remaining predictor variables have marginal p-values that are significantly less than 
#the threshold alpha.out = 0.1, so no other predictor variables will be removed. 
#By the Backward Elimination criterion, with threshold alpha.out = 0.1, our best subset of 
#predictor variables is {lcavol, lweight, svi} and the fitted model is
#lpsa = -0.26809 + 0.55164*lcavol + 0.50854*lweight + 0.66616*svi

#(b): AIC
#First, I will plot the AIC value against the number of predictor variables. Next, I will 
#implement stepwise regression via AIC criterion.

n<-nrow(prostate)
AIC.p<-n*log(rs$rss/n) + 2*(2:9)

plot(2:9, AIC.p , main = "AIC against # of Predictors",
     xlab = "(p) Number of Predictors (including the intercept)", ylab = "AIC")
#The minimum AIC value appears to be at p = 5 or p = 6 (or 4 or 5 predictors since intercept is
#is included). To be sure, we use which.min function to locate where the minimum occurs.

min(AIC.p)
# minimum AIC value = -61.37439
which.min(AIC.p)
# p = 5

#The minimum occurs at p = 5. Because it includes the intercept term, we attain the minimum AIC.p
#value with 4 predictor variables. According to the AIC criterion, 4 predictor variables is optimal.

#If we refer to our Best Subsets data table (bs) above, we see that the best subset of 4 predictor 
#variables is {lcavol, lweight, lbph, svi}.

#Stepwise with AIC, starting from full model:
g.full<-lm(lpsa ~ ., data = prostate)
g7<-step(g.full, direction = "both")
#Starting from the full model, the AIC value was -58.32. The predictors gleason, pgg45, and lcp
#were then removed stepwise, respectively, according to the AIC value that would result when
#each is removed. According to the stepwise method with AIC, the best subset for the model is 
#{lcavol, lweight, lbph, svi, age}. This model is similar to the result using AIC criterion, but
#the predictor variable age is included here. 

#(c): Adjusted R^2
plot(2:9, rs$adjr2, main = "Adjusted R^2 against # of predictors", 
     xlab = "Number of Predictors (including intercept)", ylab = "Adjusted R^2", type = "l")
#The Adjusted R^2 values increase on the interval [2,8], and decreases from (8,9]. Therefore, the
#maximum Adjusted R^2 value occurs at p = 8. Thus, according to the adjusted R^2 criterion, the
#optimal number of predictor variables for the model is 7 (since the intercept term is included).

#If we refer to the Best Subsets table (bs) above, the best subset of 7 predictor variables is
#{lcavol, lweight, age, lbph, svi, lcp, pgg45}. 

#(d): Mallow's Cp
#Mallow's Cp value for this data was made available from the regsubsets function above.
#We want to find the model where Cp is approximately equal to p (number of predictors).
#We will plot the Mallow's Cp values against p (number of predictors) and fit a line to the plot.
#The value of p corresponding to the data point with the smallest distance from the line should
#be the optimal number of predictor variables according to Mallow's Cp criterion.
plot(2:9, rs$cp, main = "Mallow's Cp against # of predictors", 
     xlab = "Number of predictors (including intercept)", ylab = "Mallow's Cp")
abline(0,1)
#Because p = 9 is the full model, it will always be on the line since Cp = SSEp/MSE + 2p - n.
#Variable selection would be redundant if the full model was the optimal model, so we will
#assume that p = 9 is not the p we seek. The next closest data point occurs at p = 6.
#Thus, the number of predictors in the best subset is 5, according to Mallow's Cp criterion.

#Referring to our the Best Subset (bs) data table, the best subset with 5 predictor variables is
#{lcavol, lweight, age, lbph, svi}.


#All graphics/plots for Part 1:
layout(matrix(c(1,0,2,3), 2,2, byrow = TRUE), widths = c(1,1), heights = c(1,1))
plot(2:9, AIC.p , main = "AIC against # of Predictors",
     xlab = "(p) Number of Predictors (including the intercept)", ylab = "AIC")
plot(2:9, rs$adjr2, main = "Adjusted R^2 against # of predictors", 
     xlab = "Number of Predictors (including intercept)", ylab = "Adjusted R^2", type = "l")
plot(2:9, rs$cp, main = "Mallow's Cp against # of predictors", 
     xlab = "Number of predictors (including intercept)", ylab = "Mallow's Cp")
abline(0,1)

#Part 2
library(MASS)
set.seed(1) #setting a random seed for reproducibility
partition<-sample(2, nrow(prostate), replace = TRUE, prob = c(0.5, 0.5))
train.data<-prostate[partition==1,]
test.data<-prostate[partition==2,]
train<-scale(train.data)
test<-scale(test.data)
g.ridge<-lm.ridge(train[, 9] ~ train[, -9], lambda = c(seq(0, 500, 0.01)))
par(mfrow=c(1,1))
matplot(g.ridge$lambda, t(g.ridge$coef), type = "l", lty = 1, xlab = expression(lambda),
        ylab = expression(hat(beta)))
select(g.ridge)
#Using generalized cross-validation (GCV), a suitable lambda is 9.52. The coefficients do
#appear to begin to stabilize and converge to Beta = 0 around that value. 

coef(g.ridge)[953,]

train.coeff<-coef(g.ridge)[953,] #GCV lambda = 9.52, variance = 0.01.

y.train<-train[,9]
y.test<-test[,9]
x.train<-train[,-9]
x.test<-test[,-9]
k<-nrow(test.data)

g.ridge2<-lm.ridge(y.train ~ x.train, lambda = 9.52)
b0.ridge<-mean(train.data[,9])  #Since intercept excluded in ridge regression
pred.value<-scale(test[,1:8],center = FALSE, scale = g.ridge2$scales)%*%g.ridge2$coef
fit.values<-x.test%*%coef(g.ridge2)[2:9]
for(i in 1:k) {SSE = sum(fit.values[i] - (test[i,9] + b0.ridge))^2}
MSE = SSE/k
  
#The problem doesn't ask for this, but I'm going to compare the MSE with the MSE for the OLS model.
g.ols<-lm(y.train ~ x.train)
fit.ols<-x.test%*%coef(g.ols)[2:9]
fit.ols2<-coef(g.ols)[1] + (coef(g.ols)[2])*x.test[,1] + (coef(g.ols)[3])*x.test[,2] +
  (coef(g.ols)[4])*x.test[,3] + (coef(g.ols)[5])*x.test[,4] + (coef(g.ols)[6])*x.test[,5] +
  (coef(g.ols)[7])*x.test[,6] + (coef(g.ols)[8])*x.test[,7] + (coef(g.ols)[9])*x.test[,8]
coeff.ols<-coef(g.ols)
b0.hat<-coeff.ols[1] 
SSE.ols<-sum((y.test - fit.ols - b0.hat)^2)
MSE.ols<-SSE.ols/k
#MSE.ols < MSE.ridge; thus, our choice of lamba is sufficient in that the model with bias fits
#the data better than the unbiased OLS mode.

#An alternative way to do Part 2:
#Using glmnet package for this alternative method.
library(glmnet)
x<-model.matrix(lpsa ~., data = prostate)[,-1]
y<-prostate$lpsa
#We first fit the ridge regression model:
library(glmnet)
grid=10^seq(10,-2,length=100)
ridge.mod<-glmnet(x,y,alpha=0,lambda=grid) #alpha=0 calls for Ridge Regression
set.seed(1) #setting a random seed for reproducibility
trainx<-sample(1:nrow(x), nrow(x)/2)
testx<-(-trainx)
y.testx<-y[testx]
#Want to find a suitable lambda using cross validation:
set.seed(1)
cv<-cv.glmnet(x[trainx ,],y[trainx],alpha=0)
plot(cv)
bestlam<-min(cv$lambda)
bestlam  #We therefore see that the best lambda that results when using the smallest cross
#validation error is 0.078243837
#We then fit a ridge regression model on the training set, and subsequently evaluate the MSE on
#the test set, using the best lambda value (above).
ridge.mod<-glmnet(x[trainx,],y[trainx],alpha=0,lambda=grid, thresh = 1e-12)
ridge.pred<-predict(ridge.mod, s=bestlam, newx=x[testx,]) 
mean((ridge.pred - y.testx)^2)
#MSE on the test set, using the ridge regression estimates is 0.8678651

#If we were to use a lambda = 0, it would be equivalent to fitting the OLS model.
#On the otherhand, if we fit for very large values of lambda (--> infinity), it is equivalent 
#to fitting the null model; that is, a model with only an intercept term.
#Let's do this to compare the mean squared error in each case, so that we may validate our
#choice of lambda:
ridge.pred.2<-predict(ridge.mod, s=0, newx=x[testx,]) #lambda = 0: OLS model
mean((ridge.pred.2 - y.testx)^2)  
#MSE for null model = 0.8722847, which is slightly greater than our MSE with best lambda value.
#There always exists a value lambda for which the MSE of the ridge model is less than the MSE of 
#the OLS model. This shows that our value of lambda satisfies this.

ridge.pred.3<-predict(ridge.mod, s=1e10, newx=x[testx,]) #very large lambda --> null model
mean((ridge.pred.3 - y.testx)^2)  
#MSE for the model approaching the null model is greater than the MSE for the ridge model.

#Thus, our choice of lambda is sufficient.