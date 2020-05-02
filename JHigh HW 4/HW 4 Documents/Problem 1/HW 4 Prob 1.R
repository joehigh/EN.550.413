#Problem 1
library(faraway)
library(leaps)
data("prostate")

#The following is the best subset of the predictor variables for predicting the response, lpsa,
#in the prostate data. The regsubsets function will be used for this (from the leaps package).
bs<-regsubsets(lpsa ~ ., data = prostate)
(rs <-summary(bs))
#This table shows the best subset of predictor variables for each corresponding number of
#predictor variables.
#In parts (a)-(d), we will use this best subset table for 4 different Variable Selection procedures.

#Part (a): Backward Elimination
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

#Part (b): AIC
#First, I will plot the AIC value against the number of predictor variables. Next, I will 
#implement stepwise regression via AIC criterion.

n<-nrow(prostate)

#[1] 97

plot(2:9, n*log(rs$rss/n) + 2*(2:9), main = "AIC against # of Predictors",
     xlab = "(p) Number of Predictors (including the intercept)", ylab = "AIC")
