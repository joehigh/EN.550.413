#Problem 4
crab.data <- read.table("HW4P4data.txt", header=T)
satellites <- (crab.data$num.satellites>0)+0  #satellite variable = {0,1}; m_i = 1 for all i
color<-factor(crab.data$color)
spine<-factor(crab.data$spine)
width<-crab.data$width
weight<-crab.data$weight


#Beginning with weight variable and will build model from here.
g1<-glm(satellites ~ weight, family = binomial(link = logit), data = crab.data)
summary(g1)
#The p-value for the weight predictor suggest that it is statistically significant. Additionally,
#the coefficient for weight is positive, suggesting that the heavier the female crab, the higher
#the probability that satellites are in close proximity.

#The weight coefficient in g1 increases the odds of a female crab having a satellite or not by
#exp(0.0018151) = 1.0018. That is, it increases the odds by 0.18% for every unit increase in weight.
#The AIC value is rather high, there is much room for improvement. Let's try to improve the model.

g2<-glm(satellites ~ weight + width, family = binomial(link = logit), data = crab.data)
summary(g2)
#The AIC didn't improve much, and the addition of weight has resulted in weight becoming
#insigificant. This can be seen in the summary: the p-value for weight = 0.21445 which is 
#insignificant at almost all significance levels.This suggests that in the presence of width, 
#weight is not statistically significant, i.e. in the presence of weight, it doesn't have any 
#predicitive power. Moreover, this suggests that weight and width have are at least moderately 
#correlated. This too can be seeb in the correlation matrix below:
cor(width, weight)
#The correlation statistic for width and weight is 0.8868715! Strongly correlated.

#It may be possible to drop one of weight or width from the model such that the fit of the
#logistic regression model improves. When weight and width are fit together, weight is not
#significant while width is marginally significant with a p-value of 0.092 (say when alpha=0.1).
#Because the goal is to determine how satellite (unattached) males choose female crabs, we
#can assume that only one of width and weight is necessary. Indeed, it is safe to assume that
#in general, the wider the female, the heavier she is. Of course, this may not always be true, but
#it certainly is safe to assume, especially because the (positive) correlation between weight 
#and width has proven to be so strong (= 0.8869).

#To be sure we can fit width and compare the AIC values.
g3<-glm(satellites ~ width, family = binomial(link = logit), data = crab.data)
summary(g3)
#AIC for width is a bit smaller than that of the model only including weight. It is safe
#to assume that the addition of weight, while width is in the model, won't improve the model and
#will actually decrease it's worth.

#Unfortunately, because every m_i = 1 (number of samples for each level of predictor), we are
#unable to use the deviance as a measure of goodness-of-fit or to compare models, so we have to
#use alternative measures, such as the AIC criterion.

#Adding factor(color); (defined above as color)
g4<-glm(satellites ~ width + color, family = binomial(link = logit), data = crab.data)
summary(g4)
#The categorical variable color does not have much of an effect on the fit of the model. 
#The p-value for width remains roughly the same and the AIC value only decreased minimially. 
#However, color is marginally significant, we will keep it in for now.

#Adding in factor(spine);  (defined above as spine)
g5<-glm(satellites ~ width + color + spine, family = binomial(link = logit), data = crab.data)
summary(g5)
#Factor(spine) did not improve nor worsen the fit of the model in any way. 
#we can get rid of spine variable.

#Going to see if a threshold probability will improve my model.
p <- fitted(g4)
# Proportion in sample with satellites
mean(satellites)  # ~ 0.64

# Using 0.64 as cutoff to calculate sensitivity and specificity:
# pred.prob > 0.64 --> satellite = 1 and pred.prob <= 0.64 --> satellite = 0
pred.prob = as.numeric(p>0.64)
table(satellites, pred.prob)


#ROC method to validate threshold 
library(ROCR)
pred.ob <- prediction(p,satellites)  # arguments = fitted probabilities; observed 0/1
#Plotting ROC curve
plot(performance(pred.ob,"tpr","fpr"))
# Area under the ROC curve
performance(pred.ob,"auc")

#a strong ROC curve, so my final model is with a threshold probability of 0.64 with the 
#predictors width and color.