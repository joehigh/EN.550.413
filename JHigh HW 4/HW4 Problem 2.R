#Problem 2
ck<-c(20,60,100,140,180,220,260,300,340,380,420,460,500)
had.ha<-c(2,13,30,30,21,19,18,13,19,15,7,8,35)
no.ha<-c(88,26,8,5,0,1,1,1,0,0,0,0,0)
CK.data<-data.frame(ck, had.ha, no.ha)
CK<-CK.data$ck
Had.ha<-CK.data$had.ha
No.ha<-CK.data$no.ha

#Part 1
logitmod<-glm(cbind(Had.ha,No.ha) ~ CK, family = binomial(link = logit), data = CK.data)
summary(logitmod)
#Our estimated parameters from the logit model are:
#beta0.hat = -3.028360,  beta1.hat = 0.035104

par(mfrow=c(2,2))   
plot(logitmod) #Producing diagnostic plots (model checking plots)
#The Residual Plot of the deviance suggests that the current model is a poor fit for the data.

#Part 2
p.obs<-Had.ha/(Had.ha + No.ha) #observed probabilities of heart attack
plot(CK, p.obs)
p.fit<-fitted(logitmod) #Predicted/Fitted probabilities of having a heart attack
par(mfrow(c(1,1)))
plot(CK, p.obs, xlab = "CK Level", ylab = "Probability of Heart Attack", main = "logit link model 1")
lines(CK, p.fit)
#From the plot, it is apparent that the higher the concentration of creatinine kinase in a 
#given patient's blood stream, the higher the probability that the patient suffered a heart attack.
#Therefore, the coefficient for CK must be positive when plotted against the probability - which
#is what we see in the logit model results as well with beta1.hat > 0.
#The odds, with beta1.hat = 0.035104, can be interpreted as follows:
#For each unit increase in CK level, the odds of having a heart attack increases by a factor
#exp(0.035104). In other words, the odds of having a heart attack at some given CK[i] + 1 level
#is e(0.035104) times the odds of having a heart attack at level CK[i].
#odds(Had.ha | CK = CK[i] + 1) = exp(0.035104)*odds(Had.ha | CK = CK[i])

#Part 3
logitmod.2<-glm(cbind(Had.ha, No.ha) ~ CK + I(CK^2) + I(CK^3), family = binomial(link = logit),
                data = CK.data)
summary(logitmod.2)
#The first thing to notice is that the AIC value for this model is significantly smaller
#than the AIC value for the model in part 1. Thus, this model is already showing signs of being
#a better fit to the data.
par(mfrow=c(2,2))
plot(logitmod.2)
#The Residual Plot shows signs of a better fit to the data. There aren't any obvious signs
#of a systematic pattern about the y=0 line in the Residual plot.
p.fit2<-fitted(logitmod.2)
par(mfrow=c(1,1))
plot(CK, p.obs, xlab = "CK Level", ylab = "Probability of Heart Attack", main = "logit link model 2")
lines(CK, p.fit2)
#The curve is clearly a much better fit to the data. Most of the points fall directly onto the
#curve.
anova(logitmod, logitmod.2, test = "Chisq") #F-test comparing the two models.
#p-value = 8.038e-06 which is less then any significance level alpha. Thus, our 
#model with powers of CK fits the data better.

#Part 4
probitmod<-glm(cbind(Had.ha,No.ha) ~ CK, family = binomial(link = probit), data = CK.data)
summary(probitmod)
#We see that the coefficients are quite different for the probit model although they do have 
#the same sign. In general, the interpretation of the coefficient for CK relates to the
#probability of having a heart attack in a similar way.
p.fit3<-fitted(probitmod)
plot(CK, p.obs, xlab = "CK Level", ylab = "Probability of Having a Heart Attack", main = "probit link model")
lines(CK, p.fit3)
p.prob<-predict(probitmod, type = "response")
l.prob<-predict(logitmod, type = "response")
p.prob[10]  #CK = 380 is in the 10th row of the matrix.
l.prob[10]
#According to the probit model, the estimated probability of having a heart attack with a CK level
#of 380 (or a concentration of 380 - I'm not sure what units are being using) is 0.9999996
#while the logit model estimates it to be 0.9999667, which is very similar. The models agree
#strongly in this area of the data. However, this is may not be true for all levels of CK.

p.prob[12] #CK = 460 in 12th row of matrix
l.prob[12]
#There is an even strong relationship at CK = 460

#Another way to compute these values:
#For CK = 380
ilogit(coef(logitmod)[1] + coef(logitmod)[2]*380)  #Inverse Logit Function
pnorm(coef(probitmod)[1] + coef(probitmod)[2]*380) 
#For CK = 460
ilogit(coef(logitmod)[1] + coef(logitmod)[2]*460)
pnorm(coef(probitmod)[1] + coef(probitmod)[2]*460)

#same results as above.
