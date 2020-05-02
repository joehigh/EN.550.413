#Problem 1
library(faraway)
data("cars")
summary(cars)
Y<-cars$dist
X<-cars$speed
txy<-sum((X-mean(X))*(Y-mean(Y)))
txx<-sum((X-mean(X))^2)
beta1.hat<-txy/txx
beta0.hat<-mean(Y)-beta1.hat*mean(X)
Yhat<-beta0.hat+beta1.hat*X
residuals<-Y-Yhat
n<-length(X)
dof<-n-2
SSE<-sum((residuals)^2)
MSE<-(1/dof)*SSE
std.residuals<-residuals/sqrt(MSE)
layout(matrix(c(1,0,2,3), 2, 2, byrow = TRUE), widths = c(1,1), heights = c(1,1))
plot(X,Y, main="Fitted Regression Plot")
curve(beta0.hat + beta1.hat*x, seq(min(X), max(Y), by=1), add=TRUE)
plot(X,std.residuals, ylab="Residuals", main="Residual Plot against X")
curve(0+0*x, seq(min(X), max(std.residuals)),add = TRUE, lty=2, col="red")
theoretical<-qnorm((1:length(residuals)/(length(residuals)+1)))
plot(sort(theoretical),sort(residuals), xlab = "Theoretical", ylab = "Residual", main = "Q-Q Plot")
#The Residual plot suggests a nonconstant variance as does the Q-Q Plot. 
#As x increases, the modulus of the residual values increase.
#It is clear from the QQ-Plot that there is clear departure from normality. 
#That is to say that the error terms are not normally distributed.
#Now we implement a more robust method to test of constancy of variance
#Brown-Forsythe/modified Levene test (from lawstat package):
levene.test(std.residuals, group = (Y >= 45), location= "median")
#[1] t.BF = 5.918 and a p-value = 0.01876
alpha<-0.05
tstat<-qt(1-(alpha/2),df=dof)
#[1] t.BF = 5.918 > tstat = 2.010635 and p-value=0.01876 < alpha=0.05.
#Thus, we reject the null Hypothesis at 5% significance level. 
#i.e. The variance of the error terms are non-constant and depend on x.

#Part 2) Now to test for Normality
#Null Hypothesis, H0: The error terms are normally distributed.
#Chosen alpha = 0.05
library(nortest)
shapiro.test(residuals)
#Shapiro-Wilk Normality Test Data:
#p-value = 0.02152, test statistics W = 0.94509
#at the 5% significance level, we reject the null hypothesis
#That is, the error terms are not normally distributed.

#Part 3) Outliers
