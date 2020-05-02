#Problem 3
#Part 1
library(aprean3)
data("dse13e")
summary(dse13e)
Y<-dse13e$m
X<-dse13e$t
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
layout(matrix(c(1,2), 1, 2, byrow = TRUE), widths = c(1,1), heights = c(1,1))
plot(X,Y, main = "Regression Plot")
curve(beta0.hat + beta1.hat*x, seq(min(X), max(Y), by=1), add=TRUE)
plot(X,std.residuals, ylab="Residuals", main="Residual Plot against X")
curve(0+0*x, seq(min(X), max(std.residuals)),add = TRUE, lty=2, col="red")
#From the residual plot, the residuals increase with time, implying non-constant variance.
#It appears that they are serially corellated - dependent on X (time).
#It appears that remedial measures can be taken to improve the fit of the data.
SSTO<-sum((Y-mean(Y))^2)
R2<-1-(SSE/SSTO)
#R2 = 0.6855, a moderately strong fit. 68.55% of the variance can be explained by the model.
#Let's validate this conclusion with a lack of fit test.
#H0: The regression function is linear, that there is no lack of fit.
uniq.X<-unique(X)
Y.mean<-numeric(length(uniq.X))
for(i in 1:length(uniq.X)) {Y.mean[i]<-mean(Y[X==uniq.X[i]])}
SSPE.partial<-numeric(length(uniq.X))
for(i in 1:length(uniq.X)) {SSPE.partial[i]<-sum((Y[X==uniq.X[i]] - Y.mean[i])^2)}
SSPE<-sum(SSPE.partial)
SSER<-sum(residuals^2)
k<-length(uniq.X)
df.F<-n-k
df.R<-n-2
F.stat<-((SSER-SSPE)*(df.F))/((df.R - df.F)*SSPE)
alpha<-0.05
F.critical<-qf(1-alpha, df.R - df.F, df.F)
F.pvalue<-pf(F.stat, df.R - df.F, df.F, lower.tail = FALSE)
#F.stat = 0.6238, F.critical = 2.0960, p-value = 0.8519
#==> F.stat < F.critical and p-value > alpha = 0.05
#[1] Thus, we can conclude the null that the data is linear.
#However, it is clear the linear association can be be improved/remediated.
#Let's try and improve it with a transformation

#Part 2
#Because the residual variance is relatively non-constant, we transform on Y.
#Because the linear relationship isn't very strong, we also transform on X.
Y.prime<-log(dse13e$m)
X.prime<-log(dse13e$t)
uxy<-sum((X.prime-mean(X.prime))*(Y.prime-mean(Y.prime)))
uxx<-sum((X.prime-mean(X.prime))^2)
b1.hat<-uxy/uxx
b0.hat<-mean(Y.prime)-b1.hat*mean(X.prime)
Yhat.prime<-b0.hat+b1.hat*X.prime
residuals.prime<-Y.prime-Yhat.prime
SSE.prime<-sum((residuals.prime)^2)
MSE.prime<-SSE.prime/(n-2)
std.residualsprime<-residuals.prime/sqrt(MSE.prime)
layout(matrix(c(1,2), 1, 2, byrow = TRUE), widths = c(1,1), heights = c(1,1))
plot(X.prime,Y.prime, main = "Transformed Regression Plot")
abline(b0.hat, b1.hat)
plot(X.prime, std.residualsprime, ylab="Std.Residuals", main="T.Residual Plot against X")
curve(0+0*x, seq(min(X.prime), max(std.residualsprime)),add = TRUE, lty=2, col="red")
#Because the original plot was curvelinear with a positive exponential shape, it made
#sense to transform with the natural logarithm.
#Furthermore, because there was departure from both a linear relationship and constant variance,
#both the predictor and response variable were transformed.
#From the transformed plots, the relationship has an improved linear relationship and
#the variance of the residuals show signs of constant variance.

#Now for a robust test on the linear relationship of the transformed data.
SSTO.prime<-sum((Y.prime-mean(Y.prime))^2)
R2.prime<-1-(SSE.prime/SSTO.prime)
#R2.prime = 0.8056, where R2 for the original fit was 0.6855.
#Thus, the regression model is now more appropriate for the transformed data.