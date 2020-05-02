library(SenSrivastava)
data(E8.12)
summary(E8.12)
Y<-E8.12$y
X<-E8.12$x
txy<-sum((X-mean(X))*(Y-mean(Y)))
txx<-sum((X-mean(X))^2)
beta1.hat<-txy/txx
beta0.hat<-mean(Y)-beta1.hat*mean(X)
Yhat<-beta0.hat+beta1.hat*X
plot(X,Y, main = "Regression Plot Before Deletion/Refit")
text(X, Y, labels = E8.12$Country, cex = 0.45, pos = 1, col = "blue")
curve(beta0.hat + beta1.hat*x, seq(min(X), max(Y), by=1), add=TRUE)
#From the plot, it appears that the United States data point does not agree with the rest of the data.
#The presence of the United States point appears to influence the regression line by decreasing
#the steepness of where it appears the true line should be when assuming no other data point is an outlier.
#If we assume that the United States is not an outlier, it appears that the Great Britain data point
#influences the regression line's slope by increasing it in the positive direction.

#Now we delete the United States observation and make an inference on its influence.
data.deleteUS<-E8.12[-4,]
Y.deleteUS<-data.deleteUS$y
X.deleteUS<-data.deleteUS$x
sxy<-sum((X.deleteUS-mean(X.deleteUS))*(Y.deleteUS-mean(Y.deleteUS)))
sxx<-sum((X.deleteUS-mean(X.deleteUS))^2)
b1.hat<-sxy/sxx
b0.hat<-mean(Y.deleteUS)-b1.hat*mean(X.deleteUS)
Yhat.deleteUS<-b0.hat+b1.hat*X.deleteUS
plot(X.deleteUS,Y.deleteUS, main = "Refitted Plot After Deleting United States")
text(X.deleteUS, Y.deleteUS, labels = data.deleteUS$Country, cex = 0.45, pos = 1, col = "blue")
curve(b0.hat + b1.hat*(x), seq(min(X.deleteUS), max(Y.deleteUS), by=1), add=TRUE)