#Problem 2
#Part a
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
#slope estimate (beta 1 hat) = 0.229115337755516
#intercept estimate (beta 0 hat) = 65.7488570215489
#Fitted Regression line: Yhat = 65.7489 + 0.2291*X
#From the plot, it appears that the United States data point does not agree with the rest of the data.
#The presence of the United States point appears to influence the regression line by decreasing
#the steepness of where it appears the true line should be when assuming no other data point is an outlier.
#If we assume that the United States is not an outlier, it appears that the Great Britain data point
#influences the regression line's slope by increasing it in the positive direction.
#However, if we only remove the United States data point, the fit appears to improve greatly.

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
#slope estimate (beta 1 hat) = 0.357668028221314
#intercept estimate (beta 0 hat) = 13.5534348310435
#Fitted Regression line: Yhat = 13.5534 + 0.3567*X
#[1] The slope increases by a factor of 0.357668/0.229115 = 1.561090, and the intercept decreases by 
#approximately 52 units.
#Thus, the United States data point had a significant influence on the regression model.
#In particular, it shifted the regression line upward in the positive direction.

#Now we delete the Great Britain observation and make an inference on its influence.
data.deleteGB<-E8.12[-6,]
Y.deleteGB<-data.deleteGB$y
X.deleteGB<-data.deleteGB$x
uxy<-sum((X.deleteGB-mean(X.deleteGB))*(Y.deleteGB-mean(Y.deleteGB)))
uxx<-sum((X.deleteGB-mean(X.deleteGB))^2)
b1.hat2<-uxy/uxx
b0.hat2<-mean(Y.deleteGB)-b1.hat2*mean(X.deleteGB)
Yhat.deleteGB<-b0.hat2+b1.hat2*X.deleteGB
plot(X.deleteGB,Y.deleteGB, main = "Refitted Plot After Deleting Great Britain")
text(X.deleteGB, Y.deleteGB, labels = data.deleteGB$Country, cex = 0.45, pos = 1, col = "blue")
curve(b0.hat2 + b1.hat2*(x), seq(min(X.deleteGB), max(Y.deleteGB), by=1), add=TRUE)
#slope estimate (beta 1 hat) = 0.158802220224097
#intercept estimate (beta 0 hat) = 90.7999766565224
#Fitted Regression line: Yhat = 90.8000 + 0.1588*X
#[1] The slope decreases by a factor of 0.229115/0.158802 = 1.442772 and the intercept increases by
#approximately 25 units.
#Thus, the Great Britain data point had a significant influence on the regression model.
#In particular, it shifted the regression line down, decreasing in steepness.

#Part b
#[1] First and foremost, this relationship will undoubtedly have outliers considering multiple variables
#influence male dealth. Lung cancer is of course one of the leading causes, but so are car
#accidents (as seen in Problem 4), other types of cancer, other diseases, murder/suicides, etc.
#Without considering other predictor variables, the model will assuredly have outliers (as seen in 4c)

#[1] However, if we want to only consider the lung cancer per capita cigarette consumption predictor
#variable, it may be possible to transform the variables to reduce the number of outliers.
#This is also seen in problem 4 as well as problem 3.

#[1] Another plausible explanation for the outliers, or influential points, in the data is that
#the data only considers the relationship between male deaths and cigarette consumption per capita.
#"Per capita" isn't limited to any gender. It is likely the case that some countries have a higher
#percentage of women per capita that smoke cigarettes than do men, whereas other countries may have
#a higher percentage of men per capita that smoke cigarettes. This could very well lead to outlier
#data points. For instance, the United States' predictor variable may consist of a large percentage
#of women. If these women were removed from consideration, the United States data point may possibly
#translate to the left with the other data points while maintaining the same Y value, thus eliminating
#the United States as an outlier/influential point.
#A similar argument can be made for the Great Britain data point.

#Plotting all graphs together while keeping deleted data points in each plot:
layout(matrix(c(1,0,2,3), 2, 2, byrow = TRUE), widths = c(1,1), heights = c(1,1))
plot(X,Y, main = "Regression Plot Before Deletion/Refit")
text(X, Y, labels = E8.12$Country, cex = 0.45, pos = 1, col = "blue")
curve(beta0.hat + beta1.hat*x, seq(min(X), max(Y), by=1), add=TRUE)
plot(X,Y, main = "Refitted Plot After Deleting United States")
text(X, Y, labels = E8.12$Country, cex = 0.45, pos = 1, col = "blue")
curve(b0.hat + b1.hat*(x), seq(min(X.deleteUS), max(Y.deleteUS), by=1), add=TRUE)
plot(X,Y, main = "Refitted Plot After Deleting Great Britain")
text(X, Y, labels = E8.12$Country, cex = 0.45, pos = 1, col = "blue")
curve(b0.hat2 + b1.hat2*(x), seq(min(X.deleteGB), max(Y.deleteGB), by=1), add=TRUE)