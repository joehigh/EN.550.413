#Problem 4, Part (a)
df <- read.table("26929_lordex.txt", sep = "", header = T)
## Now make a new column called age.binarize
df$age.binarize <- (df$AGEMOS <= 78)
df$age.binarize <- factor(df$age.binarize, levels = c(T, F), labels = c("younger", "older"))
## Now make a new column called score.difference
df$score.difference <- df$Final - df$Initial
score.difference<-df$score.difference
age.binarize<-df$age.binarize
age<-ifelse(df$AGEMOS >=79, 1, 0) #age = 1 if "older" and 0 if "younger"
txx<-sum((age-mean(age))^2)
txy<-sum((age-mean(age))*score.difference)
beta1.hat<-txy/txx
beta0.hat<-mean(score.difference)-(beta1.hat*mean(age))
n<-length(df$AGEMOS)
dof<-n-2
residuals<-(score.difference-beta0.hat-(beta1.hat*age))
MSE<-(1/dof)*sum(residuals^2)
s2.beta1hat<-(MSE)/txx
s.beta1hat<-(s2.beta1hat)^(1/2)
teststatistic<-beta1.hat/s.beta1hat
alpha<-0.05
qt(c(alpha/2, 1-(alpha/2)), df=dof)
#Our t-teststatistic falls outside of the confidence interval. Thus, we reject
#null hypothesis H0: beta1.hat = 0. Therefore, there is a linear association
#so our beta1.hat value is statistically significant.
#The estimated slope coefficient, beta1.hat = -0.253862212943633
#The slope estimate implies that the score difference for older children is smaller than that of younger children.
#Thus, the estimate suggests that the children's memory improves with age.

#Part (b)
final<-df$Final
initial<-df$Initial
txf<-sum((final-mean(final))*(age-mean(age)))
tii<-sum((initial-mean(initial))^2)
tfi<-sum((final-mean(final))*(initial-mean(initial)))
txi<-sum((age-mean(age))*(initial-mean(initial)))
b1.hat<-(((txf*tii)-(tfi*txi))/((txx*tii)-(txi)^2))
b2.hat<-(((txx*tfi)-(txi*txf))/((txx*tii)-(txi*txi)))
b0.hat<-mean(final)-(b1.hat*mean(initial))-(b2.hat*mean(age))
