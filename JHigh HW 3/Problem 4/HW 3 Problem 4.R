#Problem 4
View(kuiper)
price <- kuiper$Price
mileage <- kuiper$Mileage
make <- kuiper$Make
type <- kuiper$Type
cylinder <- kuiper$Cylinder
liter <- kuiper$Liter
model <- kuiper$Model
trim <- kuiper$Trim
doors <- kuiper$Doors
cruise <- kuiper$Cruise
sound <- kuiper$Sound
leather <- kuiper$Leather

levels(make) <- 1:6
make
levels(model) <- 1:32
model
levels(type) <- 1:5
type
levels(trim) <- 1:47
trim

#Regression Model with price of vehicle as the response (naturally) and mileage, make, cylinder,
#leather interior, and sound. 
#It is assumed that there will be a strong relationship between some of these, though I do think 
#variables such as sound and leather will seem influential considering not everyone looks for 
#these properties in a vehicle.
r <- lm(formula = price ~ mileage + make + cylinder + leather + sound, data = kuiper)
summary(r)
#The very large F-statistic value of 440.1 on 9, dof = 794, with a p-value of 2.2e-16 suggests
#that this model the model is very strong, and hence significant. However, it only tells us that 
#at least one of the predictor variables is significant, though this relationship would have to 
#to be considerably strong to yield such a result. Therefore, it is likely more than one variable
#yielding the strength of this model. It is significant at all alpha levels of significance.

#Removing sound and leather from the regression model to determine the resulting model:
r2 <- lm(formula = price ~ mileage + make + cylinder, data = kuiper)
summary(r2)
#The resulting test statistic is even larger where the p-value is relatively the same. I assume
#that this p-value is a limiting value for very large F-statistic values. This model is even
#stronger than the original just as assumed before the model was run. This suggests that either
#sound or leather had a negative impact on the linear relationship between the nonzero coefficients.
#I still assume that there is more than one variable that is nonzero.

#We will now determine, definitively, if one of sound or leather impacted the relationship, or 
#their coefficients are nonzero. We will use anova to do so.


#Analysis of Variance
anova(r2,r)
#The p-value = 0.1098. Thus, at the alpha = 0.05 significance level, we accept the null
#hypothesis, that the reduced model is correct. That is to say that the model without sound and
#leather is the stronger model (as assumed). It further states that we cannot reject the assumption
#that both coefficients for sound and leather were zero. Thus, sound and leather aren't particularly
#strong factors determining the price of a vehicle on average.

#Let's try to find which variable has the strong predictive power

#Deleting cylinder from the model
r3 <- lm(formula = price ~ mileage + make, data = kuiper)
summary(r3)
#The F-statistic decreased in value, suggesting that cylinder was had significant predictive
#power or at least had significant predictive power when paired with other mileage and/or make.

#Simple Linear Regression with Price vs. mileage
r4<-lm(formula = price ~ mileage, data = kuiper)
summary(r4)
#The F-statistic decreased significantly suggesting that mileage of a vehicle on it's own does 
#not have strong predictive power of the response variable, price of the vehicle. 

#This suggests that when mileage of the vehicle, make of the vehicle, and cylinder are 
#considered together simultaneously, we obtain a strong model with a very strong relationship
#between them.

#Let's run a diagnostic on our r2 regression since it was our strongest model.
par(mfrow = c(3, 2))
plot(r2, which = c(1:6))
summary(influence.measures(r2))
#There are several influential points in the dffit column which isn't too suprising considering
#the market for cars. The leverage points are relatively high considering there are 804 data points.
#Those points with leverage of 0.02 or higher are likely to influence the data.
#The QQ-Plot suggest that there is a sharp dispersion of normality for the error terms.
#Moreover, the Residual against fitted values plot shows that the variance of the error
#terms are far from constant (i.e. nonconstant). It actually seems that they are serially
#correlated. 
#These results are surprising, but not completely suprising considering the assumption of 
#strong influence from many variables in the automobile industry.