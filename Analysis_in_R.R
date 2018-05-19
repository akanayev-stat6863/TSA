# this is a comment
x <- c(0.1,1.1,2.3,4.1, 5.6, 8.3) # create a vector of 6 values, these were just made up
y <- c(8.7, 6.5, 3.1, 3.3, -1.1, -0.9) #create a second vector
plot(x,y,xlab = "this places a label on the x axis", ylab = "this labels the y axis")
title("this is a demo")
# either "<-" or "=" can be used for assignments, but "<-" makes more sense.
# The reader should be aware that "=" and "<-" are not always interchangeable in R


help(tiff)
mean(x) #produces the output: 3.583333 which is the mean of the x values.
sum(x) #produces the output: 21.5 as expected (21.5/6 is the mean).
var(y) #produces the output: 15.28667, the sample variance of y.

sum <- 0
for (j in 1:10) {sum=sum+j^2}

TRUE=FALSE
TRUE==FALSE
NA-NA
1/Inf
InfInf
1^Inf
y<-2; x<-3, y==x
0/0
1-TRUE
1_FALSE
TRUE FALSE
TRUE/FALSE
0^0

sum<-0
for (j in 1:20) {sum=sum+j}
fac<-1
for (j in 1:20) {fac=fac*j}
factorial(20)

log(15)+tan(pi*2)

n<-50;
x<-c(1:n);
#simulate n=50, random normal errors with mean 0 and standard deviation 3
error<-rnorm(n, 0, 3)
#the model is y= 3 + 0.5*x + random error (white noise in this case)
y<-3+0.5*x + error
plot(x,y,pch="*") #make a scatterplot of what was simulated
title("The line y = 3+ 0.5*x, n = 50")

fit <- lm(y ~ x)
anova(fit)
names(fit)
fit$coefficients

plot(x, y) #plot of original data
lines(x,fit$f) #add a line with the fitted values
predict<-fit$coeff[1] + fit$coeff[2]*x
plot(x,predict)

summary(fit)
help(summary)
summary.lm

mean(x)
var(x)
qqnorm(fit$residuals)

plot(fit$fitted, fit$residuals) #plot the residual versus the fitted values
abline(0,0) #add a line with slope 0 and mean 0
title("Residuals Plot")
