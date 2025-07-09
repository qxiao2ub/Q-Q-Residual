---
title: ""
output:
  pdf_document: default
  html_document:
    df_print: paged
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```



--------------------------------------------------------------------------------------

## Problem 1

Using the pressure data under datasets package, fit a model with O3 pressure as the response and temperature. To get the data set, copy and paste the r command: data(pressure,package="datasets"). (35 points)

a-) Check all the regression model assumptions visually. State your findings. (10 points)

```{r}
library(readr)
data(pressure,package="datasets")
```
```{r}
p1.reg = lm(pressure ~ temperature, data=pressure)
summary (p1.reg)
```
\textcolor{blue}{Both intercept and coefficient are significant, and R value is 0.5742 not bad for this linear regression. P-value is 0.000171 very small and shows significant too.}

```{r}
par(mfrow=c(2,2))
plot(p1.reg)
```
\textcolor{blue}{Ploting residuals vs. fitted the points are distributed on both sides of horizontal line Residual=0 and we do not see a pattern for the dots. By Q-Q residual we can see point 19 is outlier and for the rest poitns' residuals satisfy normal distribution. The scale-location is not bad only point 19 stands away from others. By the last figure the residuals vs. Leverage we can see only point 19 is outlier.}

b-) Use the Box–Cox method to determine the best transformation on the response and fit the model. (10 points)

```{r}
library(MASS)
par(mfrow=c(1,1))
```
```{r}
boxcox(p1.reg,lambda=seq(-5,5,by=.1))
```
```{r}
boxcox(p1.reg,lambda=seq(-1,1,by=.1))
```

```{r}
boxcox(p1.reg,lambda=seq(0.11,.13,by=.0001))
```
\textcolor{blue}{$\lambda=0.12$ gives the maximum log-likelihood. $95\%$ confidence interval for $\lambda$ is roughly between 0.115 and .0125, so using log(x) for transformation.}

```{r}
f1=lm(log(pressure)~temperature,data=pressure)
summary(f1)
```


c-) Compare the regression model in part "a-)" against the model built in part "b-)" and comment on the differences.(10 points)

\textcolor{blue}{We can see part (b) using Log function for pressure as Y, the R value is 0.9646 and p-value is 3.07e-12 which performs better than part (a) linear regression.}

d-) State the regression in part "b-)" in its original units.(5 points)

\textcolor{blue}{The regression in part (b) is: Log(pressure)=-6.068144 + 0.039792*temperature, where both intercept and coefficient are significant.}

## Problem 2

Commercial properties data set. A commercial real estate company evaluates vacancy rates, square footage, rental rates, and operating expenses for commercial properties in a large metropolitan area in order to provide clients with quantitative information upon which to make rental decisions. The data are taken from 81 suburban commercial properties include the age (X1), operating expenses and taxes (X2), vacancy rates
(X3), total square footage (X4), and rental rates (Y). (35 points) 

a-) Obtain the scatter plot matrix and the correlation matrix. Interpret these and state your principal findings.(5 points)

```{r}
p2data = read.csv("/cloud/project/Commercial Properties Data Set.csv")
```

```{r}
# Create a scatter plot matrix using the pairs() function 
pairs(p2data[,1:5], pch=19,main="Scatter Plot Matrix for p2 dataset")
```

\textcolor{blue}{Above is scatter plot matrix.}

```{r}
res =cor(p2data)
round(res,4)
```
```{r}
library(corrplot)
corrplot(res, type="upper",order="hclust",tl.col="red",tl.srt=30)
```

\textcolor{blue}{Above is the plot for correlation matrix.}


b-) Fit regression model for four predictor variables to the data. State the estimated regression function.(5 points)

```{r}
p2.reg = lm(p2data$Y ~ p2data$X1+p2data$X2+p2data$X3+p2data$X4, data = p2data)
summary(p2.reg)
```

\textcolor{blue}{So the fitted regression model for four predictor variables is: Y = 12.2 - 0.142X1 +0.282X2+0.6193X3+7.924e-06 X4 }

c-) Obtain the residuals and prepare a box plot of the residuals. Does the distribution appear to be fairly symmetrical? (5 points)

```{r}
library(auditor)
boxplot(p2.reg$residuals, main="Problem2 Residual Box Plot")
```

\textcolor{blue}{It is not perfect symmetrical but faily symmetrical.}

d-) Plot the residuals against Y and each predictor variable. Also prepare a normal probability plot. Analyze your plots and summarize your findings.(5 points)

```{r}
par(mfrow=c(2,2))
plot(p2.reg)
```

\textcolor{blue}{Looking at the above 1st residual vs. fitted plot, the residual plot shows no any departures from regression model because the red line does not show a pattern and it distributes on both sides of dashed horizontal residual=0 line. But a possible nonlinear relationship between the predicted outcome and variables which
may not captured by this linear regression model.}

```{r}
shapiro.test(p2.reg$residuals)
```


\textcolor{blue}{A normal probability plot of residuals are shown in the above 2nd Q-Q residual plot, and th Shapiro test p-value=0.4174>0.05 showing
not significant, so stays on $H_0$ it is noramlly distributed per the residuals which we can also tell from the Q-Q plot. we see the residuals satisfies a normal distribution.}

e-) Divide the 81 cases into two groups. placing the 40 cases with the smallest fitted values into group 1 and the remaining cases into group 2. Conduct the Brown-Forsythe test.(10 points)

```{r}
# Add the fitted values as a new column in the dataframe
p2data$fitted_value=p2.reg$fitted.values
```

```{r}
#ascending by fitted values
library('dplyr')
p2data_ascen = arrange (p2data, fitted_value)
```

```{r}
group1=head(p2data_ascen,40)
group2=tail(p2data_ascen,41)
```

```{r}
#have Brown-Forsythe Test
library(dplyr)
library(onewaytests)
bf.test(p2qe_2$Y ~ p2qe_2$Group, data = p2qe_2)
```

\textcolor{blue}{We can see P value is supper small as 7.282466e-13, so difference is statistically significant. That means the variance between the Y value are not equal.}

f-) Perform a Box-Cox transformation analysis and decide if Y needs to be transformed.(5 points)

```{r}
boxcox(p2.reg,lambda=seq(-5,5,by=.1))
```

```{r}
boxcox(p2.reg,lambda=seq(-.5,3,by=.1))
```

\textcolor{blue}{We see $\lambda=1.2$ very close to 1 gives the maximum log-likelihood. $95\%$ confidence interval for $\lambda$ is roughly between 0 and 2.4, so no need to transform Y.}

```{r}
f2 = lm(p2data$Y^{1.2} ~ p2data$X1+p2data$X2+p2data$X3+p2data$X4, data = p2data)
summary(f2)
```

## Problem 3 

Using the teengamb data in the Faraway r library (data(teengamb)), fit a model with gamble as the response and the other variables as predictors (30 points total).

```{r}
library(faraway)
data(teengamb)
```

```{r}
p3.reg = lm(gamble ~ sex+status+income+
              verbal, data=teengamb)
summary(p3.reg)
```

a-) Predict the data(teengamb) amount that a male with average (given these data) status, income and verbal score would gamble along with an appropriate 95% CI (5 points).

```{r}
# calculate status mean by male group
# sex = 0 is male
aggregate(teengamb$status,list(teengamb$sex), FUN=mean)
```
```{r}
# calculate income mean by male group
# sex = 0 is male
aggregate(teengamb$income,list(teengamb$sex), FUN=mean)
```

```{r}
# calculate verbal mean by male group
# sex = 0 is male
aggregate(teengamb$verbal,list(teengamb$sex), FUN=mean)
```

```{r}
new.data=data.frame(sex=c(0),status=c(52.00000),income=c(4.976071),verbal=c(6.821429))
predict(p3.reg, newdata = new.data, interval = 'confidence')
```

\textcolor{blue}{So the prediction value is 29.775 with 95 percentage CI between 21.12132 and 38.42868.}

b-) Repeat the prediction for a male with maximal values (for this data) of status, income and verbal score. Which CI is wider and why is this result expected? (5 points)

```{r}
# calculate status max by male group
# sex = 0 is male
aggregate(teengamb$status,list(teengamb$sex), FUN=max)
```

```{r}
# calculate income max by male group
# sex = 0 is male
aggregate(teengamb$income,list(teengamb$sex), FUN=max)
```

```{r}
# calculate verbal max by male group
# sex = 0 is male
aggregate(teengamb$verbal,list(teengamb$sex), FUN=max)
```

```{r}
new.data1=data.frame(sex=c(0),status=c(75),income=c(15),verbal=c(10))
predict(p3.reg, newdata = new.data1, interval = 'confidence')
```

\textcolor{blue}{Male with maximul values has a wider CI as the maximum values have higher variance leading to wider CI range.}

c-) Fit a model with sqrt (gamble) as the response but with the same predictors. Now predict the response and give a 95% prediction interval for the individual in "a-)". Take care to give your answer in the original units of the response (5 points).

```{r}
p3.reg1 = lm(sqrt(gamble)~sex+status+income+verbal, data=teengamb)
summary(p3.reg1)
```

```{r}
#following is repeat part (a) using sqrt(gamb) as response
new.data=data.frame(sex=c(0),status=c(52.00000),income=c(4.976071),verbal=c(6.821429))
predict(p3.reg1, newdata = new.data, interval = 'confidence')
```

```{r}
c(4.390678^2,3.595865 ^2,5.18549^2)
```

\textcolor{blue}{So the original response is prediction=19.27805 with 95 percent CI between 12.93025 and 26.88931}

d-) Repeat the prediction for the model in "c-)" for a female with status=20, income=1, verbal = 10. Comment on the credibility of the result (5 points).

```{r}
# female =1
new.data2=data.frame(sex=c(1),status=c(20),income=c(1),verbal=c(10))
predict(p3.reg1, newdata = new.data2, interval = 'confidence')
```

```{r}
c((-2.08648)^2,(-4.445937)^2,0.272978^2)
```

\textcolor{blue}{When square the answer and put back to the original units, the upper bound is smaller than the lower bound per CI, so it is not proper here.}

e-) Investigate the possibility of interactions between sex and the other predictors in the model you built in part "a-)". Interpret your final model. (10 points)

```{r}
res1 =cor(teengamb)
round(res1,4)
```

\textcolor{blue}{By correlation matrix, we can see females are less likely than males for teen gambling. The both status and gamble have stronger negative correlation to genders. }


