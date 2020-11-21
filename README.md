                         #EDUCATIONAL ATTAINMENT AND NEW AGE DIGITAL ENTERTAINMENT :
        ARE VIDEO GAMES A DETERMINING FACTOR IN THE ACADEMIC ACHIEVEMENT OF HIGH SCHOOL STUDENTS?#

ANDRADE SILVA, Luis Eduardo 
Master Thesis
Master in Social Sciences, UC3M


This document includes the complete rmarkdown (raw code) from the paper «EDUCATIONAL ATTAINMENT AND NEW AGE DIGITAL ENTERTAINMENT :
ARE VIDEO GAMES A DETERMINING FACTOR IN THE ACADEMIC ACHIEVEMENT OF HIGH SCHOOL STUDENTS?» 


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
setwd("/Users/Luisinho/Desktop/Github.GP1")
```
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
setwd("/Users/Luisinho/Desktop/Github.GP1")
```

*************
# 0. Data and previous:

```{r, echo=FALSE}
rm(list=ls())  # Clear workspace
data <- readRDS(file = "data.Rds")
data<-data[complete.cases(data),]
attach(data)
```

Libraries:

```{r}
library(usethis)
library(devtools)
library(carData)
library(car)
library(lmtest)
library(zoo)
library(mctest)
library(formattable)
```

##. Pre-analysis of the data and OLS.

What is the correlation between IND variables? 

```{r}
cor(income,gender, use = "complete.obs") 
cor(income,gpa, use = "complete.obs") 
cor(income,age, use = "complete.obs") 
cor(income,video, use = "complete.obs") 
cor(income,religion, use = "complete.obs")
cor(income,leisure, use = "complete.obs") 

cor(gpa,age, use = "complete.obs") 
cor(gpa,video, use = "complete.obs")
cor(gpa,religion, use = "complete.obs") 
cor(gpa,gender, use = "complete.obs") 
cor(gpa,leisure, use = "complete.obs") 
cor(video,religion, use = "complete.obs") 
cor(video,leisure, use = "complete.obs") 
cor(religion,leisure, use = "complete.obs") 
```

The correlation between every pair of variables doesn't seem very high.  
 
### Are these variables jointly significant? 

I tried to test if the variables I have established in the theoretical part are jointly significant. As I explained, it is expected that the main independent theoretical variable, HPW videogames, is correlated (negatively, at least as is expected) with leisureing. For that, is better to use f-test. So I start designing the hypothesis. An F-test will allow us to test the null that all of the coefficients are equal zero; that is that these variables jointly significance. So:

H0: video-leisure=0.
H1: video-leisure=/=0.

Knowing this, I can conduct the test.

```{r}
#library(MASS)
OLS<-lm(formula = hsgpa ~ age2 + gender + (gpa^2) + religion + video + 
    year + leisure + race + income, data = data)

lht(OLS, c("video = 0", "leisure = 0", "income=0"), white.adjust = "hc1")
```

Knowing the null hypothesis that is stated, and that the F-stat for the test is 31.4, with the p-value of (<2.2e-15). Df = 3 tells me the number of restrictions being tested. Obviously with such a low p-value I can reject the null hypothesis.

### Variance-covariance matrix.

The var-cov matrix of my regression.

```{r}
library(sandwich)
var_cov<-vcovHC(OLS, type = "HC4")
coeftest(OLS, df = Inf, var_cov)
```

Now vcovHC() function creates a variance “ covariance matrix. The first parameter is the regression model I have made. Keep omega as NULL. The type parameter refers to what measure of heteroskedasticity is being used. There are 5 types from HC0 to HC4. HC4 is the latest and I use that. You can read more about it in the sandwich package.
This is my variance- covariance matrix. The independent variables are listed both in column and rows. The diagonal elements show the variance of each variable with itself. The diagonal values should have been constant, but since they vary, we can start to detect the presence of heteroskedasticity.

### a. Multicolinearity check.

```{r}
par(mfrow=c(2,2))
plot(OLS)

X<-data[,1:12]
#install.packages("GGally")
library(GGally)
# ggpairs(X) #funciona, pero tarde tres horas.
```

And the best cor matrix:

```{r}
library(corpcor)
cor2pcor(cov(X))
```

And diagnosis:

```{r}
library(mctest)
omcdiag(X,hsgpa)
imcdiag(X,hsgpa)
```

The calculated value of the Chi-square test statistic is found to be highly significant thereby implying the presence of multicollinearity in the model specification.
This induces me to go for the next step of Farrar – Glauber test (F – test) for the location of the multicollinearity. Non of the variables F-statistic is very high, therefore I'm not sure which ones are the cause of multicolinearity.
Therefore, for examining the pattern of multicollinearity, it is required to conduct t-test for correlation coefficient. As there are 10 explanatory variables, there will be several pairs of partial correlation coefficients. In R, there are several packages for getting the partial correlation coefficients along with the t- test for checking their significance level. I'll use the ‘ppcor’ package to compute the partial correlation coefficients along with the t-statistic and corresponding p-values.

```{r}
library(MASS)
library(ppcor)
pcor(X, method = "pearson")
```

There are sligh correlctions between gpa&hsgpa (which makes sense), videogames&gender (-0.4609669112, maybe problematic), slighly high between gpa&leisure(-0.152744844) and videogames&tv(0.270858361), but nothing really worring.
Not only that even some of the low correlation coefficients are also found to be highyl significant. Thus, the Farrar-Glauber test points out that X1 is the root cause of all multicollinearity problem.
There are several remedial measure to deal with the problem of multicollinearity such Prinicipal Component Regression, Ridge Regression, Stepwise Regression etc.
However, in the present case, I can't go for the exclusion of the variables for which the VIF values are above 10 (I dont have any) and as well as the concerned variable logically seems to be redundant. Age and experience will certainly be correlated. So, why to use both of them? If I use ‘age’ or ‘age-squared’, it will reflect the experience of the respondent also. Thus, I try to build a model by excluding ‘experience’, estimate the model and go for further diagnosis for the presence of multicollinearity.
As the problem seems to be more with tv, I'm going to try to exclude it and also try using recode sqrt from the varibales age and gpa.

```{r}
age2<-sqrt(age)

OLS2<-lm(data=data, hsgpa~age2+gender+(gpa^2)+religion+video+year+leisure+race+income) 
summary(OLS2)
```

I also seem to have reduced the vif
```{r}
vif(OLS)
```
I can check now again the multicoliniarity issue.


### b. Homoscedasticity problem.
Now I focused in the next problem: it seems that I also have a problem of Heterocedasticity. There are several ways to control this, but lets first focus on measuring if this is right.

# 1. Previous. Bivariate OLS:
The most simple OLS I have to run is the effect of the main independent variable (viodegames) on my dependent (average SAT grade).

```{r}
OLS<-lm(data=data, hsgpa~video) 
summary(OLS)
plot(OLS, c(1))

attach(data)

data$video<-log(data$video)
```

The model is hsgpa = 4.862415 + 0.25757 (video)+u.

The videogames seems to be significant.

All variables are significantly statistically different from zero at the 5% level against a two-sided alternative, but prpblck the not significant at 1% level. R-squared is 0.3124

The linearity asumptions seems clear here.

##  Check for multicolinearity.

The second MLR assumption is no Perfect Collinearity (Multicollinearity). I can check this here:

```{r}
cor1<-cor(hsgpa,video, use="complete.obs") #complete.obs is a method to compute
cor1
```

The correlation between log(income) and prppov is 0.0359588. Is a not very high, although is a positive correlation (so when one increases, the other variable increases). This is in the same line I was expecting.

```{r}
H<-c("hsgpa", "video")
X<-data[H]

library(corpcor)
cor2pcor(cov(X))
```

```{r}
library(mctest)
imcdiag(X,hsgpa)
```

With such a correlation, almost no vif and no indicator of multicolinearity, there is no reason to worry about this. 

##  Heteroscedasticity.

Now I'm going to check for the homoeskedasticity and no autocorrelation assumption.

```{r}
library(olsrr)
ols_test_breusch_pagan(OLS, rhs = TRUE)
```

Under this simple test, the moodel variance seems not constant, violating one of the main assumption of Gauss- Markov as the variance is expected to be constant.
Nevertheless, as the only purpose of the first model is to be use as a preliminary context of the effects and ideas, this is not consider important. As the rest of the hypotesis are found, I just need to take into account that the efficiency is not very high, as can be expected from a singre explanatory variable model, and more knowing the nature of the variable and topic.


# 2. Previous. Trivariate model.

The second model I'm  going to run is the effect of videogames and income in the educational attainment. 

```{r}
OLS2<-lm(hsgpa~video+income, data=data)
summary(OLS2)
```

The model is hsgpa = 21.229784 + 0.263522 (video)+ 0.186210(income).
The videogames seems to be significant.
Even if this results can seem weird, they are in the line of previous researches, where they observed that there is a more direct effect of hobbies on attainment that income or class, as they are use as an indirect path on ineauqlity transmision.
Precisilly, if I run also this regression, this path seems to be clear.

```{r}
o<-lm(data=data, video~income)
summary(o)
```

All variables are significantly statistically different from zero at the 5% level against a two-sided alternative, but prpblck the not significant at 1% level. R-squared is 0.3124 
The linearity asumptions seems clear here.

##  Gauss-Markov assumptions:
Multicollinearity check:

```{r}
H<-c("hsgpa", "video", "income")
X<-data[H]

library(corpcor)
cor2pcor(cov(X))
```

No high correlation observed.

```{r}
imcdiag(X,hsgpa)
```

As in the previous case, there is no reason to worry about this. 
Heteroscedasticity and no autocorrelation assumption:

```{r}
ols_test_breusch_pagan(OLS2, rhs = TRUE)
```

Under this simple test, the moodel variance seems constant. Ever throught, this model is not expected to be efficient.

# 3. Previous. Multivariate OLS.
The basic multiple OLS based on the theory presented is the following:

```{r}
OLS3<-lm(data=data, hsgpa~age+gender+gpa+religion+video+year+leisure+tv+race) 
summary(OLS)
#plot(OLS, c(1))
```

##. Markov Gauss Assumptions
Seems like I could have multicolinearity. Le's check:

### a. Multicolinearity check.

```{r}
par(mfrow=c(2,2))
plot(OLS)

X<-data[,1:12]
#install.packages("GGally")
library(GGally)
# ggpairs(X) #funciona, pero tarda 17 horas.
```

And the best cor matrix:

```{r}
library(corpcor)
cor2pcor(cov(X))
```

And diagnosis:

```{r}
library(mctest)
omcdiag(X,hsgpa)
imcdiag(X,hsgpa)
```

The calculated value of the Chi-square test statistic is found to be  highly significant thereby implying the presence of multicollinearity in the model specification.
This induces us to go for the next step of Farrar – Glauber test (F – test) for the location of the multicollinearity. Non of the variables F-statistic is very high, therefore we are not sure which ones are the cause of multicolinearity.
Therefore, for examining the pattern of multicollinearity, it is required to conduct t-test for correlation coefficient. As there are 10 explanatory variables, there will be several pairs of partial correlation coefficients. In R, there are several packages for getting the partial correlation coefficients along with the t- test for checking their significance level. We’ll the ‘ppcor’ package to compute the partial correlation coefficients along with the t-statistic and corresponding p-values.

```{r}
library(MASS)
library(ppcor)
pcor(X, method = "pearson")
```

There are sligh correlations between gpa&hsgpa (which makes sense), videogames&gender (-0.4609669112, maybe problematic), slighly high between gpa&leisure(-0.152744844) and videogames&tv(0.270858361), but nothing really worring.
Not only that even some of the low correlation coefficients are also found to be highyl significant. Thus, the Farrar-Glauber test points out that X1 is the root cause of all multicollinearity problem.
There are several remedial measure to deal with the problem of multicollinearity such Prinicipal Component Regression, Ridge Regression, Stepwise Regression etc.
However, in the present case, I can't go for the exclusion of the variables for which the VIF values are above 10 (I dont have any) and as well as the concerned variable logically seems to be redundant. Age and experience will certainly be correlated. So, why to use both of them? If I use ‘age’ or ‘age-squared’, it will reflect the experience of the respondent also. Thus, wI try to build a model by excluding ‘experience’, estimate the model and go for further diagnosis for the presence of multicollinearity.
As the problem seems to be more with tv, I'm going to try to exclude it and also try using recode sqrt from the varibales age and gpa.

```{r}
attach(data)
summary(data)
data$age2<-sqrt(age)

OLS4<-lm(data=data, hsgpa~age2+gender+(gpa^2)+religion+video+year+leisure+race+income)

summary(OLS4)
```

I also seem to have reduced the vif

```{r}
vif(OLS4)
```
I can check now again the multicoliniarity issue.


### b. Homoscedasticity problem.
Now I have to focus in the next problem: it seems that I also have a problem of Heterocedasticity. There are several ways to control this, but lets first focus on measuring if this is right.

#### Measuring the variance 4 ways:
One of the assumptions made about residuals/errors in OLS regression is that the errors have the same but unknown variance. This is known as constant variance or homoscedasticity. When this assumption is violated, the problem is known as heteroscedasticity.

*Consequences of Heteroscedasticity*
The OLS estimators and regression predictions based on them remains unbiased and consistent.
The OLS estimators are no longer the BLUE (Best Linear Unbiased Estimators) because they are no longer efficient, so the regression predictions will be inefficient too.
Because of the inconsistency of the covariance matrix of the estimated regression coefficients, the tests of hypotheses, (t-test, F-test) are no longer valid.

olsrr provides the following 4 tests for detecting heteroscedasticity:

Bartlett Test
Breusch Pagan Test
Score Test
F Test

### Bartlett Test. (Variances not equal)

Bartlett’s test is used to test if variances across samples is equal. It is sensitive to departures from normality. The Levene test is an alternative test that is less sensitive to departures from normality.
I can perform the test using 2 continuous variables, one continuous and one grouping variable, a formula or a linear model.

```{r}

#install.packages("olsrr")
library(olsrr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(purrr)
library(tibble)
#install.packages("nortest")
library(nortest)
#install.packages("goftest")
library(goftest)
```


```{r}
model<-lm(data=data, hsgpa~age2+gender+(gpa^2)+religion+video+year+leisure+race+income) 
myvars<-c("gender", "hsgpa", "gpa", "age", "video", "religion", "year", "leisure", "sport", "tv", "race", "weight", "income", "race2")
ols_test_bartlett(data, myvars)
```

I'm testing the hypothesis that the group variances are equal. With a p-value less than 0.05, I reject the null hypothesis at the 0.05 significance level. I conclude that there is enought evidence to claim that the variances are not equal.
Equal variances across samples is called homogeneity of variance. The Levene test is less sensitive than the Bartlett test to departures from normality. If I have strong evidence that my data do in fhsgpa come from a normal, or nearly normal, distribution, then Bartlett's test has better performance.

### Breusch Pagan Test (No constant variances)
Breusch Pagan Test was introduced by Trevor Breusch and Adrian Pagan in 1979. It is used to test for heteroskedasticity in a linear regression model and assumes that the error terms are normally distributed. It tests whether the variance of the errors from a regression is dependent on the values of the independent variables. It is a x2 test.
You can perform the test using the fitted values of the model, the predictors in the model and a subset of the independent variables. It includes options to perform multiple tests and p value adjustments. The options for p value adjustments include Bonferroni, Sidak and Holm’s method.

```{r}
ols_test_breusch_pagan(model, rhs = TRUE)
```

Here the Null Hypothesis: Equal/constant variances. Is rejected, so seems no constant variance.


### Score Test (variance not homogenous)

Test for heteroskedasticity under the assumption that the errors are independent and identically distributed (i.i.d.). I can perform the test using the fitted values of the model, the predictors in the model and a subset of the independent variables.
Use fitted values of the model

```{r}
ols_test_score(model, rhs = TRUE)
```
Reject the null: variance is heterogeneus.

### F Test (Variance is not homogenous)

F Test for heteroskedasticity under the assumption that the errors are independent and identically distributed (i.i.d.). I can perform the test using the fitted values of the model, the predictors in the model and a subset of the independent variables.
Use independent variables of the model

```{r}
ols_test_f(model, rhs = TRUE)
```

Reject null: Variance is not homogenous

### Conclusions for the variance tests:
It seems clear that I do have a problem in the variance, as it is not constant neither homogeneus. The assumption of the homogeneity of the variance is not found, as the level of the variance is not constant accross the sample. When there is not homogeneity that can lead to a problem of efficiency.
While heteroskedasticity does not cause bias in the coefficient estimates, it does make them less precise; lower precision increases the likelihood that the coefficient estimates are further from the correct population value.

# 4.  Fix the heteroskedasticty problem.
As is clear I have this problem. The first one in to fix this, and the second is to use a more appropiate method to deal with it.
I have 5 possibilities:

### 1. Box-Cox transformation. (FAIL, variance of residuals is constant)

Box-cox transformation is a mathematical transformation of the variable to make it approximate to a normal distribution. Often, doing a box-cox transformation of the Y variable solves the issue, which is exactly what I am going to do now.

```{r}
#install.packages("caret")

library(lattice)
library(caret)

df<-data
distBCMod <- caret::BoxCoxTrans(df$hsgpa)
print(distBCMod)
```
The model for creating the box-cox transformed variable is ready. Lets now apply it on the dependet and append it to a new dataframe

```{r}
df <- cbind(df, hsgpa_new=predict(distBCMod, df$hsgpa)) # append the transformed variable
head(df, 6)
```

The transformed data for the new regression model is ready. Lets build the model and check for heteroscedasticity.

```{r}
lmMod_bc <- lm(data=df, hsgpa~age2+gender+(gpa^2)+religion+video+year+leisure+race+income)
bptest(lmMod_bc)
```

With a p-value of < 2.2e-15, we keep to fail to reject the null hypothesis (that variance of residuals is not constant) and therefore infer that ther residuals are still heterocedastic.

```{r}
plot(lmMod_bc, c(1))
```

Looks more or less the same. Complete fail.

### 2. robustbase. (Nice for bootstraping, but not for here) 

Another way of dealing with heteroskedasticity is to use the lmrob() function from the {robustbase} package. This package is quite interesting, and offers quite a lot of functions for robust linear, and nonlinear, regression models. Running a robust linear regression is just the same as with lm():

```{r}
#install.packages("robustbase")
library(robustbase)
```

Now the OLS:

```{r}
lmrobfit <- lmrob(data=data,hsgpa~age2+gender+(gpa^2)+religion+video+year+leisure+race+income)
summary(lmrobfit)
```

This gives me different estimates than when fitting a linear regression model. The estimates should be the same, only the standard errors should be different. This is because the estimation method is different, and is also robust to outliers.

Finally, it is also possible to bootstrap the standard errors. For this I will use the bootstrap() function from the {modelr} package:

```{r}
#install.packages("modelr")
#library(modelr)

#resamples <- 100

#boot_education <- df %>% 
#modelr::bootstrap(resamples)
```

The column strap contains resamples of the original data. I will run my linear regression from before on each of the resamples:

```{r}
#(boot_lin_reg <- boot_education %>% 
 #       mutate(regressions = 
 #                  map(strap, 
 ##                   ~lm(hsgpa~age2+gender+(gpa^2)+religion+video+year+
 #                         leisure+race+income, 
 #                          data = df))))

```

I have added a new column called regressions which contains the linear regressions on each bootstrapped sample. Now, I will create a list of tidied regression results:

```{r}
#(
 #   tidied <- boot_lin_reg %>% 
 #       mutate(tidy_lm = 
  #                 map(regressions, broom::tidy))
#)
``` 

I stoped the code as it takes a long time to run (not fixing it properly).


### 3. Coeftest. (Seems to help) 

Now to check whether the data has heteroskedasticity or not, I will construct a variance-covariance matrix.

```{r}
library(sandwich)
library(lmtest)
vcovHC(OLS2, omega = NULL, type = "HC4")
```

Now vcovHC() function creates a variance “ covariance matrix. The first parameter is the regression model I have made. Keep omega as NULL. The type parameter refers to what measure of heteroskedasticity is being used. There are 5 types from HC0 to HC4. HC4 is the latest and I use that. You can read more about it in the sandwich package.

Finally to remove this heteroskedasticity, I use the coeftest() function in R.

```{r}
coeftest(OLS, df = Inf, vcovHC(OLS, omega = NULL, type = "HC4"))
```

This seem to have fixed the standard errors in my regression.

### 4. Weighted Least Squares estimates (WLS) (same, slightly better) 
model:

```{r}
LM<-lm(data=data, hsgpa~age2+gender+(gpa^2)+religion+video+year+leisure+race+income) 
```

Now I pit the residuals:
```{r}
resid = LM$residuals
summary(resid)
```

Now, as I want log(uˆ2), I create this dependent variable.

```{r}
logredid<-log(resid^2) 
summary(logredid)
```

And I can run the regression:

```{r}
LM2<-lm(logredid~age2+gender+(gpa^2)+religion+video+year+leisure+race+income)
summary(LM2)
```

And the fitted values.

```{r}
fit = fitted(LM2)  # Calculate fitted value 
summary(fit)
sq = sqrt(exp(fit)) # Square root and exponentiate this to get an estimate for the standard deviation  
summary(sq)
```

And now I can compute a WLS:

```{r}
WLS = lm(hsgpa~age2+gender+(gpa^2)+religion+video+year+leisure+race+income, weights = 1/sq)
summary(WLS)
```

While the OLS was:

```{r}
summary(OLS2)
```

The coefficients look pretty similar from the OLS to the WLS. I can see that the standard errors have slighly fallen in the WLS model.

Now I can chest to see if there is still heteroskedasticity in the WLS model.

```{r}
ncvTest(WLS)  # Non-constant variance test for WLS residuals
```

Sadly, it seems that I haven't completely removed all of the heteroskedasticity. Therefore it would  make sense to now include robust standard errors as I clearly don’t have avoid the problem with heteroskedasticty.


### 5. Heteroskedasticity Robust Standard Errors
Although heteroskedasticity does not produce biased OLS estimates, it leads to a bias in the variance-covariance matrix. This means that standard model testing methods such as t tests or F tests cannot be relied on any longer. This post provides an intuitive illustration of heteroskedasticity and covers the calculation of standard errors that are robust to it.

For this we need the restricted and unrestricted models:

```{r}
# Estimate the model
model<-lm(formula = hsgpa ~ video, data = data)

# Print estimates and standard test statistics
summary(model)
```


## Robustness, F and  T-test.
Since I already know that the model above suffers from heteroskedasticity, I want to obtain heteroskedasticity robust standard errors and their corresponding t values. In R the function coeftest from the lmtest package can be used in combination with the function vcovHC from the sandwich package to do this.

The first argument of the coeftest function contains the output of the lm function and calculates the t test based on the variance-covariance matrix provided in the vcov argument. The vcovHC function produces that matrix and allows to obtain several types of heteroskedasticity robust versions of it. In my case we obtain a simple White standard error, which is indicated by type = "HC0". Other, more sophisticated methods are described in the documentation of the function, vcovHC.

```{r}
# Load libraries
library("lmtest")
library("sandwich")

# Robust t test
coeftest(model, vcov = vcovHC(model, type = "HC0"))
```

Post-hypothesis testing.

```{r}
# Estimate unrestricted model
model_unres <- lm(formula = hsgpa ~ age2 + gender + (gpa^2) + religion + video + 
    year + leisure + race + income, data = data)

# F test
anova(model, model_unres)
```

For a heteroskedasticity robust F test we perform a Wald test using the waldtest function, which is also contained in the lmtest package. It can be used in a similar way as the anova function, i.e., it uses the output of the restricted and unrestricted model and the robust variance-covariance matrix as argument vcov. Based on the variance-covariance matrix of the unrestriced model we, again, calculate White standard errors.

```{r}
waldtest(model, model_unres, vcov = vcovHC(model_unres, type = "HC0"))
```

The heteroskedasticity is not complitely avoid in this last model, but this is the best model possible if I still want to use OLS. As the heteroskedasticity does not cause any bias, I just need to take into account the decrease in efficiency that is expected to be avoid using this other approaches presented.

# 5. General checks and comprobations.
## Correlations. 

```{r}
cor(income,gender, use = "complete.obs") 
cor(income,gpa, use = "complete.obs") 
cor(income,age, use = "complete.obs") 
cor(income,video, use = "complete.obs") 
cor(income,religion, use = "complete.obs")
cor(income,leisure, use = "complete.obs") 
cor(gpa,age, use = "complete.obs") 
cor(gpa,video, use = "complete.obs")
cor(gpa,religion, use = "complete.obs") 
cor(gpa,gender, use = "complete.obs") 
cor(gpa,leisure, use = "complete.obs") 
cor(video,religion, use = "complete.obs") 
cor(video,leisure, use = "complete.obs") 
cor(religion,leisure, use = "complete.obs") 
```

The correlation between every pair of variables doesn't seem very high.  
 
## Variance-covariance matrix.
The var-cov matrix of my regression.

```{r}
var_cov<-vcovHC(OLS, type = "HC4")
coeftest(OLS, df = Inf, var_cov)
```

Now vcovHC() function creates a variance “ covariance matrix. The first parameter is the regression model I have made. Keep omega as NULL. The type parameter refers to what measure of heteroskedasticity is being used. There are 5 types from HC0 to HC4. HC4 is the latest and we use that. You can read more about it in the sandwich package.
This is my variance- covariance matrix. The independent variables are listed both in column and rows. The diagonal elements show the variance of each variable with itself. The diagonal values should have been constant, but since they vary, we can detect the presence of heteroskedasticity!

To get the F test:

```{r}
 waldtest(OLS, vcov = vcovHC)
```

For a heteroskedasticity robust F test we perform a Wald test using the waldtest function, which is also contained in the lmtest package. It can be used in a similar way as the anova function, i.e., it uses the output of the restricted and unrestricted model and the robust variance-covariance matrix as argument vcov. Is based on the variance-covariance matrix of the unrestriced model.
Another try:

```{r}
# Estimate the model
model <- lm(hsgpa ~ income)

# Robust t test
coeftest(model, vcov = vcovHC(model, type = "HC0"))
coeftest(model, vcov = vcovHC(model, type = "HC0"))

# Estimate unrestricted model
model_unres <- lm(data=data, hsgpa~+gender+religion+video+year+leisure+race+income)

# F test
anova(model, model_unres)

#wald.
waldtest(model, model_unres, vcov = vcovHC(model_unres, type = "HC0"))
```

## Jointly significance 
We can test if the variables are jointly significant now. For that, is better to use f-test (*no p-value of a normal regression, the p-value f the t-test)*. So we start making the hypothesis. An F-test will allow us to test the null that all of the coefficients are equal zero; that is that these variables jointly significance. So:

H0: hseval-log(income)=0.
H1: hseval-log(income)=/=0.

Knowing this, we can conduct the test.

```{r}
library(carData)
library(car)

lm(formula = hsgpa ~ income + gender + video + religion + 
    leisure, data = data)

lht(OLS, c("video = 0"), white.adjust = "hc1")

#Command explanations:
# LM2: tells lht which regression results to use (here the last OLS with all independent)
#c("lhseval = 0", "lincome = 0", "prppov=0") expresses the null hypothesis, with each
#restriction in quotation marks.
#white.adjust = “hc1” assures that the heteroskedasticity-corrected standard errors are
#used for the test. Always include this (part of the code)
```

Knowing the null hypothesis that is stated, and that the F-stat for the test is 31.389, with the p-value of (<2.2e-16). Df = 3 tells me the number of restrictions being tested. Obviously with such a low p-value we can reject the null hypothesis.

# 6. 2SLS (FINAL MODELS). IV selection and check.
Instrumental variables (IVs) are used to control for confounding and measurement error in observational studies. They allow for the possibility of making causal inferences with observational data. Like propensity scores, IVs can adjust for both observed and unobserved confounding effects. Other methods of adjusting for confounding effects, which include stratification, matching and multiple regression methods, can only adjust for observed confounders. IVs have primarily been used in economics research, but have recently begun to appear in epidemiological studies.

## Criteria.
There are two main criteria for defining an IV:
(i) It causes variation in the treatment variable;
(ii) It does not have a direct effect on the outcome variable, only indirectly through the treatment variable.
A reliable implementation of an IV must satisfy these two criteria and utilize a sufficient sample size to allow for reasonable estimation of the treatment effect. If the first assumption is not satisfied, implying that the IV is associated with the outcome, then estimation of the IV effect may be biased. If the second assumption is not satisfied, implying that the IV does not affect the treatment variable then the random error will tend to have the same effect as the treatment. When selecting an IV, one must ensure that it only affects whether or not the treatment is received and is not associated with the outcome variable.

### Limitations:
Although IVs can control for confounding and measurement error in observational studies they have some limitations. I must be careful when dealing with many confounders and also if the correlation between the IV and the exposure variables is small. Both weak instruments and confounders produce large standard error which results in imprecise and biased results. Even when the two key assumptions are satisfied and the sample size is large, IVs cannot be used as a substitute for the use of clinical trials to make causal inference, although they are often useful in answering questions that an observational study can not. In general, instrumental variables are most suitable for studies in which there are only moderate to small confounding effects. They are least useful when there are strong confounding effects.

## Check "Year" as a proper IV.
In the dataset I'm using, the variable "year" appears as a proper IV variable, at least in the theoretical approach. There's no reason to think that taking the exam on one year or another can influence the grades. At the same time, seems reasonable to think that videogames are highly influenced by this, given that this kind of entertainment has been becoming more a more popular with the pass of the years. 

```{r}
library(zoo)
library(lmtest)
library(survival)
library(carData)
library(car)
library(sandwich)
library(AER)
library(stargazer)

#OLS3<-lm(hsgpa ~ + gender + religion + video + 
#year + leisure + race + income, data = data)
```

## Two-Stage Least Squared Estimator (Main predictor).
I'm going to try to use this IV in a TSLS model. For that, we start computing the first stage regression:

```{r}
s1 <- lm(video~ year)

coeftest(s1, vcov = vcovHC, type = "HC1")
```

This model predicts the relation between hours per week playing video games and the year the interview was conducted. This appears to be as expected.

```{r}
s1_pred <- s1$fitted.values
```

The second stage regression is:

```{r}
s2 <- lm(hsgpa ~ s1_pred)
coeftest(s2, vcov = vcovHC)
```

The model will be: hsgpa= -14.64901(1.49014)+16.85159(video).
Now I can perform the TSLS to check the results obtained.

```{r}
ivreg1 <- ivreg(log(hsgpa) ~ video | year, data = data)

coeftest(ivreg1, vcov = vcovHC, type = "HC1")
```

The result in both are the same, as expected.

## Two Stage Least Squared (Bivariate)
I'm going to use this bivariate model:

log(hsgpa)=b0+b1video+b2gender+u.

In this case I'm going to add a log to the dependent variable (hsgpa) to avoid negative values and facilitate the visual recognition of the influences.

```{r}
ivreg2 <- ivreg(log(hsgpa) ~ video + gender |gender + year, data = data)

coeftest(ivreg2, vcov = vcovHC, type = "HC1")
```

All seems to be as expected.

## Two-Stage Least Squared (Multiple)
I'm going to use the same model in part 3:

hsgpa=b0+b1video+gender+religion+leisure+race+income+u.

As previously, I'm going to add a log to the dependent variable (hsgpa).

```{r}
library(AER)
ivreg3 <- ivreg(log(hsgpa) ~ video + gender+ religion +leisure+race2+ income | gender+ religion +leisure+race2+ income+ year, data = data)

coeftest(ivreg3, vcov = vcovHC, type = "HC1")
```

###  Checking validity.

We have to check for weakness, check if the IV exogeneity is violated (correlation between instrument and error term)
First the robust coefficient summaries for all:

```{r}
coeftest(ivreg1, vcov = vcovHC, type = "HC1")
coeftest(ivreg2, vcov = vcovHC, type = "HC1")
coeftest(ivreg3, vcov = vcovHC, type = "HC1")
```

And now the relevance:

```{r}
#First model:
m1<-lm(log(hsgpa)~video)
linearHypothesis(m1, 
                 "video = 0", 
                 vcov = vcovHC, type = "HC1")
```

Perfect.

```{r}
#Second model:

m2 <- lm(log(hsgpa) ~ video + gender)
linearHypothesis(m2, 
                 "video = 0", 
                 vcov = vcovHC, type = "HC1")
```

Accepted at 0.001%.
And the last model:

```{r}
m3 <- lm(log(hsgpa) ~ video + gender + religion + leisure + race + income)

linearHypothesis(m3, 
                 c("income = 0", "gender = 0", "video=0", "religion=0", "leisure=0", "race=0"), 
                 vcov = vcovHC, type = "HC1")
```

No problem here neither.

This seem to say that this estimator is perfectly adecuate.

### Diagnostics:
```{r}
require(lmtest)

summ.fit1 <- summary(ivreg1, vcov. = function(x) vcovHC(x, type="HC0"), diagnostics=T)
summ.fit2 <- summary(ivreg2, vcov. = function(x) vcovHC(x, type="HC0"), diagnostics=T)
summ.fit3 <- summary(ivreg3, vcov. = function(x) vcovHC(x, type="HC0"), diagnostics=T)

summ.fit1$diagnostics
summ.fit2$diagnostics
summ.fit3$diagnostics

```

This presentation provides a decent overview with worked examples.
Weak instruments means that the instrument has a low correlation with the endogenous explanatory variable. This could result in a larger variance in the coefficient, and severe finite-sample bias. From the help file for AER, it says it does an F-test on the first stage regression; the null is that the instrument is weak. For the models, the null is rejected, so I can move forward with the assumption that the instrument is sufficiently strong.
Wu-Hausman tests that IV is just as consistent as OLS, and since OLS is more efficient, it would be preferable. The null here is that they are equally consistent; in this output, Wu-Hausman is significant at the p<0.1 level, that would mean IV is consistent and OLS is not.
Sargan tests overidentification restrictions. The idea is that if I have more than one instrument per endogenous variable, the model is overidentified, and I have some excess information. All of the instruments must be valid for the inferences to be correct. So it tests that all exogenous instruments are in fhsgpa exogenous, and uncorrelated with the model residuals. If it is significant, it means that I don't have valid instruments (somewhere in there, as this is a global test). In this case, this isn't a concern. 
Same for direct imputations:
```{r}
gaze.lines.ivreg.diagn <- function(x, col="p-value", row=1:3, digits=2){
    stopifnot(is.list(x))
    out <- lapply(x, function(y){
        stopifnot(class(y)=="summary.ivreg")
        y$diagnostics[row, col, drop=FALSE]
    })
    out <- as.list(data.frame(t(as.data.frame(out)), check.names = FALSE))
    for(i in 1:length(out)){
        out[[i]] <- c(names(out)[i], round(out[[i]], digits=digits))
    }
    return(out)
}
gaze.lines.ivreg.diagn(list(summ.fit1, summ.fit2), row=1:2)
gaze.lines.ivreg.diagn(list(summ.fit1, summ.fit2), col=4, row=1:2, digits=2)
```

# 7. Nice plots for the paper.
In this section I`m going to run some codes for graphs

## Regression coefplot coefficients (Used).

Libraries:

```{r}
library(ggplot2)
library(igraph)
library(carData)
library(car)
library(GGally)
library(coefplot)
```

Coeff. Regression plot.

```{r}
coefplot(ivreg1)
coefplot(ivreg2)
coefplot(ivreg3) #interesting one 
```

## Correlations plot:

```{r}
M<-data[,c("hsgpa", "video", "gender", "religion", "leisure", "race", "income")]
pairs(M)
```

## Marginal effects.

```{r}
library(ggeffects)

library(ggplot2)
mydf <- ggpredict(ivreg3, terms = "video")

ggplot(mydf, aes(x, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) 
```
