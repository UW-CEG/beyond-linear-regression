##############################
##############################
##                          ##
## Beyond Linear Regression ##      
##                          ##
##############################
##############################

# Full Citation: Beyond linear regression: a quick reference for analyzing common data types in education research
# Authors: Elli Theobald, Melissa Aikens, Sarah Eddy, Hannah Jordt
  # Corresponding Author Contact: ellij@uw.edu
  # Date: Spring 2019

# Description: 
  # This code uses logistic regression to analyze a binary outcome variable

#################################
### Load Statistical Packages ###
#################################

# to install packages if they are not already installed:
install.packages("car")
install.packages("effects")
install.packages("pscl")
install.packages("sjstats")

# to load packages:
library(car) # this is a package to help test the assumptions
library(effects) # this is a package to help interpret results
library(pscl) # this package is useful for evaluating model fit
library(sjstats) # this package is useful for evaluating model fit


#############################
### Set working Directory ###
#############################

#change setwd to folder where data is saved on your computer
setwd("")


###################################
### Load, Check, and Clean Data ###
###################################

logisticData <- read.csv("logisticData.csv",header=T)
  names(logisticData)

# Dataset contains the following 7 variables:
  # interest: a numeric scale score indicating a student's interest in using mathematics to understand biology; range from 1 (low) - 7 (high)
  # utility: a numeric scale score indicating a student's perceptions of how useful mathematics is to their life science career; range from 1 (low) - 7 (high)
  # cost: a numeric scale score indicating a student's anxiety/worry over incorporating mathematics into their biology courses; range from 1 (low) - 7 (high)
  # course: a binary variable indicating whether a student reports being unlikely (0) or likely (1) to take an elective mathematical modeling course
  # gender: a binary variable indicating whether a student is male (M) or female (F)
  # year: a categorical variable indicating a student's year in college: 1, 2, 3, or 4
  # high.math: a categorical variable indicating the highest high school math course a student took: calculus (Calc), pre-calculus (pCalc), algebra (Alg), or statistics (Stats)

#Examine data structure
str(logisticData)
  # you can see that R considers the binary variable "course" to be an integer ("int") - it's actually categorical, so we need to change that
  # you can also see that year in school (year) is considered to be integer, but this too should be categorical
  # finally, you can see that gender and highest high school math (high.math) are factors, but we'll need to make sure we set a meaningful reference level for comparison

#specify the binary variable as factor with 0 as reference variable
logisticData$course <- factor(logisticData$course,levels=c(0,1))


#year in school needs to be re-coded as a factor; year 1 is made the reference level
logisticData$year <- factor(logisticData$year,levels=c(1,2,3,4))

#specify "male" as reference level for gender and "calculus" as reference level for highest high school math
logisticData$gender <- factor(logisticData$gender,levels=c("M","F"))
logisticData$high.math <- factor(logisticData$high.math,levels=c("Calc","pCalc","Alg","Stats"))

str(logisticData)
#looks better!

#Examine the outcome data 
summary(logisticData$course)
table(logisticData$gender, logisticData$course)
table(logisticData$year, logisticData$course)
table(logisticData$high.math, logisticData$course)
# Only a few students with algebra or stats as their highest high school math course, but we'll nevertheless look at it in our model for this exercise

# Caveats - this code does not do the following things, which are worth taking the time to do with any data set before conducting analyses:
  # looking for outliers and checking multicollinearity between predictors
  # removing observations with NAs
  # making lots of different visual plots of the data to become familiar with it


#################
### Fit model ###
#################

#Predict reporting being likely to take a mathematical modeling course based on interest, utility value, cost, gender, year in school, and highest high school math course taken
mod1 <- glm(course ~ interest + utility + cost + gender + year + high.math,
         data=logisticData, family=binomial(link="logit"))
summary(mod1)



#################################
### Testing Model Assumptions ###
#################################
##### Assumtion 1:
  # the log-odds of the outcome is linearly related to the continuous predictor variables
 
# Check each continuous variable graphically

# Caclulate predicted probabilities of model & create smooth scatterplot of logit of predicted values 
  # against continuous predictors (Hosmer et al., 2013)
    # predicted values with type="response" are the y-values predicted by the regression model
    # the predicted values obtained this way are identical to fitted values obtain by using the code: fitted(mod1)
pred.prob <- predict(mod1, type = "response")
scatter.smooth(logisticData$interest, logit(pred.prob)) #looks good
scatter.smooth(logisticData$utility, logit(pred.prob)) #hmmm...some curvature
scatter.smooth(logisticData$cost, logit(pred.prob))  #hmmm...some curvature

#Because there appears to be some curvature, we would probably explore the probit or complementary log-log models as alternate models and see if they provide better fits to the data
  #However, for the sake of this exercise, let's move on to how to interpret the regression output


#########################
### Interpret Results ###
#########################
summary(mod1)
# From the output, immediately we see that interest, cost, and year 4 are significant
  # For a one unit increase in interest, there is an increase of 0.71 in the log-odds of reporting being likely to take a modeling course
  # For a one unit increase in cost, there is a decrease of 0.27 in the log-odds of reporting being likely to take a modeling course
  # Compared to 1st-year students, fourth year students have a decrease of 0.52 in the log-odds of reporting being likely to take a modeling course


# The log-odds coefficients are not intuitive, so let's convert them to odds-ratios
  # We'll also compute 95% confidence intervals based on profile-likelihood limits
(odds <- cbind("Odds-Ratio"=exp(mod1$coef), exp(confint(mod1))))

#Looking at the significant variables from the regression output, we can now interpret them as:
  #For a one unit increase in interest, the odds of a student reporting they are likely to take a mathematical modeling course are 2 times greater than the odds of a student reporting being unlikely to take a modeling course
    #Or, the odds of a student reporting to be likely to take a mathematical modeling course increase by 100% for every one unit increase in interest 
  #For a one unit increase in cost, the odds of a student reporting they are likely to take a mathematical modeling course are 0.77 times as great as the odds of a student reporting being unlikely to take a modeling course
    #Or, the odds of a student reporting to be likely to take a mathematical modeling course decrease by 23% for every unit increase in cost
  #The odds of a fourth-year student reporting they are likely to take a mathematical modeling course, compared to unlikely to take a mathematical modeling course, are 0.60 times as great as a first-year student
    #Or, the odds of a fourth-year student reporting they are likely to take a mathematical modeling course are 40% lower than the odds of a first-year student

#You can also convert the odds-ratios less than 1 by using the reciprocal of the odds-ratio; we can see the odds of a student reporting being UNlikely to taking a mathematical modeling course
(odds.cost <- 1/odds[4])
(odds.year4 <- 1/odds[8])

#For a one unit increase in cost, the odds of a student reporting they are unlikely to take a mathematical modeling course is 1.3 times greater than the odds of reporting being likely to take a modeling course
  #Or, the odds of a student reporting to be unlikely to take a mathematical modeling course increase by 30% for every unit increase in cost
#The odds of a fourth-year student reporting they are unlikely to take a mathematical modeling course, compared to reporting they are likely to take a modeling course, are 1.68 times greater than a first-year student
  #Or, the odds of a fourth-year student reporting they are unlikely to take a mathematical modeling course are 68% higher than the odds of a first-year student
  #Or, the odds of a first-year student reporting they are likely to take a mathematical modeling course, compared to reporting they are unlikely to take a modeling course, are 1.68 times higher than the odds of a fourth-year student

# Taking the reciprocal is the same as changing the reference level of your binary variable
  # For example, we can change the reference of our binary response variable so we are now modeling students 
  # reporting they are unlikely to take a mathematical modeling course

logisticData$course <- factor(logisticData$course, levels=c("1","0"))

mod2 <- glm(course ~ interest + utility + cost + gender + year + high.math,
             data=logisticData, family=binomial(link="logit"))
  summary(mod2)
  
(odds.2 <- cbind("Odds-Ratio"=exp(mod2$coef), exp(confint(mod2))))

# The odds for cost and fourth-year students compared to first-year students 
  # are the same as we calculated with the reciprocal above
summary(mod1)

# Likewise, for the categorical variable year in school, we could run a logistic regression 
  # modeling likely to take a mathematical modeling course, but change the reference level of year in 
  # school to be "4"

  # rename 0 as reference of outcome
logisticData$course <- factor(logisticData$course, levels=c("0","1"))
  # name 4 as reference of year
logisticData$year <- factor(logisticData$year, levels=c("4","3","2","1"))

mod3 <- glm(course ~ interest + utility + cost + gender + year + high.math,
             data=logisticData, family=binomial(link="logit"))
  summary(mod3)
  
(odds.3 <- cbind("Odds-Ratio"=exp(mod3$coef), exp(confint(mod3))))
# The odds for first-year students compared to fourth-year students mirror those described above when we took the reciprocal


# Calculate pseudo-R^2 values
  # These values are most useful when you want to compare logistic regression models 
  # (e.g., a full model to a null model)
  # See text in paper for discussion and references on which pseudo-R^2 values to use

# using the pscl package:
pR2(mod1)
# McFadden = McFadden's pseudo-R^2
# r2ML = Cox-Snell R^2
# r2CU = Nagelkerke R^2


# using the sjstats package
# For Cox and Snell and Nagelkerke pseudo-R2:
r2(mod1)

# For Tjur's D:
cod(mod1)


# Using the Effects package to understand the results

# Effect of interest on outcome
interestEff <- Effect("interest", mod1)
plot(interestEff, ylab="Probability of Reporting Being Likely\nto Take a Modeling Course", xlab= "Interest", 
     rug=FALSE,main=F)


#Effect of cost on outcome
costEff <- Effect("cost", mod1)
plot(costEff, ylab="Probability of Reporting Being Likely\nto Take a Modeling Course", xlab= "Cost", 
     rug=FALSE,main=F)


#Effect of year in school on outcome
yearEff<-summary(effect("year", mod1))


#basic bar graph
par(xpd=T,mar=c(5,5,2,2))
year.lab <- c("First","Second","Third","Fourth")
barx <- barplot(yearEff$effect,beside=T,ylim=c(0,1),las=1,names.arg=year.lab,col="lightsteelblue1")
arrows(barx,yearEff$upper,barx,yearEff$lower,angle=90,code=3,length=0.08,lwd=1)
par(xpd=F)
abline(h=0,col="black")
mtext("Probability of Reporting Being Likely\nto Take a Modeling Course",side=2,line=2.8)
mtext("Year in School",side=1,line=2.8)
