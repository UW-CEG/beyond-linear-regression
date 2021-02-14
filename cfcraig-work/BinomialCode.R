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
  # This code uses binomial regression to analyze an outcome that is a proportion based on a count of binary data

#################################
### Load Statistical Packages ###
#################################

# to install packages if they are not already installed:
install.packages("car")
install.packages("effects")

# to load packages:
library(car) # this is a package to help test the assumptions
library(effects) # this is a package to help interpret results


#############################
### Set working Directory ###
#############################

#change setwd to folder where data is saved on your computer
setwd("")

###################################
### Load, Check, and Clean Data ###
###################################

#binomialData=data.frame(read.csv("binomial.csv",header=T),na.strings="")

binomial <- read.csv("binomialData.csv",header=T)
  head(binomial)

# Data Set contains the following 5 variables:
  # Gender: a binary variable indicating whether a student is male (M) or female (F)
  # FirstGen: a binary variable indicating whether a student is first-generation in college (FirstGen) or continuing-generation (NotFirstGen)
  # GPA: a continuous variable representing a student's college GPA (max: 4)
  # NumComplete: a count variable indicating how many practice exams a student completed (these were not for credit)
  # NumPossible: the total number of practice exams a student could have completed

#Examine data structure
str(binomial)
#you can see that gender and firstgen are factors, as they should be, but we'll need to make sure we set a meaningful reference level for comparison

#specify "male" as reference level for gender, "NotFirstGen" as reference level for FirstGen
binomial$Gender <- factor(binomial$Gender,levels=c("M","F"))
binomial$FirstGen <- factor(binomial$FirstGen,levels=c("NotFirstGen","FirstGen"))

# Examine the outcome data
summary(binomial$NumComplete)
table(binomial$NumComplete)
table(binomial$NumComplete, binomial$Gender)
table(binomial$NumComplete, binomial$FirstGen)
  # not a large number of first-gen students; may not have enough power to detect an effect

#Caveats - this code does not do the following things, which are worth taking the time to do with any data set before conducting analyses:
  #looking for outliers or checking multicollinearity between predictors
  #removing observations with NAs
  #making lots of different visual plots of the data to become familiar with it


#################
### Fit model ###
#################

# Predict probability of taking a practice exam by student characteristics: gender, first-gen status, and GPA
  # Instead of a column of the number of practice exams that students did not complete ("failures"), 
  # we subtracted the number of exams submitted from the total number of exams possible
mod1 <- glm(cbind(NumComplete,NumPossible-NumComplete) ~ GPA + Gender + FirstGen,
            data=binomial, family=binomial(link="logit"))
summary(mod1)

# From the output, immediately we see that GPA and gender are significant
  # A one unit increase in GPA leads to a decrease of 1.40 in the log-odds of taking a practice exam
  # Compared to male students, female students have an increase of 0.17 in the log-odds of taking a practice exam


# You can also fit the model with the proportion of practice exams completed as the y variable, 
  # and the total number of practice exams possible as a weight
binomial$PropComplete <- (binomial$NumComplete/binomial$NumPossible)

mod1.1 <- glm(PropComplete ~ GPA + Gender + FirstGen,
              data=binomial, family=binomial(link="logit"), weight=NumPossible)
summary(mod1.1)
# the results are the same as above


#################################
### Testing Model Assumptions ###
#################################
##### Assumtion 1:
  # the log-odds of the outcome is linearly related to the continuous predictor variables

# Check each continuous variable graphically; this requires the car package

# Examine a plot of the log-odds of the proportions as a function of each continuous predictor 
  # (in this case, only GPA)
# There should be an approximate linear relationship
# Note: you cannot take the logit of 0 or 1, so if there are 0's or 1's in your proportion data, 
  # R will convert 0/n to a proportion of 0.025 and n/n to a proportion of 0.975 by default; 
  # these can be changed using "adjust"
scatter.smooth(binomial$GPA, logit(binomial$PropComplete,adjust=0.05))  # adjust=0.05 changes 0 and 1 proportions to 0.05 and 0.95, respectively
# looks good!


# Calculate predicted probabilities of model & create smooth scatterplot of logit of 
  # predicted values against continuous predictors
  # predicted values with type="response" are the y-values predicted by the regression model
  # the predicted values obtained this way are identical to fitted values obtained by using the code: fitted(fit)
pred.prob <- predict(mod1, type = "response")
scatter.smooth(binomial$GPA, logit(pred.prob))
# looks good!


# Test for overdispersion
# Function from GLMM FAQ (Bolker et al.; https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html) 
overdisp_fun <- function(model) {
       rdf <- df.residual(model)
       rp <- residuals(model,type="pearson")
       Pearson.chisq <- sum(rp^2)
       prat <- Pearson.chisq/rdf
       pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
       c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
   }
overdisp_fun(mod1)
# ratio should be ~1, but here it is 3, indicating overdispersion
  # p < 0.05, which indicates overdispersion
  # Consider fitting a quasi-binomial model

#########################
### Interpret Results ###
#########################
# Note that we should probably consider fitting a quasi-binomial model,
  # but for illustrative purposes we will interpret mod1 here

# The log-odds coefficients are not intuitive, so let's convert them to odds-ratios
  # We'll also compute 95% confidence intervals based on profile-likelihood limits
(odds <- exp(cbind(OR=coef(mod1), confint(mod1))))


# Looking at the significant variable from the regression output, we can now interpret them as:
  # For a one unit increase in GPA, the odds of a student completing a practice exam are 0.25 times as great as the odds of not completing the practice exam
  # Or, the odds of a student completing a practice exam decrease by 75% for every unit increase in GPA
  # The odds of a female student completing a practice exam, compared to the odds of not completing an exam, are 1.2 times greater than the odds of a male student
  # Or, the odds of a female student completing a practice exam are 19% greater than the odds of a male student


# You can also convert the odds-ratios less than 1 by using the reciprocal of the odds-ratio; 
  # we can see the odds of a student not comleting a practice exam
(odds.gpa<-1/odds[2])

# For a one unit increase in GPA, the odds of a student not completing a practice exam are 4.1 times greater 
  # than the odds of completing a practice exam
  # Or, the odds of a student not completing a practice exam increase by 307% for every unit increase in GPA


# Using the Effects package to understand the results

# Effect of GPA on outcome
gpaEff <- Effect("GPA", mod1)
plot(gpaEff, ylab="Probability of Completing a Practice Exam", xlab= "GPA", 
     rug=FALSE,main=F)


#Effect of gender on outcome
genEff <- summary(effect("Gender", mod1))

#basic bar graph
par(xpd=T,mar=c(5,5,2,2))
gen.lab<-c("Male", "Female")
barx<-barplot(genEff$effect,beside=T,ylim=c(0,1),las=1,names.arg=gen.lab,col="lightsteelblue1")
arrows(barx,genEff$upper,barx,genEff$lower,angle=90,code=3,length=0.08,lwd=1)
par(xpd=F)
abline(h=0,col="black")
mtext("Probability of Completing a Practice Exam",side=2,line=2.8)
