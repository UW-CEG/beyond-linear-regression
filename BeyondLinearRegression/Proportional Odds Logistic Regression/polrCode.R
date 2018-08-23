##############################
##############################
##                          ##
## Beyond Linear Regression ##
##			                    ##
##############################
##############################

# Full Citation: Beyond Linear Regression: a quick reference for analyzing common data types in education research
# Authors: Elli Theobald, Melissa Aikens, Sarah Eddy, Hannah Jordt
  # Corresponding Author Contact: ellij@uw.edu
  # Date: Spring 2019

# Description: 
  # This code uses proportional odds logistic regression to analyze Likert-scale data

#################################
### Load Statistical Packages ###
#################################

# to install packages if they are not already installed:
install.packages("MASS")
install.packages("effects")
install.packages("nnet")
install.packages("Hmisc")

# to load packages:
library(MASS) # this has the functions to fit proportional odds models
library(effects) # this is a package to help interpret results
library(nnet) # this has the function for fitting multinomial models (testing assumptions)
library(Hmisc) # this library allows us to test an assumption of proportional odds models


#############################
### Set working Directory ###
#############################

#change setwd to folder where data is saved on your computer
setwd("")

###################################
### Load, Check, and Clean Data ###
###################################

polrData <- read.csv("polrData.csv",head=T)
  names(polrData)

# Data Set contains the following 6 variables:
  # Gender: a binary variable indicating whether a student is male (M) or female (F)
  # Ethinicity: a categorical variable indicating which broad group category a student belongs to: Asian-American, International, URM (i.e. underrepresented minority), White
  # FirstGeneration: a binary variable indicating whether a student is first-generation in college (1) or continuing-generation (0)
  # CourseGrade: a continuous variable representing a student's course grade (4.0 scale)
  # Activity Type: a binary variable indicating type of activity a student completed: Interactive, Constructive
  # Dominator: a categorical ordinal variable, indicating the extent to which a student agreed there was a dominator in their group (on a scale of 1-6 with a higher value indicating a higher level of agreement)

# plot a histogram as a way to check the data 
hist(polrData$Dominator)

# Summarize the outcome data as a way to check the data
summary(polrData$Dominator)

# The "Dominator" outcome is a Likert-scale question (levels 1-6)
  # the model treats this as a factor
  # Assign Dominator as a factor
levels(as.factor(polrData$Dominator))

# Relevel the Ethnicity variable so that the reference group is White students
polrData$Ethnicity <- relevel(polrData$Ethnicity, ref="White")

# Caveats - this code does not do the following things.
  # These things (and others) are worth doing with any dataset:
    # looking for outliers, checking multicollinearity between predictors
    # Removing observations with NAs

  
#################
### Fit model ###
#################

# Output of proportional odds models do not include p-values.
  # Thus, hypothesis testing usally happens with model selection.


# Employ backward selection to determine the best fitting model:
# Start with a complex model that tests the hypotheses of interest
  # Students vary the amount they agree or disagree to there being a dominator with different Activity Types 
  # Full model: dominator ~ demographics + ActivityType*Demographics

# Complex model
  # Tests the hypothesis that Treatment (ActivityType) has a disproportionate effect on
    # Students of different Genders, First Generation Students, and Ethnicity students
mod1 <- polr(as.factor(Dominator) ~ Gender + FirstGeneration + CourseGrade + Ethnicity +
               ActivityType +  
               Gender*ActivityType + FirstGeneration*ActivityType + Ethnicity*ActivityType 
               , data=polrData, Hess=T)
  summary(mod1)
  AIC(mod1) # 1418.718
  
  
# Remove First Generation*ActivityType interaction because it is the interaction which 
  # has the t-value closest to zero
mod2 <- polr(as.factor(Dominator) ~ Gender + FirstGeneration + CourseGrade + Ethnicity +
                 ActivityType +  
                 Gender*ActivityType + Ethnicity*ActivityType 
               , data=polrData, Hess=T)
  summary(mod2)
  AIC(mod1,mod2) # 2 is better (lower AIC)
  
# Remove Ethnicity*ActivityTypeinteraction because it is the interaction which 
    # has the t-value closest to zero
mod3 <- polr(as.factor(Dominator) ~ Gender + FirstGeneration + CourseGrade + Ethnicity +
                 ActivityType +  
                 Gender*ActivityType 
               , data=polrData, Hess=T)
  summary(mod3)
  AIC(mod2,mod3) # 3 is better (lower AIC)

# Remove Gender*ActivityType interaction because it is the interaction which
    # has the t-value closest to zero
mod4 <- polr(as.factor(Dominator) ~ Gender + FirstGeneration + CourseGrade + Ethnicity +
                 ActivityType 
               , data=polrData, Hess=T)
  summary(mod4)
  AIC(mod3,mod4) # 4 is better (because it is simpler and the AIC is within 2)
  
# Remove First.Generation status because it has the t-value closest to zero
mod5 <- polr(as.factor(Dominator) ~ Gender + CourseGrade + Ethnicity +
                 ActivityType 
               , data=polrData, Hess=T)
  summary(mod5)
  AIC(mod4,mod5) # 5 is better (lower AIC)
  
# Remove Gender because it has the t-value closest to zero
mod6 <- polr(as.factor(Dominator) ~  CourseGrade + Ethnicity +
                 ActivityType 
               , data=polrData, Hess=T)
  summary(mod6)
  AIC(mod5,mod6) # 6 is better (lower AIC)

# Remove Ethnicity because it has the lowest t-value 
mod7 <- polr(as.factor(Dominator) ~ CourseGrade + 
                 ActivityType 
               , data=polrData, Hess=T)
  AIC(mod6,mod7) # 6 is better (need to put Ethnicity back in)

# Put Ethnicity back in, Remove Grade  
mod8 <- polr(as.factor(Dominator) ~ Ethnicity +
               ActivityType 
               , data=polrData, Hess=T)
  AIC(mod6,mod8) # 6 is better (need to put grade back in)

# Put grade back in, Remove ActivityType  
mod9 <- polr(as.factor(Dominator) ~ CourseGrade + Ethnicity
               , data=polrData, Hess=T)
  AIC(mod6,mod9) # 6 is better 

# Compare to the null
mod0 <- polr(as.factor(Dominator) ~ 1
               , data=polrData, Hess=T)
  AIC(mod6,mod0) # 6 is better 

# Best model:
mod6 <- polr(as.factor(Dominator) ~  CourseGrade + Ethnicity +
                 ActivityType 
               , data=polrData, Hess=T)
  summary(mod6)
  

#################################
### Testing Model Assumptions ###
#################################
##### Assumtion 1:
  # Are the categories ordered?
  # test this assumption by fitting a multinomial model 
  # if a multinomial model fits better (as determined with AIC), categories are not ordered
  
MultiNomMod <- multinom(Dominator ~ CourseGrade + Ethnicity +
                            ActivityType, data=polrData)
  AIC(MultiNomMod)
  AIC(mod6) # the polr model has lower AIC so is preferred

##### Assumtion 2:
    # Are the log-odds between each level proportional?

# create a function to create a new variable that is greater than or equal to each level of the outcome
    # and run a logistic regression with each lower level
  sf <- function(y) {
    c('Y>=1' = qlogis(mean(y >= 1)),
      'Y>=2' = qlogis(mean(y >= 2)),
      'Y>=3' = qlogis(mean(y >= 3)),
      'Y>=4' = qlogis(mean(y >= 4)),
      'Y>=5' = qlogis(mean(y >= 5)),
      'Y>=6' = qlogis(mean(y >= 6)))
  }
  
# Generate a table that describes the log-odds for each value of each response for level of each predictor
  # This is what we would get without the proportional odds assumption
  
  (s <- with(dominator, summary(as.numeric(Dominator) ~ CourseGrade + Ethnicity + ActivityType, fun=sf)))
  
  # for the assumption to be met, the difference between Y>=2 values and Y>=3 values 
  # should be the same as the difference between Y>=3 vs Y>=4; 
  # ie, the transition between each value of the outcome needs to be the same
  # for each level of each predictor 
  # when this is the case, this is saying that the probability of selection 2 vs. 3 is the same as the probability 
  # of selecting 3 vs. 4, etc. 
  
  
  # compare each level to the level higher; 
  # these numbers should be the same across the table for each row (ie, each level of each predictor)
  s[, 7] <- s[, 7] - s[, 6]
  s[, 6] <- s[, 6] - s[, 5]
  s[, 5] <- s[, 5] - s[, 4]
  s[, 4] <- s[, 4] - s[, 3] 
  s[, 3] <- s[, 3] - s[, 3] 
  s # print
  
  # these values look relatively reasonable
  # the direction is the same for each row
  # the magnitude changes some, especially for Ethnicity = Asian American, for example
  # could consider fitting models with clm in the ordinal package 
    # and relaxing the proportional odds assumption
  
  
#########################
### Interpret Results ###
#########################
summary(mod6)
# We see that after the interactive activity,
  # the log-odds of answering one point higher are -0.572 that of the log-odds on the Constructive activity
  
# To make the output easier to interpret, convert the coefficients to odds by exponentiating
  # Calculate the odds ratios and confidence intervals of the estimates and report a table:
exp(cbind(OR=coef(mod6),confint(mod6)))
# we see that all else equal, the odds of agreeing one level more that someone dominated your group:
  # increase on average by 0.76 for every additional point in course grade 
    # (students with higher course grades agree less that someone dominated their group)
  # increase on average by 1.67 for Asian-American students relative to white students
  # increase on average by 3.35 for international students relative to white students
  # increase on average by 1.39 for URM students relative to white students
    # (students of all ethnicities agree more that someone dominated their group, compared to white students)
  # increase on average by 0.56 on the interactive activity compared to the constructive activity
    # (students on the interactive activity agree less that someone dominated their group compared to the constructive activity)

# The Effects package can also help visualize the results
  # Here we plot each of the effects mentioned above:

# Plot the effect of Treatment on reporting a Dominator
domEff <- Effect("ActivityType",mod6)
  plot(domEff, style="stacked", ylab="Probability of Response", xlab= "Treatment", 
       main="Effect of Treatment on Reporting a Dominator")
  
# Plot the effect of Ethnicity on reporting a Dominator
EthnicitydomEff <- Effect("Ethnicity",mod6)
  plot(EthnicitydomEff, style="stacked", ylab="Probability of Response", xlab= "Ethnicity", 
       main="Effect of Ethnicity on Reporting a Dominator")
  
# Plot the effect of Course Grade on reporting a Dominator
GradedomEff <- Effect("CourseGrade",mod6)
  plot(GradedomEff, style="stacked", ylab="Probability of Response", xlab= "Course Grade", 
       main="Effect of Course Grade on Reporting a Dominator")
  
  
  