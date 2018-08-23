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
  # This code uses Poisson regression to analyze unbounded count data

#################################
### Load Statistical Packages ###
#################################

# to install packages if they are not already installed:
install.packages("MASS")  
install.packages("pscl") 
install.packages("effects") 

# to load packages:
library(MASS) # this has function for fitting a negative binomial model (a special case of Poisson)
library(pscl) # this conducts an overdispersion test and fitting a zero-inflated Poisson model (a special case of Poisson)
library(effects) # this is a package to help interpret results


#############################
### Set working Directory ###
#############################

#change setwd to folder where data is saved on your computer
setwd("")

###################################
### Load, Check, and Clean Data ###
###################################

PoissonData <- as.data.frame(read.csv("PoissonData.csv", header = TRUE))
  names(PoissonData)

# Data Set contains the following 3 variables:
  # HandRaises: a continuous variable indicating the number of times a student raised their hand in class during the course 
  # StudentMajor: a binary variable indicating whether a student is a physics major (0) or a non-physics major (1)
  # TotalExamPoints: a continuous variable indicating the number of total exam points obtain during the course

# plot a histogram as a way to check the data 
hist(PoissonData$HandRaises)

# Summarize the outcome data as a way to check the data
summary(PoissonData$HandRaises)

# Reminder: within the StudentMajor variable, 0 = physics majors and 1 = non-physics majors. 
# The model treats StudentMajor as a factor, therefore it must be assigned as a factor in the data set
PoissonData$StudentMajor<-as.factor(PoissonData$StudentMajor)

# Relevel the StudentMajor variable so that the reference group is physics majors
PoissonData$StudentMajor<-relevel(PoissonData$StudentMajor, ref="0")

# Caveats - this code does not do the following things.
  # These things (and others) are worth doing with any dataset:
    # looking for outliers, checking multicollinearity between predictors, etc.
    # Removing observations with NAs

  
#################
### Fit model ###
#################

# Model testing the variables we're interested in:
# i.e. whether students' major type and course grade predicts the number of 
# times they raise their hand during the course.
mod1 <- glm(formula = HandRaises ~ StudentMajor + TotalExamPoints, family = "poisson", data = PoissonData)
  summary(mod1)


#################################
### Testing Model Assumptions ###
#################################

##### Assumtion 1: Are the data overdispersed?
# Negative binomial models are similar to Poisson regression models, but 
  # don't require the variance of the response variable to be fixed at the mean.
  # Thus, negative binomial models are used when overdispersion is detected in your data.

# To determine if your data is overdispersed first run a negative binomial model 
  # using the glm.nb function, and then conduct an overdispersion test on 
  # that model. The output will include a p-value indicating whether your data is 
  # significantly overdispersed.

# An AIC value is also provided in the output of the negative binomial regression 
  # and can be compared to the AIC of the original Poisson regression. If the AIC 
  # of the negative binomial model is lower (by at least 2) than that of the Poisson 
  # regression model, the former better fits your data, and indicates your data is 
  # overdispsersed. 

# to run the negative binomial model:
mod2 <- glm.nb(HandRaises ~ StudentMajor + TotalExamPoints, data = PoissonData)
  summary(mod2)

# note that the output of the negative binomail model is formatted similarly to 
  # the output of the Poisson regression and can be interpreted in the same way
  # (see Appendix A5-1 for example on how to interpret the model output).

# to conduct the overdispersion test:
odTest(mod2)
  # This null hypothesis is that the data is Poisson distributed. 
    # The chi-squared test statistic (4095.6022) is greater than the critical value of the 
    # test statistic at alpha=0.05 (2.7055). This difference is statistically significant with 
    # a p value of <2.2e-16, indicating the data are overdispersed.


##### Assumtion 2: Does the data contain an excess of zeros?

# Datasets may contain excess zeros compared to what is expected under Poisson
  # or negative binomial models. Two options for addressing this are below, each
  # appropriate under slightly different conditions. 

# To conduct a zero-inflated poisson, i.e. when there are excess zeros and there are two 
  # underlying processes to explain why a zero was observed (e.g. a student didn't show up 
  # to class, as opposed to just didn't raise their hand). The zero-inflated Poisson is the 
  # appropriate choice to use for the purposes of our example.
mod3 <- zeroinfl(HandRaises ~ StudentMajor + TotalExamPoints, data = PoissonData)
  summary(mod3)

# To conduct a Hurdle Poisson (i.e. when there are excess zeros and there is only one 
  # underlying process to explain why a zero was observed). 
mod4 <- hurdle(HandRaises ~ StudentMajor + TotalExamPoints, data = PoissonData)
  summary(mod4)

# The zero-inflated Poisson and hurdle models produce two output tables. The first
  # provides the coefficients relevant to interpret the count model given there was a 
  # chance of a non-zero value occurring (in the zero-inflated Poisson model) or that 
  # a non-zero did occur (in the hurdle model). The second provides coefficients relevant
  # to interpreting the effect of the predictors on the likelihood of a zero value occurring.
  # See Appendix A for example on how to interpret the model output.

# The histogram indicates an excess of zeros. Additionally, our overdispersion test 
  # indicates our data is overdispersed. Thus we can account for both special cases by
  # designating the distribution as a negative binomial distribution (dist = "negbin") 
  # within our zero-inflated Poisson model.

# Fit the zero-inflated negative binomial model  
mod5 <- zeroinfl(HandRaises ~ StudentMajor + TotalExamPoints, data = PoissonData, dist = "negbin")
  summary(mod5)

# To check which of the models above best fits the data, the AIC value
  # of each can be compared, with the lowest value indicating the best model. 
  # To determine whether the negative binomial model (Model2) or the zero-inflated Poisson
AIC(mod2, mod5) # model5 (zero-inflated negative binomial) is preferred

  
#########################
### Interpret Results ###
#########################
summary(mod5)
# The Poisson regression model, negative binomial model, and count portions of the
  # hurdle and zero-inflated Poisson models automatically transform the data so that
  # the models are fit on the logarithmic scale. Thus, to ease in interpretation, we 
  # exponentiate the coefficients within each model to return odds ratios.

# Calculate the odds ratios and confidence intervals of the estimates and report a table:
(exp(cbind(OR=coef(mod5),confint(mod5))))


# The first part of the zero-inflated Poisson is the count model.
  # As with Poisson regression, it models the expected natural log counts per one-unit increase in the predictor variable.
  # We have converted the count model coefficients to a value representing the multiplicative effect of the predictor on the outcome variable.
  # Thus we can interpret these values as the following:
  # The expected number of hand raises for a non-physics major is 0.29 times the expected number of hand-raises for a physics major. 
  # Or: The expected number of hand-raises in class is reduced by 71% for non-physics majors compared to physics majors. 
  # Furthermore, for every 1-unit increase in exam points for a student there is a 0.8% increase in the expected number of hand-raises in class for that student.
  # Note that Major Status is the only significant predictor in our this model. The coefficient for total exam points is not significant and
  # thus this variable does not predict whether a student will raise their hand in class.

# The zero-inflation model coefficients have been converted to odds ratios, as this second part of the zero-
  # inflated Poisson is modeling the log-odds of a zero value occuring. Thus we can interpret these values as the following:
  # Being a non-physics major decreases the odds of being among those who never raise their hand by 0.07, or 7%
  # For every one-unit increase in total exam points, the odds of a student being among those who never raise their hand increases by 1.002, or 0.2%
  # Neither of these values are statistically significant, however, meaning that total exam points and a student's major do not predict
  # whether a student will never raise their hand.

# Note that if we were to interpret they output of model1 (our original Poisson regression), we would interpret it as we do in the main text of the manuscript.

# The Effects package (used in some of the other code samples from this paper) does not support
  # zero-inflated Poisson or hurdle Poisson models. As an alternative, a researcher could consider 
  # plotting the coefficients of the various models to visualize the data.

  
  