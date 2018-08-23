##############################
##############################
##                          ##
## Beyond Linear Regression ##
##			                    ##
##############################
##############################

# Full Citation: Beyond Linear Regression: a reference for analyzing common data types in education research
# Authors: Elli Theobald, Melissa Aikens, Sarah Eddy, Hannah Jordt
  # Corresponding Author Contact: ellij@uw.edu
  # Date: Spring 2019

# Description: 
  # This code uses multinomial regression to analyze categorical data that has 
  # more than two categories and which are not ordered in a particular way

################################
################################
##                            ##
##   Multinomial Regression   ##
##                            ##
################################
################################

#################################
### Load Statistical Packages ###
#################################

# to install packages if they are not already installed:
install.packages(nnet) 
install.packages(generalhoslem) 
install.packages(effects) 

# to load packages:
library(nnet) # Package that runs the multinomial regression
library(generalhoslem) # Package for model diagnostic test
library(effects) # Package to back translate model results to help with interepretation

#############################
### Set working Directory ###
#############################

#change setwd to folder where data is saved on your computer
setwd("")


###################################
### Load, Check, and Clean Data ###
###################################

# Load data
multinomialData <- read.csv("multinomialData.csv",header=T)
  head(multinomialData)

#multinomialData<-read.table(file="multinomial.csv",header=TRUE, sep=",")

# Data set contains the following 4 variables:
  # Gender: a binary variable indicating whether a student is male (M) or female (F)
  # GroupRole: a categorical variable indicating a student's preferred role when completing groupwork: 1 = Leader; 2 = Listener; 3 = Collaborator; 4 = Recorder; 5 = other
  # GPA: a continuous variable representing a student's college GPA (max: 4)
  # ClassStanding: a categorical variable indicating a student's class standing: Freshman, Sophomore, Junior, Senior, Fifth Year, Non-Matriculated

### Step 1: Set the appropriate reference level for the outcome variable  ###
  # In this case, we have theoretical reasons for choosing Collaborator (3) to be the reference.
  # 1 = Leader; 2 = Listener; 3 = Collaborator; 4 = Recorder; 5 = other

# First, we make sure R recognizes the outcome variable is a factor
multinomialData$GroupRole <- as.factor(multinomialData$GroupRole)

# then we set the reference level. In this code r stands for reference level.
multinomialData$GroupRole<-relevel(multinomialData$GroupRole, ref="3")

### Step 2: Data cleaning   ###
  # Because multinomial involves comparing models with different variables, we need to remove
  # any observations with missing data. Keeping observations with missing data would lead to comparing 
  # models with different sample sizes, which violates the assumptions of model comparision techniques.
  # An analyst could consider imputing missing data rather than removing it.

multinomialData <- na.omit(multinomialData)

# For the categorical predictor Class Standing we have several levels that have very few observations (< 5).
  # With such small samples we cannot conclude much about them statistically, so for the sake of simplicity,
  # we remove them and focus on the levels with the largest sample sizes (Junior, Senior, and Sophmore).

summary(multinomialData$ClassStanding)

multinomialData2 <- subset(multinomialData, ClassStanding!="Fifth Year")
multinomialData2 <- subset(multinomialData2, ClassStanding!="Freshman")
multinomialData2 <- subset(multinomialData2, ClassStanding!="Non-Matriculated")
multinomialData2 <- droplevels(multinomialData2)

# Check to be sure it worked
table(multinomialData2$ClassStanding)

# Finally, we center GPA, a Continuous predictor, around its mean. 
  # Centering a continuous predictor around 
  # its mean is recommended when there is no meaningful value of 
  # zero for your predictor variable (i.e. a GPA of zero is typically not observed).
  # The interpretation of the mdel becomes "at average GPA" as opposed to "at GPA 0"
multinomialData2$GPA <- multinomialData2$GPA-3.25

# To get a sense of what the data now looks like:
summary(multinomialData2)



#################
### Fit model ###
#################

# To understand the contribution of the individual variables to explaining the variance in the outcome variable
  # we use a model selection technique.  If including a variable does not increase the fit of the model to the data
  # then that is evidence it does not have much influence. We can either chose to leave it in or remove it depending
  # on the researcher's purpose.  In this case, I will remove any variables that do not contribute to explaining the 
# variance. Once we identify the appropriate model we can interpret the regression coefficients.

#Full model. This model includes all the possible predictors. 
mod1 <- multinom(GroupRole ~ Gender + ClassStanding + GPA + 
                   Gender*ClassStanding + Gender*GPA, data=multinomialData2)

# We test the contribution of each term by dropping it from the model 
  # and comparing this reduced model to a model that includes the term of interest.
  # As in backwards modle selection, start by considering the interaction terms.

# Test importance of Gender*ClassStanding (remove Gender*ClassStanding)
mod2 <- multinom(GroupRole ~ Gender + ClassStanding + GPA +
                   Gender*GPA, data=multinomialData2)

# Likelihood ratio test comparing the explanatory power of model 2 (reduced model) to model 1 (Full model)
anova(mod2, mod1)
# Interpretation:  No, improvement of fit by including Gender x ClassStanind (i.e., Pr(Chi) is not significant)
  # This indicates that this term does not explain much of the variance in the outcome variables across all the logistic models
  # So, we remove it.

# Test importance of Gender*GPA (remove Gender*GPA)
mod3 <- multinom(GroupRole ~ Gender + ClassStanding + GPA, data=multinomialData2)

# This time the likelihood ratio test compares the model without Gender*UWGPA (mod3) to mod2 which has already had ClassStanding*Gender removed
anova(mod3, mod2)
# Interpretation: Yes, including Gender*GPA increases the explanatory power of the models (i.e., Pr(Chi) is significant)
  # This indicates this term does explain variation in the outcome variable.
  # So, we keep the interaction and its main effects (GPA + Gender) in the model.

# Test importance of ClassStanding
mod4 <- multinom(GroupRole ~ Gender + GPA + Gender*GPA, data=multinomialData2)

# This time the likelihood ratio test compares the model without ClassStanding (mod4) to mod2 which has already had ClassStanding*Gender removed
anova(mod4, mod2)
# Interpretation:  No, improvement of fit by including ClassStanding (i.e., Pr(Chi) is not significant)
  # This indicates that this term does not explain much of the variance in the outcome variables across all the logistic models
  # So, we remove it.

# Compare best model to null model (with no predictors) to establish that variables 
  # increase the predictive ability of the model
  # If the final model fits better than the null model, then it has explanatory power.
mod0 <- multinom(GroupRole ~ 1, data=multinomialData2)
anova(mod0, mod4)
# Interpretion: mod4 is better than the null model, so is selected as best fitting

# Conclusion from model selection: These analyses indicate that Gender, UWgpa, and Gender*UWgpa 
# are the main predictors (of the set of predictors we collected) the role student's prefer during
# groupwork.  But, we don't yet know how specifically they impact this.


#################################
### Testing Model Assumptions ###
#################################
# The decrile of risk goodness of fit test asseses how well the model predicts the actual data.
  # If the model results are significantly different than the actual data, then the model is somehow misspecified.
  # Although, one usually wants to split the data out into 10 ranges, because of the small sample size 
  # in this data set, we could only split the data into 5 bins (g=5).

logitgof(multinomialData2$GroupRole, fitted(mod4), g=5)
# Interpretation: The model predicted roles students prefer do not significantly differ from the raw data.
  # This indicates the model is functioning reasonably well.


#########################
### Interpret Results ###
#########################
summary(mod4)

# The Wald test statistic is not automatically included so we can follow the example from 
  # https://stats.idre.ucla.edu/r/dae/multinomial-logistic-regression/
  # to create this test statistic (Wald below)

(Wald <- summary(mod4)$coefficients/summary(mod4)$standard.errors)

# From the Wald statistic (z) we can derive a p-value for each predictor in each logistic model. 

(p <- (1 - pnorm(abs(Wald), 0, 1)) * 2)

# The coefficients are the on the log-odds scale, so we can make them easier to interpret by exponeniating them 
  # This puts on the log scale and allow us to interpret them as odds ratios
exp(coef(mod4))


# The effects package combines the results from all the logistic regressions to give us global insight
  # into the impact of the predictors on the outcome overall
  # Because one of our predictors is continuous (GPA), we set the range overwhich we want to see output.
  # In this case we see results for gender for values of GPA between -.25 and .75 and we want
  # it on .25 intervals (-.25, 0, .25, .5, .75)

# By placing summary in front of effect, R returns the lower and uppper confidence limits 
  # on the estimates as well as the estimate itself

summary(effect("Gender:GPA", mod4, xlevels=list(GPA=seq(-.25, .75, .25))))

# Plot the effect of GPA for students of different Genders
GenderGPAeff <- effect("Gender:GPA", mod4)
plot(GenderGPAeff, style="stacked", ylab="Probability of Response", xlab= "GPA (centered)", 
     main="Group Role ~ Gender*GPA")
