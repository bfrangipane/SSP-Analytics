# Bret Frangipane
# December 3, 2017
# 
# I wrote this script to model survey data for the nonprofit Sierra Service Project (SSP).
# They have permitted me to share it with future employers.
# Here I use regression and decision trees to model three metrics in the survey data:
# 'Overall Volunteer Satisfaction,' 'Would the Volunteer Recommend SSP?' and 
# 'Would the Volunteer participate in SSP again?' My objective is to determine what is 
# driving changes in those variables. 

library(readxl)      # Reads Excel Data
library(rpart)       # Creates Decision Trees
library(rpart.plot)  # Plots Decision Trees
library(RGtk2)       # Plots Decision Trees
library(rattle)      # Plots Decision Trees


#######################
##    Read  Data     ##
#######################

# Set the working Directory:
setwd("~/Documents/Consulting + Jobs/SSP Fall 2017")

# Read in Adult Survey Data Files:
adult.surveys.2016 <- read_excel("Summer Counselor Survey - 2016.xlsx", sheet = 1)
adult.surveys.2017 <- read_excel("2017 Counselors.xlsx", sheet = 1)

# Read in Youth Survey Data Files:
youth.surveys.2016 <- read_excel("Summer Youth Survey - 2016.xlsx", sheet = 1)
youth.surveys.2017 <- read_excel("2017 Youth.xlsx", sheet = 1)


#######################
##    Clean Data     ##
#######################

CleanSurveyData = function(surveys, year, is.youth.data) {
  # Wrapper function that removes the three dependent variables, 
  #   and fixes a mistake in the survey data.
  # 
  # Args: 
  #  surveys: Data Frame - Contains surveys data. Rows represent a response. 
  #           Columns represent survey question.
  #  year: Integer - The year the survey data corresponds to. Either 2016 or 2017. 
  #  is.youth.data: Boolean - If T, surveys are from youth volunteers. If F, from adult volunteers. 
  # 
  # Returns: 
  #  return.objects: a list of length four, including:
  #    1. Cleaned survey data.
  #    2. vol.recommend: a vector of volunteer responses to whether they would recommend program.
  #    3. vol.satisfaction: a vector of volunteer responses to how satisfied they were with program.
  #    4. vol.likely.to.return: a vector of responses to whether they would return to ssp.

  ## Remove the Three Dependent Variables from Predictors ##
  # 1. Volunteer Recommendation (Responses: No, Maybe, Yes)
  if (is.youth.data) { 
    vol.recommend <- surveys$`Would you recommend SSP to your friends or other young people?` 
    surveys$`Would you recommend SSP to your friends or other young people?` <- NULL
  } else {
    vol.recommend <- surveys$`Would you recommend SSP to other parents and colleagues?`   
    surveys$`Would you recommend SSP to other parents and colleagues?` <- NULL
  }
  
  # 2. Volunteer Satisfaction (Responses: rating 1-5)
  vol.satisfaction <- surveys$`Overall, how satisfied were you with this experience?`
  surveys$`Overall, how satisfied were you with this experience?` <- NULL
  
  # 3. Volunteer Likelihood to Return (Responses: No, Maybe, Yes)
  vol.likely.to.return <- surveys$`Would you attend this program again?`
  surveys$`Would you attend this program again?` <- NULL
  
  
  ## Fix mistake in the survey data ##
  if (is.youth.data) {
    unique(surveys$`What week did you attend?`)
    # [1] "Week 1 (start date July 2)"  "Week 2 (start date July 9)"  "Week 3 (start date July 16)"
    # [4] "Week 4 (start date July 23)" "Week 5(start date July 30)"  "Week 6 (start date July 31)"
    
    # Correct the starting date from July 31 to August 6:
    surveys$`What week did you attend?`[surveys$`What week did you attend?` == "Week 6 (start date July 31)"] <- "Week 6 (start date August 6)"
   
    # Add space after '5':
    surveys$`What week did you attend?`[surveys$`What week did you attend?` == "Week 5(start date July 30)"] <- "Week 5 (start date July 30)"
    }
  
  return.objects = list(surveys, vol.recommend, vol.satisfaction, vol.likely.to.return)
  return(return.objects)
  
} # End of function CleanSurveyData
  
  

#######################
##  Tree Analysis    ##
#######################
  

PrepareForTrees = function(clean.surveys) {
  # Subsets survey data to include only survey questions appropriate for decision trees,
  #   that is, data that is either numeric or factorlike.
  #
  # Args:
  #   clean.surveys: data.frame - contains cleaned surveys. 
  # 
  # Returns: 
  #   data.cleaned.for.trees - data ready for making decision trees.
  
  
  KeepColumnsForTrees <- function(data.column) {
    # Determines whether data.column should be used in decision tree. 
    # 
    # Args: 
    #  data.column: vector - contains the responses to a survey question. 
    # 
    # Returns:
    #  boolean - T if data.column is class numeric or factor or has fewer than 15 unique responses.
    # 
    if (is.numeric(data.column)) {
      return(T) 
    } else if (is.factor(data.column)) {
      return(T)
    } else if (length(unique(data.column)) < 15) {
      return(T)
    } 
    return(F)
  }
  
  columns.for.trees <- sapply(clean.surveys, KeepColumnsForTrees)  # returns a boolean vector with T for every column to keep. 
  data.cleaned.for.trees <- clean.surveys[ , columns.for.trees]  
  
  return(data.cleaned.for.trees)
} # End of function PrepareForTrees
  

## Build Tree Models ##
BuildDecisionTrees = function(packaged.data) {
  # Builds Decision Trees
  # 
  # Args: 
  #   packaged.data: a list of length four, including: 
  #     1. Prepped survey data from PrepareForTrees
  #     2. vol.recommend: a vector of volunteer responses to whether they would recommend program.
  #     3. vol.satisfaction: a vector of volunteer responses to how satisfied they were with program.
  #     4. vol.likely.to.return: a vector of responses to whether they would recommend program.
  # 
  # Returns: 
  #   trees: a list of three trees, modeling:
  #     1. Volunteer Recommendation
  #     2. Volunteer Satisfaction
  #     3. Volunteer Likelihood to Return
  
  data.cleaned.for.trees <- packaged.data[[1]]  # Comes from function PrepareForTrees
  vol.recommend          <- packaged.data[[2]]
  vol.satisfaction       <- packaged.data[[3]]  
  vol.likely.to.return   <- packaged.data[[4]]

  # 1. Decision Tree to Predict Volunteer Recommendation
  tree.recommend <- rpart(vol.recommend ~ ., data = data.cleaned.for.trees, maxdepth = 2)
  fancyRpartPlot(tree.recommend, main = "Volunteer Recommendation")  # Prints plot of tree
  
  # 2. Decision Tree to Predict Volunteer Satisfaction
  tree.satisfaction <- rpart(vol.satisfaction ~ ., data = data.cleaned.for.trees, maxdepth = 2)
  fancyRpartPlot(tree.satisfaction, main = "Volunteer Satisfaction")  # Prints plot of tree
  
  # 3. Decision Tree to Predict Volunteer Likelihood to Return
  tree.likely.to.return <- rpart(vol.likely.to.return ~ ., data = data.cleaned.for.trees, maxdepth = 2)
  fancyRpartPlot(tree.likely.to.return, main = "Volunteer Likelihood to Return")  # Prints plot of tree
  
  trees <- list(tree.recommend, tree.satisfaction, tree.likely.to.return)
  return(trees)
}
  
#######################
##    Regression     ##
#######################

PrepareForRegression = function(clean.surveys) {
  # Subsets survey data to include only questions appropriate for regression.
  #
  # Args:
  #   clean.surveys: data.frame - contains cleaned surveys. 
  # 
  # Returns: 
  #   regression.package - list of two including:
  #     1. survey data with which to build regression model.
  #     2. char vector of variables names for this survey data.
  
  KeepColumnsForRegression <- function(data.column) {
    # Determines whether data.column should be used in decision tree. 
    # 
    # Args: 
    #  data.column: vector - contains the responses to a survey question.
    # 
    # Returns: 
    #   boolean - T only if data.column is class numeric and has fewer than 5% missing values.
    if (is.numeric(data.column)) {
      if (sum(is.na(data.column))/length(data.column) < .05) {
        return(T)
      }
    } 
    return(F)
  }
  
  columns.for.regression <- sapply(clean.surveys, KeepColumnsForRegression)  # returns a boolean vector with T for every column to keep.
  data.cleaned.for.regression <- clean.surveys[ , columns.for.regression]
  
  regression.variables <- names(data.cleaned.for.regression)
  names(data.cleaned.for.regression) <- 1:length(regression.variables)  # For concise reading of results, replace questions with numbers.
  
  regression.package <- list(data.cleaned.for.regression, regression.variables)
  return(regression.package)
}  # End of function Prepare for Regression

BuildRegressionModels = function(packaged.data) {
  # Builds Ordinary and Logistic Regression Models, depending on type of dependent variable. 
  # 
  # Args: 
  #   packaged.data: a list of length four, including: 
  #     1. Prepped survey data.
  #     2. vol.recommend: a vector of volunteer responses to whether they would recommend program.
  #     3. vol.satisfaction: a vector of volunteer responses to how satisfied they were with program.
  #     4. vol.likely.to.return: a vector of responses to whether they would recommend program.
  # 
  # Returns: 
  #   trees: a list of three regressions, modeling:
  #     1. Volunteer Recommend
  #     2. Volunteer Satisfaction
  #     3. Volunteer Likelihood to Return
  
  data.cleaned.for.regression <- packaged.data[[1]]
  vol.recommend               <- packaged.data[[2]]
  vol.satisfaction            <- packaged.data[[3]]  
  vol.likely.to.return        <- packaged.data[[4]]
  
  
  # Logistic Regression to Predict Boolean Volunteer Recommendation 
  vol.recommend.yes <- grepl("Yes", vol.recommend)  # Logical vector where T means volunteer responded "Yes", would recommend SSP.
  logistic.reg.recommend <- glm(as.numeric(vol.recommend.yes) ~., data = data.cleaned.for.regression)

  # Ordinary Regression to Predict Integer Volunteer Satisfaction
  ordinary.reg.satisfaction <- glm(vol.satisfaction ~., data = data.cleaned.for.regression, na.action = na.omit)
 
  # Logistic Regression to Predict Volunteer Boolean Likelihood to Return
  vol.return.yes <- grepl("Yes", vol.likely.to.return) # Logical vector where T means volunteer responded "Yes", would return to SSP.
  logistic.reg.return <- glm(vol.return.yes ~., data = data.cleaned.for.regression)
  
  regressions <- list(logistic.reg.recommend, ordinary.reg.satisfaction, logistic.reg.return)
  return(regressions)
}



#######################
##    Run Models     ##
#######################

## Decision Trees ##
# Decision Trees for Adult Volunteers in 2016
cleaned.package <- CleanSurveyData(adult.surveys.2016, 2016, is.youth.data = F)
cleaned.package[[1]] <- PrepareForTrees(cleaned.package[[1]])
adult.trees.2016 <- BuildDecisionTrees(cleaned.package)

# Decision Trees for Adult Volunteers in 2017
cleaned.package <- CleanSurveyData(adult.surveys.2017, 2017, is.youth.data = F)
cleaned.package[[1]] <- PrepareForTrees(cleaned.package[[1]])
adult.trees.2017 <- BuildDecisionTrees(cleaned.package)

# Decision Trees for Youth Volunteers in 2016
cleaned.package <- CleanSurveyData(youth.surveys.2016, 2016, is.youth.data = T)
cleaned.package[[1]] <- PrepareForTrees(cleaned.package[[1]])
youth.trees.2017 <- BuildDecisionTrees(cleaned.package)

# Decision Trees for Youth Volunteers in 2017
cleaned.package <- CleanSurveyData(youth.surveys.2017, 2017, is.youth.data = T)
cleaned.package[[1]] <- PrepareForTrees(cleaned.package[[1]])
youth.trees.2017 <- BuildDecisionTrees(cleaned.package)


## Regression Models ##
# Regression Models for Adult Volunteers in 2016
cleaned.package <- CleanSurveyData(adult.surveys.2016, 2016, is.youth.data = F)
prepped.data <- PrepareForRegression(cleaned.package[[1]])
cleaned.package[[1]] <- prepped.data[[1]]
adult.regression.2016.vars <- prepped.data[[2]]
adult.regression.2016 <- BuildRegressionModels(cleaned.package)

# Regression Models for Adult Volunteers in 2017
cleaned.package <- CleanSurveyData(adult.surveys.2017, 2017, is.youth.data = F)
prepped.data <- PrepareForRegression(cleaned.package[[1]])
cleaned.package[[1]] <- prepped.data[[1]]
adult.regression.2017.vars <- prepped.data[[2]]
adult.regression.2017 <- BuildRegressionModels(cleaned.package)

# Regression Models for Youth Volunteers in 2016
cleaned.package <- CleanSurveyData(youth.surveys.2016, 2016, is.youth.data = T)
prepped.data <- PrepareForRegression(cleaned.package[[1]])
cleaned.package[[1]] <- prepped.data[[1]]
youth.regression.2016.vars <- prepped.data[[2]]
youth.regression.2016 <- BuildRegressionModels(cleaned.package)

# Regression Models for Youth Volunteers in 2017
cleaned.package <- CleanSurveyData(youth.surveys.2017, 2017, is.youth.data = T)
prepped.data <- PrepareForRegression(cleaned.package[[1]])
cleaned.package[[1]] <- prepped.data[[1]]
adult.regression.2017.vars <- prepped.data[[2]]
adult.regression.2017 <- BuildRegressionModels(cleaned.package)


# Display Results: 
print(adult.regression.2016.vars)
summary(adult.regression.2016[[1]])
summary(adult.regression.2016[[2]])
summary(adult.regression.2016[[3]])

print(adult.regression.2017.vars)
summary(adult.regression.2017[[1]])
summary(adult.regression.2017[[2]])
summary(adult.regression.2017[[3]])

print(youth.regression.2016.vars)
summary(youth.regression.2016[[1]])
summary(youth.regression.2016[[2]])
summary(youth.regression.2016[[3]])

print(youth.regression.2017.vars)
summary(youth.regression.2017[[1]])
summary(youth.regression.2017[[2]])
summary(youth.regression.2017[[3]])


# One example of regression model output (variables are masked intentionally):
# 
# Call:
#   glm(formula = as.numeric(vol.recommend.yes) ~ ., data = data.cleaned.for.regression)
# 
# Deviance Residuals: 
#   Min        1Q    Median        3Q       Max  
# -1.01142  -0.04784   0.00229   0.09312   0.50214  
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.092662   0.134284  -0.690 0.490744    
# `1`          0.074305   0.021480   3.459 0.000627 ***
# `2`         -0.010752   0.024699  -0.435 0.663662    
# `3`          0.018363   0.027193   0.675 0.500062    
# `4`         -0.008136   0.016907  -0.481 0.630714    
# `5`         -0.025604   0.015523  -1.649 0.100190    
# `6`          0.003887   0.014514   0.268 0.789025    
# `7`          0.015557   0.015502   1.004 0.316456    
# `8`          0.038029   0.020585   1.847 0.065744 .  
# `9`          0.055766   0.024737   2.254 0.024952 *  
# `10`         0.015298   0.020706   0.739 0.460643    
# `11`         0.013242   0.020931   0.633 0.527480    
# `12`        -0.013790   0.015791  -0.873 0.383264    
# `13`        -0.056873   0.028125  -2.022 0.044115 *  
# `14`         0.009534   0.025804   0.369 0.712057    
# `15`         0.018746   0.024033   0.780 0.436056    
# `16`         0.076157   0.032160   2.368 0.018565 *  
# `17`        -0.012950   0.029779  -0.435 0.663981    
# `18`         0.019741   0.030744   0.642 0.521336    
# `19`        -0.002678   0.026197  -0.102 0.918655    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for gaussian family taken to be 0.04318377)
# 
# Null deviance: 18.658  on 297  degrees of freedom
# Residual deviance: 12.005  on 278  degrees of freedom
# (13 observations deleted due to missingness)
# AIC: -69.418
# 
# Number of Fisher Scoring iterations: 2

# Strong evidence that when controlling for all other variables, variable 1 has a positive relationship with the metric, 
# 'Would you recommend this proram'. Recommendation: focus efforts to further improve variable 1.
