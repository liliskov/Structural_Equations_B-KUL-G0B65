# Structural Equations -----------------------------------------------------------------------------------
# Description: Assignment
# Author: Lili Vandermeersch r0691855
# input: 
# output: 
# File history:
#   190423: creation
#
# Paths ----------------------------------------------------------------------------------------------------
rm(list =ls()) # clears R environment
options(scipen=999) # disables scientific notation

setwd('/Users/lilivandermeersch/Library/Mobile Documents/com~apple~CloudDocs/❤️ MSc Statistics and Data Science ❤️')
# input <- file.path(file.choose())
# output <- file.path(file.choose())

# Packages -------------------------------------------------------------------------------------------------

### DATA MANIPULATION ###
library("haven")        
library("dplyr")      
library("psych")
library('stringr')

### MODELING ###
library("lavaan")  

### VISUALIZATION ###
library("corrplot")     
library("tidySEM") 
library("scatterplot3d")
library("plotly")
library("semPlot")

# Checking assumptions
library("MVN")

##########################################################################################
# Importing the data
ess_df <- haven::read_sav("/Users/lilivandermeersch/Library/Mobile Documents/com~apple~CloudDocs/❤️ MSc Statistics and Data Science ❤️/☀️🌸 Year 3./II./SEM/Assignment/ESS9/ESS-Data-Wizard-subset-2023-04-30/ESS9")

# Country EITHER/OR
# HU only
ess_df <- filter(ess_df, cntry == 'HU')
# BE only
# ess_df <- filter(ess_df, cntry == 'BE')

# Data Exploration ------------------------------------------------------------------
View(ess_df) 

nrow(ess_df)      # number of subjects
ncol(ess_df)      # number of variables
names(ess_df)     # names of variables

# Subsetting the data
ess_df_selected <- ess_df %>% select(
    ##   Attitudes towards Immigrants (Poltics/Immigration policy) (ATI) ## 
  impcntr, # Allow many/few immigrants from poorer countries outside Europe
  imbgeco, # Immigration bad or good for the country's economy
  imueclt, # Country's cultural life undermined or enriched by immigrants
  imwbcnt, # Immigrants make the country worse or better place to live
    ##  Personal Values (PV) ##
  ipcrtiv, # Important to think of new ideas and be creative
  ipudrst, # Important to understand different people
  ipmodst, # Important to be humble and modest, not draw attention
  ipgdtim, # Important to have a good time
  iphlppl, # Important to help people and care for others well-being
  impenv, # Important to care for nature and environment
  ##   Political procedural justice (J) ##
  sofrdst, # Society fair when income and wealth is equally distributed
  sofrwrk,  # Society fair when hard-working people earn more than others
  sofrpr, # Society fair when takes care of poor and in need, regardless of what give back
  sofrprv, # Society fair when people from families with high social status enjoy privileges
  ppldsrv, # By and large, people get what they deserve
  jstprev, # Confident that justice always prevails over injustice
  pcmpinj # Convinced that in the long run people compensated for injustices
)

# Structure and summary of data
str(ess_df_selected)
ess_df_selected %>% glimpse()
summary(ess_df_selected)

descriptive_ess <- as.data.frame(psych::describe(ess_df_selected))

descriptive_ess <- dplyr::select(descriptive_ess, 
                                 n,
                                 mean,
                                 sd,
                                 median,
                                 min,
                                 max,
                                 skew,
                                 kurtosis)

descriptive_ess

# Handling missing data --------------------------------------------------------------
# How many missing data do we have?
ess_df_selected_complete <- ess_df_selected %>% na.omit()
num_missing_obs <- nrow(ess_df_selected) - nrow(ess_df_selected_complete)
# Significant number (proportionally) of missing data. Can't be sure if all MCAR, 
# in fact probably not => ***listwise*** deletion not recommended, might introduce bias,
# decrease stat power.

# ***Case-wise*** (or ‘full information’) maximum likelihood (FIML) => The default in CFA lavaan

# ***Multiple imputation***
# Mice => more flexible with different data types than Amelia
# Amelia was used during lectures
# MICE:
# Perform multiple imputation for missing data
## => Revisit later!
######################################################################################
# Filtering out careless respondents -------------------------------------------------
# Calculate the Mahalanobis distance for each respondent
# mahalanobis_distances <- mahalanobis(ess_df_selected, center = colMeans(ess_df_selected), cov = cov(ess_df_selected))

# Add the Mahalanobis distances to the dataset
# ess_df_selected$Mahalanobis <- mahalanobis_distances

# Set a threshold for flagging potential careless respondents
# You can adjust the threshold depending on your data and specific needs
# threshold <- qchisq(0.001, df = ncol(ess_df_selected), lower.tail = FALSE)

# Flag respondents with Mahalanobis distances above the threshold
# careless_respondents <- ess_df_selected$Mahalanobis > threshold

# Filter out the careless respondents
# filtered_data1 <- ess_df_selected[!careless_respondents, ]

# Nothing to filter out!

#####################################################################################
### EFA ###
#####################################################################################
# Standardization
ess_df_selected_standardized <- data.frame(scale(ess_df_selected))
cor_matrix <- cor(ess_df_selected_standardized, use="pairwise.complete.obs")
corrplot(cor_matrix, method="color", type="upper", tl.col="black", tl.srt=45)
# Preliminary tests - if the correlation matrix is suitable for factor analysis
## Kaiser–Meyer–Olkin (KMO)
KMO(cor_matrix)
## Bartlett’s test of sphericity
cortest.bartlett(R = cor_matrix, n = 1399)
det(cor_matrix)
eigen(cor_matrix)$values
# Perform parallel analysis
# par_analysis <- fa.parallel(ess_df_selected, n.iter = 100, fa = "both")
# Parallel analysis suggests that the number of factors =  8 (EFA) and the number of components =  6 (PCA)
# or
number_items <- fa.parallel(ess_df_selected_standardized,
                            fm ='ml',
                            fa ="fa")
# Solid line -> original Kaiser criteria => 3
# Parallel => Red simulated and re-sampled data => Where the blue and red lines 
# intersect => everything above, those eigen values can't be by chance => 4
# Screeplot -> infliction point: 3
# Eigenvalues
sum(number_items$fa.values > 1) # Kaiser
sum(number_items$fa.values > 0.7) # Less conservative Kaiser

##################
# EFA - 3 factors separately  -------------------------------------------------------------
##################    
### ATI ###
###########
ess_df_selected_ATI <- ess_df %>% select(
  ##   Attitudes towards Immigrants (ATI) ## 
  impcntr, # Allow many/few immigrants from poorer countries outside Europe
  imbgeco, # Immigration bad or good for the country's economy
  imueclt, # Country's cultural life undermined or enriched by immigrants
  imwbcnt # Immigrants make the country worse or better place to live
) 

ess_df_selected_ATI_standardized <- data.frame(scale(ess_df_selected_ATI))

cor_matrix <- cor(ess_df_selected_ATI_standardized, use="pairwise.complete.obs")
corrplot(cor_matrix, method="color", type="upper", tl.col="black", tl.srt=45)

number_items <- fa.parallel(ess_df_selected_ATI_standardized,
                            fm ='ml',
                            fa ="fa")
# Solid line -> original Kaiser criteria => 1
# Parallel => Red simulated and re-sampled data => Where the blue and red lines 
# intersect => everything above, those eigen values can't be by chance => 1
# Screeplot -> infliction point: 1
# Eigenvalues
sum(number_items$fa.values > 1) # Kaiser
sum(number_items$fa.values > 0.7) # Less conservative Kaiser

# Specify the number of factors to extract (you can try different numbers)
num_factors <- 1

# Perform EFA
efa_results <- fa(ess_df_selected_ATI_standardized, nfactors = num_factors, rotate = "oblimin")

# Print EFA results
print(efa_results)

# Plot factor loadings
fa.diagram(efa_results)

fa.plot(efa_results,
        labels = colnames(ess_df_selected_ATI_standardized))

#----------------------------------------------------------------------------------
###########  
### PV ###
###########

ess_df_selected_PV <- ess_df %>% select(
  ##  Personal Values (PV) ##
  #  ipcrtiv, # Important to think of new ideas and be creative
  ipudrst, # Important to understand different people
  ipmodst, # Important to be humble and modest, not draw attention
  #  ipgdtim, # Important to have a good time
  iphlppl, # Important to help people and care for others well-being
  impenv # Important to care for nature and environment
)

ess_df_selected_PV_standardized <- data.frame(scale(ess_df_selected_PV))

number_items <- fa.parallel(ess_df_selected_PV_standardized,
                            fm ='ml',
                            fa ="fa")
# Solid line -> original Kaiser criteria => 1
# Parallel => Red simulated and re-sampled data => Where the blue and red lines 
# intersect => everything above, those eigen values can't be by chance => 1
# Screeplot -> infliction point: 1
# Eigenvalues
sum(number_items$fa.values > 1) # Kaiser
sum(number_items$fa.values > 0.7) # Less conservative Kaiser

# Specify the number of factors to extract (you can try different numbers)
num_factors <- 1

# Perform EFA
efa_results <- fa(ess_df_selected_PV_standardized, nfactors = num_factors, rotate = "oblimin")

# Print EFA results
print(efa_results)

# Plot factor loadings
fa.diagram(efa_results)

fa.plot(efa_results,
        labels = colnames(ess_df_selected_PV_standardized))

#----------------------------------------------------------------------------------
###########  
### J ###
###########

ess_df_selected_J <- ess_df %>% select(
  ##   Political procedural justice (J) ##
#  sofrdst, # Society fair when income and wealth is equally distributed
#  sofrwrk,  # Society fair when hard-working people earn more than others
#  sofrpr, # Society fair when takes care of poor and in need, regardless of what give back
  sofrprv, # Society fair when people from families with high social status enjoy privileges
  ppldsrv, # By and large, people get what they deserve
  jstprev, # Confident that justice always prevails over injustice
  pcmpinj # Convinced that in the long run people compensated for injustices
)


ess_df_selected_J_standardized <- data.frame(scale(ess_df_selected_J))

number_items <- fa.parallel(ess_df_selected_J_standardized,
                            fm ='ml',
                            fa ="fa")
# Solid line -> original Kaiser criteria => 1
# Parallel => Red simulated and re-sampled data => Where the blue and red lines 
# intersect => everything above, those eigen values can't be by chance => 1
# Screeplot -> infliction point: 1
# Eigenvalues
sum(number_items$fa.values > 1) # Kaiser
sum(number_items$fa.values > 0.7) # Less conservative Kaiser

# Specify the number of factors to extract (you can try different numbers)
num_factors <- 1

# Perform EFA
efa_results <- fa(ess_df_selected_J_standardized, nfactors = num_factors, rotate = "oblimin")

# Print EFA results
print(efa_results)

# Plot factor loadings
fa.diagram(efa_results)

fa.plot(efa_results,
        labels = colnames(ess_df_selected_J_standardized))

# sofrdst, sofrpr, sofwrk removed (low loadings on underlying factor < .3) => went back to redo the analysis.

##################
# EFA - 3 factors together  -------------------------------------------------------------
##################    
ess_df_selected2 <- ess_df_selected %>% select(
  ##   Attitudes towards Immigrants (Poltics/Immigration policy) (ATI) ## 
  impcntr, # Allow many/few immigrants from poorer countries outside Europe
  imbgeco, # Immigration bad or good for the country's economy
  imueclt, # Country's cultural life undermined or enriched by immigrants
  imwbcnt, # Immigrants make the country worse or better place to live
  ##  Personal Values (PV) ##
#  ipcrtiv, # Important to think of new ideas and be creative
  ipudrst, # Important to understand different people
  ipmodst, # Important to be humble and modest, not draw attention
#  ipgdtim, # Important to have a good time
  iphlppl, # Important to help people and care for others well-being
  impenv, # Important to care for nature and environment
  ##   Political procedural justice (J) ##
#  sofrdst, # Society fair when income and wealth is equally distributed
#  sofrwrk,  # Society fair when hard-working people earn more than others
#  sofrpr, # Society fair when takes care of poor and in need, regardless of what give back
  sofrprv, # Society fair when people from families with high social status enjoy privileges
  ppldsrv, # By and large, people get what they deserve
  jstprev, # Confident that justice always prevails over injustice
  pcmpinj # Convinced that in the long run people compensated for injustices
)
ess_df_selected_standardized2 <- data.frame(scale(ess_df_selected2))

# Perform EFA
num_factors = 3
efa_results <- fa(ess_df_selected_standardized2, nfactors = num_factors, rotate = "oblimin")

# Print EFA results
print(efa_results)

# Plot factor loadings
fa.diagram(efa_results)

fa.plot(efa_results,
        labels = colnames(ess_df_selected_standardized))

#####################################################################################
### Single-factor CFA ###
#####################################################################################
###########  
### ATI ###
###########

# Non-standardized ATI items 
ess_df_selected_ATI

# Sample observed covariance matrix
selected_ATI_cov <- cov(ess_df_selected_ATI,          # data frame
                             use = "pairwise.complete.obs" # remove NAs
)

selected_ATI_cov

selected_ATI_cor <- cov2cor(selected_ATI_cov)
selected_ATI_cor

corrplot::corrplot(selected_ATI_cor,
                   is.corr = FALSE,       # whether is a correlation matrix
                   method = "circle",     # magnitude of covariances as circles
                   type = "upper",        # remove the bottom of the covariance matrix
                   addCoef.col = "black"  # add to the plot the coefficients
)

model_ATI_4 <- '
ATI =~ impcntr + imbgeco + imueclt + imwbcnt
'

fit_ATI_4 <- cfa(model_ATI_4,             # model formula
                data = ess_df_selected_ATI  # data frame
)

summary(fit_ATI_4, 
        fit.measures = TRUE, # returns commonly used fit measures  
        standardized = TRUE  # indicates that we want standardized results
)

###########  
### PV ###
###########

# Non-standardized PV items 
ess_df_selected_PV <- ess_df %>% select(
  ##  Personal Values (PV) ##
  ipcrtiv, # Important to think of new ideas and be creative
  ipudrst, # Important to understand different people
  ipmodst, # Important to be humble and modest, not draw attention
  ipgdtim, # Important to have a good time
  iphlppl, # Important to help people and care for others well-being
  impenv # Important to care for nature and environment
)

# Sample observed covariance matrix
selected_PV_cov <- cov(ess_df_selected_PV,          # data frame
                        use = "pairwise.complete.obs" # remove NAs
)

selected_PV_cov

selected_PV_cor <- cov2cor(selected_PV_cov)
selected_PV_cor

corrplot::corrplot(selected_PV_cor,
                   is.corr = FALSE,       # whether is a correlation matrix
                   method = "circle",     # magnitude of covariances as circles
                   type = "upper",        # remove the bottom of the covariance matrix
                   addCoef.col = "black"  # add to the plot the coefficients
)

model_PV_6 <- '
PV =~ ipcrtiv + ipudrst + ipmodst + ipgdtim + iphlppl + impenv 
'

fit_PV_6 <- cfa(model_PV_6,             # model formula
                 data = ess_df_selected_PV  # data frame
)

summary(fit_PV_6, 
        fit.measures = TRUE, # returns commonly used fit measures  
        standardized = TRUE  # indicates that we want standardized results
)

#########  
### J ###
#########

# Non-standardized PV items 
ess_df_selected_J <- ess_df %>% select(
  ##   Political procedural justice (J) ##
  #  sofrdst, # Society fair when income and wealth is equally distributed
  #  sofrwrk,  # Society fair when hard-working people earn more than others
  #  sofrpr, # Society fair when takes care of poor and in need, regardless of what give back
  sofrprv, # Society fair when people from families with high social status enjoy privileges
  ppldsrv, # By and large, people get what they deserve
  jstprev, # Confident that justice always prevails over injustice
  pcmpinj # Convinced that in the long run people compensated for injustices
)

# Sample observed covariance matrix
selected_J_cov <- cov(ess_df_selected_J,          # data frame
                       use = "pairwise.complete.obs" # remove NAs
)

selected_J_cov

selected_J_cor <- cov2cor(selected_J_cov)
selected_J_cor

corrplot::corrplot(selected_J_cor,
                   is.corr = FALSE,       # whether is a correlation matrix
                   method = "circle",     # magnitude of covariances as circles
                   type = "upper",        # remove the bottom of the covariance matrix
                   addCoef.col = "black"  # add to the plot the coefficients
)

model_J_4 <- '
J =~ sofrprv + ppldsrv + jstprev + pcmpinj
'

fit_J_4 <- cfa(model_J_4,             # model formula
                data = ess_df_selected_J  # data frame
)

summary(fit_J_4, 
        fit.measures = TRUE, # returns commonly used fit measures  
        standardized = TRUE  # indicates that we want standardized results
)

#####################################################################################
### 2 and 3-factor CFA (SEM) ###
#####################################################################################
################ 
### ATI & PV ###
################

# model_2F <- '
# ATI =~ impcntr + imbgeco + imueclt + imwbcnt
# PV =~ ipcrtiv + ipudrst + ipmodst + ipgdtim + iphlppl + impenv 
# '

model_2F <- '
ATI =~ impcntr + imbgeco + imueclt + imwbcnt
PV =~ ipudrst + ipmodst + iphlppl + impenv 
'

fit_2F <- cfa(model_2F,             # model formula
                data = ess_df_selected  # data frame
)

summary(fit_2F, 
        fit.measures = TRUE, # returns commonly used fit measures  
        standardized = TRUE  # indicates that we want standardized results
)

# One has a low loading but maybe there is a theoretical explanation for that. Let's see fit indices before modifying anything.

### Model diagnostics 2F ### --------------------------------------------------------------------

# Observed covariance matrix.
lavInspect(fit_2F, "sampstat")
# Implied covariance matrix.
fitted(fit_2F)
# The gap
residuals(fit_2F)
# SEs of free parameters in the model
lavTech(fit_2F, "se")
# Parameter estimates & C.I.
parameterEstimates(fit_2F, standardized=TRUE)
# Factor loadings
inspect(fit_2F,           # fitted model 
        what="std")$lambda  # standardized factors loadings
# Residual variances
theta <- round(inspect(fit_2F, "est")$theta,3)
theta.std <- round(inspect(fit_2F, "std")$theta,3) 
r2 <- round(inspect(fit_2F, "r2"),3)

data.frame(row.names = c(),                       # empty the columns names 
           Variables = colnames(theta),           # variable names 
           "Residuals" = diag(theta),             # diagonal theta
           "Std. Residuals" = diag(theta.std),    # diagonal std. theta
           "R Squared" = r2                       # R-squared
)

### Model fit statistics
# Global
# * (some) rules of thumb: CFI/TLI>0.95,RMSEA<0.05,SRMR<0.06
# * current practice is: chi-square value + df + pvalue, RMSEA, CFI and SRMR
fitMeasures(fit_2F, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
# Local = Modification Indexes
## Local fit measures: modification indices ##
mi <- inspect(fit_2F,"mi")
mi.sorted <- mi[order(-mi$mi),] # sort from high to low mi.sorted[1:5,] # only display some large MI values
mi.sorted[1:5,] # only display some large MI values

# But ipcrtiv and ipgdtim seem to reflect very little variance in the latent factors => We remove them.

############################### 
### ATI & PV & J (Full SEM) ###
###############################

# model_3F <- '
# ATI =~ impcntr + imbgeco + imueclt + imwbcnt
# PV =~ ipcrtiv + ipudrst + ipmodst + ipgdtim + iphlppl + impenv 
# J =~ sofrprv + ppldsrv + jstprev + pcmpinj
# '

# CFA Model
model_3F <- '
ATI =~ impcntr + imbgeco + imueclt + imwbcnt
PV =~ ipudrst + ipmodst + iphlppl + impenv 
J =~ sofrprv + ppldsrv + jstprev + pcmpinj
'

fit_3F <- cfa(model_3F,             # model formula
              data = ess_df_selected2  # data frame
)

summary(fit_3F, 
        fit.measures = TRUE, # returns commonly used fit measures  
        standardized = TRUE  # indicates that we want standardized results
)

# Visualization
semPaths(fit_3F, "std", "est")

# Model Diagnostics Comparison --------------------------------------------------------------------

# anova()

#####################################################################################
# MODEL ASSUMPTIONS (CFA) ----------------------------------------------------------------------
# 1. Multivariate normality
# Calculate the multivariate kurtosis and skewness

mvn_test <- mvn(data = ess_df_selected2, # our data 
                mvnTest = c("hz")    # type of normality test to perform
)

mvn_test$multivariateNormality # not multivariate normal => Use "Robust" estimator (MLM)
# To mitigate non-normality we can use scaled χ2
# and “robust” standard errors corrections to ML estimation as in Satorra and Bentler (1988; 1994). 
# Adjustments are made to the χ2 (and χ2 based fit indices) and standard errors based on a weight matrix
# derived from an estimate of multivariate kurtosis (as said before, the parameter estimates themselves 
# are not altered).

# 2. Linearity
# Create scatter plots to visualize linearity
pairs.panels(ess_df_selected2[, c("imueclt", "jstprev", "iphlppl")], # Select one variable from each factor for visualization
             method = "pearson",
             hist.col = "#00AFBB",
             density = TRUE, 
             ellipses = TRUE)

# 3. Independence of observations
# Since the data set is a cross-sectional survey, we can assume that the observations are independent.

# 4. Adequate sample size
# Generally, sample sizes greater than 200 are considered adequate for CFA.
n <- nrow(ess_df_selected2)
cat("Sample size:", n)

# Correlation matrix
cor_matrix <- cor(ess_df_selected2, 
                  use = "pairwise.complete.obs")
cor_matrix

# Heatmap
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, diag = FALSE)

# Unloading packages -------------------------------------------------------------
detach("package:haven", unload = T)
detach("package:dplyr", unload = T)
detach("package:psych", unload = T)
detach("package:stringr", unload = T)
detach("package:lavaan", unload = T)
detach("package:corrplot", unload = T)
detach("package:tidySEM", unload = T)
detach("package:tidyverse", unload = T)
detach("GPArotation", unload = T)



