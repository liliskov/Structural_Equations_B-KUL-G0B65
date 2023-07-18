# Structural Equations -----------------------------------------------------------------------------------
# Description: Assignment
# Author: Lili Vandermeersch r0691855
#
#
#
# Paths ----------------------------------------------------------------------------------------------------
rm(list =ls()) # clears R environment
options(scipen=999) # disables scientific notation

setwd('/Users/lilivandermeersch/Library/Mobile Documents/com~apple~CloudDocs/❤️ MSc Statistics and Data Science ❤️/☀️🌸 Year 3./II./SEM/Assignment')
# input <- file.path(file.choose())
# output <- file.path("/Users/lilivandermeersch/Library/Mobile Documents/com~apple~CloudDocs/❤️ MSc Statistics and Data Science ❤️/☀️🌸 Year 3./II./SEM/Assignment/plots")

# Packages -------------------------------------------------------------------------------------------------

### DATA MANIPULATION ###
library(rio)        
library(dplyr)      
library(psych)
library(stringr)
library(purrr)

### MODELING ###
library(lavaan)  
library(MVN)
library(Amelia)
library(mice)
# library(semTools)         

### VISUALIZATION ###
library(tidySEM) 
library(ggplot2) 
library(GGally)
library(semPlot)
library(patchwork)
library(Hmisc)

### CONFLICTED ###
# library(conflicted)

################################################################################
# Importing the data
ess_df <- import("/Users/lilivandermeersch/Library/Mobile Documents/com~apple~CloudDocs/❤️ MSc Statistics and Data Science ❤️/☀️🌸 Year 3./II./SEM/Assignment/ESS7/Sav/ESS7.sav")
# Data Exploration ------------------------------------------------------------------
View(ess_df) 

# Filter for country
ess_df <- filter(ess_df, cntry == 'HU')
# ess_df <- filter(ess_df, cntry == 'BE')

nrow(ess_df)      # number of subjects
ncol(ess_df)      # number of variables
names(ess_df)     # names of variables

# Sub-setting the data
ess_df_selected <- ess_df %>% select(
# Political Efficacy
  actrolg, # Able to take active role in political group
  cptppol, # Confident in own ability to participate in politics
  psppipl, # Political system allows people to have influence on politics
  psppsgv, # Political system allows people to have a say in what government does
  ptcpplt, # Politicians care what people think
# Satisfaction with current state of affairs
  stfeco, # How satisfied with present state of economy in country
  stfgov, # How satisfied with the national government
  stfdem, # How satisfied with the way democracy works in country
  stfedu, # How satisfied with state of education in country nowadays
  stfhlth, # How satisfied with state of health services in country nowadays
# Interpersonal trust
  ppltrst, # Most people can be trusted or you can't be too careful
  pplfair,  # Most people try to take advantage of you, or try to be fair
  pplhlp, # Most of the time people helpful or mostly looking out for themselves
# Institutional trust
  trstlgl, # Trust in the legal system
  trstplc, # Trust in the police
  trstplt, # Trust in politicians
  trstep, # Trust in the European Parliament
  trstun, # Trust in the United Nations
  trstprt, # Trust in political parties
  trstprl, # Trust in country's parliament
# Miscellaneous 
  fclcntr, # Feel close to country
  lrscale, # Placement on left right scale
# Socio-demographic variables
  edlvdhu, # Highest level of education
  gndr, # Gender
  hinctnta, # Income
  eduyrs, # Education in years
  agea # Age
)

# Structure and summary of data
str(ess_df_selected)
ess_df_selected %>% glimpse()
summary(ess_df_selected)
# Highest level of education has odd values => Remove 5555 "other"
ess_df_selected["edlvdhu"][ess_df_selected["edlvdhu"] == 5555] <- NA
table(ess_df_selected$gndr)

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

# Variance-covariance matrices:

ess_df_PolEf <- ess_df_selected %>% select(
  # Political Efficacy
  actrolg, # Able to take active role in political group
  cptppol, # Confident in own ability to participate in politics
  psppipl, # Political system allows people to have influence on politics
  psppsgv, # Political system allows people to have a say in what government does
  ptcpplt, # Politicians care what people think
)

#####################################################################################
PolEf_cov <- cov(ess_df_PolEf,          # data frame 
                        use = "pairwise.complete.obs" # remove NAs 
)

PolEf_cov

PolEf_cor <- cov2cor(PolEf_cov)
PolEf_cor

corrplot::corrplot(PolEf_cor, 
                   is.corr = FALSE,       # whether is a correlation matrix 
                   method = "circle",     # magnitude of covariances as circles 
                   type = "upper",        # remove the bottom of the covariance matrix
                   addCoef.col = "black"  # add to the plot the coefficients
)


####################################################################################

ess_df_selected_lf <- ess_df %>% select(
  # Political Efficacy
  actrolg, # Able to take active role in political group
  cptppol, # Confident in own ability to participate in politics
  psppipl, # Political system allows people to have influence on politics
  psppsgv, # Political system allows people to have a say in what government does
  ptcpplt, # Politicians care what people think
  # Satisfaction with current state of affairs
  stfeco, # How satisfied with present state of economy in country
  stfgov, # How satisfied with the national government
  stfdem, # How satisfied with the way democracy works in country
  stfedu, # How satisfied with state of education in country nowadays
  stfhlth, # How satisfied with state of health services in country nowadays
  # Interpersonal trust
  ppltrst, # Most people can be trusted or you can't be too careful
  pplfair,  # Most people try to take advantage of you, or try to be fair
  pplhlp, # Most of the time people helpful or mostly looking out for themselves
  # Institutional trust
  trstlgl, # Trust in the legal system
  trstplc, # Trust in the police
  trstplt, # Trust in politicians
  trstep, # Trust in the European Parliament
  trstun, # Trust in the United Nations
  trstprt, # Trust in political parties
  trstprl # Trust in country's parliament
)

ess_df_selected_lf_cov <- cov(ess_df_selected_lf,          # data frame 
                 use = "pairwise.complete.obs" # remove NAs 
)
ess_df_selected_lf_cor <- cov2cor(ess_df_selected_lf_cov)

corrplot::corrplot(ess_df_selected_lf_cor, 
                   is.corr = FALSE,       # whether is a correlation matrix 
                   method = "circle",     # magnitude of covariances as circles 
                   type = "upper",        # remove the bottom of the covariance matrix
                   addCoef.col = "black"  # add to the plot the coefficients
)


######################################################################################
# Handling missing data --------------------------------------------------------------
na_counts <- colSums(is.na(ess_df_selected))
na_counts

md.pattern(ess_df_selected, rotate.names = TRUE) # pattern of missing data

# Significant number (proportionally) of missing data. Can't be sure if all MCAR or even MAR 
# => in fact probably not => ***list wise*** deletion not recommended, might introduce bias and decrease stat power.
# If using WLS and WLSMV, we can’t use full-information maximum-liklihood to deal with missing data (since they take complete cases). 
# One rubbish solution is to use missing='pairwise', which estimates based on pairwise availability. =>
# => Must use multiple imputation if we include less-then 5-point scales.
# => We will use 5+ point scales. The sample size is large => FIML to handle missing data, ML/MLM as estimatoor.

### Filtering out unreliable data ###
# Missing function 
percent_missing <- function(x){sum(is.na(x))/length(x)*100}
# Rows - Unit non-response
missing_rows <- apply(ess_df_selected, 1, percent_missing) # 1 is for rows
table(missing_rows)
# Rows to exclude:
rows_less30 <- subset(ess_df_selected, missing_rows <= 30) # people whose missing data we can replace
rows_more30<- subset(ess_df_selected, missing_rows > 30) # people who have too much missing data

#-------------------
# Columns - Item non-response
ess_df_selected_n <- ess_df_selected[sapply(ess_df_selected, is.numeric)]
missing_col <- apply(ess_df_selected_n, 2, percent_missing) 
table(missing_col)

ess_df_selected <- rows_less30 
#############################################################################
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

################################################################################
# MODEL ASSUMPTIONS ------------------------------------------------------------
# 1. Multivariate normality
# Non-normality doesn't affect the parameter estimates that much but it does affect the X2s and the SEs. 
# Effects will appear statistically significant when in fact they are not. 

# Histograms
# List of your variables
variables <- names(ess_df_selected)
pdf("histograms.pdf")
for (var in variables) {
  hist(ess_df_selected[[var]], main=var, xlab="Value", breaks="FD") # "FD" is Freedman-Diaconis rule and it's good for most cases
}
dev.off()

pdf("histogram_matrix.pdf")
hist.data.frame(ess_df_selected)
dev.off()

# Checking univariate normality using Shapiro-Wilk instead of Kolmogorov-Smirnov, as it is more robust to 'ties'.
# Although with large n even small deviations give significant results
# Function Shapiro-Wilk
shapiro_test_single_var <- function(data, var_name){
  shapiro_test_result <- shapiro.test(data[[var_name]][!is.na(data[[var_name]])])
  return(list("Variable" = var_name,
              "W" = round(shapiro_test_result$statistic, 2),
              "P-value" = shapiro_test_result$p.value))
}
var_names <- names(ess_df_selected)
shapiro_test_results <- lapply(var_names, function(var_name) shapiro_test_single_var(ess_df_selected, var_name))

shapiro_test_results_df <- do.call(rbind, lapply(shapiro_test_results, as.data.frame))
shapiro_test_results_df # gives W statistic and p-values for each variables (H0: normal distr.)
#-------------------------------------------------------------------------------
# Calculating the multivariate kurtosis and skewness
mvn_test <- mvn(data = ess_df_selected, # our data 
                mvnTest = c("hz")    # type of normality test to perform
)

mvn_test$multivariateNormality # not multivariate normal => Use "Robust" estimator (MLM)
# To mitigate non-normality we can use scaled χ2 and “robust” standard errors corrections to ML estimation 
# as in Satorra and Bentler (1988; 1994). 
# Adjustments are made to the χ2 (and χ2 based fit indices) and standard errors based on a weight matrix
# derived from an estimate of multivariate kurtosis (as said before, the parameter estimates themselves 
# are not altered).

# 2. Linearity => to be assesed once CFA and SEM mdels fit
# Create scatter plots to visualize relationships among some manifest variables
pdf("pairs.panels.pdf")
pairs.panels(ess_df_selected[, c("psppsgv", "stfeco", "pplfair", "trstep")], # one variable from each factor 
             method = "pearson",
             hist.col = "#00AFBB",
             density = TRUE, 
             ellipses = TRUE)
dev.off()

# 3. Independence of observations
# # Random sampling => we can assume that the observations are independent.

# 4. Adequate sample size
# Generally, sample sizes greater than 200 are considered adequate for CFA.
n <- nrow(ess_df_selected)
cat("Sample size:", n)

################################################################################
                        ### CFA - Measurement part ###
################################################################################
### Political Efficacy ###
##########################

cfa_model_PolEf <- '
PolEf =~ actrolg + cptppol + psppipl + psppsgv + ptcpplt
'

fit_cfa_PolEf <- cfa(cfa_model_PolEf,
                               data = ess_df_selected, 
                               missing = "direct",       # alias: "ml" or "fiml"
                               estimator = "ML")

summary(fit_cfa_PolEf, 
        fit.measures = TRUE, 
        standardized = TRUE 
)

# Global fit
fitMeasures(fit_cfa_PolEf, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
# Local fit (Modification Indexes)
mi <- inspect(fit_cfa_PolEf,"mi")
mi.sorted <- mi[order(-mi$mi),] # sort from high to low mi.sorted[1:5,]
mi.sorted[1:5,] # only display some large MI values

#-------------------------------------------------------------------------------
cfa_model_PolEf2 <- '
PolEf =~ actrolg + cptppol + psppipl + psppsgv + ptcpplt
actrolg ~~ cptppol
'

fit_cfa_PolEf2 <- cfa(cfa_model_PolEf2,
                     data = ess_df_selected, 
                     missing = "direct",       # alias: "ml" or "fiml"
                     estimator = "ML")

summary(fit_cfa_PolEf2, 
        fit.measures = TRUE, 
        standardized = TRUE 
)

# Global fit
fitMeasures(fit_cfa_PolEf2, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
# Local fit (Modification Indexes)
mi <- inspect(fit_cfa_PolEf2,"mi")
mi.sorted <- mi[order(-mi$mi),] # sort from high to low mi.sorted[1:5,]
mi.sorted[1:5,] # only display some large MI values

#-------------------------------------------------------------------------------
cfa_model_PolEf3 <- '
PolEf =~ actrolg + cptppol + psppipl + psppsgv + ptcpplt
actrolg ~~ cptppol
psppsgv ~~ ptcpplt
'

fit_cfa_PolEf3 <- cfa(cfa_model_PolEf3,
                      data = ess_df_selected, 
                      missing = "direct",       # alias: "ml" or "fiml"
                      estimator = "ML")

summary(fit_cfa_PolEf3, 
        fit.measures = TRUE, 
        standardized = TRUE 
)

# Global fit
fitMeasures(fit_cfa_PolEf3, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
# Local fit (Modification Indexes)
mi <- inspect(fit_cfa_PolEf3,"mi")
mi.sorted <- mi[order(-mi$mi),] # sort from high to low mi.sorted[1:5,]
mi.sorted[1:5,] # only display some large MI values

################################################################################
### Satisfaction with current state of affairs ###
##################################################

cfa_model_SatCSA <- '
SatCSA =~ stfeco + stfgov + stfdem + stfedu + stfhlth
'

fit_cfa_SatCSA <- cfa(cfa_model_SatCSA ,
                      data = ess_df_selected, 
                      missing = "direct",       # alias: "ml" or "fiml"
                      estimator = "ML")

summary(fit_cfa_SatCSA, 
        fit.measures = TRUE, 
        standardized = TRUE 
)

# Global fit
fitMeasures(fit_cfa_SatCSA, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
# Local fit (Modification Indexes)
mi <- inspect(fit_cfa_SatCSA,"mi")
mi.sorted <- mi[order(-mi$mi),] # sort from high to low mi.sorted[1:5,]
mi.sorted[1:5,] # only display some large MI values

#-------------------------------------------------------------------------------

cfa_model_SatCSA2 <- '
SatCSA =~ stfeco + stfgov + stfdem + stfedu + stfhlth
stfedu ~~ stfhlth
'

fit_cfa_SatCSA2 <- cfa(cfa_model_SatCSA2 ,
                      data = ess_df_selected, 
                      missing = "direct",       # alias: "ml" or "fiml"
                      estimator = "ML")

summary(fit_cfa_SatCSA2, 
        fit.measures = TRUE, 
        standardized = TRUE 
)

# Global fit
fitMeasures(fit_cfa_SatCSA2, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
# Local fit (Modification Indexes)
mi <- inspect(fit_cfa_SatCSA2,"mi")
mi.sorted <- mi[order(-mi$mi),] # sort from high to low mi.sorted[1:5,]
mi.sorted[1:5,] # only display some large MI values

#-------------------------------------------------------------------------------
cfa_model_SatCSA3 <- '
SatCSA =~ stfeco + stfgov + stfdem + stfedu + stfhlth
stfedu ~~ stfhlth
stfeco ~~ stfgov
'

fit_cfa_SatCSA3 <- cfa(cfa_model_SatCSA3 ,
                       data = ess_df_selected, 
                       missing = "direct",       # alias: "ml" or "fiml"
                       estimator = "ML")

summary(fit_cfa_SatCSA3, 
        fit.measures = TRUE, 
        standardized = TRUE 
)

# Global fit
fitMeasures(fit_cfa_SatCSA3, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
# Local fit (Modification Indexes)
mi <- inspect(fit_cfa_SatCSA3,"mi")
mi.sorted <- mi[order(-mi$mi),] # sort from high to low mi.sorted[1:5,]
mi.sorted[1:5,] # only display some large MI values

################################################################################
### Interpersonal trust ###
###########################
cfa_model_IPT <- '
IP =~ ppltrst + pplfair + pplhlp
'

fit_cfa_IPT <- cfa(cfa_model_IPT,
                   data = ess_df_selected, 
                   missing = "direct",       # alias: "ml" or "fiml"
                   estimator = "ML")

summary(fit_cfa_IPT, 
        fit.measures = TRUE, 
        standardized = TRUE 
)                                     

################################################################################
### Institutional trust ###
###########################

cfa_model_INSTT <- '
INSTT =~ trstlgl + trstplc + trstplt + trstep + trstun + trstprt + trstprl
'

fit_cfa_INSTT <- cfa(cfa_model_INSTT,
                   data = ess_df_selected, 
                   missing = "direct",       # alias: "ml" or "fiml"
                   estimator = "ML")

summary(fit_cfa_INSTT, 
        fit.measures = TRUE, 
        standardized = TRUE 
)                                     

# Global fit
fitMeasures(fit_cfa_INSTT, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
# Local fit (Modification Indexes)
mi <- inspect(fit_cfa_INSTT,"mi")
mi.sorted <- mi[order(-mi$mi),] # sort from high to low mi.sorted[1:5,]
mi.sorted[1:5,] # only display some large MI values

#-------------------------------------------------------------------------------
cfa_model_INSTT2 <- '
INSTT =~ trstlgl + trstplc + trstplt + trstep + trstun + trstprt + trstprl
trstplt ~~ trstprt
'

fit_cfa_INSTT2 <- cfa(cfa_model_INSTT2,
                     data = ess_df_selected, 
                     missing = "direct",       # alias: "ml" or "fiml"
                     estimator = "ML")

summary(fit_cfa_INSTT2, 
        fit.measures = TRUE, 
        standardized = TRUE 
)                                     

# Global fit
fitMeasures(fit_cfa_INSTT2, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
# Local fit (Modification Indexes)
mi <- inspect(fit_cfa_INSTT2,"mi")
mi.sorted <- mi[order(-mi$mi),] # sort from high to low mi.sorted[1:5,]
mi.sorted[1:5,] # only display some large MI values

#-------------------------------------------------------------------------------
cfa_model_INSTT3 <- '
INSTT =~ trstlgl + trstplc + trstplt + trstep + trstun + trstprt + trstprl
trstplt ~~ trstprt
trstep ~~  trstun
'

fit_cfa_INSTT3 <- cfa(cfa_model_INSTT3,
                      data = ess_df_selected, 
                      missing = "direct",       # alias: "ml" or "fiml"
                      estimator = "ML")

summary(fit_cfa_INSTT3, 
        fit.measures = TRUE, 
        standardized = TRUE 
)                                     

# Global fit
fitMeasures(fit_cfa_INSTT3, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
# Local fit (Modification Indexes)
mi <- inspect(fit_cfa_INSTT3,"mi")
mi.sorted <- mi[order(-mi$mi),] # sort from high to low mi.sorted[1:5,]
mi.sorted[1:5,] # only display some large MI values

#-------------------------------------------------------------------------------
cfa_model_INSTT4 <- '
INSTT =~ trstlgl + trstplc + trstplt + trstep + trstun + trstprt + trstprl
trstplt ~~ trstprt
trstep ~~  trstun
trstlgl ~~ trstplc
'

fit_cfa_INSTT4 <- cfa(cfa_model_INSTT4,
                      data = ess_df_selected, 
                      missing = "direct",       # alias: "ml" or "fiml"
                      estimator = "ML")

summary(fit_cfa_INSTT4, 
        fit.measures = TRUE, 
        standardized = TRUE 
)                                     

# Global fit
fitMeasures(fit_cfa_INSTT4, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
# Local fit (Modification Indexes)
mi <- inspect(fit_cfa_INSTT4,"mi")
mi.sorted <- mi[order(-mi$mi),] # sort from high to low mi.sorted[1:5,]
mi.sorted[1:5,] # only display some large MI values

################################################################################
### Trust ###
#############
cfa_model_Trust <- '
INSTT =~ trstlgl + trstplc + trstplt + trstep + trstun + trstprt + trstprl
IP =~ ppltrst + pplfair + pplhlp
'

fit_cfa_Trust <- cfa(cfa_model_Trust,
                      data = ess_df_selected, 
                      missing = "direct",       # alias: "ml" or "fiml"
                      estimator = "ML")

summary(fit_cfa_Trust, 
        fit.measures = TRUE, 
        standardized = TRUE 
)                                     

# Global fit
fitMeasures(fit_cfa_Trust, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
# Local fit (Modification Indexes)
mi <- inspect(fit_cfa_Trust,"mi")
mi.sorted <- mi[order(-mi$mi),] # sort from high to low mi.sorted[1:5,]
mi.sorted[1:5,] # only display some large MI values

#-------------------------------------------------------------------------------
cfa_model_Trust2 <- '
INSTT =~ trstlgl + trstplc + trstplt + trstep + trstun + trstprt + trstprl
IP =~ ppltrst + pplfair + pplhlp
trstplt ~~ trstprt
'

fit_cfa_Trust2 <- cfa(cfa_model_Trust2,
                     data = ess_df_selected, 
                     missing = "direct",       # alias: "ml" or "fiml"
                     estimator = "ML")

summary(fit_cfa_Trust2, 
        fit.measures = TRUE, 
        standardized = TRUE 
)                                     

# Global fit
fitMeasures(fit_cfa_Trust2, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
# Local fit (Modification Indexes)
mi <- inspect(fit_cfa_Trust2,"mi")
mi.sorted <- mi[order(-mi$mi),] # sort from high to low mi.sorted[1:5,]
mi.sorted[1:5,] # only display some large MI values

#-------------------------------------------------------------------------------
cfa_model_Trust3 <- '
INSTT =~ trstlgl + trstplc + trstplt + trstep + trstun + trstprt + trstprl
IP =~ ppltrst + pplfair + pplhlp
trstplt ~~ trstprt
trstep ~~  trstun
'

fit_cfa_Trust3 <- cfa(cfa_model_Trust3,
                      data = ess_df_selected, 
                      missing = "direct",       # alias: "ml" or "fiml"
                      estimator = "ML")

summary(fit_cfa_Trust3, 
        fit.measures = TRUE, 
        standardized = TRUE 
)                                     

# Global fit
fitMeasures(fit_cfa_Trust3, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
# Local fit (Modification Indexes)
mi <- inspect(fit_cfa_Trust3,"mi")
mi.sorted <- mi[order(-mi$mi),] # sort from high to low mi.sorted[1:5,]
mi.sorted[1:5,] # only display some large MI values

#-------------------------------------------------------------------------------
cfa_model_Trust4 <- '
INSTT =~ trstlgl + trstplc + trstplt + trstep + trstun + trstprt + trstprl
IP =~ ppltrst + pplfair + pplhlp
trstplt ~~ trstprt
trstep ~~  trstun
trstlgl ~~ trstplc
'

fit_cfa_Trust4 <- cfa(cfa_model_Trust4,
                      data = ess_df_selected, 
                      missing = "direct",       # alias: "ml" or "fiml"
                      estimator = "ML")

summary(fit_cfa_Trust4, 
        fit.measures = TRUE, 
        standardized = TRUE 
)                                     

# Global fit
fitMeasures(fit_cfa_Trust4, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
# Local fit (Modification Indexes)
mi <- inspect(fit_cfa_Trust4,"mi")
mi.sorted <- mi[order(-mi$mi),] # sort from high to low mi.sorted[1:5,]
mi.sorted[1:5,] # only display some large MI values

################################################################################
### Complete Measurement Model ###
##################################
cfa_model_POL <- '
Trust =~ INSTT + IP
SatCSA =~ stfeco + stfgov + stfdem + stfedu + stfhlth
PolEf =~ actrolg + cptppol + psppipl + psppsgv + ptcpplt

INSTT =~ trstlgl + trstplc + trstplt + trstep + trstun + trstprt + trstprl
IP =~ ppltrst + pplfair + pplhlp

trstplt ~~ trstprt
trstep ~~  trstun
trstlgl ~~ trstplc
stfedu ~~ stfhlth
actrolg ~~ cptppol
'

fit_cfa_POL <- cfa(cfa_model_POL,             
                estimator = "ML",
                missing = 'direct',
                data = ess_df_selected
)

summary(fit_cfa_POL, 
        fit.measures = TRUE,   
        standardized = TRUE 
)

fitMeasures(fit_cfa_POL, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")

# Visualization
pdf("measurement_model.pdf")
semPaths(fit_cfa_POL, what = "std", optimizeLatRes = TRUE, layout = "tree", nCharNodes = 0, intercepts = FALSE, fade = FALSE)
dev.off()

# Alternatively
lay <- get_layout("stfeco", "stfgov", "stfdem", "stfedu", "stfhlth", "actrolg", "cptppol", "psppipl", "psppsgv", "ptcpplt",
                  "","","Trust","","SatCSA", "", "" ,"PolEf", "", "",
                  "", "", "", "INSTT", "", "", "IP", "", "", "",
                  "trstlgl", "trstplc", "trstplt", "trstep", "trstun", "trstprt", "trstprl", "ppltrst", "pplfair", "pplhlp", rows = 4)
lay

plot_POL <- graph_sem(model = fit_cfa_POL,
                      #label = "est_std",
                      layout = lay,
                      angle = 170
)   

plot_POL

### Checking Normality ###------------------------------------------------------------------------
# Extracting factor scores
factor_values <- data.frame(lavPredict(fit_cfa_POL))
head(factor_values)
# Checking linearity
pdf("normality_measurement")
ggpairs(factor_values, aes(color = as.factor(ess_df_selected$gndr), alpha = 0.5), lower = list(continuous = "smooth"))
dev.off()
# Model Diagnostics Comparison --------------------------------------------------------------------
# anova() only works with estimator = "ML"

# INSTT
anova(fit_cfa_INSTT, fit_cfa_INSTT2)
anova(fit_cfa_INSTT2, fit_cfa_INSTT3)
anova(fit_cfa_INSTT3, fit_cfa_INSTT4)

# Trust
anova(fit_cfa_Trust, fit_cfa_Trust2)
anova(fit_cfa_Trust2, fit_cfa_Trust3)
anova(fit_cfa_Trust3, fit_cfa_Trust4)

# SatCSA
anova(fit_cfa_SatCSA, fit_cfa_SatCSA2)
anova(fit_cfa_SatCSA2, fit_cfa_SatCSA3)

# PolEf
anova(fit_cfa_PolEf, fit_cfa_PolEf2)
anova(fit_cfa_PolEf2, fit_cfa_PolEf3)


fit_indicesINSTT <- data.frame(
  fit_cfa_INSTT = c(round(fitMeasures(fit_cfa_INSTT)["logl"], 2), round(fitMeasures(fit_cfa_INSTT)["chisq"], 2), round(fitMeasures(fit_cfa_INSTT)["df"], 2), round(fitMeasures(fit_cfa_INSTT)["pvalue"], 2), round(fitMeasures(fit_cfa_INSTT)["cfi"], 2), round(fitMeasures(fit_cfa_INSTT)["tli"], 2), round(fitMeasures(fit_cfa_INSTT)["rmsea"], 2)),
  fit_cfa_INSTT2 = c(round(fitMeasures(fit_cfa_INSTT2)["logl"], 2), round(fitMeasures(fit_cfa_INSTT2)["chisq"], 2), round(fitMeasures(fit_cfa_INSTT2)["df"], 2), round(fitMeasures(fit_cfa_INSTT2)["pvalue"], 2), round(fitMeasures(fit_cfa_INSTT2)["cfi"], 2), round(fitMeasures(fit_cfa_INSTT2)["tli"], 2), round(fitMeasures(fit_cfa_INSTT2)["rmsea"], 2)),
  fit_cfa_INSTT3 = c(round(fitMeasures(fit_cfa_INSTT3)["logl"], 2), round(fitMeasures(fit_cfa_INSTT3)["chisq"], 2), round(fitMeasures(fit_cfa_INSTT3)["df"], 2), round(fitMeasures(fit_cfa_INSTT3)["pvalue"], 2), round(fitMeasures(fit_cfa_INSTT3)["cfi"], 2), round(fitMeasures(fit_cfa_INSTT3)["tli"], 2), round(fitMeasures(fit_cfa_INSTT3)["rmsea"], 2)),
  fit_cfa_INSTT4 = c(round(fitMeasures(fit_cfa_INSTT4)["logl"], 2), round(fitMeasures(fit_cfa_INSTT4)["chisq"], 2), round(fitMeasures(fit_cfa_INSTT4)["df"], 2), round(fitMeasures(fit_cfa_INSTT4)["pvalue"], 2), round(fitMeasures(fit_cfa_INSTT4)["cfi"], 2), round(fitMeasures(fit_cfa_INSTT4)["tli"], 2), round(fitMeasures(fit_cfa_INSTT4)["rmsea"], 2))
)
fit_indicesINSTT


fit_indicesTrust <- data.frame(
  fit_cfa_Trust = c(round(fitMeasures(fit_cfa_Trust)["logl"], 2), round(fitMeasures(fit_cfa_Trust)["chisq"], 2), round(fitMeasures(fit_cfa_Trust)["df"], 2), round(fitMeasures(fit_cfa_Trust)["pvalue"], 2), round(fitMeasures(fit_cfa_Trust)["cfi"], 2), round(fitMeasures(fit_cfa_Trust)["tli"], 2), round(fitMeasures(fit_cfa_Trust)["rmsea"], 2)),
  fit_cfa_Trust2 = c(round(fitMeasures(fit_cfa_Trust2)["logl"], 2), round(fitMeasures(fit_cfa_Trust2)["chisq"], 2), round(fitMeasures(fit_cfa_Trust2)["df"], 2), round(fitMeasures(fit_cfa_Trust2)["pvalue"], 2), round(fitMeasures(fit_cfa_Trust2)["cfi"], 2), round(fitMeasures(fit_cfa_Trust2)["tli"], 2), round(fitMeasures(fit_cfa_Trust2)["rmsea"], 2)),
  fit_cfa_Trust3 = c(round(fitMeasures(fit_cfa_Trust3)["logl"], 2), round(fitMeasures(fit_cfa_Trust3)["chisq"], 2), round(fitMeasures(fit_cfa_Trust3)["df"], 2), round(fitMeasures(fit_cfa_Trust3)["pvalue"], 2), round(fitMeasures(fit_cfa_Trust3)["cfi"], 2), round(fitMeasures(fit_cfa_Trust3)["tli"], 2), round(fitMeasures(fit_cfa_Trust3)["rmsea"], 2)),
  fit_cfa_Trust4 = c(round(fitMeasures(fit_cfa_Trust4)["logl"], 2), round(fitMeasures(fit_cfa_Trust4)["chisq"], 2), round(fitMeasures(fit_cfa_Trust4)["df"], 2), round(fitMeasures(fit_cfa_Trust4)["pvalue"], 2), round(fitMeasures(fit_cfa_Trust4)["cfi"], 2), round(fitMeasures(fit_cfa_Trust4)["tli"], 2), round(fitMeasures(fit_cfa_Trust4)["rmsea"], 2))
)
fit_indicesTrust


fit_indicesSatCSA <- data.frame(
  fit_cfa_SatCSA = c(round(fitMeasures(fit_cfa_SatCSA)["logl"], 2), round(fitMeasures(fit_cfa_SatCSA)["chisq"], 2), round(fitMeasures(fit_cfa_SatCSA)["df"], 2), round(fitMeasures(fit_cfa_SatCSA)["pvalue"], 2), round(fitMeasures(fit_cfa_SatCSA)["cfi"], 2), round(fitMeasures(fit_cfa_SatCSA)["tli"], 2), round(fitMeasures(fit_cfa_SatCSA)["rmsea"], 2)),
  fit_cfa_SatCSA2 = c(round(fitMeasures(fit_cfa_SatCSA2)["logl"], 2), round(fitMeasures(fit_cfa_SatCSA2)["chisq"], 2), round(fitMeasures(fit_cfa_SatCSA2)["df"], 2), round(fitMeasures(fit_cfa_SatCSA2)["pvalue"], 2), round(fitMeasures(fit_cfa_SatCSA2)["cfi"], 2), round(fitMeasures(fit_cfa_SatCSA2)["tli"], 2), round(fitMeasures(fit_cfa_SatCSA2)["rmsea"], 2)),
  fit_cfa_SatCSA3 = c(round(fitMeasures(fit_cfa_SatCSA3)["logl"], 2), round(fitMeasures(fit_cfa_SatCSA3)["chisq"], 2), round(fitMeasures(fit_cfa_SatCSA3)["df"], 2), round(fitMeasures(fit_cfa_SatCSA3)["pvalue"], 2), round(fitMeasures(fit_cfa_SatCSA3)["cfi"], 2), round(fitMeasures(fit_cfa_SatCSA3)["tli"], 2), round(fitMeasures(fit_cfa_SatCSA3)["rmsea"], 2))
)
fit_indicesSatCSA


fit_indicesPolEf <- data.frame(
  fit_cfa_PolEf = c(round(fitMeasures(fit_cfa_PolEf)["logl"], 2), round(fitMeasures(fit_cfa_PolEf)["chisq"], 2), round(fitMeasures(fit_cfa_PolEf)["df"], 2), round(fitMeasures(fit_cfa_PolEf)["pvalue"], 2), round(fitMeasures(fit_cfa_PolEf)["cfi"], 2), round(fitMeasures(fit_cfa_PolEf)["tli"], 2), round(fitMeasures(fit_cfa_PolEf)["rmsea"], 2)),
  fit_cfa_PolEf2 = c(round(fitMeasures(fit_cfa_PolEf2)["logl"], 2), round(fitMeasures(fit_cfa_PolEf2)["chisq"], 2), round(fitMeasures(fit_cfa_PolEf2)["df"], 2), round(fitMeasures(fit_cfa_PolEf2)["pvalue"], 2), round(fitMeasures(fit_cfa_PolEf2)["cfi"], 2), round(fitMeasures(fit_cfa_PolEf2)["tli"], 2), round(fitMeasures(fit_cfa_PolEf2)["rmsea"], 2)),
  fit_cfa_PolEf3 = c(round(fitMeasures(fit_cfa_PolEf3)["logl"], 2), round(fitMeasures(fit_cfa_PolEf3)["chisq"], 2), round(fitMeasures(fit_cfa_PolEf3)["df"], 2), round(fitMeasures(fit_cfa_PolEf3)["pvalue"], 2), round(fitMeasures(fit_cfa_PolEf3)["cfi"], 2), round(fitMeasures(fit_cfa_PolEf3)["tli"], 2), round(fitMeasures(fit_cfa_PolEf3)["rmsea"], 2))
  )
fit_indicesPolEf

################################################################################
                                  ### SEM ###
################################################################################
################################################################################
### MIMIC #### The more indicators we include => the more restrictions (e.g. that they do not
# effect other latent vars than the ones we associate them. Fit usually gets worse and worse, the more
# we include => See Lab 2. Ex.) Investigate why fit is bad => MI
################################################################################
model_mimic <-'
# Measurement model
Trust =~ INSTT + IP
SatCSA =~ stfeco + stfgov + stfdem + stfedu + stfhlth
PolEf =~ actrolg + cptppol + psppipl + psppsgv + ptcpplt

INSTT =~ trstlgl + trstplc + trstplt + trstep + trstun + trstprt + trstprl
IP =~ ppltrst + pplfair + pplhlp

# Covariances
Trust ~~ SatCSA
PolEf ~~ SatCSA
PolEf ~~ Trust
trstplt ~~ trstprt
trstep ~~  trstun
trstlgl ~~ trstplc
stfedu ~~ stfhlth
actrolg ~~ cptppol

# MIMIC
PolEf ~ lrscale
'

### Amelia ###------------------------------------------------------------------
# Amelia explicitly requires an object of data.frame class 
ess_df_selected <- data.frame(ess_df_selected)
# Imputation 
a.out <- amelia(ess_df_selected,  # original dataset with missing
                m = 15,     # number of m "completed" data sets 
                seed = 222   # set the seed
)
# We can check each "completed" dataset against our original data 
cbind(ess_df_selected$hinctnta, a.out$imputations$imp1$hinctnta)[c(75:85),]

# Fitting the model to multiple imputed data sets
fit_mimic_a <- semTools::runMI(
  model = model_mimic,         # model 
  data = a.out$imputations, # list of imputed data sets 
  fun = "sem",              # lavaan function 
  estimator = "MLR"
)

summary(fit_mimic_a, fit.measures = TRUE, standardized = TRUE)

### FIML ###--------------------------------------------------------------------
fit_mimic_fiml <- sem(model_mimic,             
                   estimator = "ML",
                   missing = 'direct',
                   data = ess_df_selected
)

summary(fit_mimic_fiml, 
        fit.measures = TRUE,   
        standardized = TRUE 
)

fitm_mimic <- fitMeasures(fit_mimic_fiml, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
fitm_mimic

## Modification Indexes
mi <- inspect(fit_mimic_fiml,"mi")
mi.sorted <- mi[order(-mi$mi),] # sort from high to low mi.sorted[1:5,] # only display some large MI values
mi.sorted[1:5,] # only display some large MI values

### COMPARISON ###--------------------------------------------------------------
# let's compare the fit of the different models
model_fit <-  function(lavobject) {
  vars <- c("chisq", "df", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue", "srmr")
  return(fitmeasures(lavobject)[vars] %>% data.frame() %>% round(2) %>% t())
}

table_fit_mimic <- 
  list(model_fit(fit_mimic_fiml), 
       model_fit(fit_mimic_a)) %>% 
  reduce(rbind)

rownames(table_fit_mimic) <- c("FIML", "Amelia")

table_fit_mimic

pdf("fit_mimic_fiml.pdf")
# Visualization
semPaths(fit_mimic_fiml, what = "std", optimizeLatRes = TRUE, layout = "tree", style = "ram", nCharNodes = 0, intercepts = FALSE, fade = FALSE, rotation = 2)
dev.off()

################################################################################
model_mimic2 <-'
# Measurement model
Trust =~ INSTT + IP
SatCSA =~ stfeco + stfgov + stfdem + stfedu + stfhlth
PolEf =~ actrolg + cptppol + psppipl + psppsgv + ptcpplt

INSTT =~ trstlgl + trstplc + trstplt + trstep + trstun + trstprt + trstprl
IP =~ ppltrst + pplfair + pplhlp

# Covariances
Trust ~~ SatCSA
PolEf ~~ SatCSA
PolEf ~~ Trust
trstplt ~~ trstprt
trstep ~~  trstun
trstlgl ~~ trstplc
stfedu ~~ stfhlth
actrolg ~~ cptppol

# MIMIC
PolEf ~ hinctnta
'

fit_mimic2 <- sem(model_mimic2,             
                 estimator = "ML",
                 missing = 'direct',
                 data = ess_df_selected
)

summary(fit_mimic2, 
        fit.measures = TRUE,   
        standardized = TRUE 
)

fitm_mimic2 <- fitMeasures(fit_mimic2, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
fitm_mimic2

## Modification Indexes
mi <- inspect(fit_mimic2,"mi")
mi.sorted <- mi[order(-mi$mi),] # sort from high to low mi.sorted[1:5,] # only display some large MI values
mi.sorted[1:5,] # only display some large MI values

################################################################################
model_mimic3 <-'
# Measurement model
Trust =~ INSTT + IP
SatCSA =~ stfeco + stfgov + stfdem + stfedu + stfhlth
PolEf =~ actrolg + cptppol + psppipl + psppsgv + ptcpplt

INSTT =~ trstlgl + trstplc + trstplt + trstep + trstun + trstprt + trstprl
IP =~ ppltrst + pplfair + pplhlp

# Covariances
Trust ~~ SatCSA
PolEf ~~ SatCSA
PolEf ~~ Trust
trstplt ~~ trstprt
trstep ~~  trstun
trstlgl ~~ trstplc
stfedu ~~ stfhlth
actrolg ~~ cptppol

# MIMIC
PolEf ~ hinctnta + fclcntr
'

fit_mimic3 <- sem(model_mimic3,             
                  estimator = "ML",
                  missing = 'direct',
                  data = ess_df_selected
)

summary(fit_mimic3, 
        fit.measures = TRUE,   
        standardized = TRUE 
)

fitm_mimic3 <- fitMeasures(fit_mimic3, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
fitm_mimic3

## Modification Indexes
mi <- inspect(fit_mimic3,"mi")
mi.sorted <- mi[order(-mi$mi),] # sort from high to low mi.sorted[1:5,] # only display some large MI values
mi.sorted[1:5,] # only display some large MI values

################################################################################
model_mimic4 <-'
# Measurement model
Trust =~ INSTT + IP
SatCSA =~ stfeco + stfgov + stfdem + stfedu + stfhlth
PolEf =~ actrolg + cptppol + psppipl + psppsgv + ptcpplt

INSTT =~ trstlgl + trstplc + trstplt + trstep + trstun + trstprt + trstprl
IP =~ ppltrst + pplfair + pplhlp

# Covariances
Trust ~~ SatCSA
PolEf ~~ SatCSA
PolEf ~~ Trust
trstplt ~~ trstprt
trstep ~~  trstun
trstlgl ~~ trstplc
stfedu ~~ stfhlth
actrolg ~~ cptppol

# MIMIC
PolEf ~ hinctnta + agea
'

fit_mimic4 <- sem(model_mimic4,             
                  estimator = "ML",
                  missing = 'direct',
                  data = ess_df_selected
)

summary(fit_mimic4, 
        fit.measures = TRUE,   
        standardized = TRUE 
)

fitm_mimic4 <- fitMeasures(fit_mimic4, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
fitm_mimic4

## Modification Indexes
mi <- inspect(fit_mimic4,"mi")
mi.sorted <- mi[order(-mi$mi),] # sort from high to low mi.sorted[1:5,] # only display some large MI values
mi.sorted[1:5,] # only display some large MI values

pdf("fit_mimic4.pdf")
# Visualization
semPaths(fit_mimic4, what = "std", optimizeLatRes = TRUE, layout = "tree", style = "ram", nCharNodes = 0, intercepts = FALSE, fade = FALSE, rotation = 2)
dev.off()

################################################################################
model_mimic5 <-'
# Measurement model
Trust =~ INSTT + IP
SatCSA =~ stfeco + stfgov + stfdem + stfedu + stfhlth
PolEf =~ actrolg + cptppol + psppipl + psppsgv + ptcpplt

INSTT =~ trstlgl + trstplc + trstplt + trstep + trstun + trstprt + trstprl
IP =~ ppltrst + pplfair + pplhlp

# Covariances
Trust ~~ SatCSA
PolEf ~~ SatCSA
PolEf ~~ Trust
trstplt ~~ trstprt
trstep ~~  trstun
trstlgl ~~ trstplc
stfedu ~~ stfhlth
actrolg ~~ cptppol

# MIMIC
PolEf ~ hinctnta + agea + gndr
'

fit_mimic5 <- sem(model_mimic5,             
                  estimator = "ML",
                  missing = 'direct',
                  data = ess_df_selected
)

summary(fit_mimic5, 
        fit.measures = TRUE,   
        standardized = TRUE 
)

fitm_mimic5 <- fitMeasures(fit_mimic5, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
fitm_mimic5

## Modification Indexes
mi <- inspect(fit_mimic5,"mi")
mi.sorted <- mi[order(-mi$mi),] # sort from high to low mi.sorted[1:5,] # only display some large MI values
mi.sorted[1:5,] # only display some large MI values

################################################################################
model_mimic6 <-'
# Measurement model
Trust =~ INSTT + IP
SatCSA =~ stfeco + stfgov + stfdem + stfedu + stfhlth
PolEf =~ actrolg + cptppol + psppipl + psppsgv + ptcpplt

INSTT =~ trstlgl + trstplc + trstplt + trstep + trstun + trstprt + trstprl
IP =~ ppltrst + pplfair + pplhlp

# Covariances
Trust ~~ SatCSA
PolEf ~~ SatCSA
PolEf ~~ Trust
trstplt ~~ trstprt
trstep ~~  trstun
trstlgl ~~ trstplc
stfedu ~~ stfhlth
actrolg ~~ cptppol

# MIMIC
PolEf ~ hinctnta + agea + edlvdhu
'

fit_mimic6 <- sem(model_mimic6,             
                  estimator = "ML",
                  missing = 'direct',
                  data = ess_df_selected
)

summary(fit_mimic6, 
        fit.measures = TRUE,   
        standardized = TRUE 
)

fitm_mimic6 <- fitMeasures(fit_mimic6, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
fitm_mimic6

## Modification Indexes
mi <- inspect(fit_mimic6,"mi")
mi.sorted <- mi[order(-mi$mi),] # sort from high to low mi.sorted[1:5,] # only display some large MI values
mi.sorted[1:5,] # only display some large MI values

pdf("fit_mimi6.pdf")
# Visualization
semPaths(fit_mimic6, what = "std", optimizeLatRes = TRUE, layout = "spring", style = "ram", nCharNodes = 0, intercepts = FALSE, fade = FALSE, rotation = 2)
dev.off()

################################################################################
data.frame(
           "w/lrscale" = round(fitm_mimic[,1],2),
           "w/hinctnta" = round(fitm_mimic2[,1],2),
           "w/hinctnta + fclcntr" = round(fitm_mimic3[,1],2),
           "w/hinctnta + agea" = round(fitm_mimic4[,1],2),
           "w/hinctnta + agea + gndr" = round(fitm_mimic5[,1],2),
           "w/hinctnta + agea + eduhl" = round(fitm_mimic6[,1],2)
)
################################################################################
### MEDIATION  ### 
##################
model_mediation <- '
## Political efficacy ##
PolEf =~ actrolg + cptppol + psppipl + psppsgv + ptcpplt
actrolg ~~ cptppol

## SatCSA ##
SatCSA =~ stfeco + stfgov + stfdem + stfedu + stfhlth
stfedu ~~ stfhlth

## Direct effect ##
PolEf ~ c*hinctnta

## Mediator ##
SatCSA ~ a*hinctnta
PolEf ~ b*SatCSA

## Indirect effect (a*b) ##
ab := a*b
## Total effect ##
total := c + (a*b)
'

fit_mediation_fiml <- cfa(model_mediation, # model formula
                     estimator = "ML",
                     missing = 'direct',
                     data = ess_df_selected
)

summary(fit_mediation_fiml, 
        fit.measures = TRUE,   
        standardized = TRUE 
)


# Imputation 
a.out <- amelia(ess_df_selected,  # original dataset with missing
                m = 15,     # number of m "completed" data sets 
                seed = 22   # set the seed
)
# We can check each "completed" dataset against our original data 
cbind(ess_df_selected$hinctnta, a.out$imputations$imp1$hinctnta)[c(75:85),]

# Fitting the model to multiple imputed data sets
fit_mediation_a <- semTools::runMI(
  model = model_mediation,         # model 
  data = a.out$imputations, # list of imputed data sets 
  fun = "sem",              # lavaan function 
  estimator = "MLR"         # estimator
)

summary(fit_mediation_a,
        fit.measures=TRUE,
        standardized=TRUE)

### COMPARISON ###--------------------------------------------------------------
# let's compare the fit of the different models
model_fit <-  function(lavobject) {
  vars <- c("chisq", "df", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue", "srmr")
  return(fitmeasures(lavobject)[vars] %>% data.frame() %>% round(2) %>% t())
}

table_fit_medi <- 
  list(model_fit(fit_mediation_fiml), 
       model_fit(fit_mediation_a)) %>% 
  reduce(rbind)

rownames(table_fit_medi) <- c("FIML", "Amelia")

table_fit_medi

pdf("fit_mediation_fiml.pdf")
# Visualization
semPaths(fit_mediation_fiml, what = "std", optimizeLatRes = TRUE, layout = "tree", nCharNodes = 0, intercepts = FALSE, fade = FALSE, edge.color = "black")
dev.off()

################################################################################
model_mediation2 <- '
## Political efficacy ##
PolEf =~ actrolg + cptppol + psppipl + psppsgv + ptcpplt
actrolg ~~ cptppol

## Trust ##
Trust =~ INSTT + IP
INSTT =~ trstlgl + trstplc + trstplt + trstep + trstun + trstprt + trstprl
IP =~ ppltrst + pplfair + pplhlp
trstplt ~~ trstprt
trstep ~~  trstun
trstlgl ~~ trstplc

## Direct effect ##
PolEf ~ c*hinctnta

## Mediator ##
Trust ~ a*hinctnta
PolEf ~ b*Trust

## Indirect effect (a*b) ##
ab := a*b
## Total effect ##
total := c + (a*b)
'

fit_mediation_fiml2 <- cfa(model_mediation2, # model formula
                          estimator = "ML",
                          missing = 'direct',
                          data = ess_df_selected
)

summary(fit_mediation_fiml2, 
        fit.measures = TRUE,   
        standardized = TRUE 
)


# Imputation 
a.out <- amelia(ess_df_selected,  # original dataset with missing
                m = 15,     # number of m "completed" data sets 
                seed = 20   # set the seed
)
# We can check each "completed" dataset against our original data 
cbind(ess_df_selected$hinctnta, a.out$imputations$imp1$hinctnta)[c(75:85),]

# Fitting the model to multiple imputed data sets
fit_mediation_a2 <- semTools::runMI(
  model = model_mediation2,         # model 
  data = a.out$imputations, # list of imputed data sets 
  fun = "sem",              # lavaan function 
  estimator = "MLR"         # estimator
)

summary(fit_mediation_a2,
        fit.measures=TRUE,
        standardized=TRUE)

### COMPARISON ###--------------------------------------------------------------
# let's compare the fit of the different models
model_fit <-  function(lavobject) {
  vars <- c("chisq", "df", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue", "srmr")
  return(fitmeasures(lavobject)[vars] %>% data.frame() %>% round(2) %>% t())
}

table_fit_medi2 <- 
  list(model_fit(fit_mediation_fiml2), 
       model_fit(fit_mediation_a2)) %>% 
  reduce(rbind)

rownames(table_fit_medi2) <- c("FIML", "Amelia")

table_fit_medi2

pdf("fit_mediation_fiml2.pdf")
# Visualization
semPaths(fit_mediation_fiml2, what = "std", optimizeLatRes = TRUE, layout = "tree", nCharNodes = 0, intercepts = FALSE, fade = FALSE, rotation = 2)
dev.off()

################################################################################
model_mediation3 <- '
## Political efficacy ##
PolEf =~ actrolg + cptppol + psppipl + psppsgv + ptcpplt
actrolg ~~ cptppol

## SatCSA ##
SatCSA =~ stfeco + stfgov + stfdem + stfedu + stfhlth
stfedu ~~ stfhlth

## Direct effect(s) ##
PolEf ~ c*agea + c1*edlvdhu + c2*hinctnta + c3*gndr + c4*fclcntr + c5*lrscale 

## Mediator ##
# Path A
SatCSA ~ a*agea
SatCSA ~ a1*edlvdhu
SatCSA ~ a2*hinctnta
SatCSA ~ a3*gndr
SatCSA ~ a4*fclcntr
SatCSA ~ a5*lrscale

# Path B
PolEf ~ b*SatCSA

## Indirect effect (a*b) ##
ab_age := a*b
a1b_edl := a1*b
a2b_inco := a2*b
a3b_gndr := a3*b
a4b_fclcntr := a4*b
a5b_lrscale := a5*b


## Total effect ##
total_age := c + (a*b)
total1_edl := c1 + (a1*b)
total2_inco := c2 + (a2*b)
total3_gndr := c3 + (a3*b)
total4_fclcntr := c4 + (a4*b)
total5_lrscale := c5 + (a5*b)
'

fit_mediation3 <- cfa(model_mediation3, 
                     estimator = "ML",
                     missing = 'direct',
                     data = ess_df_selected
)

summary(fit_mediation3, 
        fit.measures = TRUE,   
        standardized = TRUE 
)

fitMeasures(fit_mediation3, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")

pdf("fit_mediation3")
# Visualization
semPaths(fit_mediation3, what = "std", optimizeLatRes = TRUE, layout = "spring", style = "lisrel2", nCharNodes = 0, intercepts = FALSE, fade = FALSE, edge.color = "black")
dev.off()

# Global fit
fitMeasures(fit_mediation3, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
# Local fit (Modification Indexes)
mi <- inspect(fit_mediation3,"mi")
mi.sorted <- mi[order(-mi$mi),] # sort from high to low mi.sorted[1:5,]
mi.sorted[1:5,] # only display some large MI values

################################################################################
### Measurement equivalence ###
###############################
# Is Political efficacy measured equivalently across genders?
### Configural Invariance ###---------------------------------------------------

# Changing group to factor
ess_df_selected$gndr <- factor(ess_df_selected$gndr,
                      levels = c("1", "2"),         # levels 
                      labels = c("Male", "Female")) # labels 


fit_configural <- cfa(cfa_model_PolEf3, 
                      data = ess_df_selected,
                      missing = 'direct',
                      group = "gndr")


summary(fit_configural, fit.measures = TRUE, standardized=TRUE)

fitMeasures(fit_configural, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
# Local fit (Modification Indexes)
mi <- inspect(fit_configural,"mi")
mi.sorted <- mi[order(-mi$mi),] # sort from high to low mi.sorted[1:5,]
mi.sorted[1:5,] # only display some large MI values

### Metric Invariance (also called “weak” invariance) ###-----------------------

fit_metric <- cfa(cfa_model_PolEf3, 
                  data = ess_df_selected,
                  group = "gndr",
                  missing = 'direct',
                  group.equal = c("loadings")
)


summary(fit_metric, fit.measures = TRUE, standardized=TRUE)
# standardized loadings are set equal across groups

fitMeasures(fit_metric, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
# Local fit (Modification Indexes)
mi <- inspect(fit_metric,"mi")
mi.sorted <- mi[order(-mi$mi),] # sort from high to low mi.sorted[1:5,]
mi.sorted[1:5,] # only display some large MI values

### Scalar Invariance (also called “strong” invariance) ###---------------------
# People who score zero on latent should score the same. Answer should not depend 
# on group membership, only on teh latent factor.

fit_scalar <- cfa(cfa_model_PolEf3, 
                  data = ess_df_selected,
                  group = "gndr",
                  missing = 'direct',
                  group.equal = c("loadings",
                                  "intercepts")
)


summary(fit_scalar, fit.measures = TRUE, standardized=TRUE)
# The average female scores 2.195 on actrolg (because it's the value if latent equals zero,
# but since we set the latent mean zero for model identification purposes, 
# this gives us the average for group 'Female')

#=> We can compare latent means starting from scalar => PolEf is lower for women 0 vs 0.171
# Is it a big diff? Can this be generalized for a bigger population = p-value sign., so yes (0.022).
# There is a diff between men and women in the latent mean.
# Strength = standardized solution = 0.122

fitMeasures(fit_scalar, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
# Local fit (Modification Indexes)
mi <- inspect(fit_scalar,"mi")
mi.sorted <- mi[order(-mi$mi),] # sort from high to low mi.sorted[1:5,]
mi.sorted[1:5,] # only display some large MI values

### Strict Invariance ###-------------------------------------------------------

fit_strict <- cfa(cfa_model_PolEf3, 
                  data = ess_df_selected,
                  group = "gndr",
                  missing = 'direct',
                  group.equal = c("loadings",
                                  "intercepts",
                                  "residuals")
)


summary(fit_strict, fit.measures = TRUE, standardized=TRUE)

fitMeasures(fit_strict, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")

### Structural Invariance ###---------------------------------------------------

fit_structural <- cfa(cfa_model_PolEf3, 
                      data = ess_df_selected,
                      group = "gndr",
                      missing = 'direct',
                      group.equal = c("loadings",
                                      "intercepts",
                                      "residuals",
                                      "lv.variances", 
                                      "lv.covariances") # 1 latent var, no covariances
)


summary(fit_structural, fit.measures = TRUE, standardized=TRUE)
fitMeasures(fit_structural, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")

### Evaluating measurement invariance ###---------------------------------------

model_fit <-  function(lavobject) {
  vars <- c("df", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue", "srmr")
  return(fitmeasures(lavobject)[vars] %>% data.frame() %>% round(2) %>% t())
}

table_fit <- 
  list(model_fit(fit_configural), 
       model_fit(fit_metric), 
       model_fit(fit_scalar), 
       model_fit(fit_strict),
       model_fit(fit_structural)) %>% 
  reduce(rbind)


rownames(table_fit) <- c("Configural", "Metric", "Scalar","Strict","Structural")

table_fit

# Comparing nested models with Anova (based on the chi square difference test)

table_anova <- list(anova(fit_configural, fit_metric),
                    anova(fit_metric, fit_scalar),
                    anova(fit_scalar, fit_strict),
                    anova(fit_strict, fit_structural)) %>%  
  reduce(rbind) %>% 
  .[-c(3, 5, 7),]

table_anova
# Invariance is achieved if there is a non-significant chi-square change
# If p-value non-significant no distance between two models => choose more parsimonious.
# So far we have metric invariance (massive n => small differences show up too).

# Anova usis X^2 => sensitive to departures from normality an dlarge sample size => detects small deviations
# Other fit indices indicate that we do get a reasonable fit.

# More Robust comparison using Bootstrap
# Using the bootstrapLavaan() function to obtain bootstrapped estimates of the model parameters
# boot_configural <- bootstrapLavaan(fit_configural, R = 1000)
# boot_metric <- bootstrapLavaan(fit_metric, R = 100)
boot_scalar <- bootstrapLavaan(fit_scalar, R = 100)
boot_strict <- bootstrapLavaan(fit_strict, R = 100)
boot_structural <- bootstrapLavaan(fit_structural, R = 100)

# Compare the bootstrapped estimates
summary(boot_scalar, fit.measures = TRUE)
summary(boot_strict, fit.measures = TRUE)
summary(boot_structural, fit.measures = TRUE)

################################################################################
### Partial Invariance ### 
################################################################################
# Can we achieve partial scalar invariance?

# Effects of releasing equality constraints across groups:
lavTestScore(fit_scalar) # First output multivariate score test (i.e. Lagrange multiplier test). 
# It indicates whether freeing all equality constraints represents an improvement in fit 
# over the base model (global misfit). In this case we reject the H0.
# 2nd part univariate score test => p17. == .p36 has largest X2 diff.
# It gives MI for each constrained parameter.

# psppsgv => relates to group 2 (Females)
# ustart - value fixed at that value
parTable(fit_scalar)
# the intercepts for psppsgv in the two different blocks of the model 
# appear to be significantly different from each other.
# Man and women answer different at level zero latent factor. Gender influences how they respond.

fit_scalar_psppsgv <- cfa(cfa_model_PolEf3, 
                          data = ess_df_selected,
                          group = "gndr",
                          group.equal = c("loadings", 
                                          "intercepts"),
                          missing = 'direct',
                          group.partial = c(psppsgv~1)
                          
)

summary(fit_scalar_psppsgv, fit.measures = TRUE, standardized=TRUE)
anova(fit_metric, fit_scalar_psppsgv)
#-------------------------------------------------------------------------------
lavTestScore(fit_scalar_psppsgv)
parTable(fit_scalar_psppsgv)

fit_scalar_psppsgv_ptcpplt <- cfa(cfa_model_PolEf3, 
                          data = ess_df_selected,
                          group = "gndr",
                          group.equal = c("loadings", 
                                          "intercepts"),
                          missing = 'direct',
                          group.partial = c(psppsgv~1, ptcpplt ~1)
                          
)
summary(fit_scalar_psppsgv_ptcpplt, fit.measures = TRUE, standardized=TRUE)

table_anova2 <- 
  list(anova(fit_configural, fit_metric),
       anova(fit_metric, fit_scalar),
       anova(fit_metric, fit_scalar_psppsgv),
       anova(fit_metric, fit_scalar_psppsgv_ptcpplt)) %>%  
  reduce(rbind) %>% 
  .[-c(3, 5, 7),]
table_anova2

#-------------------------------------------------------------------------------
lavTestScore(fit_scalar_psppsgv_ptcpplt)
parTable(fit_scalar_psppsgv_ptcpplt)

fit_scalar_psppsgv_ptcpplt_psppipl <- cfa(cfa_model_PolEf3, 
                                  data = ess_df_selected,
                                  group = "gndr",
                                  group.equal = c("loadings", 
                                                  "intercepts"),
                                  missing = 'direct',
                                  group.partial = c(psppsgv~1, ptcpplt ~1, psppipl ~1)
                                  
)

summary(fit_scalar_psppsgv_ptcpplt_psppipl, fit.measures = TRUE, standardized=TRUE)

table_anova3 <- 
  list(anova(fit_configural, fit_metric),
       anova(fit_metric, fit_scalar),
       anova(fit_metric, fit_scalar_psppsgv),
       anova(fit_metric, fit_scalar_psppsgv_ptcpplt),
       anova(fit_metric, fit_scalar_psppsgv_ptcpplt_psppipl)) %>%  
  reduce(rbind) %>% 
  .[-c(3, 5, 7, 9),]

table_anova3 # partial scalar invariance reached

################################################################################
### Multi-group SEM ### 
################################################################################

# We include structural part too. (Can be anything, MIMIC, mediation, etc.)
# Group specific direct and indirect effects 
# We test regression path invariance.

# Group.equal ="loadings" => Metric invariance => We want to compare effects size across groups =>
# => Group specific direct and indirect effects => We need metric invariance.

# Unconstrained model:
model_mediation_mg <- '
# Political efficacy ##
PolEf =~ actrolg + cptppol + psppipl + psppsgv + ptcpplt
actrolg ~~ cptppol

## SatCSA ##
SatCSA =~ stfeco + stfgov + stfdem + stfedu + stfhlth
stfedu ~~ stfhlth

## Direct effect ##
PolEf ~ c("c1", "c2")*hinctnta

## Mediator ##
SatCSA ~ c("a1", "a2")*hinctnta
PolEf ~ c("b1", "b2")*SatCSA

## Indirect effect (a*b) ##
a1b1 := a1*b1
a2b2 := a2*b2

## Total effect c + (a*b) ##
total1 := c1 + (a1*b1)
total2 := c2 + (a2*b2)
'

fit_mediation_mg <- cfa(model_mediation_mg,      
                        data = ess_df_selected, 
                        estimator = "ML",
                        missing = 'direct',
                        group = "gndr",             
                        group.equal = c("loadings")  # equal loadings => metric invariance
                        
)

summary(fit_mediation_mg, fit.measures = TRUE, standardized=TRUE)
# 56 degrees of freedom. Poor fit according to x^2, but good according to alternative indices.
# Factor loadings equal across group - "Estimate"
# Regression effects not equal, because we gave them different names.

#-------------------------------------------------------------------------------

# Fixing the loadings and the path coefficients = regression effects equal across groups.
model_mediation_mg_cons <- '
# Political efficacy ##
PolEf =~ actrolg + cptppol + psppipl + psppsgv + ptcpplt
actrolg ~~ cptppol

## SatCSA ##
SatCSA =~ stfeco + stfgov + stfdem + stfedu + stfhlth
stfedu ~~ stfhlth

## Direct effect ##
PolEf ~ c("c1", "c1")*hinctnta

## Mediator ##
SatCSA ~ c("a1", "a1")*hinctnta
PolEf ~ c("b1", "b1")*SatCSA

## Indirect effect (a*b) ##
a1b1 := a1*b1

## Total effect c + (a*b) ##
total1 := c1 + (a1*b1)
'

fit_mediation_mg_cons <- cfa(model_mediation_mg_cons,      
                             data = ess_df_selected,
                             estimator = "ML",
                             group = "gndr",   
                             missing = 'direct',
                             group.equal = c("loadings")    
                             
)

summary(fit_mediation_mg_cons, fit.measures = TRUE, standardized=TRUE)

# Anova comparison of the two model: (The restricted model will never be a better fit, never have a lower X2.
# H0: They perform equally well (We chose the more parsimonious). HA: They don't => We need to settle for the less restrictive model (less parsimonious). )

anova(fit_mediation_mg, fit_mediation_mg_cons)
# The insignificant P-value implies that the unconstrained and the constrained models are not statistically significantly different. 
# In this case, we go for the more parsimonious=constrained model.
# This means that the path coefficients vary very little by group. 
# and we can analyse the pooled data in a single global model.
################################################################################

# Unconstrained model
model_mediation_mg_full <- '
## Political efficacy ##
PolEf =~ actrolg + cptppol + psppipl + psppsgv + ptcpplt
actrolg ~~ cptppol

## SatCSA ##
SatCSA =~ stfeco + stfgov + stfdem + stfedu + stfhlth
stfedu ~~ stfhlth

## Direct effect ##
PolEf ~ c("c_inc_1", "c_inc_2")*hinctnta
PolEf~ c("c_age_1", "c_age_2")*agea
PolEf ~ c("c_edu_1", "c_edu_2")*edlvdhu

## Mediator ##
# Path A
SatCSA ~ c("a_inc_1", "a_inc_2")*income
SatCSA ~ c("a_age_1", "a_age_2")*age
SatCSA ~ c("a_edu_1", "a_edu_2")*edlvdhu

# Path B
PolEf ~ c("b1", "b2")*SatCSA

## Indirect effect (a*b) ##
# G1 
ab_inc_g1 := a_inc_1*b1
ab_age_g1 := a_age_1*b1
ab_edu_g1 := a_edu_1*b1

# G2 
ab_inc_g2 := a_inc_2*b2
ab_age_g2 := a_age_2*b2
ab_edu_g2 := a_edu_2*b2

## Total effect c + (a*b) ##
# G1 
total_inc_g1 := c_inc_1 + (a_inc_1*b1)
total_age_g1 := c_age_1 + (a_age_1*b1)
total_edu_g1 := c_edu_1 + (a_edu_1*b1)

# G2 
total_inc_g2 := c_inc_2 + (a_inc_2*b2)
total_age_g2 := c_age_2 + (a_age_2*b2)
total_edu_g2 := c_edu_2 + (a_edu_2*b2)
'

fit_mediation_mg <- cfa(model_mediation_mg,      
                        data = ess_df_selected,
                        estimator = "ML",
                        group = "gndr",   
                        missing = 'direct',
                        group.equal = c("loadings")
)

summary(fit_mediation_mg, 
        standardized=TRUE, 
        fit.measures = TRUE
)

anova(fit_mediation_mg, fit_mediation_mg_cons)

# Total and indirect effect and paths do not differ significantly across groups.
# The insignificant P-value implies that the unconstrained and the constrained models are not statistically significantly different. 
# In this case, we go for the more parsimonious=constrained model.
# This means that the path coefficients vary very little by group. 
# and we can analyse the pooled data in a single global model.
# Factor loadings, regression coefficients equal across groups.

semPaths(fit_mediation_mg, what = "std", optimizeLatRes = TRUE, layout = "spring", style = "lisrel2", nCharNodes = 0, intercepts = FALSE, fade = FALSE)

# Unloading packages -----------------------------------------------------------
detach("package:rio", unload = T)
detach("package:dplyr", unload = T)
detach("package:psych", unload = T)
detach("package:stringr", unload = T)
detach("package:purrr", unload = T)
detach("package:lavaan", unload = T)
detach("package:MVN", unload = T)
detach("package:Amelia", unload = T)  
detach("package:mice", unload = T)
detach("package:semTools", unload = T)
detach("package:tidySEM", unload = T)
detach("package:ggplot2", unload = T)
detach("package:semPlot", unload = T)
detach("package:patchwork", unload = T)
detach("package:conflicted", unload = T)
detach("package:GGally", unload = T)
detach("package:Hmisc", unload = T)

