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
output <- file.path("/Users/lilivandermeersch/Library/Mobile Documents/com~apple~CloudDocs/❤️ MSc Statistics and Data Science ❤️/☀️🌸 Year 3./II./SEM/Assignment/plots")

# Packages -------------------------------------------------------------------------------------------------

### DATA MANIPULATION ###
library(haven)        
library(dplyr)      
library(psych)
library(stringr)
library(purrr)

### MODELING ###
library(lavaan)  
library(GPArotation)
library(MVN)
library(Amelia)
library(mice)

### VISUALIZATION ###
library(corrplot)     
library(tidySEM) 
library(plotly)
library(semPlot)
library(patchwork)
library(Hmisc)

##########################################################################################
# Importing the data
ess_df <- haven::read_sav("/Users/lilivandermeersch/Library/Mobile Documents/com~apple~CloudDocs/❤️ MSc Statistics and Data Science ❤️/☀️🌸 Year 3./II./SEM/Assignment/ESS7/ESS-Data-Wizard-subset-2023-05-03/ESS7.sav")

# Data Exploration ------------------------------------------------------------------
View(ess_df) 

nrow(ess_df)      # number of subjects
ncol(ess_df)      # number of variables
names(ess_df)     # names of variables

# Filter for country
# ess_df <- filter(ess_df, cntry == 'HU')
ess_df <- filter(ess_df, cntry == 'BE')

# Missing values
# count NAs per variable
na_counts <- colSums(is.na(ess_df))
# print the results
na_counts

# Subsetting the data
ess_df_selected <- ess_df %>% select(
  imtcjob, # Immigrants take jobs away in country or create new jobs
  imbleco, # Taxes and services: immigrants take out more than they put in or less
  imwbcrm, # Immigrants make country's crime problems worse or better
  gvtrimg, # Compared to yourself government treats new immigrants better or worse
  smctmbe, # Some cultures: much better or all equal 
  smegbhw, # Some races or ethnic groups: born harder working
  smegbli, # Some races or ethnic groups: born less intelligent
  fclcntr, # Feel close to country
  pplstrd, # Better for a country if almost everyone shares customs and traditions
  lwdscwp, # Law against ethnic discrimination in workplace good/bad for a country
  rlgueim, # Religious beliefs and practices undermined or enriched by immigrants
  acetalv, # People of minority race/ethnic group in current living area
  dfegcf, # Different race or ethnic group: have any close friends
  dfegcon, # Different race or ethnic group: contact, how often
  dfeghbg, # Different race or ethnic group: contact, how bad or good
  imdetmr, # Immigrant different race/ethnic group majority: married closerelative
  imdetbs, # Immigrant different race/ethnic group majority: your boss
  qfimedu, # Qualification for immigration: good educational ImPolicy
  qfimlng, # Qualification for immigration: speak country's official language
  qfimchr, # Qualification for immigration: Christian background
  qfimwht, # Qualification for immigration: be white
  qfimwsk, # Qualification for immigration: work skills needed in country
  qfimcmt, # Qualification for immigration: committed to way of life in country
  almuslv, # Allow many or few Muslims to come and live in country
  algyplv, # Allow many or few Gypsies to come and live in country
  aljewlv, # Allow many or few Jewish people to come and live in country
  gvrfgap, # Government should be generous judging applications for refugee status
  impcntr, # Allow many/few immigrants from poorer countries outside Europe
  eimpcnt, # Allow many/few immigrants from poorer countries in Europe
  imbgeco, # Immigration bad or good for the country's economy
  imueclt, # Country's cultural life undermined or enriched by immigrants
  imwbcnt, # Immigrants make the country worse or better place to live
  gndr, # Gender
  hinctnta, # Income
  eduyrs, # Education in years
  agea, # Age
  pplfair,
  pplhlp,
  ppltrst
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
# ess_df_selected_complete <- ess_df_selected %>% na.omit()
# num_missing_obs <- nrow(ess_df_selected) - nrow(ess_df_selected_complete) # listwise deletion => not recommended

# count NAs per variable
na_counts <- colSums(is.na(ess_df_selected))
# print the results
na_counts

# Significant number (proportionally) of missing data. Can't be sure if all MCAR, 
# in fact probably not => ***list-wise*** deletion not recommended, might introduce bias,
# decrease stat power.

# ***Case-wise*** (or ‘full information’) maximum likelihood (FIML) => The default in CFA lavaan

# ***Multiple imputation***
# Mice => more flexible with different data types than Amelia
# Amelia was used during lectures-------------------------------------------------

#---------------------------------------------------------------------------------
# MICE:
# Perform multiple imputation for missing data
## => Revisit later!

md.pattern(ess_df_selected, rotate.names = TRUE) # pattern of missing data

# haven_labelled columns to numeric or factor so that mice can handle it
ess_df_selected_imp <- ess_df_selected %>% mutate(across(where(is.labelled), as.numeric))

imputed_data <- mice(ess_df_selected_imp, m=5, method="pmm", maxit=10)
# summary(imputed_data)

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

###########################################################################################
### EFA ### (off-the-records) :)                                                          
###########################################################################################
# Standardization
ess_df_selected_standardized <- data.frame(scale(ess_df_selected_imp))
cor_matrix <- cor(ess_df_selected_standardized, use="pairwise.complete.obs")
corrplot(cor_matrix, method="color", type="upper", tl.col="black", tl.srt=45)
# Preliminary tests - if the correlation matrix is suitable for factor analysis
## Kaiser–Meyer–Olkin (KMO)
KMO(cor_matrix)
## Bartlett’s test of sphericity
cortest.bartlett(R = cor_matrix, n = 1698)
det(cor_matrix)
eigen(cor_matrix)$values
# Perform parallel analysis
# par_analysis <- fa.parallel(ess_df_selected, n.iter = 100, fa = "both")
# Parallel analysis suggests that the number of factors =  10 
# or
number_items <- fa.parallel(ess_df_selected_standardized,
                            fm ='ml',
                            fa ="fa")

# Solid line -> original Kaiser criteria => 3
# Parallel => Red simulated and re-sampled data => Where the blue and red lines 
# intersect => everything above, those eigen values can't be by chance => 8?
# Screeplot -> infliction point: 4
# Eigenvalues
sum(number_items$fa.values > 1) # Kaiser
sum(number_items$fa.values > 0.7) # Less conservative Kaiser

# Specifying the number of factors to extract 
num_factors <- 9

# Perform EFA
efa_results1 <- fa(ess_df_selected_standardized, nfactors = num_factors, rotate = "oblimin", fm = "ml")

# Print EFA results
print(efa_results1)

# Plot factor loadings
fa.diagram(efa_results1)

# fa.plot(efa_results, labels = colnames(ess_df_selected_standardized))

#-----------------------------------------------------------------------------------
# Extract factor loadings
loadings <- as.data.frame.matrix(efa_results1$loadings)
# loadings <- as.data.frame.matrix(efa_results2$loadings)

# Prepare the data for ggplot2
loadings$variable <- rownames(loadings)
loadings <- reshape2::melt(loadings, id.vars = "variable", variable.name = "factor")

# Create the bar graph
ggplot(data = loadings, aes(x = variable, y = value, fill = factor)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Factor Loadings Bar Graph",
       x = "Variables",
       y = "Factor Loadings") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_brewer(palette = "Set3", name = "Factors")

#############################################################################
### CFA  ###
#############################################################################
                                                         
####################################
### Specifying the CFA model (1) ###
####################################
cfa_model <- '
PerceivedImpact =~ imbleco + imwbcnt + imtcjob + imbgeco + imwbcrm + imueclt + pplstrd
GFEnmity =~ aljewlv + almuslv + algyplv + eimpcnt + impcntr
ImPolicy=~ qfimedu + qfimlng + qfimwsk + qfimcmt
InterethnicRel =~ dfegcon + dfegcf + acetalv
Ethnocentrism =~ imdetbs + imdetmr + qfimchr + qfimwht
'


fit_cfa <- cfa(cfa_model,             # model formula
               estimator = "MLM",
               data = ess_df_selected_imp  # data frame
               )

fit_cfa_wlsmv <- cfa(cfa_model, 
                    ordered = c("imbleco", "imwbcnt", "imtcjob", "imbgeco", "imwbcrm", "imueclt", "pplstrd",
                                "aljewlv", "almuslv", "algyplv", "eimpcnt", "impcntr",
                                "qfimedu", "qfimlng", "qfimwsk", "qfimcmt",
                                "dfegcon", "dfegcf", "acetalv", 
                                "imdetbs", "imdetmr", "qfimchr", "qfimwht"),
                    data = ess_df,
                    estimator = "WLSMV")
                    
summary(fit_cfa, 
        fit.measures = TRUE, # returns commonly used fit measures  
        standardized = TRUE  # indicates that we want standardized results
)
# fclcntr => seem to have no significant correlation with either underlying factor
# acetalv also seems to have low factor loading => reverse coded compared to other two =>
# => Equiesence bias probably (since data was filtered for careless respondents)

summary(fit_cfa_wlsmv, 
        fit.measures = TRUE, # returns commonly used fit measures  
        standardized = TRUE  # indicates that we want standardized results
)

### Model fit statistics (1) ###
## Global
# * (some) rules of thumb: CFI/TLI>0.95,RMSEA<0.05,SRMR<0.06
# * current practice is: chi-square value + df + pvalue, RMSEA, CFI and SRMR
fitMeasures(fit_cfa, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
fitMeasures(fit_cfa_wlsmv, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
## Local = Modification Indexes
mi <- inspect(fit_cfa,"mi")
mi.sorted <- mi[order(-mi$mi),] # sort from high to low mi.sorted[1:5,] # only display some large MI values
mi.sorted[1:5,] # only display some large MI values

##################################
### Model re-specification (2) ###
##################################
cfa_model2 <- '
PerceivedImpact =~ imbleco + imwbcnt + imtcjob + imbgeco + imwbcrm + imueclt + pplstrd
GFEnmity =~ aljewlv + almuslv + algyplv + eimpcnt + impcntr
ImPolicy=~ qfimedu + qfimlng + qfimwsk + qfimcmt
InterethnicRel =~ dfegcon + dfegcf + acetalv
Ethnocentrism =~ imdetbs + imdetmr + qfimchr + qfimwht
eimpcnt ~~ impcntr
'

fit_cfa2 <- cfa(cfa_model2,
                estimator = "MLM",
                data = ess_df_selected_imp
                )

summary(fit_cfa2, 
        fit.measures = TRUE, # returns commonly used fit measures  
        standardized = TRUE  # indicates that we want standardized results
)

### Model fit statistics (2) ###
## Global
# * (some) rules of thumb: CFI/TLI>0.95,RMSEA<0.05,SRMR<0.06
# * current practice is: chi-square value + df + pvalue, RMSEA, CFI and SRMR
fitMeasures(fit_cfa2, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
## Local = Modification Indexes
mi <- inspect(fit_cfa2,"mi")
mi.sorted <- mi[order(-mi$mi),] # sort from high to low mi.sorted[1:5,] # only display some large MI values
mi.sorted[1:5,] # only display some large MI values

##################################################################################
### Model re-specification (3) ###
#######################################
cfa_model3 <- '
PerceivedImpact =~ imbleco + imwbcnt + imtcjob + imbgeco + imwbcrm + imueclt + pplstrd
GFEnmity =~ aljewlv + almuslv + algyplv + eimpcnt + impcntr
ImPolicy=~ qfimedu + qfimlng + qfimwsk + qfimcmt
InterethnicRel =~ dfegcon + dfegcf + acetalv
Ethnocentrism =~ imdetbs + imdetmr + qfimchr + qfimwht
eimpcnt ~~ impcntr
qfimchr ~~ qfimwht
'

fit_cfa3 <- cfa(cfa_model3,       
                estimator = "MLM",
                data = ess_df_selected_imp
)

summary(fit_cfa3, 
        fit.measures = TRUE, # returns commonly used fit measures  
        standardized = TRUE  # indicates that we want standardized results
)

### Model fit statistics ###
## Global
# * (some) rules of thumb: CFI/TLI>0.95,RMSEA<0.05,SRMR<0.06
# * current practice is: chi-square value + df + pvalue, RMSEA, CFI and SRMR
fitMeasures(fit_cfa3, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
## Local = Modification Indexes
mi <- inspect(fit_cfa3,"mi")
mi.sorted <- mi[order(-mi$mi),] # sort from high to low mi.sorted[1:5,] # only display some large MI values
mi.sorted[1:5,] # only display some large MI values

##################################################################################
### Model re-specification (4) ###
#######################################
cfa_model4 <- '
PerceivedImpact =~ imbleco + imwbcnt + imtcjob + imbgeco + imwbcrm + imueclt 
GFEnmity =~ aljewlv + almuslv + algyplv + eimpcnt + impcntr
ImPolicy=~ qfimedu + qfimlng + qfimwsk + qfimcmt
InterethnicRel =~ dfegcon + dfegcf 
Ethnocentrism =~ imdetbs + imdetmr + qfimchr + qfimwht
eimpcnt ~~ impcntr
qfimchr ~~ qfimwht
'

fit_cfa4 <- cfa(cfa_model4,             # model formula
                estimator = "MLM",
                data = ess_df_selected_imp
)

summary(fit_cfa4, 
        fit.measures = TRUE, # returns commonly used fit measures  
        standardized = TRUE  # indicates that we want standardized results
)

### Model fit statistics ###
## Global
# * (some) rules of thumb: CFI/TLI>0.95,RMSEA<0.05,SRMR<0.06
# * current practice is: chi-square value + df + pvalue, RMSEA, CFI and SRMR
fitMeasures(fit_cfa4, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
## Local = Modification Indexes
mi <- inspect(fit_cfa4,"mi")
mi.sorted <- mi[order(-mi$mi),] # sort from high to low mi.sorted[1:5,] # only display some large MI values
mi.sorted[1:5,] # only display some large MI values

##################################################################################
### Model re-specification (5) ###
#######################################
cfa_model5 <- '
PerceivedImpact =~ imbleco + imwbcnt + imtcjob + imbgeco + imwbcrm  
GFEnmity =~ aljewlv + almuslv + algyplv + eimpcnt + impcntr
ImPolicy=~ qfimedu + qfimlng + qfimwsk 
InterethnicRel =~ dfegcon + dfegcf + acetalv
Ethnocentrism =~ imdetbs + imdetmr + qfimchr + qfimwht
eimpcnt ~~ impcntr
qfimchr ~~ qfimwht
'

fit_cfa5 <- cfa(cfa_model5,             # model formula
                estimator = "MLM",
                data = ess_df_selected_imp
)

summary(fit_cfa5, 
        fit.measures = TRUE, # returns commonly used fit measures  
        standardized = TRUE  # indicates that we want standardized results
)

### Model fit statistics ###
## Global
# * (some) rules of thumb: CFI/TLI>0.95,RMSEA<0.05,SRMR<0.06
# * current practice is: chi-square value + df + pvalue, RMSEA, CFI and SRMR
fitMeasures(fit_cfa5, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
## Local = Modification Indexes
mi <- inspect(fit_cfa5,"mi")
mi.sorted <- mi[order(-mi$mi),] # sort from high to low mi.sorted[1:5,] # only display some large MI values
mi.sorted[1:5,] # only display some large MI values

##################################################################################
### Model re-specification (6) ###
#######################################
cfa_model6 <- '
PerceivedImpact =~ imbleco + imwbcnt + imtcjob + imbgeco + imwbcrm  
GFEnmity =~ aljewlv + almuslv + algyplv + eimpcnt + impcntr
ImPolicy=~ qfimedu + qfimlng + qfimwsk 
InterethnicRel =~ dfegcon + dfegcf + acetalv
Ethnocentrism =~ imdetbs + imdetmr + qfimchr + qfimwht
eimpcnt ~~ impcntr
qfimchr ~~ qfimwht
qfimlng ~~ qfimwsk
'

fit_cfa6 <- cfa(cfa_model6,             # model formula
                estimator = "MLM",
                data = ess_df_selected_imp
)

summary(fit_cfa6, 
        fit.measures = TRUE, # returns commonly used fit measures  
        standardized = TRUE  # indicates that we want standardized results
)

### Model fit statistics ###
## Global
# * (some) rules of thumb: CFI/TLI>0.95,RMSEA<0.05,SRMR<0.06
# * current practice is: chi-square value + df + pvalue, RMSEA, CFI and SRMR
fitMeasures(fit_cfa6, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
## Local = Modification Indexes
mi <- inspect(fit_cfa6,"mi")
mi.sorted <- mi[order(-mi$mi),] # sort from high to low mi.sorted[1:5,] # only display some large MI values
mi.sorted[1:5,] # only display some large MI values

# Visualization
semPaths(fit_cfa6, what = "std", groups = "latents" , optimizeLatRes = TRUE, layout = "tree2", nCharNodes = 0)

##################################################################################
### 1-latent factor CFAs ###
#######################################
### PerceivedImpact ###
#######################
cfa_model_PerceivedImpact <- '
PerceivedImpact =~ imbleco + imwbcnt + imtcjob + imbgeco + imwbcrm  
'

fit_cfa_PerceivedImpact <- cfa(cfa_model_PerceivedImpact,             # model formula
                 estimator = "MLM",
                 data = ess_df_selected_imp
)

summary(fit_cfa_PerceivedImpact, 
        fit.measures = TRUE, # returns commonly used fit measures  
        standardized = TRUE  # indicates that we want standardized results
)

### Model fit statistics ###
## Global
# * (some) rules of thumb: CFI/TLI>0.95,RMSEA<0.05,SRMR<0.06
# * current practice is: chi-square value + df + pvalue, RMSEA, CFI and SRMR
fitMeasures(fit_cfa_PerceivedImpact, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
## Local = Modification Indexes
mi <- inspect(fit_cfa_PerceivedImpact,"mi")
mi.sorted <- mi[order(-mi$mi),] # sort from high to low mi.sorted[1:5,] # only display some large MI values
mi.sorted[1:5,] # only display some large MI values

################
### GFEnmity ###
################
cfa_model_G <- '
GFEnmity =~ aljewlv + almuslv + algyplv + eimpcnt + impcntr
eimpcnt ~~ impcntr
'

fit_cfa_G <- cfa(cfa_model_G,             # model formula
                 estimator = "MLM",
                 data = ess_df_selected_imp
)

summary(fit_cfa_G, 
        fit.measures = TRUE, # returns commonly used fit measures  
        standardized = TRUE  # indicates that we want standardized results
)

### Model fit statistics ###
## Global
# * (some) rules of thumb: CFI/TLI>0.95,RMSEA<0.05,SRMR<0.06
# * current practice is: chi-square value + df + pvalue, RMSEA, CFI and SRMR
fitMeasures(fit_cfa_G, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
## Local = Modification Indexes
mi <- inspect(fit_cfa_G,"mi")
mi.sorted <- mi[order(-mi$mi),] # sort from high to low mi.sorted[1:5,] # only display some large MI values
mi.sorted[1:5,] # only display some large MI values

#######################################
###ImPolicy###
###########
cfa_model_ImPolicy<- '
ImPolicy=~ qfimedu + qfimlng + qfimwsk + qfimcmt
'

fit_cfa_ImPolicy<- cfa(cfa_model_ImPolicy,
                   estimator = "MLM",
                   data = ess_df_selected_imp
)

summary(fit_cfa_ImPolicy, 
        fit.measures = TRUE, # returns commonly used fit measures  
        standardized = TRUE  # indicates that we want standardized results
)

### Model fit statistics ###
## Global
# * (some) rules of thumb: CFI/TLI>0.95,RMSEA<0.05,SRMR<0.06
# * current practice is: chi-square value + df + pvalue, RMSEA, CFI and SRMR
fitMeasures(fit_cfa_ImPolicy, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
## Local = Modification Indexes
mi <- inspect(fit_cfa_ImPolicy,"mi")
mi.sorted <- mi[order(-mi$mi),] # sort from high to low mi.sorted[1:5,] # only display some large MI values
mi.sorted[1:5,] # only display some large MI values

#---------------------------------------------------------------
######################
### InterethnicRel ###
######################

cfa_model_IntEtn <- '
InterethnicRel =~ dfegcon + dfegcf + acetalv
'

fit_cfa_IntEtn <- cfa(cfa_model_IntEtn,             # model formula
                 estimator = "MLM",
                 data = ess_df_selected_imp
)

summary(fit_cfa_IntEtn, 
        fit.measures = TRUE, # returns commonly used fit measures  
        standardized = TRUE  # indicates that we want standardized results
)

#---------------------------------------------------------------
#####################
### Ethnocentrism ###
#####################

cfa_model_EC <- '
Ethnocentrism =~ imdetbs + imdetmr + qfimchr + qfimwht
qfimchr ~~ qfimwht
'

fit_cfa_EC <- cfa(cfa_model_EC,             # model formula
                 estimator = "MLM",
                 data = ess_df_selected_imp
)

summary(fit_cfa_EC, 
        fit.measures = TRUE, # returns commonly used fit measures  
        standardized = TRUE  # indicates that we want standardized results
)

fitMeasures(fit_cfa_EC, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
mi <- inspect(fit_cfa_EC,"mi")
mi.sorted <- mi[order(-mi$mi),] 
mi.sorted[1:5,]

##################################################################################
### Model re-specification (7) ###
#######################################
### 4 FACTORS ###
cfa_model7 <- '
PerceivedImpact =~ imbleco + imwbcnt + imtcjob + imbgeco + imwbcrm   
GFEnmity =~ aljewlv + almuslv + algyplv + eimpcnt + impcntr
InterethnicRel =~ dfegcon + dfegcf + acetalv
Ethnocentrism =~ imdetbs + imdetmr + qfimchr + qfimwht
'

# Left out:ImPolicy=~ qfimedu + qfimlng + qfimwsk
# eimpcnt ~~ impcntr
# qfimchr ~~ qfimwht
# imdetbs ~~ imdetmr

fit_cfa7 <- cfa(cfa_model7,             # model formula
                estimator = "MLM",
                data = ess_df_selected_imp
)

summary(fit_cfa7, 
        fit.measures = TRUE, # returns commonly used fit measures  
        standardized = TRUE  # indicates that we want standardized results
)

### Model fit statistics ###
## Global
# * (some) rules of thumb: CFI/TLI>0.95,RMSEA<0.05,SRMR<0.06
# * current practice is: chi-square value + df + pvalue, RMSEA, CFI and SRMR
fitMeasures(fit_cfa7, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
## Local = Modification Indexes
mi <- inspect(fit_cfa7,"mi")
mi.sorted <- mi[order(-mi$mi),] # sort from high to low mi.sorted[1:5,] # only display some large MI values
mi.sorted[1:5,] # only display some large MI values

##################################################################################
### Model re-specification (8) ###
#######################################
### 4 FACTORS ###
cfa_model8 <- '
PerceivedImpact =~ imbleco + imwbcnt + imtcjob + imbgeco + imwbcrm   
GFEnmity =~ aljewlv + almuslv + algyplv + eimpcnt + impcntr
InterethnicRel =~ dfegcon + dfegcf + acetalv
Ethnocentrism =~ imdetbs + imdetmr + qfimchr + qfimwht
eimpcnt ~~ impcntr
'

# Left out:ImPolicy=~ qfimedu + qfimlng + qfimwsk
# qfimchr ~~ qfimwht
# imdetbs ~~ imdetmr

fit_cfa8 <- cfa(cfa_model8,             # model formula
                estimator = "MLM",
                data = ess_df_selected_imp
)

summary(fit_cfa8, 
        fit.measures = TRUE, # returns commonly used fit measures  
        standardized = TRUE  # indicates that we want standardized results
)

### Model fit statistics ###
## Global
# * (some) rules of thumb: CFI/TLI>0.95,RMSEA<0.05,SRMR<0.06
# * current practice is: chi-square value + df + pvalue, RMSEA, CFI and SRMR
fitMeasures(fit_cfa8, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
## Local = Modification Indexes
mi <- inspect(fit_cfa8,"mi")
mi.sorted <- mi[order(-mi$mi),] # sort from high to low mi.sorted[1:5,] # only display some large MI values
mi.sorted[1:5,] # only display some large MI values

##################################################################################
### Model re-specification (9) ###
#######################################
### 4 FACTORS ###
cfa_model9 <- '
PerceivedImpact =~ imbleco + imwbcnt + imtcjob + imbgeco + imwbcrm   
GFEnmity =~ aljewlv + almuslv + algyplv + eimpcnt + impcntr
InterethnicRel =~ dfegcon + dfegcf + acetalv
Ethnocentrism =~ imdetbs + imdetmr + qfimchr + qfimwht
eimpcnt ~~ impcntr
qfimchr ~~ qfimwht
'

# Left out:ImPolicy=~ qfimedu + qfimlng + qfimwsk
# imdetbs ~~ imdetmr

fit_cfa9 <- cfa(cfa_model9,             # model formula
                estimator = "MLM",
                data = ess_df_selected_imp
)

summary(fit_cfa9, 
        fit.measures = TRUE, # returns commonly used fit measures  
        standardized = TRUE  # indicates that we want standardized results
)

### Model fit statistics ###
## Global
# * (some) rules of thumb: CFI/TLI>0.95,RMSEA<0.05,SRMR<0.06
# * current practice is: chi-square value + df + pvalue, RMSEA, CFI and SRMR
fitMeasures(fit_cfa9, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
## Local = Modification Indexes
mi <- inspect(fit_cfa9,"mi")
mi.sorted <- mi[order(-mi$mi),] # sort from high to low mi.sorted[1:5,] # only display some large MI values
mi.sorted[1:5,] # only display some large MI values

### Model diagnostics  ### 
# Observed covariance matrix.
lavInspect(fit_cfa9, "sampstat")
# Implied covariance matrix.
fitted(fit_cfa9)
# The gap
residuals(fit_cfa9)
# SEs of free parameters in the model
lavTech(fit_cfa9, "se")
# Parameter estimates & C.I.
parameterEstimates(fit_cfa9, standardized=TRUE)
# Factor loadings
inspect(fit_cfa9,           # fitted model 
        what="std")$lambda  # standardized factors loadings
# Residual variances
theta <- round(inspect(fit_cfa9, "est")$theta,3)
theta.std <- round(inspect(fit_cfa9, "std")$theta,3) 
r2 <- round(inspect(fit_cfa9, "r2"),3)

data.frame(row.names = c(),                       # empty the columns names 
           Variables = colnames(theta),           # variable names 
           "Residuals" = diag(theta),             # diagonal theta
           "Std. Residuals" = diag(theta.std),    # diagonal std. theta
           "R Squared" = r2                       # R-squared
)

# Visualization
semPaths(fit_cfa9, what = "std", groups = "latents" , optimizeLatRes = TRUE, layout = "tree2", nCharNodes = 0)

# Model Diagnostics Comparison --------------------------------------------------------------------
# Anova() only works with estimator = "ML"
# 5-factor
anova(fit_cfa, fit_cfa2) 
anova(fit_cfa2, fit_cfa3)

# 4-factor
anova(fit_cfa7, fit_cfa8) 
anova(fit_cfa8, fit_cfa9)

fit_indices <- data.frame(
  Fit_Indices = c("logl","chisq", "df", "pvalue", "cfi", "tli","rmsea"),
  fit_cfa7 = c(round(fitMeasures(fit_cfa7)["logl"], 2), round(fitMeasures(fit_cfa7)["chisq"], 2), round(fitMeasures(fit_cfa7)["df"], 2), round(fitMeasures(fit_cfa7)["pvalue"], 2), round(fitMeasures(fit_cfa7)["cfi"], 2), round(fitMeasures(fit_cfa7)["tli"], 2), round(fitMeasures(fit_cfa7)["rmsea"], 2)),
  fit_cfa8 = c(round(fitMeasures(fit_cfa8)["logl"], 2), round(fitMeasures(fit_cfa8)["chisq"], 2), round(fitMeasures(fit_cfa8)["df"], 2), round(fitMeasures(fit_cfa8)["pvalue"], 2), round(fitMeasures(fit_cfa8)["cfi"], 2), round(fitMeasures(fit_cfa8)["tli"], 2), round(fitMeasures(fit_cfa8)["rmsea"], 2)),
  fit_cfa9 = c(round(fitMeasures(fit_cfa9)["logl"], 2), round(fitMeasures(fit_cfa9)["chisq"], 2), round(fitMeasures(fit_cfa9)["df"], 2), round(fitMeasures(fit_cfa9)["pvalue"], 2), round(fitMeasures(fit_cfa9)["cfi"], 2), round(fitMeasures(fit_cfa9)["tli"], 2), round(fitMeasures(fit_cfa9)["rmsea"], 2))
)

fit_indices

#  The ***"MLR"*** estimator provides a scaled chi-square test statistic, 
# which has a scaling correction factor. To compare nested models with the "MLR" estimator, 
# we need to perform a scaled chi-square difference test ......

#############################################################################
### 2nd order CFA  ###
#############################################################################
# Second-order CFA model => Attitudes Towards Immigration and Ethnic/Cultural Diversity
cfa_model9_2nd_order <- '
  Attitude =~ PerceivedImpact + GFEnmity + InterethnicRel + Ethnocentrism

  PerceivedImpact =~ imbleco + imwbcnt + imtcjob + imbgeco + imwbcrm   
  GFEnmity =~ aljewlv + almuslv + algyplv + eimpcnt + impcntr
  InterethnicRel =~ dfegcon + dfegcf + acetalv
  Ethnocentrism =~ imdetbs + imdetmr + qfimchr + qfimwht

  eimpcnt ~~ impcntr
  qfimchr ~~ qfimwht
'

fit_cfa9_2nd_order <- cfa(cfa_model9_2nd_order,
                          estimator = "MLM",
                          data = ess_df_selected_imp
                          )

summary(fit_cfa9_2nd_order,
        fit.measures = TRUE,
        standardized = TRUE)

fitMeasures(fit_cfa9_2nd_order, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
## Local = Modification Indexes
mi <- inspect(fit_cfa9_2nd_order,"mi")
mi.sorted <- mi[order(-mi$mi),] # sort from high to low mi.sorted[1:5,] # only display some large MI values
mi.sorted[1:5,] # only display some large MI values

#---------------------------------------------------------------------------------
# Second-order CFA model 2
cfa_model9_2nd_order2 <- '
  Attitude =~ PerceivedImpact + GFEnmity + InterethnicRel + Ethnocentrism

  PerceivedImpact =~ imbleco + imwbcnt + imtcjob + imbgeco + imwbcrm   
  GFEnmity =~ aljewlv + almuslv + algyplv + eimpcnt + impcntr
  InterethnicRel =~ dfegcon + dfegcf + acetalv
  Ethnocentrism =~ imdetbs + imdetmr + qfimchr + qfimwht

  eimpcnt ~~ impcntr
  qfimchr ~~ qfimwht
  InterethnicRel ~~ Ethnocentrism
'
fit_cfa9_2nd_order2 <- cfa(cfa_model9_2nd_order2,
                           estimator = "MLM",
                           data = ess_df_selected_imp)
summary(fit_cfa9_2nd_order2, fit.measures = TRUE, standardized = TRUE)

fitMeasures(fit_cfa9_2nd_order2, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")

# Visualization
semPaths(fit_cfa9_2nd_order2, what = "std", groups = "latents" , optimizeLatRes = TRUE, layout = "tree", nCharNodes = 0)

# Comparison: 1st 2nd order model and last 1st order (Anova)
anova(fit_cfa9, fit_cfa9_2nd_order)

# Comparison: 2 second order models (Anova)
anova(fit_cfa9_2nd_order, fit_cfa9_2nd_order2)
#####################################################################################
### MIMIC ### (Adding the SEM part)
#############

ess_df_selected_imp$age = ess_df_selected_imp$agea
ess_df_selected_imp$gender = ess_df_selected_imp$gndr
ess_df_selected_imp$income = ess_df_selected_imp$hinctnta
ess_df_selected_imp$fair = ess_df_selected_imp$pplfair
ess_df_selected_imp$help = ess_df_selected_imp$pplhlp
ess_df_selected_imp$trusts = ess_df_selected_imp$ppltrst

View(ess_df_selected_imp)

# Data Exploration
descriptive_ess_imp <- as.data.frame(psych::describe(ess_df_selected_imp))

descriptive_ess_imp <- dplyr::select(descriptive_ess_imp, 
                                 n,
                                 mean,
                                 sd,
                                 median,
                                 min,
                                 max,
                                 skew,
                                 kurtosis)

descriptive_ess_imp

### MIMIC MODEL ###
# Now that we are more confident that our measurement model is valid:
model_mimic <-'
Attitude =~ PerceivedImpact + GFEnmity + InterethnicRel + Ethnocentrism

  PerceivedImpact =~ imbleco + imwbcnt + imtcjob + imbgeco + imwbcrm   
  GFEnmity =~ aljewlv + almuslv + algyplv + eimpcnt + impcntr
  InterethnicRel =~ dfegcon + dfegcf + acetalv
  Ethnocentrism =~ imdetbs + imdetmr + qfimchr + qfimwht

  eimpcnt ~~ impcntr
  qfimchr ~~ qfimwht
  InterethnicRel ~~ Ethnocentrism

Attitude ~ gender + age + eduyrs + income
'

fit_mimic <- sem(model_mimic, # model formula
                 estimator = "MLM",   
                 data=ess_df_selected_imp      # data frame
)

summary(fit_mimic, standardized=TRUE)

# Fit not satisfactory => output shouldn't be interpreted.
fitMeasures(fit_mimic, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")

# MIMIC 2 --------------------------------------------------------------------------
model_mimic2 <-'
Attitude =~ PerceivedImpact + GFEnmity + InterethnicRel + Ethnocentrism

  PerceivedImpact =~ imbleco + imwbcnt + imtcjob + imbgeco + imwbcrm   
  GFEnmity =~ aljewlv + almuslv + algyplv + eimpcnt + impcntr
  InterethnicRel =~ dfegcon + dfegcf + acetalv
  Ethnocentrism =~ imdetbs + imdetmr + qfimchr + qfimwht

  eimpcnt ~~ impcntr
  qfimchr ~~ qfimwht
  InterethnicRel ~~ Ethnocentrism

Attitude ~ gender
'

fit_mimic2 <- sem(model_mimic2, # model formula
                  estimator = "MLM",
                  data=ess_df_selected_imp      # data frame
)

summary(fit_mimic2, standardized=TRUE)

# Fit satisfactory
fitMeasures(fit_mimic2, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")


# MIMIC 3 ----------------------------------------------------------------------------------

model_mimic3 <-'
Attitude =~ PerceivedImpact + GFEnmity + InterethnicRel + Ethnocentrism

  PerceivedImpact =~ imbleco + imwbcnt + imtcjob + imbgeco + imwbcrm   
  GFEnmity =~ aljewlv + almuslv + algyplv + eimpcnt + impcntr
  InterethnicRel =~ dfegcon + dfegcf + acetalv
  Ethnocentrism =~ imdetbs + imdetmr + qfimchr + qfimwht

  eimpcnt ~~ impcntr
  qfimchr ~~ qfimwht
  InterethnicRel ~~ Ethnocentrism

Attitude ~ age
'

fit_mimic3 <- sem(model_mimic3, # model formula
                  estimator = "MLM",
                  data=ess_df_selected_imp      # data frame
)

summary(fit_mimic3, standardized=TRUE)

# Fit not satisfactory
fitMeasures(fit_mimic3, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")

# MIMIC 4 ------------------------------------------------------------------------------------------

model_mimic4 <-'
Attitude =~ PerceivedImpact + GFEnmity + InterethnicRel + Ethnocentrism

  PerceivedImpact =~ imbleco + imwbcnt + imtcjob + imbgeco + imwbcrm   
  GFEnmity =~ aljewlv + almuslv + algyplv + eimpcnt + impcntr
  InterethnicRel =~ dfegcon + dfegcf + acetalv
  Ethnocentrism =~ imdetbs + imdetmr + qfimchr + qfimwht

  eimpcnt ~~ impcntr
  qfimchr ~~ qfimwht
  InterethnicRel ~~ Ethnocentrism

Attitude ~ eduyrs
'

fit_mimic4 <- sem(model_mimic4, # model formula
                  estimator = "MLM",
                  data=ess_df_selected_imp      # data frame
)

summary(fit_mimic4, standardized=TRUE)

# Fit better but not satisfactory
fitMeasures(fit_mimic4, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")

# MIMIC 5 ------------------------------------------------------------------------------------------

model_mimic5 <-'
Attitude =~ PerceivedImpact + GFEnmity + InterethnicRel + Ethnocentrism

  PerceivedImpact =~ imbleco + imwbcnt + imtcjob + imbgeco + imwbcrm   
  GFEnmity =~ aljewlv + almuslv + algyplv + eimpcnt + impcntr
  InterethnicRel =~ dfegcon + dfegcf + acetalv
  Ethnocentrism =~ imdetbs + imdetmr + qfimchr + qfimwht

  eimpcnt ~~ impcntr
  qfimchr ~~ qfimwht
  InterethnicRel ~~ Ethnocentrism

Attitude ~ income
'

fit_mimic5 <- sem(model_mimic5, # model formula
                  estimator = "MLM",
                  data=ess_df_selected_imp      # data frame
)

summary(fit_mimic5, standardized=TRUE)

# Fit better but still not not satisfactory
fitMeasures(fit_mimic5, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")

# MIMIC SUB 1 Ethnocentrism ---------------------------------------------------

model_mimic_sub1 <-'
Ethnocentrism =~ imdetbs + imdetmr + qfimchr + qfimwht
qfimchr ~~ qfimwht

Ethnocentrism ~ gender + age + eduyrs + income
'

fit_mimic_sub1 <- sem(model_mimic_sub1, # model formula
                      estimator = "MLM",
                      data=ess_df_selected_imp      # data frame
)

summary(fit_mimic_sub1, standardized=TRUE)

fitMeasures(fit_mimic_sub1, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
# Fit not satisfactory.

# MIMIC SUB 2 PerceivedImpact 

model_mimic_sub2 <-'
PerceivedImpact =~ imbleco + imwbcnt + imtcjob + imbgeco + imwbcrm   
PerceivedImpact ~ gender + age + eduyrs + income
'

fit_mimic_sub2 <- sem(model_mimic_sub2, # model formula
                      estimator = "MLM",
                      data=ess_df_selected_imp      # data frame
)

summary(fit_mimic_sub2, standardized=TRUE)

fitMeasures(fit_mimic_sub2, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
# Ok fit, but very tiny effects. It seems that perception of PerceivedImpact is not really 
# structured along age gradients, at least not in a linear fashion, 
# Edyrs => If education increases by 1 std => Perception of PerceivedImpact increases by .191 std
# Partial correlations in the table (e.g. effect of one, controlling for all others. )

# The impact of these 4 vars on preceived PerceivedImpact
# => If education increases by 1 unit, (lack of) perceived PerceivedImpact increases by 0.052 standard deviation (reverse coded).
# => Age non-significant.
# => If it's a male, lack of perceived PerceivedImpact decreases by 0.193 standard deviation.
# => If income increases by one standard deviation => lack of preceived PerceivedImpacth increases by .079 std.

# MIMIC SUB 3 GFEnmity 

model_mimic_sub3 <-'
GFEnmity =~ aljewlv + almuslv + algyplv + eimpcnt + impcntr 
eimpcnt ~~ impcntr
GFEnmity ~ gender + age + eduyrs + income
'

fit_mimic_sub3 <- sem(model_mimic_sub3, # model formula
                      estimator = "MLM",
                      data=ess_df_selected_imp      # data frame
)

summary(fit_mimic_sub3, standardized=TRUE)

fitMeasures(fit_mimic_sub3, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
# Very good fit => interpret

# MIMIC SUB 4 InterethnicRel

model_mimic_sub4 <-'
InterethnicRel =~ dfegcon + dfegcf + acetalv
InterethnicRel ~ gender + age + eduyrs + income
'

fit_mimic_sub4 <- sem(model_mimic_sub4, # model formula
                      estimator = "MLM",
                      data=ess_df_selected_imp      # data frame
)

summary(fit_mimic_sub4, standardized=TRUE)

fitMeasures(fit_mimic_sub3, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
# Very good fit => interpret.

# let's organize our plot on 4 rows 
# this help our readers by having a more comprehensible plot

# We don't combine these free into a single mimic with reduced attitude, as that was not our approved measurement model.

#####################################################################################
### MEDIATION 1 ### 
###################
model_mediation <- '
## Attitude towards immigration policy ##
Pol =~ qfimedu + qfimlng + qfimwsk + qfimcmt

## Perception of (lack of) PerceivedImpact ##
PerceivedImpact =~ imbleco + imwbcnt + imtcjob + imbgeco + imwbcrm

## Direct effect ##
Pol ~ c*income

## Mediator ##
PerceivedImpact ~ a*income
Pol ~ b*PerceivedImpact

## Indirect effect (a*b) ##
ab := a*b
## Total effect ##
total := c + (a*b)
'

fit_mediation <- cfa(model_mediation, # model formula
                     estimator = "MLM",
                     data=ess_df_selected_imp                # data frame
)

summary(fit_mediation, standardized=TRUE)

fitMeasures(fit_mediation, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
# Bad fit, but we knew that because Pol/ImPolicyalready had a poor fit on its own. 
# Maybe find another variable!

###################
### Mediation 2 ###
###################

model_mediation2 <- '
## Perception of (lack of) PerceivedImpact ##
PerceivedImpact =~ imbleco + imwbcnt + imtcjob + imbgeco + imwbcrm

## Trust in people in general ##
Trust =~ fair + help + trusts

## Direct effect ##
PerceivedImpact ~ c*income

## Mediator ##
Trust ~ a*income
PerceivedImpact ~ b*Trust

## Indirect effect (a*b) ##
ab := a*b
## Total effect ##
total := c + (a*b)
'

fit_mediation2 <- cfa(model_mediation2, # model formula
                      estimator = "MLM",
                      data=ess_df_selected_imp                # data frame
)

summary(fit_mediation2, standardized=TRUE)

lay <- get_layout(
  "fair", "help", "trusts", "", "",
  "", "Trust", "", "", "", 
  "income", "", "PerceivedImpact", "","",
  "imbleco","imwbcnt","imtcjob", "imbgeco", "imwbcrm",
  rows = 4)

fitMeasures(fit_mediation2, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
# Very good fit

plot_mediation <- graph_sem(model = fit_mediation2,   # model fit
                            layout = lay,        # layout
                            angle = 170          # adjust the arrows 
                            #label = "est_std"   # get standardized results (not rounded)
)

plot_mediation

###################
### Mediation 3 ### 
###################

model_mediation3 <- '
## Perception of (lack of) PerceivedImpact ##
PerceivedImpact =~ imbleco + imwbcnt + imtcjob + imbgeco + imwbcrm

## Trust in people in general ##
Trust =~ fair + help + trusts

## Direct effect ##
PerceivedImpact ~ c*eduyrs

## Mediator ##
Trust ~ a*eduyrs
PerceivedImpact ~ b*Trust

## Indirect effect (a*b) ##
ab := a*b
## Total effect ##
total := c + (a*b)
'

fit_mediation3 <- cfa(model_mediation3, # model formula
                      estimator = "MLM",
                      data=ess_df_selected_imp                # data frame
)

summary(fit_mediation3, standardized=TRUE)

lay <- get_layout(
  "fair", "help", "trusts", "", "",
  "", "Trust", "", "", "", 
  "eduyrs", "", "PerceivedImpact", "", "",
  "imbleco",  "imwbcnt" , "imtcjob", "imbgeco ", "imwbcrm",
  rows = 4)

fitMeasures(fit_mediation3, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
# Very good fit

plot_mediation <- graph_sem(model = fit_mediation3,   # model fit
                            layout = lay,        # layout
                            angle = 170          # adjust the arrows 
                            #label = "est_std"   # get standardized results (not rounded)
)

plot_mediation
# Explain restrictions of the models! E.g., no arrow = no correlation = no association 
# p-value zero => we should reject the model is a good fit, but if alternative measures we normally look at are OK =>
# we can assume it is a reasonable approximation.
# Cut off for factor loading: .40
# r^2 = square of factor laoding => .40^2 => .16

# We can use tidySEM to extract direct and indirect effects.

###################
### Mediation 4 ###
###################

model_mediation4 <- '
## Ethnocentrism ##
Ethnocentrism =~ imdetbs + imdetmr + qfimchr + qfimwht
qfimchr ~~ qfimwht

## Trust in people in general ##
Trust =~ fair + help + trusts

## Direct effect ##
Ethnocentrism ~ c*gender

## Mediator ##
Trust ~ a*gender
Ethnocentrism ~ b*Trust

## Indirect effect (a*b) ##
ab := a*b
## Total effect ##
total := c + (a*b)
'

fit_mediation4 <- cfa(model_mediation4, # model formula
                      estimator = "MLM",
                      data=ess_df_selected_imp                # data frame
)

summary(fit_mediation4, standardized=TRUE)

lay <- get_layout(
  "fair", "help", "trusts", "", "",
  "", "Trust", "", "", "", 
  "gender", "", "Ethnocentrism", "", "",
  "",  "imdetbs" , "imdetmr", "qfimchr", "qfimwht",
  rows = 4)

fitMeasures(fit_mediation4, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
# Very good fit

plot_mediation <- graph_sem(model = fit_mediation4,   # model fit
                            layout = lay,        # layout
                            angle = 170          # adjust the arrows 
                            #label = "est_std"   # get standardized results (not rounded)
)

plot_mediation

###################
### Mediation 5 ###
###################

model_mediation5 <- '
## Perception of (lack of) PerceivedImpact ##
PerceivedImpact =~ imbleco + imwbcnt + imtcjob + imbgeco + imwbcrm

## Trust in people in general ##
Trust =~ fair + help + trusts

## Direct effect ##
PerceivedImpact ~ c*age

## Mediator ##
Trust ~ a*age
PerceivedImpact ~ b*Trust

## Indirect effect (a*b) ##
ab := a*b
## Total effect ##
total := c + (a*b)
'

fit_mediation5 <- cfa(model_mediation5, # model formula
                      estimator = "MLM",
                      data=ess_df_selected_imp                # data frame
)

summary(fit_mediation5, standardized=TRUE)

lay <- get_layout(
  "fair", "help", "trusts", "", "",
  "", "Trust", "", "", "", 
  "age", "", "PerceivedImpact", "","",
  "imbleco","imwbcnt","imtcjob", "imbgeco", "imwbcrm",
  rows = 4)

fitMeasures(fit_mediation5, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
# Very good fit

plot_mediation <- graph_sem(model = fit_mediation5,   # model fit
                            layout = lay,        # layout
                            angle = 170          # adjust the arrows 
                            #label = "est_std"   # get standardized results (not rounded)
)

plot_mediation

######################################################################################
### Measurement equivalence ###
###############################
# Measurement Equivalence is achieved if a measurement instrument produces equivalent results, 
# regardless of some unrelated properties of the test subjects.
# The absence of measurement equivalence would imply some degree of distortion of the results (“bias”). 
# For instance, an IQ test that favors males by including “gender-biased test items”

#############################
### Configural Invariance ###-------------------------------------------------------
#############################

# lavaan requires the grouping variable to be a factor 
# gender is coded as 1 Male, 2 Female
ess_df_selected_imp$gender <- factor(ess_df_selected_imp$gender,
                      levels = c("1", "2"),         # levels 
                      labels = c("Male", "Female")) # labels 

### Ethnocentrism ###
model_Ethnocentrism <- '
Ethnocentrism =~ imdetbs + imdetmr + qfimchr + qfimwht
qfimchr ~~ qfimwht
'

fit_configuralEthnocentrism <- cfa(model_Ethnocentrism, 
                       estimator = "MLM",
                       data = ess_df_selected_imp,
                       group = "gender")


summary(fit_configuralEthnocentrism, standardized=TRUE)
# p-value low => this metric model is not as good as the configural.

# Loadings can differ. Only the structure needs to match. factor structure equal 
# but no parameter constraints.

## InterethnicRel ###
model_InterethnicRel <- '
InterethnicRel =~ dfegcon + dfegcf + acetalv
'

fit_configuralInterethnicRel <- cfa(model_InterethnicRel, 
                         estimator = "MLM",
                         data = ess_df_selected_imp,
                         group = "gender")

summary(fit_configuralInterethnicRel, standardized=TRUE)

## Trust ###
model_Trust <- 'Trust =~ fair + help + trusts'
fit_configuralTrust <- cfa(model_Trust, 
                         data = ess_df_selected_imp,
                         estimator = "MLM",
                         group = "gender")


summary(fit_configuralTrust, standardized=TRUE)

#########################
### Metric Invariance ###-------------------------------------------------------
#########################
# constrain factor loadings equal across groups. This shows that the construct 
# has the same meaning across groups. Metric invariance is necessary for correlations and regressions.

# we can simply tell lavaan what should be constrained across groups 
### Ethnocentrism ###

fit_metricEthnocentrism <- cfa(model_Ethnocentrism, 
                   estimator = "MLM",
                   data = ess_df_selected_imp,
                   group = "gender",
                   group.equal = c("loadings")
)

summary(fit_metricEthnocentrism, standardized=TRUE)

### InterethnicRel ###

fit_metricInterethnicRel <- cfa(model_InterethnicRel, 
                     estimator = "MLM",
                     data = ess_df_selected_imp,
                     group = "gender",
                     group.equal = c("loadings")
)


summary(fit_metricInterethnicRel, standardized=TRUE)
# "Estimates" set equal (unstandardized factor loadings)
# Standardized factor loadings are not equal => if variances are not equal across group
# => standardized factor loadings won't be equal
# (.p2.): enumerating particular parameters in lavaan. Used to set these parameters equal.

# p-value high => this metric model is better than the configural one

# Estimates at intercept are actually mean responses.

### Trust ###
fit_metricTrust <- cfa(model_Trust, 
                     estimator = "MLM",
                     data = ess_df_selected_imp,
                     group = "gender",
                     group.equal = c("loadings")
)


summary(fit_metricTrust, standardized=TRUE)

#########################
### Scalar Invariance ###-------------------------------------------------------
#########################
# Scalar invariance requires item intercepts and factor loadings equal across groups. 
# This is important for assessing mean difference of the latent variable across groups.
# Scalar non-invariance:A group tend to systematically give higher or lower item response. 
# For instance, female respondents might express higher support for child care services.
#=> From this level on, we can compare latent means.

### Ethnocentrism ###

fit_scalarEthnocentrism <- cfa(model_Ethnocentrism, 
                     data = ess_df_selected_imp,
                     estimator = "MLM",
                     group = "gender",
                     group.equal = c("loadings",
                                     "intercepts")
)


summary(fit_scalarEthnocentrism, standardized=TRUE)


### InterethnicRel ###

fit_scalarInterethnicRel <- cfa(model_InterethnicRel, 
                  data = ess_df_selected_imp,
                  estimator = "MLM",
                  group = "gender",
                  group.equal = c("loadings",
                                  "intercepts")
)


summary(fit_scalarInterethnicRel, standardized=TRUE)

### Trust ###

fit_scalarTrust <- cfa(model_Trust, 
                     data = ess_df_selected_imp,
                     estimator = "MLM",
                     group = "gender",
                     group.equal = c("loadings",
                                     "intercepts")
)


summary(fit_scalarTrust, standardized=TRUE)
# To find where missfit is located we could request modification indices.

#########################
### Strict Invariance ###-------------------------------------------------------
#########################

fit_strictEthnocentrism <- cfa(model_Ethnocentrism, 
                  data = ess_df_selected_imp,
                  group = "gender",
                  estimator = "MLM",
                  group.equal = c("loadings",
                                  "intercepts",
                                  "residuals")
)


summary(fit_strictEthnocentrism, standardized=TRUE)

#############################
### Structural Invariance ###-------------------------------------------------------
#############################

fit_structuralEthnocentrism <- cfa(model_Ethnocentrism, 
                      data = ess_df_selected_imp,
                      group = "gender",
                      estimator = "MLM",
                      group.equal = c("loadings",
                                      "intercepts",
                                      "residuals",
                                      "lv.variances", 
                                      "lv.covariances")
)


summary(fit_structuralEthnocentrism, standardized=TRUE)

#########################################
### Evaluating measurement invariance ###-------------------------------------------------------
#########################################

# Extracting the fit indices

model_fit <-  function(lavobject) {
  vars <- c("df", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue", "srmr")
  return(fitmeasures(lavobject)[vars] %>% data.frame() %>% round(2) %>% t())
}

table_fit <- 
  list(model_fit(fit_configuralEthnocentrism), 
       model_fit(fit_metricEthnocentrism), 
       model_fit(fit_scalarEthnocentrism), 
       model_fit(fit_strictEthnocentrism),
       model_fit(fit_structuralEthnocentrism)) %>% 
  reduce(rbind)


rownames(table_fit) <- c("Configural", "Metric", "Scalar","Strict","Structural")

table_fit

# One group is not more polarized than the other. 

# Comparing the nested models using the anova function

table_anova <- list(anova(fit_configuralEthnocentrism, fit_metricEthnocentrism),
                    anova(fit_metricEthnocentrism, fit_scalarEthnocentrism),
                    anova(fit_scalarEthnocentrism, fit_strictEthnocentrism),
                    anova(fit_strictEthnocentrism, fit_structuralEthnocentrism)) %>%  
  reduce(rbind) %>% 
  .[-c(3, 5, 7),]

table_anova

# Invariance is achieved if there is a non-significant chi-square change.
# Invariance is NOT achieved if there is a significant chi-square change.

##########################
### Partial invariance ###-------------------------------------------------------
##########################
# MIs to check where fir goes wrong => we can free certain constraints
# lavTestScore(fit_strict) 
# The usual cut-off value is 3.84, but this should be be adjusted based on sample size and number of tests conducted (type I error)
# Look for biggest X^2 in local fit part. "p-value" column shows new p-value after modofication.
# parTable(fit_strict) => Overview of all possible parameters that are in the model. 
# fit_strict_gvslvue <- cfa(model_ws, data = ess_df, group = "gndr", group.equal = c("loadings", "intercepts", "residuals"), group.partial = c(gvslvue ~~  gvslvue))
# lavTestScore(fit_strict_gvslvue)
# table_anova <- 
#  list(anova(fit_configural, fit_metric),
#       anova(fit_metric, fit_scalar),
#       anova(fit_scalar, fit_strict_gvslvue)) %>%  
#  reduce(rbind) %>% 
#  .[-c(3, 5),]
# table_anova

#####################################################################################
### Multi-group SEM ### We include structural part too. (Can be anything, MIMIC, mediation, etc.)
#######################
# Group specific direct and indirect effects 
# We test regression path invariance.

# Group.equal ="loadings" => Metric invariance => We want to compare effects size across groups =>
# => Group specific direct and indirect effects => We need metric invariance.

# Unconstrained model:

model_mediation_mg <- '
## Ethnocentrism ##
Ethnocentrism =~ imdetbs + imdetmr + qfimchr + qfimwht
qfimchr ~~ qfimwht

## Trust ##
Trust =~ fair + help + trusts

## Direct effect ##
Ethnocentrism ~ c("c1", "c2")*income

## Mediator ##
Trust ~ c("a1", "a2")*income
Ethnocentrism ~ c("b1", "b2")*Trust

## Indirect effect (a*b) ##
a1b1 := a1*b1
a2b2 := a2*b2
## Total effect c + (a*b) ##
total1 := c1 + (a1*b1)
total2 := c2 + (a2*b2)
'


fit_mediation_mg <- cfa(model_mediation_mg,       # model formula
                        data = ess_df_selected_imp, # data frame
                        estimator = "MLM",
                        group = "gender",              # grouping variable (G)
                        group.equal = c("loadings")  # equal loadings => Metric invariance
                        
)

summary(fit_mediation_mg, fit.measures = TRUE, standardized=TRUE)
# 56 degrees of freedom. Poor fit according to x^2, but good according to alternative indices.
# Factor loadings equal across group - "Estimate"
# Regression effects not equal, because we gave them different names.

#------------------------------------------------------------------------------------------
# Fixing the loadings and the path coefficients = regression effects equal across groups.
model_mediation_mg_cons <- '
## Ethnocentrism ##
Ethnocentrism =~ imdetbs + imdetmr + qfimchr + qfimwht
qfimchr ~~ qfimwht

## Trust ##
Trust =~ fair + help + trusts

## Direct effect ##
Ethnocentrism ~ c("c1", "c1")*income

## Mediator ##
Trust ~ c("a1", "a1")*income
Ethnocentrism ~ c("b1", "b1")*Trust

## Indirect effect (a*b) ##
a1b1 := a1*b1
## Total effect c + (a*b) ##
total1 := c1 + (a1*b1)
'

fit_mediation_mg_cons <- cfa(model_mediation_mg_cons,       # model formula
                             data = ess_df_selected_imp,
                             estimator = "MLM",
                             group = "gender",                # grouping variable (G)
                             group.equal = c("loadings")    # equal loadings # metric invariance SEM level
                             
)

summary(fit_mediation_mg_cons, fit.measures = TRUE, standardized=TRUE)

# Anova comparison of the two model: (The restricted model will never be a better fit, never have a lower X2.
# H0: They perform equally well (We chose the more parsimonious). HA: They don't => We need to settle for the less restrictive model (less parsimonious). )

anova(fit_mediation_mg, fit_mediation_mg_cons)
# The insignificant P-value implies that the unconstrained and the constrained models are not statistically significantly different. 
# This means that the path coefficients vary very little by group. In this case, we go for the more parsimonious model.

######################################
### Multi-group mediation analysis ### 
######################################
# Unconstrained model

model_mediation_mg <- '
## Ethnocentrism ##
Ethnocentrism =~ imdetbs + imdetmr + qfimchr + qfimwht
qfimchr ~~ qfimwht

## Trust ##
Trust =~ fair + help + trusts

## Direct effect ##
Ethnocentrism ~ c("c_inc_1", "c_inc_2")*income
Ethnocentrism ~ c("c_age_1", "c_age_2")*age

## Mediator ##
Trust ~ c("a_inc_1", "a_inc_2")*income
Trust ~ c("a_age_1", "a_age_2")*age
Ethnocentrism ~ c("b1", "b2")*Trust

## Indirect effect (a*b) ##
# G1 
ab_inc_g1 := a_inc_1*b1
ab_age_g1 := a_age_1*b1

# G2 
ab_inc_g2 := a_inc_2*b2
ab_age_g2 := a_age_2*b2

## Total effect c + (a*b) ##
# G1 
total_inc_g1 := c_inc_1 + (a_inc_1*b1)
total_age_g1 := c_age_1 + (a_age_1*b1)

# G1 
total_inc_g2 := c_inc_2 + (a_inc_2*b2)
total_age_g2 := c_age_2 + (a_age_2*b2)
'

fit_mediation_mg <- cfa(model_mediation_mg,       # model formula
                        data = ess_df_selected_imp,
                        estimator = "MLM",
                        group = "gender",              # grouping variable (G)
                        group.equal = c("loadings")  # equal loadings 
                        
)

summary(fit_mediation_mg, 
        standardized=TRUE, 
        fit.measures = TRUE
)
# Fit not satisfactory. Tried all combinations.

#####################################################################################
# Subsetting the data for testing model assumptions
ess_df_selected_test <- ess_df_selected_imp %>% select(
  imtcjob, # Immigrants take jobs away in country or create new jobs
  imbleco, # Taxes and services: immigrants take out more than they put in or less
  imwbcrm, # Immigrants make country's crime problems worse or better
  acetalv, # People of minority race/ethnic group in current living area
  dfegcf, # Different race or ethnic group: have any close friends
  dfegcon, # Different race or ethnic group: contact, how often
  imdetmr, # Immigrant different race/ethnic group majority: married closerelative
  imdetbs, # Immigrant different race/ethnic group majority: your boss
  qfimchr, # Qualification for immigration: Christian background
  qfimwht, # Qualification for immigration: be white
  almuslv, # Allow many or few Muslims to come and live in country
  algyplv, # Allow many or few Gypsies to come and live in country
  aljewlv, # Allow many or few Jewish people to come and live in country
  impcntr, # Allow many/few immigrants from poorer countries outside Europe
  eimpcnt, # Allow many/few immigrants from poorer countries in Europe
  imbgeco, # Immigration bad or good for the country's economy
  imueclt, # Country's cultural life undermined or enriched by immigrants
  imwbcnt, # Immigrants make the country worse or better place to live
  fair,
  help,
  trusts,
  income,
  eduyrs,
  gender
)

# MODEL ASSUMPTIONS (CFA) ----------------------------------------------------------------------
# 1. Multivariate normality
# Non-normality doesn't affect the parameter estimates that much but it does affect the X2s and the SEs. 
# Effects will appear statistically significant when in fact they are not. 

# Histograms
hist.data.frame(ess_df_selected_test[1:8])
hist.data.frame(ess_df_selected_test[9:16])
hist.data.frame(ess_df_selected_test[16:22])

# Calculating the multivariate kurtosis and skewness
mvn_test <- mvn(data = ess_df_selected_test, # our data 
                mvnTest = c("hz")    # type of normality test to perform
)

mvn_test$multivariateNormality # not multivariate normal => Use "Robust" estimator (MLM)
# To mitigate non-normality we can use scaled χ2 and “robust” standard errors corrections to ML estimation 
# as in Satorra and Bentler (1988; 1994). 
# Adjustments are made to the χ2 (and χ2 based fit indices) and standard errors based on a weight matrix
# derived from an estimate of multivariate kurtosis (as said before, the parameter estimates themselves 
# are not altered).

# 2. Linearity
# Create scatter plots to visualize linearity
pairs.panels(ess_df_selected_test[, c("imbleco", "aljewlv", "dfegcon", "imdetbs")], # Select one variable from each factor for visualization
             method = "pearson",
             hist.col = "#00AFBB",
             density = TRUE, 
             ellipses = TRUE)

# 3. Independence of observations
# Since the data set is a cross-sectional survey, we can assume that the observations are independent.

# 4. Adequate sample size
# Generally, sample sizes greater than 200 are considered adequate for CFA.
n <- nrow(ess_df_selected_test)
cat("Sample size:", n)

# Correlation matrix
cor_matrix <- cor(ess_df_selected_test, 
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
detach("package:mice", unload = T)
detach("package:lavaan", unload = T)
detach("package:GPArotation", unload = T)
detach("package:corrplot", unload = T)
detach("package:tidySEM", unload = T)
detach("package:semPlot", unload = T)
detach("package:MVN", unload = T)
detach("package:purrr", unload = T)
detach("package:Amelia", unload = T)
detach("package:patchwork", unload = T)
detach("package:plotly", unload = T)
detach("package:Hmisc", unload = T)


  