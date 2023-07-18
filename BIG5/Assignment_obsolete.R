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
### Data Import ###
library(readxl)

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

library("MVN")

# Data Exploration ------------------------------------------------------------------
BIG5 <- read_excel("BIG5/data.xlsx")
View(BIG5) 
names(BIG5) 

# Filter dataset for participants from Great Britain
gb_data <- subset(BIG5, country == "GB")

# Structure and summary of data
str(gb_data)
gb_data %>% glimpse()
summary(gb_data)

descriptive_gb <- as.data.frame(psych::describe(gb_data))

descriptive_gb <- dplyr::select(descriptive_gb, 
                                 n,
                                 mean,
                                 sd,
                                 median,
                                 min,
                                 max,
                                 skew,
                                 kurtosis)

descriptive_gb

# Checking for missing data 
colSums(is.na(gb_data)) # no missing data

# Create a new data frame with the relevant variables
big5_gb_data <- gb_data[, c("O1", "O2", "O3", "O4", "O5", "O6", "O7", "O8", "O9", "O10",
                         "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10",
                         "E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10",
                         "A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10",
                         "N1", "N2", "N3", "N4", "N5", "N6", "N7", "N8", "N9", "N10")]
# MODEL ASSUMPTIONS (CFA) ----------------------------------------------------------------------
# 1. Multivariate normality
# Calculate the multivariate kurtosis and skewness
mvnorm_stats <- MVN::mvn(big5_gb_data, mvnTest = "mardia")
print(mvnorm_stats)

mvn_test <- mvn(data = big5_gb_data, # our data 
                mvnTest = c("hz")    # type of normality test to perform
)

mvn_test$multivariateNormality

# To mitigate non-normality we can use scaled χ2
# and “robust” standard errors corrections to ML estimation as in Satorra and Bentler (1988; 1994). 
# Adjustments are made to the χ2 (and χ2 based fit indices) and standard errors based on a weight matrix
# derived from an estimate of multivariate kurtosis (as said before, the parameter estimates themselves 
# are not altered).

# 2. Linearity
# Create scatter plots to visualize linearity
pairs.panels(big5_gb_data[, c("O1", "C1", "E1", "A1", "N1")], # Select one variable from each factor for visualization
             method = "pearson",
             hist.col = "#00AFBB",
             density = TRUE, 
             ellipses = TRUE)

# 3. Independence of observations
# Since the data set is a cross-sectional survey, we can assume that the observations are independent.

# 4. Adequate sample size
# Generally, sample sizes greater than 200 are considered adequate for CFA.
n <- nrow(big5_gb_data)
cat("Sample size:", n)

# Correlation matrix
cor_matrix <- cor(big5_gb_data)
cor_matrix

# Heatmap
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, diag = FALSE)

# Reverse coded items 
# reverse_coded_items <- c("AS7", "AS8", "AS9","AS10", "SC6", "SC7", "SC8", "SC9", "SC10", "AD5", "AD6", "AD7", "AD8", "AD9", "AD10")
# Reverse the scores for reverse-coded items
# pers_df[reverse_coded_items] <- 6 - pers_df[reverse_coded_items]

# Correlation matrix for highly correlated vars only
threshold <- 0.7
high_corr_indices <- which((cor_matrix > threshold | cor_matrix < -threshold) & cor_matrix != 1, arr.ind = TRUE)
unique_indices <- unique(c(high_corr_indices[,1], high_corr_indices[,2]))
high_corr_matrix <- cor_matrix[unique_indices, unique_indices]

# Heatmap
corrplot(high_corr_matrix, method = "color", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, diag = FALSE)
# only one highly corr N7 and N8. We leave them in for now. Corr = 0.7963398

# Confirmatory Factor Analysis --------------------------------------------------------------
# For each factor separately
# Define separate CFA models for each factor
Openness_model <- '
  Openness =~ O1 + O2 + O3 + O4 + O5 + O6 + O7 + O8 + O9 + O10
'
Conscientiousness_model <- '
  Conscientiousness =~ C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10
'
Extraversion_model <- '
  Extraversion =~ E1 + E2 + E3 + E4 + E5 + E6 + E7 + E8 + E9 + E10
'
Agreeableness_model <- '
  Agreeableness =~ A1 + A2 + A3 + A4 + A5 + A6 + A7 + A8 + A9 + A10
'
Neuroticism_model <- '
  Neuroticism =~ N1 + N2 + N3 + N4 + N5 + N6 + N7 + N8 + N9 + N10
'

Openness_fit <- cfa(Openness_model, data = big5_gb_data)
Conscientiousness_fit <- cfa(Conscientiousness_model, data = big5_gb_data)
Extraversion_fit <- cfa(Extraversion_model, data = big5_gb_data)
Agreeableness_fit <- cfa(Agreeableness_model, data = big5_gb_data)
Neuroticism_fit <- cfa(Neuroticism_model, data = big5_gb_data)

# Display the results
summary(Openness_fit, fit.measures = TRUE, standardized=TRUE)
summary(Conscientiousness_fit, fit.measures = TRUE, standardized=TRUE)
summary(Extraversion_fit, fit.measures = TRUE, standardized=TRUE)
summary(Agreeableness_fit, fit.measures = TRUE, standardized=TRUE)
summary(Neuroticism_fit, fit.measures = TRUE, standardized=TRUE)

# Performing a separate CFA for each factor can be useful for understanding the structure 
# and relationships within each of the factors individually. However, the Big Five Personality model 
# is based on the idea that these five factors represent the underlying structure of personality. 
# So, it would make more sense to test the full 5-factor model using CFA to see if the 
# hypothesized structure fits the data well.

# Define the full 5-factor CFA model
full_model <- '
  Openness =~ O1 + O2 + O3 + O4 + O5 + O6 + O7 + O8 + O9 + O10
  Conscientiousness =~ C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10
  Extraversion =~ E1 + E2 + E3 + E4 + E5 + E6 + E7 + E8 + E9 + E10
  Agreeableness =~ A1 + A2 + A3 + A4 + A5 + A6 + A7 + A8 + A9 + A10
  Neuroticism =~ N1 + N2 + N3 + N4 + N5 + N6 + N7 + N8 + N9 + N10
'

# Fit the full 5-factor CFA model
full_fit <- cfa(full_model, data = big5_gb_data)

# Display the results with fit measures
summary(full_fit, fit.measures = TRUE, standardized=TRUE)



g## TO DO: fit indices for both. maybe devide to test and training
# measurement invariance, etc. (follow lab sessions and book)!


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



