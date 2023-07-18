# Human Values Scale

ess_df_selected <- ess_df %>% select(
impdiff, # Important to try new and different things in life
impenv, # Important to care for nature and environment
impfree, # Important to make own decisions and be free
impfun, # Important to seek fun and things that give pleasure
imprich, # Important to be rich, have money and expensive things
impsafe, # Important to live in secure and safe surroundings
imptrad, # Important to follow traditions and customs
ipadvnt, # Important to seek adventures and have an exciting life
ipbhprp, # Important to behave properly
ipcrtiv, # Important to think new ideas and being creative
ipeqopt, # Important that people are treated equally and have equal opportunities
ipfrule, # Important to do what is told and follow rules
ipgdtim, # Important to have a good time
iphlppl, # Important to help people and care for others well-being
iplylfr, # Important to be loyal to friends and devote to people close
ipmodst, # Important to be humble and modest, not draw attention
iprspot, # Important to get respect from others
ipshabt, # Important to show abilities and be admired
ipstrgv, # Important that government is strong and ensures safety
ipsuces, # Important to be successful and that people recognize achievements
ipudrst # Important to understand different people
)

ess_df_selected_standardized <- data.frame(scale(ess_df_selected))
cor_matrix <- cor(ess_df_selected_standardized, use="pairwise.complete.obs")
corrplot(cor_matrix, method="color", type="upper", tl.col="black", tl.srt=45)
# Preliminary tests - if the correlation matrix is suitable for factor analysis
## Kaiser–Meyer–Olkin (KMO)
KMO(cor_matrix)
## Bartlett’s test of sphericity
cortest.bartlett(R = cor_matrix, n = 1399)
det(cor_matrix)
# Perform parallel analysis
# par_analysis <- fa.parallel(ess_df_selected, n.iter = 100, fa = "both")
# or
number_items <- fa.parallel(ess_df_selected_standardized,
                            fm ='ml',
                            fa ="fa")
# Solid line -> original Kaiser criteria => 2
# Parallel => Red simulated and re-sampled data => Where the blue and red lines 
# intersect => everything above, those eigen values can't be by chance => 4
# Screeplot -> infliction point: 2 

# Parallel
sum(number_items$fa.values > 1) 
sum(number_items$fa.values > 0.7) 

# Specify the number of factors to extract (you can try different numbers)
num_factors <- 5

# Perform EFA
efa_results <- fa(ess_df_selected_standardized, nfactors = num_factors, rotate = "oblimin")

# Print EFA results
print(efa_results)

print(efa_results$loadings, cutoff = 0.3)

# Plot factor loadings
fa.diagram(efa_results)

fa.plot(efa_results,
        labels = colnames(ess_df_selected_standardized))

###########
### CFA ###
###########
# Non-standardized items

ess_df_selected <- ess_df %>% select(
      # Traditional values and beliefs / Community Values
  impenv, # Important to care for nature and environment   
  impfree, # Important to make own decisions and be free
  impsafe, # Important to live in secure and safe surroundings   
  imptrad, # Important to follow traditions and customs    
  ipbhprp, # Important to behave properly      
  ipeqopt, # Important that people are treated equally and have equal opportunities    
  iphlppl, # Important to help people and care for others well-being  
  iplylfr, # Important to be loyal to friends and devote to people close     
  ipmodst, # Important to be humble and modest, not draw attention     
  ipstrgv, # Important that government is strong and ensures safety        
  ipudrst, # Important to understand different people  
      # Modern values and beliefs / Individualistic values
  impdiff, # Important to try new and different things in life      
  impfun, # Important to seek fun and things that give pleasure    
  imprich, # Important to be rich, have money and expensive things  
  ipadvnt, # Important to seek adventures and have an exciting life
  ipcrtiv, # Important to think new ideas and being creative     
  ipgdtim, # Important to have a good time      
  iprspot,  # Important to get respect from others     
  ipshabt, # Important to show abilities and be admired        
  ipsuces  # Important to be successful and that people recognize achievements       
)

# Sample observed covariance matrix
selected_cov <- cov(ess_df_selected,          # data frame
                    use = "pairwise.complete.obs" # remove NAs
)

selected_cov

selected_cor <- cov2cor(selected_cov)
selected_cor

corrplot::corrplot(selected_cor,
                   is.corr = FALSE,       # whether is a correlation matrix
                   method = "circle",     # magnitude of covariances as circles
                   type = "upper"        # remove the bottom of the covariance matrix
)

###########################
### Collectivist values ###
###########################
model_tradition <- '
 Tradition =~ impenv + impfree + impsafe + imptrad + ipeqopt + iphlppl + iplylfr + ipstrgv + ipudrst
'

fit_tradition <- cfa(model_tradition,             # model formula
              data = ess_df_selected  # data frame
)

summary(fit_tradition, 
        fit.measures = TRUE, # returns commonly used fit measures  
        standardized = TRUE  # indicates that we want standardized results
)

### Model fit statistics
# Global
# * (some) rules of thumb: CFI/TLI>0.95,RMSEA<0.05,SRMR<0.06
# * current practice is: chi-square value + df + pvalue, RMSEA, CFI and SRMR
fitMeasures(fit_tradition, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
# Local = Modification Indexes
## Local fit measures: modification indices ##
mi <- inspect(fit_tradition,"mi")
mi.sorted <- mi[order(-mi$mi),] # sort from high to low mi.sorted[1:5,] # only display some large MI values
mi.sorted[1:5,] # only display some large MI values
#------------------------------
model_tradition2 <- '
 Tradition =~ impenv + impsafe + ipbhprp + ipeqopt + iphlppl + iplylfr
'
fit_tradition2 <- cfa(model_tradition2,             # model formula
                     data = ess_df_selected  # data frame
)

summary(fit_tradition2, 
        fit.measures = TRUE, # returns commonly used fit measures  
        standardized = TRUE  # indicates that we want standardized results
)


fitMeasures(fit_tradition2, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
mi <- inspect(fit_tradition2,"mi")
mi.sorted <- mi[order(-mi$mi),] 
mi.sorted[1:5,] 

############################
### Modern/Individualism ###
############################

model_ego <- '
 Individum =~ impfun + imprich + ipgdtim + ipshabt + ipsuces     
'
fit_ego <- cfa(model_ego,             # model formula
                     data = ess_df_selected  # data frame
)

summary(fit_ego, 
        fit.measures = TRUE, # returns commonly used fit measures  
        standardized = TRUE  # indicates that we want standardized results
)

fitMeasures(fit_ego, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
mi <- inspect(fit_ego,"mi")
mi.sorted <- mi[order(-mi$mi),] 
mi.sorted[1:5,] 

----------------------------------------------------------

model_ego <- '
 Individum =~ impfun + imprich + ipgdtim + ipshabt + ipsuces  
 impfun ~~ ipgdtim
'
fit_ego <- cfa(model_ego,             # model formula
               data = ess_df_selected  # data frame
)

summary(fit_ego, 
        fit.measures = TRUE, # returns commonly used fit measures  
        standardized = TRUE  # indicates that we want standardized results
)

fitMeasures(fit_ego, c("logl","AIC", "BIC", "chisq", "df", "pvalue", "cfi", "tli","rmsea"), output = "matrix")
mi <- inspect(fit_ego,"mi")
mi.sorted <- mi[order(-mi$mi),] 
mi.sorted[1:5,] 
impfun ~~ ipgdtim

#---------------------------------------------------------------
model_ego <- '
 Individum =~ imprich + ipgdtim + ipshabt + ipsuces  
'
fit_ego <- cfa(model_ego,             # model formula
               data = ess_df_selected  # data frame
)

summary(fit_ego, 
        fit.measures = TRUE, # returns commonly used fit measures  
        standardized = TRUE  # indicates that we want standardized results
)

############
# SEM Model
model_4F <- '
Individum =~ imprich + ipgdtim + ipshabt + ipsuces 
Tradition =~ impenv + impsafe + ipbhprp + ipeqopt + iphlppl + iplylfr
ATI =~ impcntr + imbgeco + imueclt + imwbcnt
J =~ sofrprv + ppldsrv + jstprev + pcmpinj
'

fit_4F <- cfa(model_4F,             # model formula
              data = ess_df  # data frame
)

summary(fit_4F, 
        fit.measures = TRUE, # returns commonly used fit measures  
        standardized = TRUE  # indicates that we want standardized results
)

# Visualization
semPaths(fit_4F, "std", "est")


