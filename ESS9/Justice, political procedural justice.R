# Justice, Political procedural justice

ess_df_selected <- ess_df %>% select(
  btminfr, # Bottom 10% full-time employees in country, earning less than[amount], how fair
  evfredu, # Everyone in country fair chance achieve level of education theyseek
  evfrjob, # Everyone in country fair chance get job they seek
#  frlgrsp, # Fair level of [weekly/monthly/annual] gross pay for you
#  frlneti, # Fair level of [weekly/monthly/annual] net [pay/pensions/socialbenefits] for you
  frprtpl, # Political system in country ensures everyone fair chance toparticipate in politics
#  fvgabc, # Filter variable: ask about pay, pensions, or social benefits
#  grspfr, # Would you say your gross pay is unfairly low, fair, or unfairly high
#  grsplet, # Which letter describes your gross pay
#  grspnum, # What is your usual [weekly/monthly/annual] gross pay
  gvintcz, # Government in country takes into account the interests of allcitizens
  ifredu, # Compared other people in country, fair chance achieve level of
  ifrjob, # Compared other people in country, fair chance get job I seek
#  infqbst, # Your [pay/pensions/social benefits], which frequency do you knowbest
  jstprev, # Confident that justice always prevails over injustice
  netifr, # Your net [pay/pensions/social benefits] is unfairly low, fair, or unfairlyhigh
#  netilet, # Which letter describes your net [pay/pensions/social benefits]
#  netinum, # Your usual [weekly/monthly/annual] net [pay/pensions/socialbenefits]
  occinfr, # Net [pay/pensions/social benefits] of people same occupation asyou in country, how fair
  pcmpinj, # Convinced that in the long run people compensated for injustices
 poltran, # Decisions in country politics are transparent
 ppldsrv, # By and large, people get what they deserve
 recexp, # Influence decision to recruit in country: person's on-the-jobexperience
 recgndr, # Influence decision to recruit in country: person's gender
 recimg, # Influence decision to recruit in country: person has immigrantbackground
 recknow, # Influence decision to recruit in country: person knows someone inorganisation
 recskil, # Influence decision to recruit in country: person's knowledge andskills
 sofrdst, # Society fair when income and wealth is equally distributed
 sofrpr, # Society fair when takes care of poor and in need, regardless of whatgive back
 sofrprv, # Society fair when people from families with high social status enjoyprivileges
 sofrwrk, # Society fair when hard-working people earn more than others
 topinfr, # Top 10% full-time employees in country, earning more than[amount], how fair
 wltdffr, # Differences in wealth in country, how fair
# iincsrc # Respondent's main source of income
)

# count NAs per variable
na_counts <- colSums(is.na(ess_df_selected))
# print the results
na_counts

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
# Solid line -> original Kaiser criteria => 3
# Parallel => Red simulated and re-sampled data => Where the blue and red lines 
# intersect => everything above, those eigen values can't be by chance => 8
# Screeplot -> infliction point: 8
# Eigenvalues (PCA)
sum(eigen(cor_matrix)$values > 1) 

# Parallel
sum(number_items$fa.values > 1) # Kaiser
sum(number_items$fa.values > 0.7) # New Kaiser

# Specify the number of factors to extract (you can try different numbers)
num_factors <- 2

# Perform EFA
efa_results <- fa(ess_df_selected_standardized, nfactors = num_factors, rotate = "oblimin")

# Print EFA results
print(efa_results)

print(efa_results$loadings, cutoff = 0.3)

# Plot factor loadings
fa.diagram(efa_results)

fa.plot(efa_results,
        labels = colnames(ess_df_selected_standardized))

#############
# CFA 
# Non-standardized  

# Recruitment
ess_df_selected <- ess_df %>% select(
  recexp,                                              
  recgndr,                                            
  recimg,                                               
  recknow,                                           
  recskil                                         
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
                   type = "upper",        # remove the bottom of the covariance matrix
                   addCoef.col = "black"  # add to the plot the coefficients
)

model_rec <- '
Rec =~ recexp + recgndr + recimg + recknow + recskil
'

fit_rec <- cfa(model_rec,             # model formula
              data = ess_df_selected  # data frame
)

summary(fit_rec, 
        fit.measures = TRUE, # returns commonly used fit measures  
        standardized = TRUE)
