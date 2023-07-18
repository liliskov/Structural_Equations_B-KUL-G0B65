# Std politics

ess_df_selected <- ess_df %>% select(
  actrolga, # Able to take active role in political group
  badge, # Worn or displayed campaign badge/sticker last 12 months
  bctprd, # Boycotted certain products last 12 months
  clsprty, # Feel closer to a particular party than all other parties
  contplt, # Contacted politician or government official last 12 months
  cptppola, # Confident in own ability to participate in politics
  euftf, # European Union: European unification go further or gone too far
  freehms, # Gays and lesbians free to live life as they wish
#  gincdif, # Government should reduce differences in income levels
  hmsacld, # Gay and lesbian couples right to adopt children
  hmsfmlsh, # Ashamed if close family member gay or lesbian
  lrscale, # Placement on left right scale
  pbldmn, # Taken part in lawful public demonstration last 12 months
  polintr, # How interested in politics
  psppipla, # Political system allows people to have influence on politics
  psppsgva, # Political system allows people to have a say in what governmentdoes
  pstplonl, # Posted or shared anything about politics online last 12 months
  sgnptit, # Signed petition last 12 months
  stfdem, # How satisfied with the way democracy works in country
  stfeco, # How satisfied with present state of economy in country
  stfedu, # State of education in country nowadays
  stfgov, # How satisfied with the national government
  stfhlth, # State of health services in country nowadays
  stflife, # How satisfied with life as a whole
  trstep, # Trust in the European Parliament
  trstlgl, # Trust in the legal system
  trstplc, # Trust in the police
  trstplt, # Trust in politicians
  trstprl, # Trust in country's parliament
  trstprt, # Trust in political parties
  trstun, # Trust in the United Nations
  vote, # Voted last national election
  wrkorg, # Worked in another organisation or association last 12 months
  wrkprty, # Worked in political party or action group last 12 months
  impcntr, # Allow many/few immigrants from poorer countries outside Europe
  imbgeco, # Immigration bad or good for country's economy
  imueclt, # Country's cultural life undermined or enriched by immigrants
  imwbcnt # Immigrants make country worse or better place to live
)

# Social-liberalism
ess_df_selected <- ess_df %>% select(
  freehms, # Gays and lesbians free to live life as they wish
  hmsacld, # Gay and lesbian couples right to adopt children
  hmsfmlsh, # Ashamed if close family member gay or lesbian
  impcntr, # Allow many/few immigrants from poorer countries outside Europe
  imbgeco, # Immigration bad or good for country's economy
  imueclt, # Country's cultural life undermined or enriched by immigrants
  imwbcnt # Immigrants make country worse or better place to live
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
# Solid line -> original Kaiser criteria => 4 or 5
# Parallel => Red simulated and re-sampled data => Where the blue and red lines 
# intersect => everything above, those eigen values can't be by chance => 9
# Screeplot -> infliction point: 6
# Eigenvalues: 
sum(eigen(cor_matrix)$values > 1) 

# Parallel
sum(number_items$fa.values > 1) 
sum(number_items$fa.values > 0.7) 

# Specify the number of factors to extract (you can try different numbers)
num_factors <- 1

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
# CFA on social-liberalism
# Non-standardized  
# Social-liberalism
ess_df_selected <- ess_df %>% select(
  freehms, # Gays and lesbians free to live life as they wish
  hmsacld, # Gay and lesbian couples right to adopt children
  hmsfmlsh, # Ashamed if close family member gay or lesbian
  impcntr, # Allow many/few immigrants from poorer countries outside Europe
  imbgeco, # Immigration bad or good for country's economy
  imueclt, # Country's cultural life undermined or enriched by immigrants
  imwbcnt # Immigrants make country worse or better place to live
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

model_sl <- '
SL =~ freehms + hmsacld + imueclt + hmsfmlsh + impcntr + imbgeco + imueclt + imwbcnt
'

fit_sl <- cfa(model_sl,             # model formula
                 data = ess_df_selected  # data frame
)

summary(fit_sl, 
        fit.measures = TRUE, # returns commonly used fit measures  
        standardized = TRUE  # indicates that we want standardized results
)


