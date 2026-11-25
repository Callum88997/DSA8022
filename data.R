# Task 1. Data Wrangling and Descriptives ----

set.seed(2026)
packages <- c("bootnet", "corrplot","igraph", "NetworkComparisonTest", "psych", "qgraph","rempsyc", "tidyverse", "flextable", "lavaan", "lavaanPlot", "ggplot2")

## Install and load the relevant packages (5 marks). ----

### Install Packages ----

# install.packages(packages)

### Load Packages ----

lapply(packages, library, character.only = TRUE)

## Import the .csv dataset into R (5 marks). ----

dfSummative <- read.csv('data_summative.csv')

## Visually inspect the dataset by using the head() or the View() functions (5 marks). ----

# Structure of Dataset
str(dfSummative)

# Summary of Dataset
summary(dfSummative)

# Descriptive stats of overall Dataset
describe(dfSummative)

# Column names of dataset
colnames(dfSummative)

# Check there is no missing values
sum(is.na(dfSummative)) # no missing values

# First 6 rows of dataset
head(dfSummative)

# First 6 rows of dataset
tail(dfSummative)

# Get columns of interest
dfUpdated <- select(dfSummative, -X)

# Pairs Plot the data
pairs(select(dfUpdated, -seniority),
      main = "Correlation Plots with Trend Lines", 
      pch = 16, 
      lower.panel = panel.smooth,
      upper.panel = NULL)

#
# From the pairs plot we can see that mostly all variables are correlated with one another. 
# The only exception to this is the age variable which doesn't appear to be correlated with any other variable.
# This is seen from the red line in all the age plots being almost completely flat.
# For this reason we drop age from being a variable of interest.
#

# Covariance matrix (rounded to two decimals)
cov_matrix <- round(cov(select(dfUpdated, -seniority)), 2)
cov_matrix

# Correlation matrix (rounded to two decimals)
cor_matrix <- round(cor(select(dfUpdated, -seniority)), 2)
cor_matrix

#
# The age variable not having any correlation with any other variable is further supported above in the correlation matrix.
# Age has a max correlation of 0.04 with x4
# This further supports dropping age from variables of interest
# 

dfUpdated <- dfUpdated %>% select(-age)

## Obtain descriptive statistics by seniority group using psych::describeBy (10 marks). ----

# Descriptive stats
desc <- dfUpdated %>% psych::describeBy(group = .$"seniority")

# Table - high seniority group
data.frame(desc$`High Seniority`) %>% rownames_to_column("Variable") %>%
  filter(Variable != "seniority") %>%
  nice_table(title = "High Seniority Group - Descriptive statistics")

#
# The table displays the descriptive stats for the 'high seniority' group.
# Here we see that for this group the data ranges in skew from -0.88 for x9 to 0.59 for x8
# Since there is a massive range in the skew of the variables we conclude the data is not normally distributed.
#

# Table - low seniority group
data.frame(desc$`Low Seniority`) %>% rownames_to_column("Variable") %>%
  filter(Variable != "seniority") %>%
  nice_table(title = "Low Seniority Group - Descriptive statistics")

#
# The table displays the descriptive stats for the 'low seniority' group.
# Here we see that for this group the data ranges in skew from -0.82 for x9 to 0.59 for x8
# Since there is a massive range in the skew of the variables we conclude the data is not normally distributed.
#

# Create histograms for all variables

# Variables' names to support interpretation and for histogram plot ordering
node_names <- paste0('x', 1:13)

# Obtain group-based histograms
dfUpdated %>% pivot_longer(cols = -seniority, 
               names_to = "variable", 
               values_to = "value") %>%
  mutate(variable = factor(variable, levels = node_names)) %>%
  
  ggplot(aes(x = value, fill = factor(seniority))) +
  geom_histogram(bins = 30, color = "black", alpha = 0.6, position = "identity") +
  facet_wrap(~variable, scales = "free") +
  scale_fill_manual(values = c("High Seniority" = "steelblue", "Low Seniority" = "coral"),
                    labels = c("High Seniority", " Low Seniority"),
                    name = "Seniority Group") +
  theme_minimal() +
  labs(title = "Distribution of All Variables by Seniority Group",
       x = "Value",
       y = "Frequency")

#
# The histograms further support the data not being normally distributed
#

# Task 2: Network analysis ----

# Get dataset for each seniority
highData <- dfUpdated %>% filter(seniority == "High Seniority") %>% select(-seniority)

lowData <- dfUpdated %>% filter(seniority == "Low Seniority") %>% select(-seniority)

## Compute Spearman correlation matrices by gender group (10 marks). ----

#
# Since the data is not normally distributed, which we found above.
# We use Spearman's method to calculate correlation.
#

# Calculate correlation matrix using Spearman's method - high
cor_matrix_high <- highData %>% cor(, use = "pairwise.complete.obs", method = "spearman")

# Calculate correlation matrix using Spearman's method - low
cor_matrix_low <- lowData %>% cor(, use = "pairwise.complete.obs", method = "spearman")

# Transpose layout for two graphs on the same row and two columns
layout(t(1:2)) 

# Heatmap visualization - high
corrplot::corrplot(cor_matrix_high, 
                   method = "color",
                   type = "upper",
                   tl.col = "black",
                   tl.cex = 0.6,
                   title = "High Seniority Group",
                   mar = c(0, 0, 2, 0))

# Heatmap visualization - low
corrplot::corrplot(cor_matrix_low, 
                   method = "color",
                   type = "upper",
                   tl.col = "black",
                   tl.cex = 0.6,
                   title = "Low Seniority Group",
                   mar = c(0, 0, 2, 0))

#
# In the heatmaps for each seniority group we can see that most the variables are correlated with each other.
#

## Estimate networks using bootnet::estimateNetwork with EBICglasso regularisation, and plot them using qgraph (10 marks). ----

# Full variable names for legend
var_names = c(
  "Used digital systems to manage daily operations",
  
  "Relied on data dashboards to monitor performance",
  
  "Used integrated software to coordinate workflows",
  
  "Adopted new digital tools to improve efficiency",
  
  "Tested new operational processes",
  
  "Introduced new technologies into production",
  
  "Piloted alternative ways of organising work",
  
  "Encouraged teams to try new methods",
  
  "Implemented newly developed industry practices",
  
  "Reviewed operational costs regularly",
  
  "Sought lower-cost suppliers or partners",
  
  "Delayed spending to reduce expenses",
  
  "Reduced operational costs through efficiency measures"
)

# Estimate network separately by seniority group - high
net_high <- highData %>% 
  estimateNetwork(
    default = "EBICglasso",
    corMethod = "spearman"
  )

# Estimate network separately by seniority group - low
net_low <- lowData %>% 
  estimateNetwork(
    default = "EBICglasso",
    corMethod = "spearman"
  )

# Transpose layout
layout(t(1:1)) 

# Save a function for plotting
plot_net <- function(graph_obj, plot_title){
  qgraph(
    graph_obj,
    labels = node_names,
    vsize = 6,
    cut = 0.10,
    border.width = 1.5,
    border.color = "black",
    nodeNames = var_names,
    label.cex = 1.1,
    theme = "colorblind",
    layoutScale = c(0.8, 0.8),
    legend = TRUE,
    legend.cex = .50,
    title = plot_title
  )
}

# Visualise the network - high Seniority
plot_net(net_high$graph, plot_title = "High Seniority")

#
# The network diagram shows all the connections between the different nodes
# Blue lines indicate positive weights whereas red lines indicate a negative weight.
# The thicker the lines greater the weight is between the two nodes.
# We see that there is a strong negative connection between x2 and x3.
# We also see that x10 and x13 have alot of strong connections with other nodes.
#

# Visualise the network - low Seniority
plot_net(net_low$graph, plot_title = "Low Seniority")

#
# The network diagram shows all the connections between the different nodes
# Blue lines indicate positive weights whereas red lines indicate a negative weight.
# The thicker the lines greater the weight is between the two nodes.
# We see that there is a strong negative connection between x2 and x3.
# We also see that x10 and x13 have alot of strong connections with other nodes.
#

## Produce centrality plots for Strength, Expected Influence, and Betweenness (10 marks). ----

# Save vectors of centrality indices of interest
cen_vector <- c("Strength", "ExpectedInfluence", "Betweenness")

# Centrality (sample estimates)
cent_high <- qgraph::centrality(net_high$graph)
cent_low <- qgraph::centrality(net_low$graph)

# Centrality plot for Strength, Expected Influence, Betweenness - high seniority
qgraph::centralityPlot(
  net_high$graph,
  include = cen_vector,
  scale = "z-scores"
) +
  ggtitle("High Seniority, Centrality Indices")

#
# The centralitiy plot shows that x13 and x10 have a higher standardised strength and expected influence compared to the other nodes.
# We also see that x7 and x9 expected influence isn't much smaller than x10 and x13.
# Whereas x11 has a lower standardised strength and expected influence compared to the other nodes.
# We also see that x10 and x13 lie on the shortess path between nodes more frequent than other nodes.
#

# Centrality plot for Strength, Expected Influence, Betweenness - low seniority
qgraph::centralityPlot(
  net_low$graph,
  include = cen_vector,
  scale = "z-scores"
) +
  ggtitle("Low Seniority, Centrality Indices")

#
# The centralitiy plot shows that x13 and x10 have a higher standardised strength and expected influence compared to the other nodes.
# We also see that x7 and x9 expected influence isn't much smaller than x10 and x13.
# Whereas x11 has a lower standardised strength and expected influence compared to the other nodes.
# We also see that x4 and x13 lie on the shortess path between nodes more frequent than other nodes.
#

## Bootstrapping ----

# Bootstrap edges and centrality (10 non-parametric resamples)
# Save function
boot_fun <- function(net){
  bootnet(
    net,
    nBoots = 10, # Increase to at least 1000 or higher when running for any real-world report
    default = "EBICglasso",
    corMethod = "spearman",
    type = "nonparametric",
    statistics = c("edge", "strength", "expectedInfluence", "betweenness"),
    verbose = FALSE
  )
}

# Bootstrapping - high seniority
boot_cent_high <- boot_fun(net_high)

# Bootstrapping - low seniority
boot_cent_low <- boot_fun(net_low)

# This summarises the bootstrap distribution into mean + 95% CIs by node and statistic - high
cent_ci_table_high <- boot_cent_high$bootTable %>%
  group_by(type, node1, node2) %>%
  summarise(
    Estimate = mean(value, na.rm = TRUE),
    CI_Lower = quantile(value, 0.025, na.rm = TRUE),
    CI_Upper = quantile(value, 0.975, na.rm = TRUE),
    n_reps = sum(!is.na(value)),
    .groups = "drop"
  ) %>%
  arrange(type, desc(abs(Estimate))) %>% 
  # Retain only centrality estimates with CIs excluding zero
  filter((CI_Lower > 0 & CI_Upper > 0) | (CI_Lower < 0 & CI_Upper < 0))

# This summarises the bootstrap distribution into mean + 95% CIs by node and statistic - low
cent_ci_table_low <- boot_cent_low$bootTable %>%
  group_by(type, node1, node2) %>%
  summarise(
    Estimate = mean(value, na.rm = TRUE),
    CI_Lower = quantile(value, 0.025, na.rm = TRUE),
    CI_Upper = quantile(value, 0.975, na.rm = TRUE),
    n_reps = sum(!is.na(value)),
    .groups = "drop"
  ) %>%
  arrange(type, desc(abs(Estimate))) %>% 
  # Retain only centrality estimates with CIs excluding zero
  filter((CI_Lower > 0 & CI_Upper > 0) | (CI_Lower < 0 & CI_Upper < 0))

# Print results
nice_table(cent_ci_table_high, title = "Bootstrapping - High Seniority Group")

#
# The table shows all the estimates for the centrality metrics that have a stable range.
# Doesn't include 0 and doesn't have a big range.
# This confirms our analysis of x10 and x13 lying on lots of shortest paths between nodes.
# It also confirms x13 and x10 having the highest strength and expected influence.
#

# Print results
nice_table(cent_ci_table_low, title = "Bootstrapping - Low Seniority Group")

#
# The table shows all the estimates for the centrality metrics that have a stable range.
# Doesn't include 0 and doesn't have a big range.
# This confirms our analysis of x10 and x13 lying on lots of shortest paths between nodes.
# It also confirms x13 and x4 having the highest strength and expected influence.

## Network Comparison Test ----

# Save function for estimator used by NCT (same as used in previous analyses)
estim <- function(x) {
  out <- bootnet::estimateNetwork(
    x,
    default = "EBICglasso",
    corMethod = "spearman",
  )
  out$graph
}

# Run NCT focusing on centrality
nct_out <- NCT(
  highData, lowData,
  it = 10, # Increase to at least 1000
  binary.data = FALSE,
  test.edges = TRUE,
  test.centrality = TRUE,
  estimator = estim,
  p.adjust.methods = "fdr",
  verbose = FALSE
) %>% 
  # Suppress warnings to prevent text overflooding
  suppressMessages() %>% 
  suppressWarnings() %>%
  invisible()

# Inspect output - Global Network Structure
cat(paste0("Test statistic M = ", round(nct_out$nwinv.real, 2),
           ", p-value (FDR) = ", round(nct_out$nwinv.pval, 3)))

#
# Since the p-value is not signifcant we fail to reject the null hypothesis in favour of the alternate.
# Therefore we conclude that there is no significant difference between the different networks.
#

# Inspect output - Edges
nct_out$einv.pvals %>% data.frame(.) %>%
  mutate(Test.statistic.E = round(Test.statistic.E, 2)) %>% 
  mutate(p.value = round(p.value, 3)) %>% 
  rownames_to_column(., "Variable") %>% 
  select(Variable, Var1, Var2, Test.statistic.E, p.value) %>% 
  nice_table(title = "Network Comparison Test - Edges (FDR Correction)")

#
# Since the p-value is not significant for any of the pairs we fail to reject the null hypothesis in favour of the alternate.
# Therefore we conclude that there is no significant difference between edge level differences in the network.
#

# Inspect output - Centrality
nct_out$diffcen.pval %>% 
  data.frame(.) %>%
  mutate(across(everything(.), ~round(., 3))) %>% 
  rownames_to_column(., "Variable") %>% 
  nice_table(title = "Network Comparison Test - Centrality Indices (FDR Correction)")

#
# In the table we see that the strength and expected influence for all the nodes is identically with a value of 0.94,
# except for x3 which has an expected influence of 1.00 
#

#
# Overall, we see that the two different seniority groups display very similar results.
#

# Task 3: Latent variable modelling (CFA) ----

## Define CFA Model ----

model = "
UDT =~ x1 + x2 + x3 + x4
IaE =~ x5 + x6 + x7 + x8 + x9
CSB =~ x10 + x11 + x12 + x13
"

## Fit Model ----

fit <- cfa(model, data = dfSummative, std.lv = TRUE)

## fit indices (10 marks), standardised estimates (10 marks) ----

summary(fit, standardized = TRUE, fit.measures = TRUE)

# Standardised Estimates
#
# Using the Std.all column we see that we have negatve loadings for x2, x4, x6, x11.
# This suggests these variables have inverse effects on the latent variable.
# We also see that 'Use of digital tools' and 'Innovation and experimentation' and 'Use of digital tools' 
# and 'Cost-saving behaviour' have negative covariance,
# which suggest they have inverse effects on each other.
# We also see that 'Innovation and experimentation' and 'Cost-saving behaviour' have a positive covariance.
# Another thing we notice that most variables have an absolute loading > 0.30 and even some greater than 0.7.
# x11 is the only variable with an absolute loading value less than 0.30. This suggests the loading on x11 doesn't have that strong of an effect on model.
#

# Fit Indices
#
# We see that Root Mean Square Error of Approximation is 0.066 < 0.08 therefore, our model is an acceptable fit but not a good fit.
# We see that Standardized Root Mean Square Residual is 0.046 < 0.06 therefore, our model is a good fit.
# The Comparative Fit Index (CFI) is 0.909 > 0.90 therefore, our model is a good fit.
# The Tucker-Lewis Index (TLI) is 0.886 < 0.90 therefore, our model is a poor fit.
# The Chi-Square test p-value is 0.000 < 0.05. Therefore, our test is significant at the 5% level suggesting our model is a good fit

#
# Overall we conclude that the model is a acceptable level of fit to the dataset. Although there is room for improvement.
#
## Model Plot ----

# Create the plot, including standardised coefficients and covariances
lavaanPlot(
  fit,  # The model to plot
  coefs = TRUE,
  stand = TRUE,
  covs = TRUE
)

#
# The plot further strengthens our analysis of the standardised estimates.
#