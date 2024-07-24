#This script outlines the commands and necessary steps for model construction, forecasting future outcomes, and conducting cross-validation metrics.

#!/usr/bin/env Rscript 

# Load necessary libraries
library(INLA) # For Bayesian modeling
library(tidyverse) # For data manipulation
library(sf) # For spatial operations
library(lattice) # For lattice plots

# Set INLA options
inla.setOption(num.threads = 32)

# Read graph data
g = inla.read.graph(filename = "graph_geobr")


# Load and preprocess the dataset
df <- read.csv("data.csv")

cod_scen = 1 # Scenario code: 1 or 3 or 5


################################################################################
# Filtering steps (depends on the objective)
# For models without forecasting, filter only historical data (scenario 0)
# For forecasting scenarios 2021-2040 and 2041-2060, use specified scenario code
################################################################################
df <- df %>% 
  filter(scenario == 0 | scenario == cod_scen) 

# Create BDI category (division into quantile)
df <- df %>%
  mutate(BDI_cat = case_when(
    BDI_pop_stand < -0.7299586 ~ 1,
    BDI_pop_stand >= -0.7299586 & BDI_pop_stand < 0.6786737 ~ 2,
    BDI_pop_stand >= 0.6786737 ~ 3
  )) %>%
  mutate(BDI_cat = as.factor(BDI_cat))

# Create season factor
df <- df %>%
  mutate(season = case_when(
    month <= 3 ~ 3, #summer
    month > 3 & month <= 6 ~ 4, #autumn
    month > 6 & month <= 9 ~ 1, # winter
    month >= 10 ~ 2 # spring
  )) %>%
  mutate(season = as.factor(season))

# Rearrange the dataset by micro, month, and year
df <- df %>%
  arrange(cod_mcr, year, month)

# Create ID variables
df$id_micro <- rep(1:length(unique(df$cod_mcr)), each = 12*24) 
df$ID.time <- rep(1:(12*24), 557)
df$ID.time2 <- df$ID.time 


#Adding a one-month lag to the future tmi_lag1 variable.
df$tmi_lag1_replace <- df$tmi_lag1

df$tmi_lag1[df$year == 2030 & df$month == 1] <- 
  df$tmi_lag1_replace[df$year == 2030 & df$month == 12]

df$tmi_lag1[df$year == 2030 & df$month == 2] <- 
  df$tmi_lag1_replace[df$year == 2030 & df$month == 1]

df$tmi_lag1[df$year == 2030 & df$month == 3] <- 
  df$tmi_lag1_replace[df$year == 2030 & df$month == 2]

df$tmi_lag1[df$year == 2030 & df$month == 4] <- 
  df$tmi_lag1_replace[df$year == 2030 & df$month == 3]

df$tmi_lag1[df$year == 2030 & df$month == 5] <- 
  df$tmi_lag1_replace[df$year == 2030 & df$month == 4]

df$tmi_lag1[df$year == 2030 & df$month == 6] <- 
  df$tmi_lag1_replace[df$year == 2030 & df$month == 5]

df$tmi_lag1[df$year == 2030 & df$month == 7] <- 
  df$tmi_lag1_replace[df$year == 2030 & df$month == 6]

df$tmi_lag1[df$year == 2030 & df$month == 8] <- 
  df$tmi_lag1_replace[df$year == 2030 & df$month == 7]

df$tmi_lag1[df$year == 2030 & df$month == 9] <- 
  df$tmi_lag1_replace[df$year == 2030 & df$month == 8]

df$tmi_lag1[df$year == 2030 & df$month == 10] <- 
  df$tmi_lag1_replace[df$year == 2030 & df$month == 9]

df$tmi_lag1[df$year == 2030 & df$month == 11] <- 
  df$tmi_lag1_replace[df$year == 2030 & df$month == 10]

df$tmi_lag1[df$year == 2030 & df$month == 12] <- 
  df$tmi_lag1_replace[df$year == 2030 & df$month == 11]


df$tmi_lag1[df$year == 2050 & df$month == 1] <- 
  df$tmi_lag1_replace[df$year == 2050 & df$month == 12]

df$tmi_lag1[df$year == 2050 & df$month == 2] <- 
  df$tmi_lag1_replace[df$year == 2050 & df$month == 1]

df$tmi_lag1[df$year == 2050 & df$month == 3] <- 
  df$tmi_lag1_replace[df$year == 2050 & df$month == 2]

df$tmi_lag1[df$year == 2050 & df$month == 4] <- 
  df$tmi_lag1_replace[df$year == 2050 & df$month == 3]

df$tmi_lag1[df$year == 2050 & df$month == 5] <- 
  df$tmi_lag1_replace[df$year == 2050 & df$month == 4]

df$tmi_lag1[df$year == 2050 & df$month == 6] <- 
  df$tmi_lag1_replace[df$year == 2050 & df$month == 5]

df$tmi_lag1[df$year == 2050 & df$month == 7] <- 
  df$tmi_lag1_replace[df$year == 2050 & df$month == 6]

df$tmi_lag1[df$year == 2050 & df$month == 8] <- 
  df$tmi_lag1_replace[df$year == 2050 & df$month == 7]

df$tmi_lag1[df$year == 2050 & df$month == 9] <- 
  df$tmi_lag1_replace[df$year == 2050 & df$month == 8]

df$tmi_lag1[df$year == 2050 & df$month == 10] <- 
  df$tmi_lag1_replace[df$year == 2050 & df$month == 9]

df$tmi_lag1[df$year == 2050 & df$month == 11] <- 
  df$tmi_lag1_replace[df$year == 2050 & df$month == 10]

df$tmi_lag1[df$year == 2050 & df$month == 12] <- 
  df$tmi_lag1_replace[df$year == 2050 & df$month == 11]


# Considering minimum temperature as non-linear
df$ID.TMI <- inla.group(df$tmi_lag1, n = 25, method = "cut", idx.only = TRUE)

#Adding a two-month lag to the future pre_lag2_stand  variable.
df$pre_lag2_stand_replace <- df$pre_lag2_stand


df$pre_lag2_stand[df$year == 2030 & df$month == 1] <- 
  df$pre_lag2_stand_replace[df$year == 2030 & df$month == 11]

df$pre_lag2_stand[df$year == 2030 & df$month == 2] <- 
  df$pre_lag2_stand_replace[df$year == 2030 & df$month == 12]

df$pre_lag2_stand[df$year == 2030 & df$month == 3] <- 
  df$pre_lag2_stand_replace[df$year == 2030 & df$month == 1]

df$pre_lag2_stand[df$year == 2030 & df$month == 4] <- 
  df$pre_lag2_stand_replace[df$year == 2030 & df$month == 2]

df$pre_lag2_stand[df$year == 2030 & df$month == 5] <- 
  df$pre_lag2_stand_replace[df$year == 2030 & df$month == 3]

df$pre_lag2_stand[df$year == 2030 & df$month == 6] <- 
  df$pre_lag2_stand_replace[df$year == 2030 & df$month == 4]

df$pre_lag2_stand[df$year == 2030 & df$month == 7] <- 
  df$pre_lag2_stand_replace[df$year == 2030 & df$month == 5]

df$pre_lag2_stand[df$year == 2030 & df$month == 8] <- 
  df$pre_lag2_stand_replace[df$year == 2030 & df$month == 6]

df$pre_lag2_stand[df$year == 2030 & df$month == 9] <- 
  df$pre_lag2_stand_replace[df$year == 2030 & df$month == 7]

df$pre_lag2_stand[df$year == 2030 & df$month == 10] <- 
  df$pre_lag2_stand_replace[df$year == 2030 & df$month == 8]

df$pre_lag2_stand[df$year == 2030 & df$month == 11] <- 
  df$pre_lag2_stand_replace[df$year == 2030 & df$month == 9]

df$pre_lag2_stand[df$year == 2030 & df$month == 12] <- 
  df$pre_lag2_stand_replace[df$year == 2030 & df$month == 10]


df$pre_lag2_stand[df$year == 2050 & df$month == 1] <- 
  df$pre_lag2_stand_replace[df$year == 2050 & df$month == 11]

df$pre_lag2_stand[df$year == 2050 & df$month == 2] <- 
  df$pre_lag2_stand_replace[df$year == 2050 & df$month == 12]

df$pre_lag2_stand[df$year == 2050 & df$month == 3] <- 
  df$pre_lag2_stand_replace[df$year == 2050 & df$month == 1]

df$pre_lag2_stand[df$year == 2050 & df$month == 4] <- 
  df$pre_lag2_stand_replace[df$year == 2050 & df$month == 2]

df$pre_lag2_stand[df$year == 2050 & df$month == 5] <- 
  df$pre_lag2_stand_replace[df$year == 2050 & df$month == 3]

df$pre_lag2_stand[df$year == 2050 & df$month == 6] <- 
  df$pre_lag2_stand_replace[df$year == 2050 & df$month == 4]

df$pre_lag2_stand[df$year == 2050 & df$month == 7] <- 
  df$pre_lag2_stand_replace[df$year == 2050 & df$month == 5]

df$pre_lag2_stand[df$year == 2050 & df$month == 8] <- 
  df$pre_lag2_stand_replace[df$year == 2050 & df$month == 6]

df$pre_lag2_stand[df$year == 2050 & df$month == 9] <- 
  df$pre_lag2_stand_replace[df$year == 2050 & df$month == 7]

df$pre_lag2_stand[df$year == 2050 & df$month == 10] <- 
  df$pre_lag2_stand_replace[df$year == 2050 & df$month == 8]

df$pre_lag2_stand[df$year == 2050 & df$month == 11] <- 
  df$pre_lag2_stand_replace[df$year == 2050 & df$month == 9]

df$pre_lag2_stand[df$year == 2050 & df$month == 12] <- 
  df$pre_lag2_stand_replace[df$year == 2050 & df$month == 10]


#  Saving a copy before inserting NA values for cross-validation step
df_beforeNA <- df


################################################################################
# The command below, 'Convert cases to NA,' should only be executed for the steps involving forecasting! Also, if the purpose is the cross-validation step, then the desired time and/or space set should be indexed so that they can be replaced by missing values (NA).
################################################################################

# Convert cases to NA for specific time or space
df$cases[df$year == 2030] <- NA
df$cases[df$year == 2050] <- NA

# Define hyperparameters for the model
hyper.iid <- list(theta = list(prior = "pc.prec", param = c(1, 0.01)))
rho_hyper <- list(theta = list(prior = "pccor1", param = c(0, 0.9)))

# Define the model formula
formula <- cases ~ 1 + season +
  pre_lag2_stand +  
  infra_urb_stand + 
  NDWI_stand + 
  BDI_cat + 
  micro_ele_stand +
  f(id_micro, model = 'besagproper', graph = g, group = ID.time , 
    control.group = list(model = 'ar1', hyper = rho_hyper)) +
  f(ID.time2, model = "iid", hyper = hyper.iid) +
  f(ID.TMI, model = 'rw1', hyper = hyper.iid, scale.model = TRUE)

# Scale population by 100,000
df$pop_100000 <- df$pop / 100000

# Fit the model
t_0 <- Sys.time() # Record start time

mm <- inla(formula,
           family = "poisson", data = df,
           offset = log(pop_100000),
           control.predictor = list(link = 1, compute = TRUE),
           control.compute = list(dic = TRUE, waic = TRUE, 
                                  return.marginals.predictor = TRUE, 
                                  config = TRUE),
           control.fixed = list(prec.intercept = 1, prec = 1))

t_1 <- Sys.time() # Record end time
t_1 - t_0 # Calculate duration

#  Display model summary
summary(mm)

# Check linear predictor and fitted values for missing cases
ii <- which(is.na(df$cases))
round(mm$summary.linear.predictor[ii[1:5],], 3)
round(mm$summary.fitted.values[ii[1:5],], 3)

# Marginal posterior for the linear predictor
y.mplp <- mm$summary.linear.predictor$mean[ii]
y.pred <- mm$summary.fitted.values$mean[ii]

#  Plot observed cases vs fitted values
plot(df$cases, mm$summary.fitted.values[, 1])

# Correlation of observed cases with fitted values
cor(df$cases, mm$summary.fitted.values[, 1], method = "spearman", use = "complete.obs")
cor(df$cases, mm$summary.fitted.values[, 1], method = "pearson", use = "complete.obs")

################################################################################
# Forecasting
################################################################################
set.seed(112510)

# Sampling from the posterior distribution using INLA with 1000 samples
post.samples = inla.posterior.sample(n = 1000, result = mm, 
                                     use.improved.mean=TRUE)

# Convert samples to natural scale by taking the exponential 
fit.matrix = do.call(cbind, lapply(post.samples, function(X)
  exp(X$latent[startsWith(rownames(X$latent), "Pred")])))

# Take only the forecasting part
fit.matrix.ii = fit.matrix[ii,]

# Sample from a Poisson distribution using values from the joint posterior distribution
distribution
set.seed(11)
pois.samples = apply(fit.matrix.ii, 2, function(Z) rpois(n = length(Z),
                                                         lambda = Z))

# Calculate 95% CI and median
IC95 = apply(pois.samples, 1, function(x) quantile(x, 
                                                  probs = c(0.025, 0.5, 0.975)))

# Convert Poisson samples to a data frame and calculate forecast uncertainty metrics
pois.samples.df = as.data.frame(pois.samples) 

Forecast.Uncert = pois.samples.df  %>%
  rowwise() %>%
  mutate(
    mean = mean(c_across(V1:V1000)),
    median = median(c_across(V1:V1000)),
    sd = sd(c_across(V1:V1000)),
    LL = quantile(c_across(V1:V1000), probs = 0.025),
    UL = quantile(c_across(V1:V1000), probs = 0.975)
  )


# Save the workspace image
save.image("model.RData")

################################################################################
## Cross-validation metrics
################################################################################

# Assign real values to the Forecast.Uncert object
Forecast.Uncert$Y <- df_beforeNA$cases[ii]

# Calculating coverage probability
cov.prob <- mean((Forecast.Uncert$LL <= Forecast.Uncert$Y) &
                   (Forecast.Uncert$UL > Forecast.Uncert$Y))

# Calculate correlation with mean prediction
cor_mean_pred <- cor(Forecast.Uncert$Y, Forecast.Uncert$mean, 
                     method = "spearman")

# Calculate correlation with summary fitted values
cor_summary_fitted <- cor(Forecast.Uncert$Y, mm$summary.fitted.values$mean[ii], 
                          method = "spearman")

# Calculate the differences between real values and predictions
real_values <- Forecast.Uncert$Y
predictions <- Forecast.Uncert$mean

differences <- real_values - predictions

# Calculate Mean Absolute Error (MAE)
"MAE:"
mae <- mean(abs(diferencas))
mae




