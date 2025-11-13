rm(list = ls())

# Loading libraries
library(dplyr)
library(MASS)

set.seed(10162025)

# Sample size
n <- 10968
n_provinces <- 28

# Simulate province data
province_data <- data.frame(
  province_id = 1:n_provinces,
  NOx = rgamma(n_provinces, shape = 2, rate = 0.3) * 3.5,
  pcGDP = rlnorm(n_provinces, meanlog = log(5.641), sdlog = 0.3),
  Houseprice = rlnorm(n_provinces, meanlog = log(7.585), sdlog = 0.3),
  Gini = rnorm(n_provinces, mean = 0.497, sd = 0.04),
  k = rlnorm(n_provinces, meanlog = log(15.206), sdlog = 0.2),
  h = rlnorm(n_provinces, meanlog = log(3.752), sdlog = 0.08)
)

# Ensure realistic ranges
province_data$NOx <- pmax(province_data$NOx, 0.1)
province_data$pcGDP <- pmax(province_data$pcGDP, 2.5)
province_data$Houseprice <- pmax(province_data$Houseprice, 4)
province_data$Gini <- pmax(pmin(province_data$Gini, 0.7), 0.3)
province_data$k <- pmax(province_data$k, 7)
province_data$h <- pmax(pmin(province_data$h, 5.5), 2.5)

# Assign individuals to provinces
sim_data <- data.frame(
  individual_id = 1:n,
  province_id = sample(1:n_provinces, n, replace = TRUE, prob = runif(n_provinces))
)
sim_data <- merge(sim_data, province_data, by = "province_id")

# Simulate ALL individual characteristics from Table 3
sim_data$Age <- round(rnorm(n, mean = 50.4, sd = 16.9))
sim_data$Age <- pmax(pmin(sim_data$Age, 95), 18)
sim_data$Age2 <- sim_data$Age^2

sim_data$Gender <- rbinom(n, 1, 0.468)
sim_data$Han <- rbinom(n, 1, 0.078)
sim_data$City <- rbinom(n, 1, 0.590)
sim_data$Party <- rbinom(n, 1, 0.104)
sim_data$Married <- rbinom(n, 1, 0.784)
sim_data$Work <- rbinom(n, 1, 0.617)

# Income with heavy tail
sim_data$Income <- exp(rnorm(n, mean = log(2.354), sd = 1.2))
sim_data$Income <- pmin(sim_data$Income, 500)

sim_data$HouseArea <- rnorm(n, mean = 116, sd = 91)
sim_data$HouseArea <- pmax(sim_data$HouseArea, 0)

sim_data$Medicare <- rbinom(n, 1, 0.912)

# Education with ordinal distribution
edu_probs <- c(0.15, 0.40, 0.30, 0.15)
sim_data$Education <- sample(0:3, n, replace = TRUE, prob = edu_probs)

# Health with ordinal distribution
health_probs <- c(0.05, 0.15, 0.30, 0.35, 0.15)
sim_data$Health <- sample(1:5, n, replace = TRUE, prob = health_probs)

# Emotion with ordinal distribution
emotion_probs <- c(0.05, 0.15, 0.30, 0.35, 0.15)
sim_data$Emotion <- sample(1:5, n, replace = TRUE, prob = emotion_probs)

# Fair with ordinal distribution
fair_probs <- c(0.10, 0.20, 0.35, 0.25, 0.10)
sim_data$Fair <- sample(1:5, n, replace = TRUE, prob = fair_probs)

# Social class level
sim_data$Level <- round(rnorm(n, mean = 4.316, sd = 1.638))
sim_data$Level <- pmax(pmin(sim_data$Level, 10), 1)

# Create interaction terms
sim_data$E_NOx <- sim_data$Education * sim_data$NOx
sim_data$H_NOx <- sim_data$Health * sim_data$NOx

# Create ventilation coefficient as instrumental variable
sim_data$VC <- 50 - 0.5 * sim_data$NOx + rnorm(n, 0, 10)

# Generate latent SWB using paper's specification
latent_SWB <- 2.0 - 0.011 * sim_data$NOx + 
  0.156 * log(sim_data$pcGDP) + 
  0.05 * sim_data$Education + 
  0.15 * sim_data$Health - 
  0.005 * sim_data$Age + 
  0.02 * sim_data$Gender + 
  0.08 * log(sim_data$Income) - 
  0.003 * sim_data$E_NOx + 
  0.004 * sim_data$H_NOx + 
  0.10 * sim_data$City + 
  0.08 * sim_data$Married +
  0.06 * sim_data$Work + 
  0.05 * sim_data$Medicare + 
  0.12 * sim_data$Fair +
  0.09 * sim_data$Level - 
  0.10 * sim_data$Emotion + 
  rnorm(n, 0, 0.6)

# Convert to ordered SWB matching paper's distribution
cut_points <- c(-Inf, 2.0, 2.8, 3.6, 4.4, Inf)
sim_data$SWB <- as.numeric(cut(latent_SWB, cut_points))
sim_data$SWB_factor <- factor(sim_data$SWB, ordered = TRUE)

write_rds(sim_data, "./data/dat_final.rds", compress = "gz")
