rm(list = ls())


library(tidyverse)
library(MASS)
library(readr)
library(ordinal)
library(kableExtra)
library(modelsummary)

set.seed(10162025)


options(scipen = 999)
options("modelsummary_format_numeric_latex" = "plain")

# -------------------------------------------------
sim_data <- read_rds("./data/dat_final.rds")


# Descriptive statistics -
# Create the descriptive statistics table

table3_data <- data.frame(
  Variables = c("SWB", "pcGDP", "NOx", "Houseprice", "Gini", "Age", "Age2", 
                "Gender", "Han", "City", "Party", "Married", "Work", "Income",
                "HouseArea", "Medicare", "Education", "Health", "Emotion", 
                "Fair", "Level", "k", "h"),
  Definition = c(
    "Subjective well-being (very happy = 5, happy = 4, ordinary = 3, unhappy = 2, very unhappy = 1)",
    "Per capita GDP (×10^4 yuan)",
    "Nitrogen oxide emission (kg/m²)",
    "Residential price (yuan/m²)",
    "Gini coefficient",
    "Age",
    "Age-squared",
    "Gender (male = 1, female = 0)",
    "Nationality (Han = 1, other = 0)",
    "Household registration (urban = 1, country = 0)",
    "Party membership (communist = 1, other = 0)",
    "Marital status (not single = 1, single = 0)",
    "Work status (employed = 1, unemployed = 0)",
    "Annual occupational income (×10^4 yuan/person)",
    "Residential area (square meters)",
    "Whether participating in medical insurance (Yes = 1, No = 0)",
    "Education level (not educated = 0, compulsory education = 1, senior high = 2, bachelor degree or higher = 3)",
    "Self-rated health status (very unhealthy = 1, unhealthy = 2, average = 3, healthy = 4, very healthy = 5)",
    "Depression or frustration in the past four weeks (never depressed = 1, rarely = 2, sometimes = 3, often = 4, always = 5)",
    "Social justice (very unfair = 1, unfair = 2, ordinary = 3, fair = 4, very fair = 5)",
    "Self-assessment of social class, ranging from 1 (the lowest) to 10 (the top)",
    "Physical capital stock per capita (×10^4 yuan/person)",
    "Human capital stock per capita (person-year/person)"
  ),
  Mean = c(
    mean(sim_data$SWB),
    mean(sim_data$pcGDP) / 10000,  # Convert to 10^4 yuan
    mean(sim_data$NOx),
    mean(sim_data$Houseprice),
    mean(sim_data$Gini),
    mean(sim_data$Age),
    mean(sim_data$Age2),
    mean(sim_data$Gender),
    mean(sim_data$Han),
    mean(sim_data$City),
    mean(sim_data$Party),
    mean(sim_data$Married),
    mean(sim_data$Work),
    mean(sim_data$Income) / 10000,  # Convert to 10^4 yuan
    mean(sim_data$HouseArea),
    mean(sim_data$Medicare),
    mean(sim_data$Education),
    mean(sim_data$Health),
    mean(sim_data$Emotion),
    mean(sim_data$Fair),
    mean(sim_data$Level),
    mean(sim_data$k) / 10000,  # Convert to 10^4 yuan
    mean(sim_data$h)
  ),
  SD = c(
    sd(sim_data$SWB),
    sd(sim_data$pcGDP) / 10000,
    sd(sim_data$NOx),
    sd(sim_data$Houseprice),
    sd(sim_data$Gini),
    sd(sim_data$Age),
    sd(sim_data$Age2),
    sd(sim_data$Gender),
    sd(sim_data$Han),
    sd(sim_data$City),
    sd(sim_data$Party),
    sd(sim_data$Married),
    sd(sim_data$Work),
    sd(sim_data$Income) / 10000,
    sd(sim_data$HouseArea),
    sd(sim_data$Medicare),
    sd(sim_data$Education),
    sd(sim_data$Health),
    sd(sim_data$Emotion),
    sd(sim_data$Fair),
    sd(sim_data$Level),
    sd(sim_data$k) / 10000,
    sd(sim_data$h)
  ),
  Min = c(
    min(sim_data$SWB),
    min(sim_data$pcGDP) / 10000,
    min(sim_data$NOx),
    min(sim_data$Houseprice),
    min(sim_data$Gini),
    min(sim_data$Age),
    min(sim_data$Age2),
    min(sim_data$Gender),
    min(sim_data$Han),
    min(sim_data$City),
    min(sim_data$Party),
    min(sim_data$Married),
    min(sim_data$Work),
    min(sim_data$Income) / 10000,
    min(sim_data$HouseArea),
    min(sim_data$Medicare),
    min(sim_data$Education),
    min(sim_data$Health),
    min(sim_data$Emotion),
    min(sim_data$Fair),
    min(sim_data$Level),
    min(sim_data$k) / 10000,
    min(sim_data$h)
  ),
  Max = c(
    max(sim_data$SWB),
    max(sim_data$pcGDP) / 10000,
    max(sim_data$NOx),
    max(sim_data$Houseprice),
    max(sim_data$Gini),
    max(sim_data$Age),
    max(sim_data$Age2),
    max(sim_data$Gender),
    max(sim_data$Han),
    max(sim_data$City),
    max(sim_data$Party),
    max(sim_data$Married),
    max(sim_data$Work),
    max(sim_data$Income) / 10000,
    max(sim_data$HouseArea),
    max(sim_data$Medicare),
    max(sim_data$Education),
    max(sim_data$Health),
    max(sim_data$Emotion),
    max(sim_data$Fair),
    max(sim_data$Level),
    max(sim_data$k) / 10000,
    max(sim_data$h)
  )
)

# Print the table
print(table3_data, row.names = FALSE)


# Alternatively, create a nicer formatted table using kableExtra

kbl(table3_data, 
    caption = "Table 3: Descriptive Statistics of Key Variables",
    col.names = c("Variables", "Definition", "Mean", "S.D.", "Min", "Max"),
    digits = 3,
    format = "latex") %>%
  kable_classic(full_width = FALSE) %>%
  column_spec(2, width = "4cm") %>%
  footnote(
    general = "Note: The descriptive statistics are based on 
               the simulated dataset with n = 10,968 
               observations.",
    general_title = ""
  ) 


## 1. Baseline Ordered Probit Models (Table 4)

# Model 1: Only NOx
model1 <- clm(SWB_factor ~ NOx, data = sim_data, link = "probit")
summary(model1)

# Model 2: Add log(pcGDP)
model2 <- clm(SWB_factor ~ NOx + log(pcGDP), data = sim_data, link = "probit")
summary(model2)

# Model 3: Add provincial characteristics
model3 <- clm(SWB_factor ~ NOx + log(pcGDP) + Houseprice + Gini, 
              data = sim_data, link = "probit")
summary(model3)

# Model 4: Add individual characteristics
model4 <- clm(SWB_factor ~ NOx + log(pcGDP) + Houseprice + Gini +
                Age + Gender + Education + Health + log(Income) +
                City + Married + Work + Medicare,
              data = sim_data, link = "probit")
summary(model4)

# Model 5: With Education interaction only
model5 <- clm(SWB_factor ~ NOx + log(pcGDP) + E_NOx + 
                Age + Gender + Education + Health + log(Income) +
                City + Married + Work + Medicare,
              data = sim_data, link = "probit")
summary(model5)

# Model 6: With Health interaction only  
model6 <- clm(SWB_factor ~ NOx + log(pcGDP) + H_NOx +
                Age + Gender + Education + Health + log(Income) +
                City + Married + Work + Medicare,
              data = sim_data, link = "probit")
summary(model6)

# Model 7: With both interactions (Full interaction model)
model7 <- clm(SWB_factor ~ NOx + log(pcGDP) + E_NOx + H_NOx +
                Age + Gender + Education + Health + log(Income) +
                City + Married + Work + Medicare,
              data = sim_data, link = "probit")
summary(model7)


# Create a list of all models
models <- list(
  "Model 1" = model1,
  "Model 2" = model2,
  "Model 3" = model3,
  "Model 4" = model4,
  "Model 5" = model5,
  "Model 6" = model6,
  "Model 7" = model7
)

#### coefficients rename ----
coef_rename = c(
  "NOx" = "NOx",
  "log(pcGDP)" = "log(pcGDP)",
  "E_NOx" = "E×NOx",
  "H_NOx" = "H×NOx",
  "Education" = "Education",
  "Health" = "Health"
  )

# Create Table 4 using modelsummary
modelsummary(
  models,
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  statistic = "statistic",
  coef_map = coef_rename,
  gof_map = c("nobs", "r2"),
  notes = "Dependent variable: Subjective well-being (1-5 scale). Ordered probit models. Standard errors in parentheses.",
  output = "./output/tables/table_1.tex"
)


## 2. Production Function (Table 7)
production_model <- lm(log(pcGDP) ~ log(NOx) + log(k) + log(h), 
                       data = sim_data)
modelsummary(production_model,
             stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
             statistic = "statistic",
             output = "./output/tables/table_2.tex")

## 3. IV Estimation (Table 8)

# First stage regression
first_stage <- lm(NOx ~ VC + log(pcGDP) + Education + Health + Age + Gender,
                  data = sim_data)
summary(first_stage)

# Store predicted values
sim_data$NOx_hat <- predict(first_stage)

# Second stage IV models
iv_model1 <- clm(SWB_factor ~ NOx_hat + log(pcGDP) + Education + Health +
                   Age + Gender + log(Income),
                 data = sim_data, link = "probit")
summary(iv_model1)

# IV model with interactions
iv_model_full <- clm(SWB_factor ~ NOx_hat + log(pcGDP) + 
                       E_NOx + H_NOx + Education + Health +
                       Age + Gender + log(Income),
                     data = sim_data, link = "probit")
summary(iv_model_full)


modelsummary(list(iv_model1, iv_model_full),
             stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
             statistic = "statistic",
             coef_map = c(
               "NOx_hat" = "NOx",
               "log(pcGDP)" = "log(pcGDP)",
               "E_NOx" = "E×NOx",
               "H_NOx" = "H×NOx",
               "Education" = "Education",
               "Health" = "Health"
             ),
             output = "./output/tables/table_3.tex")

## 4. Calculate Key Results

# Extract coefficients from the full interaction model
alpha1 <- coef(model7)["NOx"]
alpha2 <- coef(model7)["log(pcGDP)"]
alpha3 <- coef(model7)["E_NOx"]
alpha4 <- coef(model7)["H_NOx"]

# Extract production function coefficients
delta1 <- coef(production_model)["log(NOx)"]

# Calculate effects
direct_effect <- alpha1 / alpha2
indirect_effect <- delta1
net_effect <- direct_effect + indirect_effect

# Willingness to Pay calculation
WTP <- -net_effect * mean(sim_data$pcGDP)

# Optimal reduction rate (simplified)
optimal_reduction <- 1 - (indirect_effect / abs(direct_effect))

cat("\n=== KEY REPLICATION RESULTS ===\n")
cat("Direct effect (α₁/α₂):", round(direct_effect, 4), "\n")
cat("Indirect effect (δ₁):", round(indirect_effect, 4), "\n")
cat("Net effect:", round(net_effect, 4), "\n")
cat("Willingness to Pay (RMB per capita):", round(WTP, 2), "\n")
cat("Optimal reduction rate:", round(optimal_reduction * 100, 2), "%\n")



## 5. Robustness Checks (Tables 5 & 6)

# OLS model for comparison
ols_model <- lm(SWB ~ NOx + log(pcGDP) + E_NOx + H_NOx +
                  Education + Health + Age + Gender + log(Income),
                data = sim_data)
summary(ols_model)

# Ordered Logit model
logit_model <- clm(SWB_factor ~ NOx + log(pcGDP) + E_NOx + H_NOx +
                     Education + Health + Age + Gender + log(Income),
                   data = sim_data, link = "logit")
summary(logit_model)

## 6. Visualization
ggplot(sim_data, aes(x = NOx, y = SWB)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relationship between NOx Pollution and Subjective Well-being",
       x = "NOx Emissions (kg/m²)", 
       y = "Subjective Well-being (1-5 scale)") +
  theme_minimal()

# Plot by education level
ggplot(sim_data, aes(x = NOx, y = SWB, color = factor(Education))) +
  geom_smooth(method = "lm") +
  labs(title = "NOx Pollution and SWB by Education Level",
       x = "NOx Emissions (kg/m²)",
       y = "Subjective Well-being",
       color = "Education Level") +
  theme_minimal()

## 7. Summary Table Comparison
cat("\n=== COMPARISON WITH PAPER'S RESULTS ===\n")
comparison <- data.frame(
  Result = c("NOx coefficient (Model 7)", "log(pcGDP) coefficient", 
             "Education interaction", "Health interaction",
             "Production elasticity (δ₁)", "Optimal reduction rate"),
  Paper = c("-0.011***", "0.156***", "-0.003*", "0.004***", "0.064***", "62.857%"),
  Replication = c(sprintf("%.3f", coef(model7)["NOx"]),
                  sprintf("%.3f", coef(model7)["log(pcGDP)"]),
                  sprintf("%.3f", coef(model7)["E_NOx"]),
                  sprintf("%.3f", coef(model7)["H_NOx"]),
                  sprintf("%.3f", delta1),
                  sprintf("%.1f%%", optimal_reduction * 100))
)
print(comparison)

## 8. Statistical Significance Check
cat("\n=== STATISTICAL SIGNIFICANCE ===\n")
cat("NOx coefficient p-value:", summary(model7)$coefficients["NOx", "Pr(>|z|)"], "\n")
cat("log(pcGDP) coefficient p-value:", summary(model7)$coefficients["log(pcGDP)", "Pr(>|z|)"], "\n")
cat("Education interaction p-value:", summary(model7)$coefficients["E_NOx", "Pr(>|z|)"], "\n")
cat("Health interaction p-value:", summary(model7)$coefficients["H_NOx", "Pr(>|z|)"], "\n")
cat("Production elasticity p-value:", summary(production_model)$coefficients["log(NOx)", "Pr(>|t|)"], "\n")

