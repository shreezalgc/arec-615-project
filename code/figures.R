
## =======================
## EXTENSION SETUP
## =======================

library(dplyr)
library(ggplot2)
library(purrr)

# 1. Extract coefficients and key moments -----------------------------

coefs7  <- coef(model7)
alpha1  <- coefs7["NOx"]
alpha2  <- coefs7["log(pcGDP)"]
alpha3  <- coefs7["E_NOx"]
alpha4  <- coefs7["H_NOx"]

delta1  <- coef(production_model)["log(NOx)"]

Ebar <- mean(sim_data$Education, na.rm = TRUE)
Hbar <- mean(sim_data$Health,    na.rm = TRUE)

u_p_rep <- alpha1 + alpha3 * Ebar + alpha4 * Hbar   # ∂u/∂p
p0      <- mean(sim_data$NOx,   na.rm = TRUE)       # baseline NOx
Y       <- mean(sim_data$pcGDP, na.rm = TRUE)       # baseline income
K       <- -u_p_rep / alpha2                        # as in notes

# Target abatement from paper's 62.9% -------------------------------
target_r <- 0.629
a_target <- target_r * p0

# 2. Calibrate theta and solve for optimal a* ------------------------

theta_star <- K * Y / (a_target + 0.5 * K * a_target^2)

# Cost, income, utility as functions of a
C_a    <- function(a, theta = theta_star) 0.5 * theta * a^2
y_of_a <- function(a, theta = theta_star) Y - C_a(a, theta)

u_of_a <- function(a, theta = theta_star) {
  p <- p0 - a
  y <- y_of_a(a, theta)
  ifelse(y > 0,
         alpha1 * p +
           alpha2 * log(y) +
           alpha3 * p * Ebar +
           alpha4 * p * Hbar,
         -1e9)
}

MC_a <- function(a, theta = theta_star) theta * a

# feasible max a (to keep y > 0)
a_max_feas <- sqrt(2 * Y / theta_star) * 0.99
a_upper    <- min(p0, a_max_feas)

opt_res <- optimize(function(a) -u_of_a(a), c(0, a_upper))
a_star  <- opt_res$minimum
W_star  <- -opt_res$objective
p_star  <- p0 - a_star
r_star  <- a_star / p0
tau_star <- MC_a(a_star, theta_star)

cat("theta* =", theta_star, "\n")
cat("a*     =", a_star, "\n")
cat("p*     =", p_star, "\n")
cat("r*     =", r_star, "\n")
cat("tau*   =", tau_star, "\n")


## =======================
## PLOT 1: Total & Marginal Abatement Cost vs a
## =======================

a_seq <- seq(0, a_upper, length.out = 300)
df_cost <- data.frame(
  a  = a_seq,
  C  = C_a(a_seq, theta_star),
  MC = MC_a(a_seq, theta_star)
)

p_cost <- ggplot(df_cost) +
  geom_line(aes(x = a, y = C), color = "firebrick", linewidth = 1.1) +
  geom_line(aes(x = a, y = MC), color = "steelblue", linewidth = 1.1) +
  annotate("text",
           x = a_upper * 0.65,
           y = max(df_cost$C) * 0.55,
           label = "Total Cost",
           color = "firebrick", fontface = "bold") +
  annotate("text",
           x = a_upper * 0.65,
           y = max(df_cost$MC),
           label = "Marginal Cost",
           color = "steelblue", fontface = "bold") +
  labs(
    x = "Abatement (a)",
    y = "Cost"
  ) + 
  theme_classic(base_size = 18) +
  theme(
    axis.text  = element_text(color = "black", size = 18),
    axis.ticks = element_line(color = "black", linewidth = 0.8),
    axis.title = element_text(color = "black", size = 18),
    axis.line  = element_line(color = "black", linewidth = 0.8)
  )

p_cost


## =======================
## PLOT 2: Welfare W(a) vs Abatement a
## =======================

df_W <- data.frame(
  a = a_seq,
  W = sapply(a_seq, u_of_a, theta = theta_star)
)

p_W <- ggplot(df_W, aes(x = a, y = W)) +
  geom_line(linewidth = 1.1) +
  geom_vline(xintercept = a_star, linetype = "dashed") +
  annotate("text",
           x = a_star,
           y = max(df_W$W) - 0.004,
           label = paste0("a* = ", round(a_star, 2)),
           angle = 90, vjust = -0.5) +
  labs(
    x = "Abatement (a)",
    y = "W(a)"
  ) +
  theme_classic(base_size = 18) +
  theme(
    axis.text  = element_text(color = "black", size = 18),
    axis.ticks = element_line(color = "black", linewidth = 0.8),
    axis.title = element_text(color = "black", size = 18),
    axis.line  = element_line(color = "black", linewidth = 0.8)
  ) 

p_W


## =======================
## PLOT 3: Abatement Cost vs Emissions p
## =======================

p_seq <- seq(0, p0, length.out = 300)
df_Cp <- data.frame(
  p = p_seq,
  C = C_a(p0 - p_seq, theta_star)
)

p_emissions <- ggplot(df_Cp, aes(x = p, y = C)) +
  geom_line(color = "firebrick", linewidth = 1.1) +
  geom_vline(xintercept = p_star, linetype = "dashed") +
  labs(
    x = "Emissions (p)",
    y = "Total Cost C(p)"
  ) +
  theme_classic(base_size = 18) +
  theme(
    axis.text  = element_text(color = "black", size = 18),
    axis.ticks = element_line(color = "black", linewidth = 0.8),
    axis.title = element_text(color = "black", size = 18),
    axis.line  = element_line(color = "black", linewidth = 0.8)
  ) 


p_emissions

## =======================
## PLOT: Marginal Abatement Cost vs Emissions
## =======================

p_seq <- seq(0, p0, length.out = 300)

df_MC_p <- data.frame(
  p  = p_seq,
  MC = theta_star * (p0 - p_seq)    # MC(p) = θ (p0 - p)
)

p_MC_emissions <- ggplot(df_MC_p, aes(x = p, y = MC)) +
  geom_line(color = "steelblue", linewidth = 1.1) +
  geom_vline(xintercept = p_star, linetype = "dashed") +
  geom_hline(yintercept = tau_star, linetype = "dotted") +
  annotate("text",
           x = p_star,
           y = max(df_MC_p$MC) * 0.08,
           label = paste0("p* = ", round(p_star, 2)),
           angle = 90, vjust = -0.5) +
  annotate("text",
           x = max(df_MC_p$p) * 0.55,
           y = max(df_MC_p$MC) * 0.50,
           label = "MC(a)",
           color = "steelblue",
           fontface = "bold") +
  labs(
    x = "Emissions (p)",
    y = "Marginal Cost MC(p)"
  ) +
  theme_classic(base_size = 18)+
  theme(
    axis.text  = element_text(color = "black", size = 18),
    axis.ticks = element_line(color = "black", linewidth = 0.8),
    axis.title = element_text(color = "black", size = 18),
    axis.line  = element_line(color = "black", linewidth = 0.8)
  ) 


p_MC_emissions



## =======================
## PLOT 4: Comparative Statics – a*(theta) and tau*(theta)
## =======================

# Helper to compute a* and tau* for a given theta
solve_opt_for_theta <- function(theta_val) {
  a_max_feas <- sqrt(2 * Y / theta_val) * 0.99
  a_upper    <- min(p0, a_max_feas)
  u_of_a_theta <- function(a) u_of_a(a, theta = theta_val)
  opt <- optimize(function(a) -u_of_a_theta(a), c(0, a_upper))
  a_star_val <- opt$minimum
  tau_val    <- MC_a(a_star_val, theta_val)
  data.frame(theta = theta_val,
             a_star = a_star_val,
             tau_star = tau_val)
}

theta_grid <- seq(theta_star * 0.3, theta_star * 3, length.out = 40)

df_cs <- map_dfr(theta_grid, solve_opt_for_theta)

# a*(theta)
p_cs_a <- ggplot(df_cs, aes(x = theta, y = a_star)) +
  geom_line(linewidth = 1.1) +
  geom_vline(xintercept = theta_star, linetype = "dashed") +
  annotate("text",
           x = theta_star, y = max(df_cs$a_star),
           label = "hat(theta)", angle = 90, vjust = -0.5, parse = TRUE) +
  labs(
    x = expression(theta),
    y = expression(a^"*")
  ) +
  theme_classic(base_size = 18) +
  theme(
    axis.text  = element_text(color = "black", size = 18),
    axis.ticks = element_line(color = "black", linewidth = 0.8),
    axis.title = element_text(color = "black", size = 18),
    axis.line  = element_line(color = "black", linewidth = 0.8)
  ) 



p_cs_a

# marginal abatement cost
p_cs_tau <- ggplot(df_cs, aes(x = theta, y = tau_star)) +
  geom_line(linewidth = 1.1) +
  geom_vline(xintercept = theta_star, linetype = "dashed") +
  annotate("text",
           x = theta_star, y = max(df_cs$tau_star),
           label = "hat(theta)", angle = 90, vjust = -0.5, parse = TRUE) +
  labs(
    x = expression(theta),
    y = expression(MAC)
  ) +
  theme_classic(base_size = 18) +
  theme(
    axis.text  = element_text(color = "black", size = 18),
    axis.ticks = element_line(color = "black", linewidth = 0.8),
    axis.title = element_text(color = "black", size = 18),
    axis.line  = element_line(color = "black", linewidth = 0.8)
  ) 



p_cs_tau


## =======================
## SAVE ALL EXTENSION FIGURES
## =======================

ggsave("./output/figures/fig1_cost_mc_abate.pdf",
       p_cost, width = 7, height = 5)

ggsave("./output/figures/fig2_welfare_vs_abatement.pdf",
       p_W, width = 7, height = 5)

ggsave("./output/figures/fig3_cost_vs_emissions.pdf",
       p_emissions, width = 7, height = 5)

ggsave("./output/figures/fig4_mc_vs_emissions.pdf",
       p_MC_emissions, width = 7, height = 5)

ggsave("./output/figures/fig5_comparative_statics_a_theta.pdf",
       p_cs_a, width = 7, height = 5)

ggsave("./output/figures/fig6_comparative_statics_tau_theta.pdf",
       p_cs_tau, width = 7, height = 5)


