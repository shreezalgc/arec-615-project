## EXTENSION CODE -------------------------------------------------------------

# Extract coefficients
coefs7  <- coef(model7)
alpha1  <- coefs7["NOx"]
alpha2  <- coefs7["log(pcGDP)"]
alpha3  <- coefs7["E_NOx"]
alpha4  <- coefs7["H_NOx"]

delta1  <- coef(production_model)["log(NOx)"]

Ebar <- mean(sim_data$Education)
Hbar <- mean(sim_data$Health)

# u_p and K
u_p_rep <- alpha1 + alpha3 * Ebar + alpha4 * Hbar
p0 <- mean(sim_data$NOx)
Y  <- mean(sim_data$pcGDP)
K  <- -u_p_rep / alpha2

# Target reduction
target_r <- 0.629 
a_target <- target_r * p0

# Calibrate theta
theta_star <- K * Y / (a_target + 0.5 * K * a_target^2)

# Cost, income, utility
C_a <- function(a) 0.5 * theta_star * a^2
y_of_a <- function(a) Y - C_a(a)

u_of_a <- function(a) {
  p <- p0 - a
  y <- y_of_a(a)
  ifelse(y > 0,
         alpha1*p + alpha2*log(y) + alpha3*p*Ebar + alpha4*p*Hbar,
         -1e9)
}

# Solve for optimal a
a_max <- min(p0, sqrt(2*Y/theta_star)*0.99)
opt_res <- optimize(function(a) -u_of_a(a), c(0, a_max))

a_star <- opt_res$minimum
W_star <- -opt_res$objective
p_star <- p0 - a_star
r_star <- a_star/p0
tau_star <- theta_star * a_star

cat("\n===== EXTENSION RESULTS =====\n")
cat("theta* =", theta_star, "\n")
cat("a*     =", a_star, "\n")
cat("p*     =", p_star, "\n")
cat("r*     =", r_star, "\n")
cat("tau*   =", tau_star, "\n")


