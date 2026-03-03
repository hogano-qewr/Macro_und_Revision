

# WAGE STICKINESD AND GLOBAL COMPARATIVE DYNAMICS


#  Solving for wage Stickiness (gamma): Using the characteristic equation from your text:

# --- Parameters ---
r <- 0.03    # Interest rate (consistent with your Ramsey model)
beta <- 5.0  # Weight on adjustment costs (Try changing this!)

# --- Solve for Gamma ---
# Term A is the middle coefficient of the quadratic equation
term_A <- 2 + r + (1 + r) / beta
discriminant <- term_A^2 - 4 * (1 + r)

# We take the negative square root to get the fractional (stable) root
gamma <- (term_A - sqrt(discriminant)) / 2

print(paste("The Adjustment Parameter (gamma) is:", round(gamma, 4)))
print(paste("Proportion of gap closed each period (1 - gamma):", round(1 - gamma, 4)))



# WAGE SHOCK SIMULATION

# --- Simulation ---
periods <- 20
wage_gap <- numeric(periods)

# Initial Shock: 10% deviation (0.1 in log terms)
wage_gap[1] <- 0.10 

for (t in 2:periods) {
  wage_gap[t] <- gamma * wage_gap[t-1]
}

# --- Plotting the Recovery ---
library(ggplot2)
df_wage <- data.frame(Time = 1:periods, Gap = wage_gap)

ggplot(df_wage, aes(x = Time, y = Gap)) +
  geom_line(color = "darkred", size = 1) +
  geom_point(color = "darkred") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Global Comparative Dynamics: Wage Shock Recovery",
       subtitle = paste("Stickiness (gamma) =", round(gamma, 3), 
                        "| Costs (beta) =", beta),
       x = "Periods (Years)", y = "Log Wage Gap (w - w_bar)") +
  theme_minimal()


# 2. R Code: Simulating the Economic Cost: This script calculates how much 
#   "Potential GDP" is lost over 20 periods because of the wage stickiness 
#   (gamma)

# --- Parameters ---
alpha <- 0.33  # Capital share from your Ramsey model
r <- 0.03      # Interest rate
beta <- 5.0    # Adjustment costs (Scarth's parameter)

# --- 1. Calculate Gamma (Scarth's Root) ---
term_A <- 2 + r + (1 + r) / beta
gamma <- (term_A - sqrt(term_A^2 - 4 * (1 + r))) / 2

# --- 2. Simulate the Wage Gap Recovery ---
periods <- 20
wage_gap <- 0.10 * (gamma^(0:(periods-1))) # 10% initial shock decaying by gamma

# --- 3. Calculate Output Loss ---
# Percentage loss in Y for every 1% wage is 'off'
elasticity <- (1 - alpha) / alpha 
output_loss <- -elasticity * wage_gap

# --- 4. Plot the Result ---
library(ggplot2)
df_recovery <- data.frame(
  Time = 1:periods,
  Wage_Gap = wage_gap * 100,      # As percentage
  Output_Loss = output_loss * 100 # As percentage
)

ggplot(df_recovery, aes(x = Time)) +
  geom_bar(aes(y = Output_Loss), stat = "identity", fill = "firebrick", alpha = 0.6) +
  geom_line(aes(y = Wage_Gap), color = "darkblue", size = 1) +
  annotate("text", x = 15, y = -5, label = "Red Bars = Output Loss (%)", color = "firebrick") +
  annotate("text", x = 15, y = 5, label = "Blue Line = Wage Gap (%)", color = "darkblue") +
  labs(title = "Economic Cost of Sticky Wages",
       subtitle = paste("Cumulative Output Loss:", round(sum(abs(output_loss))*100, 2), "% of GDP"),
       y = "Percentage Deviation from Equilibrium", x = "Years") +
  theme_minimal()

).


# MMONETARY SHOCK SIMULATION: 
#  This script shows how a sudden increase in the money supply temporarily lowers the Real Wage 
#   (making workers "cheaper" for firms) and thus boosts Output above the steady state.

# --- Parameters ---
alpha <- 0.33
r <- 0.03
beta <- 8.0   # Higher beta = more stickiness = bigger monetary effect
m_shock <- 0.10 # 10% increase in Money Supply

# --- 1. Calculate Scarth's Stickiness (gamma) ---
term_A <- 2 + r + (1 + r) / beta
gamma <- (term_A - sqrt(term_A^2 - 4 * (1 + r))) / 2

# --- 2. Simulate the Nominal Wage Catch-up ---
# The Target Wage (w_bar) jumped by 10% instantly.
# The Actual Wage (w) starts at 0 and chases w_bar.
periods <- 25
nominal_wage <- numeric(periods)
target_wage <- rep(m_shock, periods) # New equilibrium level

for (t in 2:periods) {
  # Scarth's Rule: move (1-gamma) toward the target
  nominal_wage[t] <- nominal_wage[t-1] + (1 - gamma) * (target_wage[t-1] - nominal_wage[t-1])
}

# --- 3. Calculate the Real Effects ---
# Real Wage Gap = Actual Wage - Target Wage
# (If actual wage is BELOW target, labour is "cheap" and output BOOMS)
real_wage_gap <- nominal_wage - target_wage
output_boost <- -((1 - alpha) / alpha) * real_wage_gap

# --- 4. Plot the "Short-Run Non-Neutrality" ---
df_monetary <- data.frame(
  Time = 1:periods,
  Wage = nominal_wage * 100,
  Output = output_boost * 100
)

library(ggplot2)
ggplot(df_monetary, aes(x = Time)) +
  geom_area(aes(y = Output), fill = "steelblue", alpha = 0.4) +
  geom_line(aes(y = Wage), color = "darkred", size = 1.2) +
  geom_hline(yintercept = m_shock * 100, linetype = "dashed") +
  annotate("text", x = 18, y = m_shock*100 + 1, label = "New Equilibrium Wage") +
  labs(title = "Monetary Shock: The Effect of Sticky Wages",
       subtitle = "Blue Area = Output Boom | Red Line = Wage Catching Up",
       y = "% Change from Baseline", x = "Years after Money Supply Increase") +
  theme_minimal()


# MONEATRY SHOCK COMBINED WITH PRODUCTIVITY CRASH (REDUCTION IN ALPHA)

# --- Parameters ---
alpha_old <- 0.33
alpha_new <- 0.30  # 3% Technology "Crash"
r <- 0.03
beta <- 10.0       # High stickiness (Scarth friction)
m_offset <- 0.04   # 4% Monetary Injection

# --- 1. Calculate Scarth's Stickiness (gamma) ---
term_A <- 2 + r + (1 + r) / beta
gamma <- (term_A - sqrt(term_A^2 - 4 * (1 + r))) / 2

# --- 2. Define the Two Scenarios ---
periods <- 30
# Scenario A: Tech Crash ONLY (Wages must fall to new lower equilibrium)
target_w_crash <- -0.05 # Equilibrium wage drops 5%
w_no_policy <- numeric(periods)

# Scenario B: Tech Crash + Monetary Help (Wages catch up to a "higher" nominal target)
target_w_policy <- target_w_crash + m_offset # Target is now only -1%
w_with_policy <- numeric(periods)

for (t in 2:periods) {
  w_no_policy[t] <- w_no_policy[t-1] + (1 - gamma) * (target_w_crash - w_no_policy[t-1])
  w_with_policy[t] <- w_with_policy[t-1] + (1 - gamma) * (target_w_policy - w_with_policy[t-1])
}

# --- 3. Calculate Output Effects ---
# Output Gap = Productivity Change - (Wage Gap effect)
out_no_policy <- (alpha_new - alpha_old) - ((1-alpha_new)/alpha_new) * (w_no_policy - target_w_crash)
out_with_policy <- (alpha_new - alpha_old) - ((1-alpha_new)/alpha_new) * (w_with_policy - target_w_policy)

# --- 4. Plotting the "Stabilisation" ---
df_policy <- data.frame(
  Time = 1:periods,
  No_Policy = out_no_policy * 100,
  With_Policy = out_with_policy * 100
)

ggplot(df_policy, aes(x = Time)) +
  geom_line(aes(y = No_Policy), color = "darkred", size = 1, linetype = "dashed") +
  geom_line(aes(y = With_Policy), color = "darkblue", size = 1.2) +
  geom_hline(yintercept = (alpha_new - alpha_old)*100, linetype = "dotted") +
  annotate("text", x = 22, y = -2, label = "Policy Offset (Blue)", color = "darkblue") +
  annotate("text", x = 22, y = -6, label = "Pure Tech Crash (Red)", color = "darkred") +
  labs(title = "Can the Central Bank Offset a Technology Crash?",
       subtitle = "Blue line shows the economy with a 4% Monetary Injection",
       y = "% Change in Output (Y)", x = "Years") +
  theme_minimal()



# INVESTMENT SHOCK (Tobin's q: Scarth value of equities)
#  Simulate a "Confidence Shock" where q suddenly jumps (e.g., due to optimistic
#      expectations about future demand), causing a surge in investment.
library(deSolve)
library(ggplot2)

library(ggplot2)

library(ggplot2)

# 1. Extreme Parameters for a steep, visible slope
r <- 0.15; delta <- 0.10; chi <- 40.0; alpha <- 0.33 
k_ss <- ((r + delta) / alpha)^(1 / (alpha - 1))

# 2. Calculate the STABLE (Negative) Slope
A12 <- - (alpha * (alpha - 1) * k_ss^(alpha - 2))
# The stable root (mu) must be negative
mu_stable <- (r - sqrt(r^2 + 4 * A12 * (k_ss / chi))) / 2
# The slope S = mu_stable / (k_ss / chi)
S_perfect <- mu_stable / (k_ss / chi)

# 3. Create Data (0.5 to 1.5 of Steady State)
k_vals <- seq(k_ss * 0.5, k_ss * 1.5, length.out = 200)
q_vals <- 1 + S_perfect * (k_vals - k_ss)
df_final <- data.frame(k_rel = k_vals / k_ss, q = q_vals)

# 4. Plot the "Rational" Saddle Path
library(ggplot2)
ggplot(df_final, aes(x = k_rel, y = q)) +
  geom_line(color = "darkblue", linewidth = 2) + 
  geom_hline(yintercept = 1, linetype = "dotted") +
  geom_vline(xintercept = 1, linetype = "dotted") +
  annotate("point", x = 1, y = 1, color = "red", size = 4) +
  labs(title = "The Rational Tobin's Q Saddle Path",
       subtitle = "Downward sloping: High K means Low Q",
       x = "Capital Stock (K) (Relative to Steady State)", 
       y = "Shadow Price (q)") +
  theme_minimal()


 # G CROWDING

# --- Parameters ---
r <- 0.05; delta <- 0.05; chi <- 10.0; alpha <- 0.33
# Initial G = 0, New G = 0.5 (Large for visibility)
G_new <- 0.5 

# 1. New Steady State Calculation
# G reduces the resources available for K accumulation
k_ss_old <- ((r + delta) / alpha)^(1 / (alpha - 1))
# In the steady state, Y - G = C + delta*K
# Higher G leads to a lower long-run capital stock (Crowding Out)
k_ss_new <- ((r + delta) / alpha)^(1 / (alpha - 1)) # K_ss itself doesn't change if r is fixed
# However, consumption is what primarily gets 'hit' in simple Ramsey

# --- 1. SET THE TARGETS ---
k_ss <- 8.2897  # Steady State Capital
c_ss_original <- 1.4293
low_c <- 1.25    # Lower bound for search
high_c <- 1.42   # Upper bound for search

# --- 2. AUTOMATIC SHOOTING LOOP ---
for (i in 1:30) {
  guess_c <- (low_c + high_c) / 2
  
  # Simulate with the current guess
  out <- try(as.data.frame(ode(y = c(k = k_ss, c = guess_c), 
                               times = seq(0, 60, by = 0.5), 
                               func = ramsey_war, parms = params)), silent = TRUE)
  
  # Logic: If Capital crashes or Consumption explodes, c0 was too HIGH
  if (inherits(out, "try-error") || any(is.na(out)) || tail(out$k, 1) < 1.0) {
    high_c <- guess_c
  } else {
    # If Capital ends up HIGHER than steady state, c0 was too LOW
    if (tail(out$k, 1) > k_ss) {
      low_c <- guess_c
    } else {
      high_c <- guess_c
    }
  }
}

# --- 3. PLOT THE WINNING PATH ---
final_c0 <- guess_c
out_stable <- as.data.frame(ode(y = c(k = k_ss, c = final_c0), 
                                times = seq(0, 60, by = 0.5), 
                                func = ramsey_war, parms = params))

ggplot(out_stable, aes(x = time)) +
  geom_hline(yintercept = 1, linetype = "dotted") +
  annotate("rect", xmin = 2, xmax = 7, ymin = 0.7, ymax = 1.1, fill = "grey", alpha = 0.2) +
  geom_line(aes(y = k/k_ss, color = "Capital (K)"), linewidth = 1.5) +
  geom_line(aes(y = c/c_ss_original, color = "Consumption (C)"), linewidth = 1.5) +
  scale_y_continuous(limits = c(0.8, 1.1)) + 
  labs(title = paste("STABLE WAR RECOVERY | c0 =", round(final_c0, 5)),
       subtitle = "The loop found the path that doesn't collapse",
       y = "Relative to Steady State", x = "Years") +
  theme_minimal()
