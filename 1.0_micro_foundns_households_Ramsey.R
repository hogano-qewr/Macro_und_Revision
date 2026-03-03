library(tidyverse)
library(deSolve)

# Simple Ramsey Dynamics in R

ramsey_model <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    # f(k) = k^alpha
    mpk <- alpha * k^(alpha - 1)
    
    dk <- k^alpha - c - (n + g + delta) * k
    dc <- (c / sigma) * (mpk - delta - rho)
    
    return(list(c(dk, dc)))
  })
}

# Define params (Example)
params <- c(alpha = 0.33, delta = 0.05, rho = 0.02, sigma = 2, n = 0.01, g = 0.02)

library(ggplot2)

# 1. Setup Parameters
alpha <- 0.33; delta <- 0.05; rho <- 0.03; n <- 0.02; sigma <- 2

# 2. Calculate Steady States
k_ss <- ((rho + delta) / alpha)^(1 / (alpha - 1))
c_ss <- k_ss^alpha - (n + delta) * k_ss

# 3. Define the Nullclines
k_vals <- seq(0.1, k_ss * 2, length.out = 100)
dot_k_0 <- k_vals^alpha - (n + delta) * k_vals # Consumption where k is stable
dot_c_0 <- c_ss                                # Consumption where c is stable

df_nullclines <- data.frame(k = k_vals, dk0 = dot_k_0)

# 4. Plotting
ggplot(df_nullclines, aes(x = k)) +
  geom_line(aes(y = dk0), color = "blue", size = 1) +      # k-nullcline
  geom_vline(xintercept = k_ss, color = "red", size = 1) +  # c-nullcline
  geom_point(aes(x = k_ss, y = c_ss), size = 4) +           # Steady State
  annotate("text", x = k_ss + 2, y = c_ss + 0.1, label = "Steady State (E)") +
  labs(title = "Ramsey Model Phase Diagram",
       subtitle = "Blue: dk/dt = 0 | Red: dc/dt = 0",
       x = "Capital (k)", y = "Consumption (c)") +
  theme_minimal()


# SHOOTING METHOD - ALGORITHM 

# 1. Recalculate Steady State for comparison
k_ss <- ((params["rho"] + params["delta"]) / params["alpha"])^(1 / (params["alpha"] - 1))
c_ss <- k_ss^params["alpha"] - (params["n"] + params["delta"]) * k_ss

# 2. Reset Bounds
low_c <- 0
high_c <- k0^params["alpha"] # Max consumption cannot exceed total output
k0 <- 1.0

for (i in 1:30) {
  guess_c <- (low_c + high_c) / 2
  
  # Run a very short simulation to see the INITIAL trend
  out <- try(as.data.frame(ode(y = c(k = k0, c = guess_c), 
                               times = seq(0, 30, by = 1), 
                               func = ramsey_system, 
                               parms = params)), silent = TRUE)
  
  # Check if it crashed or Capital hit the floor
  if (inherits(out, "try-error") || any(is.na(out)) || any(out$k < 0.01)) {
    # If k crashed, we consumed TOO MUCH
    high_c <- guess_c
  } else {
    last_k <- tail(out$k, 1)
    # If k ended up HIGHER than its starting point (growing toward infinity), 
    # and we are already above k_ss, or if k is just growing too fast:
    if (last_k > k_ss) {
      # We saved TOO MUCH (consumed too little)
      low_c <- guess_c
    } else {
      # We are still below k_ss and k is falling or stable
      high_c <- guess_c
    }
  }
}

print(paste("New Guess for c0:", round(guess_c, 6)))

# 1. Check your Steady State - is it valid?
print(k_ss) 
print(c_ss)

# 2. Test a tiny consumption value manually
test_out <- ode(y = c(k = 1.0, c = 0.1), times = seq(0, 5, by = 1), 
                func = ramsey_system, parms = params)
print(head(test_out))

# 1. Parameters (from your output)
params <- c(alpha = 0.33, delta = 0.05, rho = 0.03, n = 0.02, sigma = 2)
k_ss <- 8.2897
c_ss <- 1.4293
k0 <- 1.0

# 2. Reset Bounds
low_c <- 0.05
high_c <- c_ss # At k=1, you definitely consume less than the steady state
iterations <- 25

for (i in 1:iterations) {
  guess_c <- (low_c + high_c) / 2
  
  # Simulate forward
  out <- try(as.data.frame(ode(y = c(k = k0, c = guess_c), 
                               times = seq(0, 50, by = 1), 
                               func = ramsey_system, 
                               parms = params)), silent = TRUE)
  
  # Logic: 
  # If it crashes, C was too high.
  # If k grows PAST the steady state, C was too low (undershot).
  # If k crashes to zero, C was too high (overshot).
  
  if (inherits(out, "try-error") || any(is.na(out)) || any(out$k < 0.1)) {
    high_c <- guess_c
  } else {
    final_k <- tail(out$k, 1)
    
    if (final_k > k_ss) {
      # Economy is growing too fast/too far -> Save less (Consume more)
      low_c <- guess_c
    } else {
      # Economy is stalled or shrinking -> Save more (Consume less)
      high_c <- guess_c
    }
  }
}

final_c0 <- as.numeric(guess_c)
print(paste("The Saddle Path value for c0 is:", round(final_c0, 4)))

final_out <- as.data.frame(ode(y = c(k = k0, c = final_c0), 
                               times = seq(0, 100, by = 1), 
                               func = ramsey_system, 
                               parms = params))


# 1. Load the library FIRST
library(ggplot2)

# 2. Run the plot command SECOND (no library calls inside the + chain)
ggplot(final_out, aes(x = k, y = c)) +
  geom_path(color = "darkgreen", linewidth = 1.2) +
  annotate("point", x = k_ss, y = c_ss, color = "red", size = 3) +
  labs(title = "The Saddle Path", 
       subtitle = paste("Initial c0 =", round(final_c0, 4)),
       x = "Capital (k)", y = "Consumption (c)") +
  theme_minimal()





# new path - comparative dynamics

# 1. New Parameters (Technological Breakthrough)
params_new <- params
params_new["alpha"] <- 0.40 # Increase from 0.33

# 2. Find the NEW Steady State
k_ss_new <- ((params_new["rho"] + params_new["delta"]) / params_new["alpha"])^(1 / (params_new["alpha"] - 1))
c_ss_new <- k_ss_new^params_new["alpha"] - (params_new["n"] + params_new["delta"]) * k_ss_new

# 3. Use the Shooting Method to find the NEW c0 
# (Starting from our CURRENT capital level k0 = 1.0)
low_c <- 0.1; high_c <- 2.0
for (i in 1:25) {
  guess_c_new <- (low_c + high_c) / 2
  out_new <- try(as.data.frame(ode(y = c(k = k0, c = guess_c_new), 
                                   times = seq(0, 50, by = 1), 
                                   func = ramsey_system, parms = params_new)), silent = TRUE)
  if (inherits(out_new, "try-error") || any(out_new$k < 0.1)) { high_c <- guess_c_new }
  else { if (tail(out_new$k, 1) > k_ss_new) { low_c <- guess_c_new } else { high_c <- guess_c_new } }
}

# 4. Plot both paths
ggplot() +
  geom_path(data = final_out, aes(x = k, y = c), color = "darkgreen", size = 1, linetype = "dashed") + # Old Path
  geom_path(data = out_new, aes(x = k, y = c), color = "blue", size = 1.2) + # New Path
  annotate("point", x = k_ss, y = c_ss, color = "darkgreen", size = 3) +
  annotate("point", x = k_ss_new, y = c_ss_new, color = "blue", size = 3) +
  labs(title = "Global Comparative Dynamics: Productivity Shock",
       subtitle = "Green (Dashed) = Old Path | Blue = New Path after Alpha increases",
       x = "Capital (k)", y = "Consumption (c)") +
  theme_minimal()
