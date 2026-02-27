library(tidyverse)
library(tibble)
library(dplyr)
library(ggplot2)
library(deSolve)
library(tibble)
library(patchwork)
library(purrr)
library(gert)

## LABOUR MARKET


# Helpers: production & derivatives
f_y   <- function(k, h, alpha) k^alpha * h^(1 - alpha)
f_r   <- function(k, h, alpha) alpha * k^(alpha - 1) * h^(1 - alpha)         # rental rate
f_w   <- function(k, h, alpha) (1 - alpha) * k^alpha * h^(-alpha)           # wage (per eff. worker)

# Calibration: compute psi to hit target h_ss
calibrate_psi <- function(alpha, delta, n, g, rho, theta, phi, h_ss) {
  Ck <- (alpha / (rho + delta))^(1 / (1 - alpha))
  Cy <- Ck^alpha
  Cc <- Cy - (n + g + delta) * Ck
  if (Cc <= 0) stop("Parameters imply non-positive steady-state consumption per eff. worker. Adjust (rho, delta, n, g).")
  psi <- (1 - alpha) * Ck^alpha / (Cc^theta * h_ss^(theta + phi))
  list(psi = psi, Ck = Ck, Cy = Cy, Cc = Cc)
}

# Steady state given psi and h_ss (closed form)
steady_state_ll <- function(alpha, delta, n, g, rho, theta, phi, h_ss, psi) {
  Ck <- (alpha / (rho + delta))^(1 / (1 - alpha))
  Cy <- Ck^alpha
  Cc <- Cy - (n + g + delta) * Ck
  k_ss <- Ck * h_ss
  y_ss <- Cy * h_ss
  c_ss <- Cc * h_ss
  list(k_ss = k_ss, c_ss = c_ss, h_ss = h_ss, y_ss = y_ss)
}

# ODE with labor-leisure (h solved algebraically each t)
ramsey_ll_ode <- function(t, state, parms) {
  with(as.list(c(state, parms)), {
    # solve hours from intratemporal FOC: h = [ ((1-alpha)/psi) * k^alpha / c^theta ]^(1/(phi+alpha))
    h_unclamped <- (((1 - alpha) / psi) * k^alpha / (c^theta))^(1 / (phi + alpha))
    # numerical safety clamp
    h <- max(min(h_unclamped, 0.95), 1e-6)
    
    y   <- f_y(k, h, alpha)
    r   <- f_r(k, h, alpha)
    dk  <- y - c - (n + g + delta) * k
    dc  <- c * ((r - rho - delta) / theta)
    
    list(c(dk, dc), c(h = h, y = y, r = r))
  })
}

#  Runner
run_ramsey_ll <- function(alpha = 0.33, delta = 0.05, n = 0.01, g = 0.00,
                          rho = 0.02, theta = 2.0, phi = 2.0,
                          h_ss_target = 1/3,
                          k0 = NULL, c0 = NULL,
                          t_max = 200, dt = 0.1) {
  
  # Calibrate psi to hit target h_ss
  calib <- calibrate_psi(alpha, delta, n, g, rho, theta, phi, h_ss_target)
  psi <- calib$psi
  
  # Steady state (closed form given h_ss)
  ss <- steady_state_ll(alpha, delta, n, g, rho, theta, phi, h_ss_target, psi)
  
  # Safe initial conditions: start away from steady state to see dynamics
  if (is.null(k0)) k0 <- 0.7 * ss$k_ss
  if (is.null(c0)) c0 <- 0.7 * ss$c_ss
  state0 <- c(k = k0, c = c0)
  
  times <- seq(0, t_max, by = dt)
  
  out <- deSolve::ode(
    y     = state0,
    times = times,
    func  = ramsey_ll_ode,
    parms = list(alpha = alpha, delta = delta, n = n, g = g,
                 rho = rho, theta = theta, phi = phi, psi = psi)
  )
  
  # Coerce to plain tibble and add derived series
  out <- as.data.frame(out)
  out <- tibble::as_tibble(out) |>
    dplyr::rename(t = time) |>
    dplyr::mutate(
      t = as.numeric(t),
      k = as.numeric(k),
      c = as.numeric(c),
      h = as.numeric(h),
      y = as.numeric(y),
      r = as.numeric(r),
      i = y - c
    )
  
  # Plots
  p_k <- ggplot2::ggplot(out, ggplot2::aes(t, k)) +
    ggplot2::geom_line(color = "#2C7FB8", linewidth = 1) +
    ggplot2::labs(title = "Capital per effective worker", x = "Time", y = "k(t)") +
    ggplot2::theme_minimal()
  
  p_c <- ggplot2::ggplot(out, ggplot2::aes(t, c)) +
    ggplot2::geom_line(color = "#D95F0E", linewidth = 1) +
    ggplot2::labs(title = "Consumption per effective worker", x = "Time", y = "c(t)") +
    ggplot2::theme_minimal()
  
  p_h <- ggplot2::ggplot(out, ggplot2::aes(t, h)) +
    ggplot2::geom_line(color = "#41AB5D", linewidth = 1) +
    ggplot2::labs(title = "Hours (labor) per person", x = "Time", y = "h(t)") +
    ggplot2::theme_minimal()
  
  p_phase <- ggplot2::ggplot(out, ggplot2::aes(k, c, color = t)) +
    ggplot2::geom_path(linewidth = 1) +
    ggplot2::scale_color_viridis_c() +
    ggplot2::labs(
      title = "Phase diagram (k, c) with endogenous hours",
      x = "k", y = "c", color = "t"
    ) +
    ggplot2::theme_minimal()
  
  list(
    params = list(alpha=alpha, delta=delta, n=n, g=g, rho=rho, theta=theta, phi=phi,
                  psi=psi, h_ss_target=h_ss_target),
    steady_state = ss,
    path = out,
    plots = list(k = p_k, c = p_c, h = p_h, phase = p_phase)
  )
}


res_ll <- run_ramsey_ll(
  alpha = 0.33, delta = 0.05, n = 0.01, g = 0.00,
  rho = 0.02, theta = 2.0, phi = 2.0, h_ss_target = 1/3
)

res_ll$steady_state      # k*, c*, h*, y*
res_ll$params$psi        # Calibrated disutility weight
res_ll$plots$k
res_ll$plots$c
res_ll$plots$h
res_ll$plots$phase
