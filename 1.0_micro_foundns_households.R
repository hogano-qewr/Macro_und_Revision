library(tidyverse)
library(tibble)
library(dplyr)
library(ggplot2)
library(deSolve)
library(tibble)
library(patchwork)
library(gert)

# Helpers
f_y   <- function(k, alpha) k^alpha
f_mpk <- function(k, alpha) alpha * k^(alpha - 1)

ramsey_k_star <- function(alpha, rho, delta) {
  (alpha / (rho + delta))^(1 / (1 - alpha))
}

ramsey_c_star <- function(k_star, alpha, n, g, delta) {
  f_y(k_star, alpha) - (n + g + delta) * k_star
}

ramsey_ode <- function(t, state, parms) {
  with(as.list(c(state, parms)), {
    y   <- f_y(k, alpha)
    mpk <- f_mpk(k, alpha)
    dk  <- y - c - (n + g + delta) * k
    dc  <- c * ((mpk - rho - delta) / theta)
    list(c(dk, dc))
  })
}

run_ramsey <- function(alpha = 0.33, delta = 0.05, n = 0.01, g = 0.00,
                       rho = 0.02, theta = 2.0,
                       k0 = NULL, c0 = NULL,
                       t_max = 200, dt = 0.1) {
  
  # Steady state
  k_star <- ramsey_k_star(alpha, rho, delta)
  c_star <- ramsey_c_star(k_star, alpha, n, g, delta)
  
  if (is.null(k0)) k0 <- 0.8 * k_star
  if (is.null(c0)) c0 <- 0.8 * c_star
  state0 <- c(k = k0, c = c0)
  times  <- seq(0, t_max, by = dt)
  
  # Solve ODE and COERCE to a plain tibble
  out <- deSolve::ode(y = state0, times = times, func = ramsey_ode,
                      parms = list(alpha = alpha, delta = delta, n = n,
                                   g = g, rho = rho, theta = theta))
  
  # Coercion is key: drop 'deSolve' class and make numeric columns explicit
  out <- as.data.frame(out)   # or: tibble::as_tibble(out)
  out <- tibble::as_tibble(out) |>
    dplyr::rename(t = time) |>
    dplyr::mutate(
      t    = as.numeric(t),
      k    = as.numeric(k),
      c    = as.numeric(c),
      y    = f_y(k, alpha),
      mpk  = f_mpk(k, alpha),
      i    = y - c,
      c_dot_over_c = (mpk - rho - delta) / theta
    )
  
  # Nullclines
  c_kdot0 <- function(k) f_y(k, alpha) - (n + g + delta) * k
  k_cdot0 <- k_star
  
  # Plots
  p_k <- ggplot2::ggplot(out, ggplot2::aes(t, k)) +
    ggplot2::geom_line(color = "#2C7FB8", linewidth = 1) +
    ggplot2::labs(title = "Capital per effective worker", x = "Time", y = "k(t)") +
    ggplot2::theme_minimal()
  
  p_c <- ggplot2::ggplot(out, ggplot2::aes(t, c)) +
    ggplot2::geom_line(color = "#D95F0E", linewidth = 1) +
    ggplot2::labs(title = "Consumption per effective worker", x = "Time", y = "c(t)") +
    ggplot2::theme_minimal()
  
  p_phase <- ggplot2::ggplot(out, ggplot2::aes(k, c, color = t)) +
    ggplot2::geom_path(linewidth = 1) +
    ggplot2::geom_function(fun = c_kdot0, color = "#2C7FB8", linewidth = 1) +
    ggplot2::geom_vline(xintercept = k_cdot0, color = "#D95F0E", linetype = "dashed") +
    ggplot2::scale_color_viridis_c() +
    ggplot2::labs(
      title   = "Phase diagram (per effective worker)",
      x = "k", y = "c", color = "t",
      subtitle = expression(paste(dot(k)==0~"(blue) and "~dot(c)==0~"(red dashed)"))
    ) +
    ggplot2::theme_minimal()
  
  list(
    params = list(alpha=alpha, delta=delta, n=n, g=g, rho=rho, theta=theta),
    steady_state = list(k_star = k_star, c_star = c_star),
    path = out,
    plots = list(k = p_k, c = p_c, phase = p_phase)
  )
}

res <- run_ramsey()
res$steady_state
res$plots$k
res$plots$c
res$plots$phase