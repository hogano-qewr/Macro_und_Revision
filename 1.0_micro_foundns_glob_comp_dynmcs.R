
### GLOBAL COMPARATIVE DYNAMICS

## Minimal Ramsey runner (per effective worker)

# Production and derivatives
f_y   <- function(k, alpha) k^alpha
f_mpk <- function(k, alpha) alpha * k^(alpha - 1)

# Steady state
ramsey_k_star <- function(alpha, rho, delta) {
  (alpha / (rho + delta))^(1 / (1 - alpha))
}
ramsey_c_star <- function(k_star, alpha, n, g, delta) {
  f_y(k_star, alpha) - (n + g + delta) * k_star
}

# ODE (per effective worker)
ramsey_ode <- function(t, state, parms) {
  with(as.list(c(state, parms)), {
    y   <- f_y(k, alpha)
    mpk <- f_mpk(k, alpha)
    
    dk <- y - c - (n + g + delta) * k
    dc <- c * ((mpk - rho - delta) / theta)
    
    list(c(dk, dc))
  })
}



# Runner returning tidy path + ggplots
run_ramsey <- function(alpha = 0.33, delta = 0.05, n = 0.01, g = 0.00,
                       rho = 0.02, theta = 2.0,
                       k0 = NULL, c0 = NULL,
                       t_max = 200, dt = 0.1,
                       scenario = "baseline") {
  k_star <- ramsey_k_star(alpha, rho, delta)
  c_star <- ramsey_c_star(k_star, alpha, n, g, delta)
  
  if (is.null(k0)) k0 <- 0.8 * k_star
  if (is.null(c0)) c0 <- 0.8 * c_star
  state0 <- c(k = k0, c = c0)
  times  <- seq(0, t_max, by = dt)
  
  out <- deSolve::ode(y = state0, times = times, func = ramsey_ode,
                      parms = list(alpha = alpha, delta = delta, n = n,
                                   g = g, rho = rho, theta = theta))
  
  out <- tibble::as_tibble(as.data.frame(out)) |>
    dplyr::rename(t = time) |>
    dplyr::mutate(
      t   = as.numeric(t),
      k   = as.numeric(k),
      c   = as.numeric(c),
      y   = f_y(k, alpha),
      mpk = f_mpk(k, alpha),
      i   = y - c,
      c_dot_over_c = (mpk - rho - delta) / theta,
      scenario = scenario,
      k_star = k_star,
      c_star = c_star
    )
  
  out
}

## Global comparative dynamics — parameter sweep
#     Run multiple calibrations (e.g., ρ\rhoρ values) and overlay full paths.
# Define scenarios to compare (change any parameters you care about)
scenarios <- tibble::tibble(
  scenario = c("rho=1%", "rho=2%", "rho=3%", "rho=5%"),
  rho      = c(0.01, 0.02, 0.03, 0.05),
  alpha    = 0.33,
  delta    = 0.05,
  n        = 0.01,
  g        = 0.00,
  theta    = 2.0
)

# Common initial conditions for a fair comparison (off steady state)
# We'll set them relative to the *middle* calibration's steady state:
mid <- run_ramsey(alpha=.33, delta=.05, n=.01, g=0, rho=.02, theta=2)
k0  <- 0.8 * unique(mid$k_star)
c0  <- 0.8 * unique(mid$c_star)

# Run all scenarios and bind results
paths <- purrr::pmap_dfr(scenarios, function(scenario, rho, alpha, delta, n, g, theta) {
  run_ramsey(alpha = alpha, delta = delta, n = n, g = g, rho = rho, theta = theta,
             k0 = k0, c0 = c0, t_max = 200, dt = 0.1, scenario = scenario)
})

# Plots
p_k <- ggplot2::ggplot(paths, ggplot2::aes(t, k, color = scenario)) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::labs(title = "Capital per effective worker: global comparative dynamics",
                x = "Time", y = "k(t)") +
  ggplot2::theme_minimal()

p_c <- ggplot2::ggplot(paths, ggplot2::aes(t, c, color = scenario)) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::labs(title = "Consumption per effective worker: global comparative dynamics",
                x = "Time", y = "c(t)") +
  ggplot2::theme_minimal()

p_phase <- ggplot2::ggplot(paths, ggplot2::aes(k, c, color = scenario)) +
  ggplot2::geom_path(alpha = 0.9, linewidth = 1) +
  ggplot2::labs(title = "Phase diagram paths by scenario", x = "k", y = "c") +
  ggplot2::theme_minimal()

p_k + p_c + p_phase + patchwork::plot_layout(ncol = 1)

# Interpretation: Lower ρ\rhoρ (more patient) → higher k∗k^*k∗ and c∗c^*c∗.
#   You’ll see entire paths shifting, not just endpoints.


## Comparative dynamics with shocks (piecewise runs)

#  implement a helper that runs the model in segments, changing parameters at 
#     specified times, then stitches the paths. Works for permanent and temporary shocks.

# Piecewise runner: run in segments with changing parameters
run_piecewise <- function(segments, k0, c0, dt = 0.1) {
  # segments = list of lists, each with: duration, params (list for run_ramsey args), label
  out_all <- list()
  k_init <- k0
  c_init <- c0
  t_anchor <- 0
  
  for (seg in segments) {
    duration <- seg$duration
    label    <- seg$label
    p        <- seg$params
    
    # Run this segment
    res <- run_ramsey(
      alpha = p$alpha, delta = p$delta, n = p$n, g = p$g,
      rho = p$rho, theta = p$theta,
      k0 = k_init, c0 = c_init,
      t_max = duration, dt = dt, scenario = label
    )
    
    # Shift time to be continuous across segments
    res <- res |> dplyr::mutate(t = t + t_anchor)
    
    # Store and update initial conditions for next segment
    out_all[[length(out_all) + 1]] <- res
    k_init <- dplyr::last(res$k)
    c_init <- dplyr::last(res$c)
    t_anchor <- dplyr::last(res$t)
  }
  
  dplyr::bind_rows(out_all)
}

# 2a) Permanent impatience shock (increase ρ\rhoρ at t=50t=50t=50)

# Baseline params
p_base <- list(alpha=.33, delta=.05, n=.01, g=0.00, rho=.02, theta=2.0)
# Shocked params (permanent)
p_shock <- modifyList(p_base, list(rho = 0.04))

# Find baseline steady state to set common initial conditions
base_ss <- run_ramsey(alpha=p_base$alpha, delta=p_base$delta, n=p_base$n, g=p_base$g,
                      rho=p_base$rho, theta=p_base$theta)
k0 <- 0.9 * unique(base_ss$k_star)
c0 <- 0.9 * unique(base_ss$c_star)

paths_perm <- run_piecewise(
  segments = list(
    list(duration = 50, params = p_base,  label = "Baseline"),
    list(duration = 150, params = p_shock, label = "Permanent rho↑")
  ),
  k0 = k0, c0 = c0, dt = 0.1
)

ggplot2::ggplot(paths_perm, ggplot2::aes(t, c, color = scenario)) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::labs(title = "Permanent impatience shock at t=50",
                subtitle = "rho: 0.02 → 0.04 (permanent)",
                x = "Time", y = "c(t)") +
  ggplot2::theme_minimal()

# 2b) Temporary depreciation shock: δ\deltaδ jumps for [50, 80], then reverts

p_temp_up   <- modifyList(p_base, list(delta = 0.15))  # temporary δ↑
p_revert    <- p_base                                  # revert to baseline

paths_temp <- run_piecewise(
  segments = list(
    list(duration = 50, params = p_base,     label = "Baseline"),
    list(duration = 30, params = p_temp_up,  label = "Temporary δ↑"),
    list(duration = 120, params = p_revert,  label = "Revert")
  ),
  k0 = k0, c0 = c0, dt = 0.1
)

(p1 <- ggplot2::ggplot(paths_temp, ggplot2::aes(t, k, color = scenario)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::labs(title = "Temporary depreciation shock",
                  subtitle = "δ: 0.05 → 0.15 (t∈[50,80]) → 0.05",
                  x = "Time", y = "k(t)") +
    ggplot2::theme_minimal())

(p2 <- ggplot2::ggplot(paths_temp, ggplot2::aes(t, c, color = scenario)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::labs(x = "Time", y = "c(t)") +
    ggplot2::theme_minimal())

p1 / p2