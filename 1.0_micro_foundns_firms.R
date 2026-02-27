
library(tidyverse)
library(tibble)
library(dplyr)
library(ggplot2)
library(deSolve)
library(patchwork)
library(purrr)
library(gert)


## FIRM FACTOR PRICES


# Example (namespace-safe)
cd_output <- function(K, L, A = 1, alpha = 0.33) {
  K^alpha * (A * L)^(1 - alpha)
}
real_wage <- function(K, L, A = 1, alpha = 0.33) {
  Y <- cd_output(K, L, A, alpha)
  (1 - alpha) * (Y / L)
}
real_rental <- function(K, L, A = 1, alpha = 0.33) {
  Y <- cd_output(K, L, A, alpha)
  alpha * (Y / K)
}

firm_df <- tibble::tibble(
  K = seq(5, 50, by = 0.5),
  L = 20,
  A = 1.0,
  alpha = 0.33
) |>
  dplyr::mutate(
    Y = cd_output(K, L, A, alpha),
    w = real_wage(K, L, A, alpha),
    r = real_rental(K, L, A, alpha),
    K_over_L = K / L
  )

ggplot2::ggplot(firm_df, ggplot2::aes(K_over_L, w)) +
  ggplot2::geom_line(color = "#2C7FB8", linewidth = 1) +
  ggplot2::labs(
    title = "Real wage vs. capital–labour ratio",
    x = expression(K/L),
    y = "w"
  ) +
  ggplot2::theme_minimal()