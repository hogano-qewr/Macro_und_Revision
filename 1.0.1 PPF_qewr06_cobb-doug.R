library(tidyverse)

library(mosaic)

# Define Cobb-Douglas: Q = A * (L^a) * (K^b)
# Example: A=1, alpha=0.3, beta=0.7
f_cobb <- makeFun(A * (L^0.7) * (K^0.3) ~ L & K) 
# Calculate for L=100, K=200
f_cobb(A=1, L = 100, K = 200)


  
plotFun(A * (L^0.7) * (K^0.3) ~ L & K,
        A = 1,
        xlim = range(0, 21),
        ylim = range(0, 100)
        )

plotFun(1 * (L^0.7) * (K^0.3) ~ L & K, 
        L.lim = range(0, 21), 
        K.lim = range(0, 100),
        nlevels = 15
        )

library(lattice)

# Create a grid of points
grid <- expand.grid(L = seq(0, 21, length.out = 50), 
                    K = seq(0, 100, length.out = 50))

# Calculate the Cobb-Douglas value
grid$Q <- 1 * (grid$L^0.7) * (grid$K^0.3)

# Plot the 3D wireframe
wireframe(Q ~ L * K, data = grid, 
          shade = TRUE, 
          screen = list(z = -45, x = -60),
          main = "Cobb-Douglas 3D Surface")

library(plotly)

# 1. Create a grid of points
L_vals <- seq(1, 21, length.out = 50)
K_vals <- seq(1, 100, length.out = 50)
z_matrix <- outer(L_vals, K_vals, function(L, K) 1 * (L^0.7) * (K^0.3))

# 2. Create the interactive 3D plot
plot_ly(x = ~L_vals, y = ~K_vals, z = ~z_matrix)  |>  
  add_surface() |> 
  layout(title = "Interactive Cobb-Douglas",
         scene = list(xaxis = list(title = 'Labour (L)'),
                      yaxis = list(title = 'Capital (K)'),
                      zaxis = list(title = 'Output (Q)')))

# 1. Create the Surface Data
L_seq <- seq(1, 21, length.out = 50)
K_seq <- seq(1, 100, length.out = 50)
z_matrix <- outer(L_seq, K_seq, function(L, K) 1 * (L^0.7) * (K^0.3))

# 2. Create the Expansion Path Data (The "Line" on the surface)

L_path <- seq(1, 21, length.out = 50)
K_path <- expansion_path(L_path)
Z_path <- 1 * (L_path^0.7) * (K_path^0.3) # The height of the path on the surface

# 3. Combine in a 3D Plot
plot_ly() %>%
  add_surface(x = ~L_seq, y = ~K_seq, z = ~z_matrix, opacity = 0.8) |> 
  add_paths(x = ~L_path, y = ~K_path, z = ~Z_path, 
            line = list(color = "red", width = 10), name = "Expansion Path")  |> 
  layout(scene = list(xaxis = list(title = "Labour"),
                      yaxis = list(title = "Capital"),
                      zaxis = list(title = "Output")))




