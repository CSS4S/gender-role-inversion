library(latex2exp)

library(grid)
library(scales)
library(tidyverse)



# Identify neighboring mismatches as boundary points
get_boundary <- function(grid, res) {
  mat <- matrix(grid$dest, nrow = res, ncol = res, byrow = TRUE)
  boundary <- matrix(FALSE, nrow = res, ncol = res)
  for (i in 2:(res - 1)) {
    for (j in 2:(res - 1)) {
      center <- mat[i, j]
      neighbors <- c(mat[i-1, j], mat[i+1, j], mat[i, j-1], mat[i, j+1])
      if (any(neighbors != center & neighbors != "middle" & center != "middle")) {
        boundary[i, j] <- TRUE
      }
    }
  }
  grid$boundary <- as.vector(boundary)
  filter(grid, boundary)
}


# Main function to compute vector field + dynamics
replicator_dynamics_plot <- function(delta = 0.5, resolution = 18, steps = 5000, step_size = 0.05, tol = 0.08, scale_low = 0.025, scale_high = 0.06, arrow_alpha = 0.6) {
  # Create a coarser grid
  w_vals <- seq(0.01, 0.99, length.out = resolution)
  m_vals <- seq(0.01, 0.99, length.out = resolution)
  grid <- expand.grid(w = w_vals, m = m_vals)
  
  # Fitness means
  fW_bar <- with(grid, grid$w * grid$m + (1 - grid$w) * (1 - grid$m))
  fM_bar <- with(grid, grid$m * grid$w * (1 - delta) + (1 - grid$m) * (1 - grid$w))
  
  # Replicator dynamics
  grid <- grid %>%
    mutate(
      dw_raw = w * (1 - w) * (m - fW_bar),
      dm_raw = m * (1 - m) * (w * (1 - delta) - fM_bar),
      norm = sqrt(dw_raw^2 + dm_raw^2),
      dw = dw_raw / norm,
      dm = dm_raw / norm
    )
  
  # Saddle point: interior minimum of vector norm
  saddle <- grid %>%
    filter(m > 0.1, m < 0.9, w > 0.1, w < 0.9) %>%
    slice_min(order_by = norm, n = 1) %>%
    select(w, m) %>%
    unlist() %>%
    as.numeric()
  
  # Classify destination
  classify_end <- function(w, m) {
    for (i in 1:steps) {
      fW <- w * m + (1 - w) * (1 - m)
      fM <- m * w * (1 - delta) + (1 - m) * (1 - w)
      dw <- w * (1 - w) * (m - fW)
      dm <- m * (1 - m) * (w * (1 - delta) - fM)
      w <- w + dw * step_size
      m <- m + dm * step_size
      if (w < tol && m < tol) return("zero")
      if (w > 1 - tol && m > 1 - tol) return("one")
    }
    return("middle")
  }
  
  grid$dest <- pmap_chr(grid[, c("w", "m")], classify_end)
  grid$col <- case_when(
    grid$dest == "zero" ~ "dodgerblue",
    grid$dest == "one" ~ "hotpink",
    TRUE ~ "gray70"
  )
  
  
  
  # Trace separatrix forward and backward
  sep_forward <- trace_trajectory(saddle[1], saddle[2], delta, direction = 1)
  sep_backward <- trace_trajectory(saddle[1], saddle[2], delta, direction = -1)
  sep <- bind_rows(sep_backward, sep_forward)
  
  grid <- grid %>%
    mutate(
      arrow_scale = rescale(norm, to = c(scale_low, scale_high))  # controls visible arrow length
    )
  
  boundary_pts <- get_boundary(grid, resolution)
  
  # Plot
  ggplot(grid, aes(x = w, y = m)) +
    geom_segment(aes(xend = w + dw * arrow_scale, yend = m + dm * arrow_scale, color = col),
                 linewidth = 0.55, arrow = arrow(length = unit(0.135, "cm")), alpha = arrow_alpha) +
    scale_color_identity() +
    theme_classic(base_size = base_size) +
    coord_fixed() +
    labs(
      x = TeX("Proportion of Women Inverters ($w_0$)"),
      y = TeX("Initial proportion of Men Inverters ($m_0$)"),
      title = TeX(paste("Deviance penalty, $\\delta =$", delta)) 
    )
}
  