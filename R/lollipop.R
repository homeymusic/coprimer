plot_coprime_quantum_blob_3D_lollipop <- function(x, p,
                                                  lower_x_uncertainty, upper_x_uncertainty,
                                                  lower_p_uncertainty, upper_p_uncertainty,
                                                  depth) {
  ## 1. Generate the Stern–Brocot tree at the given depth.
  sb_tree <- stern_brocot_tree(depth)
  sb_tree <- as.data.frame(sb_tree)

  ## 2. Remove sentinel rows (where den == 0).
  valid_rows <- sb_tree$den != 0
  sb_tree <- sb_tree[valid_rows, ]

  ## 3. Compute the approximation value for each fraction.
  sb_tree$approximation <- sb_tree$num / sb_tree$den

  ## 4. Filter for the x candidates using the uncertainty bounds.
  x_lower_bound <- x - lower_x_uncertainty
  x_upper_bound <- x + upper_x_uncertainty
  x_candidates <- sb_tree[sb_tree$approximation >= x_lower_bound &
                            sb_tree$approximation <= x_upper_bound, ]
  # Assign a redundancy (lower |num|+|den| yields higher redundancy).
  x_candidates$redundancy <- 1 / (abs(x_candidates$num) + abs(x_candidates$den))

  ## 5. Similarly, filter for the p candidates.
  p_lower_bound <- p - lower_p_uncertainty
  p_upper_bound <- p + upper_p_uncertainty
  p_candidates <- sb_tree[sb_tree$approximation >= p_lower_bound &
                            sb_tree$approximation <= p_upper_bound, ]
  p_candidates$redundancy <- 1 / (abs(p_candidates$num) + abs(p_candidates$den))

  ## 6. Create the Cartesian product of the x and p candidate approximations.
  grid_df <- expand.grid(x = x_candidates$approximation,
                         p = p_candidates$approximation,
                         stringsAsFactors = FALSE)

  ## 7. Add redundancy values for x and p by matching on the approximation.
  grid_df$red_x <- x_candidates$redundancy[match(grid_df$x, x_candidates$approximation)]
  grid_df$red_p <- p_candidates$redundancy[match(grid_df$p, p_candidates$approximation)]

  ## 8. Compute the redundancy product for each grid cell.
  grid_df$redundancy_product <- grid_df$red_x * grid_df$red_p

  ## 9. Define the ellipse that represents the quantum blob.
  #     The ellipse is centered at (x, p) and the semi–axes are taken as the averages
  #     of the lower and upper uncertainties.
  semi_x <- (lower_x_uncertainty + upper_x_uncertainty) / 2
  semi_p <- (lower_p_uncertainty + upper_p_uncertainty) / 2

  ## 10. Filter the grid to keep only points inside the ellipse.
  ellipse_filter <- (((grid_df$x - x)^2) / (semi_x^2)) +
    (((grid_df$p - p)^2) / (semi_p^2)) <= 1
  grid_df <- grid_df[ellipse_filter, ]

  ## 11. Prepare data for the 3D lollipop plot.
  # For each point (x, p, redundancy_product), we will draw a vertical line from z = 0
  # to z = redundancy_product and place a marker at the top.
  n <- nrow(grid_df)

  # Preallocate vectors to hold the coordinates for the lollipop stems.
  # We create a triplet for each lollipop: start (z = 0), end (z = redundancy_product), and NA (to break the line).
  line_x <- numeric(3 * n)
  line_y <- numeric(3 * n)
  line_z <- numeric(3 * n)

  for (i in 1:n) {
    idx <- (i - 1) * 3 + 1
    # Start point at z = 0.
    line_x[idx]     <- grid_df$x[i]
    line_y[idx]     <- grid_df$p[i]
    line_z[idx]     <- 0
    # End point at z = redundancy_product.
    line_x[idx + 1] <- grid_df$x[i]
    line_y[idx + 1] <- grid_df$p[i]
    line_z[idx + 1] <- grid_df$redundancy_product[i]
    # Insert NA to break the line segment.
    line_x[idx + 2] <- NA
    line_y[idx + 2] <- NA
    line_z[idx + 2] <- NA
  }

  ## 12. Create the 3D lollipop plot using plotly.
  # The stems are drawn with mode "lines" and the lollipop heads with mode "markers".
  p_3d <- plotly::plot_ly() %>%
    plotly::add_trace(x = line_x, y = line_y, z = line_z,
                      type = "scatter3d", mode = "lines",
                      line = list(color = "gray", width = 2),
                      name = "Stem") %>%
    plotly::add_trace(x = grid_df$x, y = grid_df$p, z = grid_df$redundancy_product,
                      type = "scatter3d", mode = "markers",
                      marker = list(color = "black", size = 4),
                      name = "Redundancy") %>%
    plotly::layout(title = "3D Lollipop Quantum Blob",
                   scene = list(xaxis = list(title = "x (approximation)"),
                                yaxis = list(title = "p (approximation)"),
                                zaxis = list(title = "Redundancy Product")))

  return(p_3d)
}

# Example usage:
# In an interactive session, the following call will return a plotly widget.
# plot_coprime_quantum_blob_3D_lollipop(x = 0.5, p = 0.5,
#                                       lower_x_uncertainty = 0.1, upper_x_uncertainty = 0.1,
#                                       lower_p_uncertainty = 0.1, upper_p_uncertainty = 0.1,
#                                       depth = 5)
