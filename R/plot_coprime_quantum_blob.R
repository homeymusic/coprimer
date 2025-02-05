plot_coprime_quantum_blob <- function(x, p,
                                      lower_x_uncertainty, upper_x_uncertainty,
                                      lower_p_uncertainty, upper_p_uncertainty,
                                      depth) {
  # 1. Generate the Stern–Brocot tree at the given depth.
  sb_tree <- stern_brocot_tree(depth)
  sb_tree <- as.data.frame(sb_tree)

  # 2. Remove sentinel rows (e.g. where den == 0).
  valid_rows <- sb_tree$den != 0
  sb_tree <- sb_tree[valid_rows, ]

  # Compute the approximation value for each fraction.
  sb_tree$approximation <- sb_tree$num / sb_tree$den

  # 3. Filter for the x candidates using the uncertainty bounds.
  #    We assume that x is the center for the x–axis, so we keep fractions between
  #    x - lower_x_uncertainty and x + upper_x_uncertainty.
  x_candidates <- sb_tree[sb_tree$approximation >= (x - lower_x_uncertainty) &
                            sb_tree$approximation <= (x + upper_x_uncertainty), ]
  # Assign a redundancy: lower denominator+numerator gives higher redundancy.
  x_candidates$redundancy <- 1 / (abs(x_candidates$num) + abs(x_candidates$den))

  # 4. Similarly, filter for the p candidates.
  p_candidates <- sb_tree[sb_tree$approximation >= (p - lower_p_uncertainty) &
                            sb_tree$approximation <= (p + upper_p_uncertainty), ]
  p_candidates$redundancy <- 1 / (abs(p_candidates$num) + abs(p_candidates$den))

  # 5. Create the Cartesian product of the x and p candidate approximations.
  grid_df <- expand.grid(x = x_candidates$approximation,
                         p = p_candidates$approximation,
                         stringsAsFactors = FALSE)

  # Add redundancy values for x and p by matching on the approximation value.
  grid_df$red_x <- x_candidates$redundancy[match(grid_df$x, x_candidates$approximation)]
  grid_df$red_p <- p_candidates$redundancy[match(grid_df$p, p_candidates$approximation)]

  # Compute the redundancy product for each grid cell.
  grid_df$redundancy_product <- grid_df$red_x * grid_df$red_p

  # 6. Define the ellipse that represents the quantum blob.
  #    Here the ellipse is centered at (x, p). For an ellipse the semi–axes should be
  #    symmetric, so we take the average of the lower and upper uncertainties.
  semi_x <- (lower_x_uncertainty + upper_x_uncertainty) / 2
  semi_p <- (lower_p_uncertainty + upper_p_uncertainty) / 2

  # 7. Filter the grid to keep only points inside the ellipse.
  ellipse_filter <- ((grid_df$x - x)^2 / (semi_x^2)) + ((grid_df$p - p)^2 / (semi_p^2)) <= 1
  grid_df <- grid_df[ellipse_filter, ]

  # 8. Create the bubble plot using ggplot2.
  p_circles <- ggplot2::ggplot(grid_df, ggplot2::aes(x = x, y = p,
                                                     size = redundancy_product,
                                                     fill = redundancy_product)) +
    ggplot2::geom_point(shape = 21, color = "black", stroke = 0.2) +  # bubbles with black borders
    ggplot2::scale_size(range = c(0.5, 8)) +
    ggplot2::scale_fill_gradient(low = "lightgray", high = "black") +
    ggplot2::coord_fixed() +
    ggplot2::labs(title = "Phase Space Quantum Blob",
                  x = "x (approximation)",
                  y = "p (approximation)",
                  size = "Redundancy\nProduct",
                  fill = "Redundancy\nProduct") +
    ggplot2::theme_minimal()

  # 9. Prepare data for the ellipse boundary.
  theta <- seq(0, 2 * pi, length.out = 200)
  ellipse_df <- data.frame(x = x + semi_x * cos(theta),
                           p = p + semi_p * sin(theta))

  # 10. Overlay the ellipse.
  #     Use inherit.aes = FALSE so that the ellipse is not affected by the global mapping.
  p_circles <- p_circles +
    ggplot2::geom_path(data = ellipse_df,
                       ggplot2::aes(x = x, y = p),
                       inherit.aes = FALSE,
                       color = "gray", linewidth = 1)

  # 11. Display the plot.
  print(p_circles)
}
