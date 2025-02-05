plot_radial_heatmap <- function(uncertainty, depth, angle_step = 15) {
  # 1. Get the Stern–Brocot tree data
  sb_tree <- stern_brocot_tree(depth)

  # 2. Remove any rows with a zero denominator
  valid_rows <- sb_tree$den != 0
  sb_tree <- sb_tree[valid_rows, ]

  # 3. Compute the approximation and filter the rows
  sb_tree$approximation <- sb_tree$num / sb_tree$den
  r <- sb_tree[sb_tree$approximation >= 0 & sb_tree$approximation <= uncertainty, ]

  # 4. Compute the redundancy
  r$redundancy <- 1 / (abs(r$num) + abs(r$den))

  # 5. Create a sequence of angles (in radians)
  angles <- seq(0, 2 * pi, by = angle_step * pi / 180)

  # 6. Expand the data so that each row in r is repeated for every angle.
  #    We use base R’s merge() to create a Cartesian join.
  r_expanded <- merge(r, data.frame(theta = angles), all = TRUE)

  # 7. Compute the Cartesian coordinates for each (radius, theta) pair.
  r_expanded$x <- r_expanded$approximation * cos(r_expanded$theta)
  r_expanded$y <- r_expanded$approximation * sin(r_expanded$theta)

  # 8. Create the heat map plot using ggplot2.
  #    We use shape = 15 (a filled square) so that the many overlapping points look like a smooth heat map.
  p_heatmap <- ggplot2::ggplot(r_expanded, ggplot2::aes(x = x, y = y, fill = redundancy)) +
    ggplot2::geom_point(shape = 15, size = 2) +
    ggplot2::coord_equal() +
    ggplot2::scale_fill_gradient(low = "lightgray", high = "black") +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Radial Heatmap of Redundancy",
                  x = "x",
                  y = "y",
                  fill = "Redundancy")

  print(p_heatmap)
}


plot_radial_heatmap_polar <- function(uncertainty, depth) {
  # 1. Get the Stern–Brocot tree data
  sb_tree <- stern_brocot_tree(depth)

  # 2. Remove any rows with a zero denominator
  valid_rows <- sb_tree$den != 0
  sb_tree <- sb_tree[valid_rows, ]

  # 3. Compute the approximation and filter the rows
  sb_tree$approximation <- sb_tree$num / sb_tree$den
  r <- sb_tree[sb_tree$approximation >= 0 & sb_tree$approximation <= uncertainty, ]

  # 4. Compute the redundancy
  r$redundancy <- 1 / (abs(r$num) + abs(r$den))

  # 5. Sort the data by approximation so the rings are in order.
  r <- r[order(r$approximation), ]

  # 6. Compute the “thickness” of each ring.
  #    For the last row we use the same thickness as the previous gap.
  if (nrow(r) > 1) {
    r$thickness <- c(diff(r$approximation), tail(diff(r$approximation), n = 1))
  } else {
    r$thickness <- uncertainty / 20  # arbitrary thickness if only one row
  }

  # 7. Create a dummy variable for the angle (all rows get the same value)
  r$dummy <- 1

  # 8. Create the heat map plot using polar coordinates.
  p_heatmap_polar <- ggplot2::ggplot(r, ggplot2::aes(x = dummy, y = approximation, fill = redundancy)) +
    ggplot2::geom_tile(ggplot2::aes(width = 2 * pi, height = thickness)) +
    ggplot2::coord_polar(theta = "x") +
    ggplot2::scale_fill_gradient(low = "lightgray", high = "black") +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Radial Heatmap of Redundancy (Polar Coordinates)",
                  x = "",
                  y = "Approximation",
                  fill = "Redundancy")

  print(p_heatmap_polar)
}

plot_radial_3D_mesh <- function(uncertainty, depth, radial_steps = 100, theta_steps = 100) {
  ## 1. Obtain and process the Stern–Brocot tree data
  sb_tree <- stern_brocot_tree(depth)
  valid_rows <- sb_tree$den != 0
  sb_tree <- sb_tree[valid_rows, ]

  # Compute the approximation (fraction) and filter between 0 and the given uncertainty.
  sb_tree$approximation <- sb_tree$num / sb_tree$den
  r_data <- sb_tree[sb_tree$approximation >= 0 & sb_tree$approximation <= uncertainty, ]

  # Compute redundancy as 1 / (|num| + |den|).
  r_data$redundancy <- 1 / (abs(r_data$num) + abs(r_data$den))

  # Sort by approximation (i.e. radius).
  r_data <- r_data[order(r_data$approximation), ]

  # Ensure that the interpolation domain covers the full range [0, uncertainty].
  # (If 0 is not already in the data, add it.)
  if (!any(r_data$approximation == 0)) {
    # Here we assign the redundancy at 0 to be the maximum redundancy from the data.
    r_data <- rbind(data.frame(num = 0, den = 1,
                               approximation = 0,
                               redundancy = max(r_data$redundancy)),
                    r_data)
    r_data <- r_data[order(r_data$approximation), ]
  }
  # Also ensure that the endpoint "uncertainty" is included.
  if (!any(r_data$approximation == uncertainty)) {
    r_data <- rbind(r_data,
                    data.frame(num = NA, den = NA,
                               approximation = uncertainty,
                               redundancy = tail(r_data$redundancy, 1)))
    r_data <- r_data[order(r_data$approximation), ]
  }

  ## 2. Create an interpolation function for redundancy as a function of radius.
  # Using base R's stats::approxfun (with rule = 2 to allow extrapolation)
  redundancy_fun <- stats::approxfun(r_data$approximation, r_data$redundancy, rule = 2)

  ## 3. Create a grid in polar coordinates and compute Cartesian coordinates.
  # Sample uniformly in radius (from 0 to uncertainty) and in angle (0 to 2*pi).
  radial_seq <- seq(0, uncertainty, length.out = radial_steps)
  theta_seq  <- seq(0, 2 * pi, length.out = theta_steps)

  # Create matrices for x, y, and z using the outer() function.
  # x = r * cos(theta), y = r * sin(theta), and z is given by the interpolated redundancy.
  x_mat <- outer(radial_seq, theta_seq, function(r, theta) r * cos(theta))
  y_mat <- outer(radial_seq, theta_seq, function(r, theta) r * sin(theta))
  z_mat <- outer(radial_seq, theta_seq, function(r, theta) redundancy_fun(r))

  ## 4. Render the 3D surface using plotly.
  # We use plotly::plot_ly with type = "surface" and specify a colorscale
  p_3d <- plotly::plot_ly(x = ~x_mat, y = ~y_mat, z = ~z_mat, type = "surface",
                          colorscale = list(c(0, "lightgray"), c(1, "black")),
                          showscale = TRUE) %>%
    plotly::layout(title = "3D Mesh of Redundancy",
                   scene = list(xaxis = list(title = "X"),
                                yaxis = list(title = "Y"),
                                zaxis = list(title = "Redundancy")))

  return(p_3d)
}
