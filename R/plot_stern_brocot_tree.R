plot_stern_brocot_tree <- function(depth = 4) {
  # Generate the tree; it should include columns: label, left_parent, right_parent, depth, etc.
  tree <- stern_brocot_tree(depth)

  # For plotting, assign x and y coordinates.
  tree$x <- seq_len(nrow(tree))
  tree$y <- -tree$depth

  # Build the edges data frame
  edges <- data.frame(x = numeric(0), y = numeric(0),
                      xend = numeric(0), yend = numeric(0),
                      stringsAsFactors = FALSE)

  n <- nrow(tree)
  for (i in seq_len(n)) {
    child_depth <- tree$depth[i]

    # If left_parent is specified, try to find its index in tree$label
    if (tree$left_parent[i] != "") {
      parent_index <- match(tree$left_parent[i], tree$label)
      # Only add edge if parent's depth is exactly one less than child's depth
      if (!is.na(parent_index) && tree$depth[parent_index] == (child_depth - 1)) {
        edges <- rbind(edges, data.frame(
          x = tree$x[parent_index],
          y = tree$y[parent_index],
          xend = tree$x[i],
          yend = tree$y[i],
          stringsAsFactors = FALSE
        ))
      }
    }

    # Similarly for right_parent
    if (tree$right_parent[i] != "") {
      parent_index <- match(tree$right_parent[i], tree$label)
      if (!is.na(parent_index) && tree$depth[parent_index] == (child_depth - 1)) {
        edges <- rbind(edges, data.frame(
          x = tree$x[parent_index],
          y = tree$y[parent_index],
          xend = tree$x[i],
          yend = tree$y[i],
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  ggplot2::ggplot() +
    ggplot2::geom_segment(data = edges, ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
                          color = "gray", size = 0.8) +
    ggplot2::geom_point(data = tree, ggplot2::aes(x = x, y = y), color = "black", size = 3) +
    ggplot2::geom_text(data = tree, ggplot2::aes(x = x, y = y, label = label),
                       vjust = -0.5, size = 3) +
    ggplot2::labs(title = "", x = "", y = "") +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank())
}
