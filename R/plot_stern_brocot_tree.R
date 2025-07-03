#' Plot the Stern-Brocot Tree
#'
#' This function generates and plots the Stern-Brocot tree up to a specified depth.
#' Optionally, it can highlight a specific path within the tree.
#'
#' @param depth Integer (default: 4). The depth of the Stern-Brocot tree to generate.
#' @param path_str Character (default: ""). A string of "L" (left) and "R" (right)
#'   indicating a path from the root through the tree.
#'
#' @return A `ggplot` object visualizing the Stern-Brocot tree.
#'   The function prints the plot and returns it invisibly.
#'
#' @details
#' - The function constructs the Stern-Brocot tree up to the given depth using
#'   the `stern_brocot_tree()` function, which is expected to generate a data frame
#'   with columns `num`, `den`, `depth`, `label`, `left_parent`, and `right_parent`.
#' - Nodes are labeled using their corresponding fraction representations (`num/den`).
#' - The tree is plotted using `ggplot2`, with edges drawn between parent and child nodes.
#' - If `path_str` is provided, it follows the path from the root node ("0/1"),
#'   moving left ("L") or right ("R"), and highlights that path in black.
#'
#' @examples
#' # Plot the Stern-Brocot tree with default depth
#' plot_stern_brocot_tree()
#'
#' # Plot the tree with depth 3
#' plot_stern_brocot_tree(3)
#'
#' # Highlight a specific path ("RLR") at depth 3
#' plot_stern_brocot_tree(3, "RLR")
#'
#' @importFrom rlang .data
#' @import ggplot2
#' @export
plot_stern_brocot_tree <- function(depth = 4, path_str = "") {
  if (nchar(path_str) > 0 && depth != nchar(path_str)) {
    stop("Depth and the length of the path must be the same.")
  }

  # Generate the tree with columns: num, den, depth, label, left_parent, right_parent
  tree <- stern_brocot_tree(depth)
  tree$label <- paste0(tree$num, "/", tree$den)
  tree$x <- seq_len(nrow(tree))
  tree$y <- -tree$depth

  # Build edge list
  edges <- data.frame(x = numeric(0), y = numeric(0),
                      xend = numeric(0), yend = numeric(0),
                      stringsAsFactors = FALSE)
  for (i in seq_len(nrow(tree))) {
    d <- tree$depth[i]
    if (tree$left_parent[i] != "") {
      pi <- match(tree$left_parent[i], tree$label)
      if (!is.na(pi) && tree$depth[pi] == d - 1) {
        edges <- rbind(edges, data.frame(
          x = tree$x[pi], y = tree$y[pi],
          xend = tree$x[i], yend = tree$y[i],
          stringsAsFactors = FALSE
        ))
      }
    }
    if (tree$right_parent[i] != "") {
      pi <- match(tree$right_parent[i], tree$label)
      if (!is.na(pi) && tree$depth[pi] == d - 1) {
        edges <- rbind(edges, data.frame(
          x = tree$x[pi], y = tree$y[pi],
          xend = tree$x[i], yend = tree$y[i],
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  # Prepare for optional path highlighting
  path_nodes <- integer(0)
  highlight_edges <- NULL
  if (nchar(path_str) > 0) {
    letters <- strsplit(path_str, "")[[1]]
    root <- which(tree$depth == 0 & tree$label == "0/1")
    if (length(root) == 0) stop("Root node (0/1 at depth 0) not found!")
    path_nodes <- root
    current <- root
    for (ltr in letters) {
      if (ltr == "R") {
        child <- which(tree$left_parent == tree$label[current] & tree$depth == tree$depth[current] + 1)
      } else if (ltr == "L") {
        child <- which(tree$right_parent == tree$label[current] & tree$depth == tree$depth[current] + 1)
      } else {
        stop("Invalid character in path. Use only 'L' and 'R'.")
      }
      if (length(child) == 0) stop(paste("No child found for", ltr, "from node", tree$label[current]))
      current <- child[1]
      path_nodes <- c(path_nodes, current)
    }
    highlight_edges <- data.frame(
      x    = tree$x[path_nodes[-length(path_nodes)]],
      y    = tree$y[path_nodes[-length(path_nodes)]],
      xend = tree$x[path_nodes[-1]],
      yend = tree$y[path_nodes[-1]]
    )
  }

  tree_color <- if (length(path_str) > 0) "darkgray" else "black"
  base_tree  <- if (length(path_nodes) > 0) tree[-path_nodes, ] else tree

  # Build the plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = edges,
      ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend),
      color = "gray", linewidth = 0.8
    ) +
    ggplot2::geom_point(
      data = tree,
      ggplot2::aes(x = .data$x, y = .data$y),
      color = tree_color, size = 3
    ) +
    ggplot2::geom_text(
      data = base_tree,
      ggplot2::aes(x = .data$x, y = .data$y + 0.1, label = .data$label),
      vjust = -0.5, color = tree_color, size = 3
    ) +
    ggplot2::labs(title = "", x = "", y = "") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )

  # Overlay highlight if requested
  if (length(path_nodes) > 0) {
    p <- p +
      ggplot2::geom_segment(
        data = highlight_edges,
        ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend),
        color = "black", linewidth = 1.2
      ) +
      ggplot2::geom_point(
        data = tree[path_nodes, ],
        ggplot2::aes(x = .data$x, y = .data$y),
        color = "black", size = 4
      ) +
      ggplot2::geom_label(
        data = tree[path_nodes, ],
        ggplot2::aes(x = .data$x, y = .data$y + 0.1, label = .data$label),
        vjust = -0.5,
        fill  = "white",
        color = "black",
        label.size    = 0.5,
        label.padding = ggplot2::unit(0.15, "lines"),
        size = 3
      )
  }

  print(p)
  invisible(p)
}
