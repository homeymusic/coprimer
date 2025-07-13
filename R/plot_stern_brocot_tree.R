#' Plot the Stern-Brocot Tree
#'
#' This function generates and plots the Stern-Brocot tree up to a specified depth.
#' Optionally, it can highlight a specific path within the tree and show the traversal depth.
#'
#' @param depth Integer (default: 4). The depth of the Stern-Brocot tree to generate.
#' @param path_str Character (default: ""). A string of "L" (left) and "R" (right)
#'   indicating a path from the root through the tree.
#' @param dark_mode Logical (default: FALSE). If TRUE, uses a black background with white foreground and darker gray edges.
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
#'   moving left ("L") or right ("R"), highlights that path in the foreground color, and shows a depth strip.
#'
#' @examples
#' plot_stern_brocot_tree()
#' plot_stern_brocot_tree(3)
#' plot_stern_brocot_tree(4, "RLR", dark_mode = TRUE)
#'
#' @importFrom rlang .data
#' @import ggplot2
#' @export
plot_stern_brocot_tree <- function(depth = 4, path_str = "", dark_mode = FALSE) {
  if (nchar(path_str) > 0 && depth != nchar(path_str)) {
    stop("Depth and the length of the path must be the same.")
  }

  # Generate the tree
  tree <- stern_brocot_tree(depth)
  tree$label <- paste0(tree$num, "/", tree$den)
  tree$x <- seq_len(nrow(tree)); tree$y <- -tree$depth

  # Build edges
  edges <- do.call(rbind, lapply(seq_len(nrow(tree)), function(i) {
    d <- tree$depth[i]; res <- list()
    if (tree$left_parent[i] != "") {
      pi <- match(tree$left_parent[i], tree$label)
      if (!is.na(pi) && tree$depth[pi] == d - 1) res[[length(res)+1]] <- data.frame(
        x = tree$x[pi], y = tree$y[pi], xend = tree$x[i], yend = tree$y[i]
      )
    }
    if (tree$right_parent[i] != "") {
      pi <- match(tree$right_parent[i], tree$label)
      if (!is.na(pi) && tree$depth[pi] == d - 1) res[[length(res)+1]] <- data.frame(
        x = tree$x[pi], y = tree$y[pi], xend = tree$x[i], yend = tree$y[i]
      )
    }
    if (length(res)) do.call(rbind, res) else NULL
  }))

  # Highlight path
  path_nodes <- integer(0); highlight_edges <- NULL
  if (nchar(path_str) > 0) {
    steps <- strsplit(path_str, "")[[1]]
    root <- which(tree$depth == 0 & tree$label == "0/1")
    if (!length(root)) stop("Root node (0/1 at depth 0) not found!")
    path_nodes <- root; current <- root
    for (ltr in steps) {
      child <- switch(ltr,
                      R = which(tree$left_parent  == tree$label[current] & tree$depth == tree$depth[current]+1),
                      L = which(tree$right_parent == tree$label[current] & tree$depth == tree$depth[current]+1),
                      stop("Invalid character in path. Use only 'L' and 'R'.")
      )
      if (!length(child)) stop("No child found for path step.")
      current <- child[1]; path_nodes <- c(path_nodes, current)
    }
    highlight_edges <- data.frame(
      x    = tree$x[path_nodes[-length(path_nodes)]],
      y    = tree$y[path_nodes[-length(path_nodes)]],
      xend = tree$x[path_nodes[-1]], yend = tree$y[path_nodes[-1]]
    )
  }

  # Color settings based on mode
  if (!dark_mode) {
    bg_col  <- "white"; fg_col  <- "black"; seg_col <- "gray"
    label_fill <- "white"; label_color <- "black"
  } else {
    bg_col  <- "black"; fg_col  <- "white"; seg_col <- "gray20"
    label_fill <- "black"; label_color <- "white"
  }

  # Determine point/text color for non-highlighted tree
  tree_color <- if (length(path_str) > 0) "darkgray" else fg_col
  base_tree  <- if (length(path_nodes) > 0) tree[-path_nodes, ] else tree

  # Build plot with variable colors
  p <- ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = edges,
      ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend),
      color = seg_col, linewidth = 0.8
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
      panel.background = ggplot2::element_rect(fill = bg_col, color = NA),
      plot.background  = ggplot2::element_rect(fill = bg_col, color = NA),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text        = ggplot2::element_blank(),
      axis.ticks       = ggplot2::element_blank()
    )

  # Depth strip
  if (length(path_nodes) > 0) {
    rx   <- range(tree$x); gap <- diff(rx) * 0.02
    x0   <- max(tree$x) + gap; lo <- gap * 0.5
    df   <- data.frame(step = seq_along(path_nodes), x0 = x0, y = tree$y[path_nodes])
    segs <- data.frame(x = x0, xend = x0,
                       y = head(df$y, -1), yend = tail(df$y, -1),
                       step = head(df$step, -1))
    p <- p +
      ggplot2::geom_point(data = df, ggplot2::aes(x = x0, y = y), color = fg_col, size = 2) +
      ggplot2::geom_segment(data = segs, ggplot2::aes(x = x, xend = xend, y = y, yend = yend), color = fg_col, linewidth = 0.8) +
      ggplot2::geom_text(data = segs, ggplot2::aes(x = xend + lo, y = (y + yend) / 2, label = step), color = fg_col, hjust = 0, size = 3)
  }

  # Highlight path edges and nodes
  if (length(path_nodes) > 0) {
    p <- p +
      ggplot2::geom_segment(data = highlight_edges, ggplot2::aes(x = x, y = y, xend = xend, yend = yend), color = fg_col, linewidth = 1.2) +
      ggplot2::geom_point(data = tree[path_nodes, ], ggplot2::aes(x = x, y = y), color = fg_col, size = 4) +
      ggplot2::geom_label(data = tree[path_nodes, ], ggplot2::aes(x = x, y = y + 0.1, label = label),
                          vjust = -0.5, fill = label_fill, color = label_color,
                          label.size = 0.5, label.padding = ggplot2::unit(0.15, "lines"), size = 3
      )
  }

  # Allow labels to extend
  p <- p + ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme(plot.margin = ggplot2::margin(5, 40, 5, 5))

  print(p)
  invisible(p)
}
