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
#'   moving left ("L") or right ("R"), and highlights the path in black.
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

  # Generate the tree; assumed to have columns: num, den, depth, label, left_parent, right_parent.
  tree <- stern_brocot_tree(depth)

  # Build a simple label for each node: "num/den"
  tree$label <- paste0(tree$num, "/", tree$den)

  # Assign plotting coordinates: x = row order; y = -depth
  tree$x <- seq_len(nrow(tree))
  tree$y <- -tree$depth

  # Build the standard edge data frame (using your existing parent-child logic)
  edges <- data.frame(x = numeric(0), y = numeric(0),
                      xend = numeric(0), yend = numeric(0),
                      stringsAsFactors = FALSE)

  n <- nrow(tree)
  for(i in seq_len(n)) {
    child_depth <- tree$depth[i]
    # If a left_parent is specified and its depth is exactly one less, add an edge.
    if(tree$left_parent[i] != "" ) {
      parent_index <- match(tree$left_parent[i], tree$label)
      if(!is.na(parent_index) && tree$depth[parent_index] == (child_depth - 1)) {
        edges <- rbind(edges, data.frame(
          x = tree$x[parent_index],
          y = tree$y[parent_index],
          xend = tree$x[i],
          yend = tree$y[i],
          stringsAsFactors = FALSE
        ))
      }
    }
    # Similarly for the right parent.
    if(tree$right_parent[i] != "" ) {
      parent_index <- match(tree$right_parent[i], tree$label)
      if(!is.na(parent_index) && tree$depth[parent_index] == (child_depth - 1)) {
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

  # Start building the ggplot object with the standard edges and nodes.
  p <- ggplot2::ggplot() +
    ggplot2::geom_segment(data = edges, ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend),
                          color = "gray", size = 0.8) +
    ggplot2::geom_point(data = tree, ggplot2::aes(x = .data$x, y = .data$y), color = "gray", size = 3) +
    ggplot2::labs(title = "", x = "", y = "") +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank())

  # If a path is provided, simulate the path and highlight nodes/edges in red.
  if(nchar(path_str) > 0){
    # Break the path string into individual characters.
    path_letters <- strsplit(path_str, "")[[1]]

    # Identify the starting node: the 0/1 node at depth 0.
    root_index <- which(tree$depth == 0 & tree$label == "0/1")
    if(length(root_index) == 0) stop("Root node (0/1 at depth 0) not found!")
    path_nodes <- root_index  # initialize vector of indices along the path
    current_index <- root_index
    # For each letter in the path, move to the corresponding child.
    for(letter in path_letters){
      current_label <- tree$label[current_index]
      current_depth <- tree$depth[current_index]

      if(letter == "R"){
        # Look for the child whose left_parent equals the current node's label
        # and which has depth = current_depth + 1.
        child_index <- which(tree$left_parent == current_label & tree$depth == current_depth + 1)
      } else if(letter == "L"){
        child_index <- which(tree$right_parent == current_label & tree$depth == current_depth + 1)
      } else {
        stop("Invalid character in path. Use only 'L' and 'R'.")
      }

      if(length(child_index) == 0){
        stop(paste("No child found for", letter, "from node", current_label))
      }
      # Assume uniqueness.
      current_index <- child_index[1]
      path_nodes <- c(path_nodes, current_index)
    }

    # Build edges for the highlighted path.
    highlight_edges <- data.frame(
      x = tree$x[path_nodes[-length(path_nodes)]],
      y = tree$y[path_nodes[-length(path_nodes)]],
      xend = tree$x[path_nodes[-1]],
      yend = tree$y[path_nodes[-1]]
    )

    # Overlay the highlighted edges and nodes in red.
    p <- p +
      ggplot2::geom_segment(data = highlight_edges,
                            ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend),
                            color = "black", size = 1.2) +
      ggplot2::geom_point(data = tree[path_nodes, ],
                          ggplot2::aes(x = .data$x, y = .data$y),
                          color = "black", size = 4)
  }

  p <- p +
    ggplot2::geom_text(data = tree, ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
                       vjust = -0.5, size = 3)

  print(p)
  invisible(p)
}
