#' Plot APE Tree With Clade Triangles
#'
#' Plots an APE tree tip in which selected clades are collapsed into triangles 
#' @param tree ape tree object.
#' @param groups a character vector with the node labels to collapse
#' @param n_min if groups is NULL, all terminal clades containing a minimum of tips are collapsed
#' @param yvar value between 1 and 0 to modify the vertical span of triangles
#' @param tree_col color for tree edges and triangles
#' @param ... further arguments for tree (see plot.phylo)
#' @keywords phylogeny
#' @return Plots a phylogenetic tree.
#' @export
#' @examples
#' plot_group_tree()
plot_group_tree <- function(tree, groups = NULL, n_min = 5, yvar = 1,
  tree_col = 1, ...) {

  if(is.null(groups)) {
    
    sel_nodes <- c()
    for(i in tree$node.label) {
      sel_nodes <- c(sel_nodes, length(phylobase::descendants(as(tree,
        "phylo4"), i, type = "all")) >= n_min)
    }
    groups <- tree$node.label[sel_nodes]
    end_clade <- c()
    for(i in groups) {
      end_clade <- c(end_clade, all(grepl("^OTU",
      names(phylobase::descendants(as(tree, "phylo4"), i,
      type = "all")))))
    }
    groups <- groups[end_clade]
  }

  clades <- vector(mode = "list", length = length(groups))
  names(clades) <- groups
  for(i in groups) {
    clades[[i]] <- phylobase::descendants(as(tree, "phylo4"), i, type = "ALL")
  }
  edge_width <- rep(1, length(tree$edge.length))
  for(i in names(clades)) {
    edge_width[which(tree$edge[, 1] == range(clades[[i]])[2])] <- 0
  }
  plot(tree, show.tip.label = F, edge.width = edge_width, edge.color = tree_col,
    ...)
  for(i in names(clades)) {
    x_coords <- c(misctools::get_node_coords_x(tree, clades[[i]][1]),
      misctools::get_node_coords_x(tree, clades[[i]][2]),
      misctools::get_node_coords_x(tree, clades[[i]][length(clades[[i]])]))
    y_coords <- c(mean(c(misctools::get_node_coords_y(tree, clades[[i]][2]),
      misctools::get_node_coords_y(tree, clades[[i]][length(clades[[i]])]))),
      misctools::get_node_coords_y(tree, clades[[i]][2]),
      misctools::get_node_coords_y(tree, clades[[i]][length(clades[[i]])]))
    y_coords[2] <- y_coords[2] * (1 + 1 - yvar)
    y_coords[3] <- y_coords[3] * yvar
    polygon(x_coords, y_coords, col = tree_col, border = tree_col)
  }
}


