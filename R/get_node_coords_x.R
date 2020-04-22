#' Finds X Coordinates In A Tree Node
#'
#' Finds x coordinates in an 'ape' tree node.
#' @param tree tree in 'ape' format.
#' @param node number or character, tree node to get coordinate from.
#' @details Code taken from https://stackoverflow.com/questions/25624986/draw-on-a-phylogeny-edge-with-r
#' @keywords phylogeny
#' @export
#' @examples
#' get_node_coords_y()
get_node_coords_x <- function(tree, node) {
  if(is.character(node)) {
    node <- which(c(tree$tip.label, tree$node.label) == node)
  }
  pi <- tree$edge[tree$edge[, 2] == node, 1]
  if (length(pi)) {
    ei<-which(tree$edge[,1] == pi & tree$edge[,2] == node)
    tree$edge.length[ei] + Recall(tree, pi)
  } else {
    if(!is.null(tree$root.edge)) {
      tree$root.edge
    } else {
      0
    }
  }
}

