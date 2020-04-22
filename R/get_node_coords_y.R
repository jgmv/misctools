#' Finds Y Coordinates In A Tree Node
#'
#' Finds y coordinates in an 'ape' tree node.
#' @param tree tree in 'ape' format.
#' @param node number or character, tree node to get coordinate from.
#' @details Code taken from https://stackoverflow.com/questions/25624986/draw-on-a-phylogeny-edge-with-r
#' @keywords phylogeny
#' @export
#' @examples
#' get_node_coords_y()
get_node_coords_y <- function(tree, node) {
  if(is.character(node)) {
    node <- which(c(tree$tip.label, tree$node.label) == node)
  }
  ci <- tree$edge[tree$edge[, 1] == node, 2]
  if (length(ci) == 2) {
    mean(c(Recall(tree, ci[1]), Recall(tree, ci[2])))
  } else if (length(ci) == 0) {
    Ntip <- length(tree$tip.label)
    which(tree$edge[tree$edge[, 2] <= Ntip, 2] == node)
  } else {
    stop(paste("error", length(ci)))
  }
}
