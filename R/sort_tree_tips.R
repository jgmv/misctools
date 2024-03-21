#' Sort APE Tree Tips
#'
#' Sorts APE tree tip labels after the tree has been modified (e.g. with 'ladderize') 
#' @param tree ape tree object.
#' @param show_names if T, shows tip labels, otherwise shows order.
#' @keywords phylogeny
#' @return A list with the tip labels sorted as in the tree.
#' @export
#' @examples
#' sort_tree_tips(ape::rtree(10))
sort_tree_tips <- function(tree, show_names = T) {

  is_tip <- tree$edge[, 2] <= length(tree$tip.label)
  ordered_tips <- tree$edge[is_tip, 2]
  if(show_names) ordered_tips <- tree$tip.label[ordered_tips]
  return(ordered_tips)

}
