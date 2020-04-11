#' Sort APE Tree Tips
#'
#' Sorts APE tree tip labels after the tree has been modified (e.g. with 'ladderize') 
#' @param tree ape tree object.
#' @keywords phylogeny
#' @return A list with the tip labels sorted as in the tree.
#' @export
#' @examples
#' sort_tree_tips()
sort_tree_tips <- function(tree) {

  is_tip <- tree$edge[, 2] <= length(tree$tip.label)
  ordered_tips <- tree$edge[is_tip, 2]
  ordered_tips <- tree$tip.label[ordered_tips]
  return(ordered_tips)

}

