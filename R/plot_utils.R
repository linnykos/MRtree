#' Plot MRtree results as a dendrogram. If reference labels are provided, a pie chart is
#' shown at each tree node, detailing the label proprotions.
#'
#' @param labelmat clustering results saved in a label matrix n-by-number of partitions
#' @param prefix string indicating columns containing clustering information
#' @param ref.labels reference labels to be shown at each tree node
#' @param plot.ref boolean wheather to color the tree node by the major type
#' @param ... other parameter to pass to clustree function
#' according to reference labels
#'
#' @importFrom clustree clustree
#' @export
#' @examples
#' plot_clustree(labelmat = clust_example$clusterings, ref.labels = clust_example$ref.labels)
plot_clustree <- function(labelmat, prefix = NULL, ref.labels = NULL, plot.ref = TRUE,
                          ...) {
    require("ggraph")  # needed for guide_edge_colourbar to work (bug)
    if (is.null(prefix) | is.null(colnames(labelmat))) {
        colnames(labelmat) = paste0("layer", 1:ncol(labelmat))
        prefix = "layer"
    }

    if (length(unique(colnames(labelmat)))!=ncol(labelmat)) {
        # repeated colnames
        colnames(labelmat) = paste0("layer", 1:ncol(labelmat))
        prefix = "layer"
    }

    if (class(labelmat)[1] != "data.frame")
        labelmat = as.data.frame(labelmat)

    if (plot.ref == T & is.null(ref.labels)) {
        warnings("No reference labels are provided!")
        plot.ref = F
    }

    if (plot.ref) {
        labelmat$ref.labels = as.character(ref.labels)
        clustree::clustree(labelmat, prefix = prefix, prop_filter = 0, node_colour = "ref.labels",
                           node_colour_aggr = "getmode", node_label = "ref.labels", node_label_aggr = "getmode",
                           ...)  # cluster tree
    } else {
        # do not plot labels
        clustree::clustree(labelmat, prefix = prefix, prop_filter = 0, ...)  # cluster tree
    }
}


#' Get the mode in the vector
#'
#' @param v a vector of numeric or character
#' @return a scalar or character representing the mode of the vector
#' @export
getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}
