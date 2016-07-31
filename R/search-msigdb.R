#' Search MSigDB For a Given Gene Set
#'
#' This is a convenience function to look through the MSigDB gene set collections (C1-C7 and Hallmark) for a gene set
#' with a known name, and to return the Entrez gene IDs associated with that gene set. Pretty simple!
#' 
#' @param gene.set.name The name of the gene set for which to find gene IDs
#' @keywords msigdb
#' @return The MSigDB gene set's Entrez IDs, as a character vector. Or, a list of gene sets, if there were multiple matches. The leading part of each result gene set (the portion before the '.') indicates which gene set collection the gene set was found in.
#' @export
get_msigdb_gene_set <- function(gene.set.name) {
    # :(
    flattened.msigdb <- unlist(all.msigdb.gene.sets, recursive=FALSE)
    gene.set.matches <- grepl(gene.set.name, names(flattened.msigdb))
    return(flattened.msigdb[gene.set.matches])
}
#TODO: Example
