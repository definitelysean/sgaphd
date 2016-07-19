#' Run a simple Principle Components Analysis
#'
#' This function is a wrapper for plotting results from prcomp. It takes a matrix of expression values, and some extra
#' info to color and label the individual points (which are presumed to be samples in a gene expression experiment for
#' my purposes).
#' 
#' @param expression.matrix a matrix of gene expression values
#' @param sample.labels The names which should be drawn over each point in the plot, i.e. the column (sample) names of the expression matrix
#' @param sample.colors Colors for each sample (column) to be plotted
#' @param title.addition A subtitle to put under the main title ("Principle Components Analysis (PCA)")
#' @keywords prcomp pca
#' @return The results from prcomp()
#' @export
make.pca <- function(expression.matrix, sample.labels, sample.colors=c(), title.addition="") {
    pca.res <- prcomp(na.omit(t(scale(t(expression.matrix)))), center=FALSE, scale=FALSE);
    percent.variance <- summary(pca.res)$importance["Proportion of Variance", ] * 100;
    
    # Note: 10% extra space is alloted in order to give more room for the sample labels
    plot(
        pca.res$rotation[,c("PC1","PC2")], pch=21, cex=2, col="black", bg=sample.colors,
        xlim=1.1 * range(pca.res$rotation[,"PC1"]), ylim=1.1 * range(pca.res$rotation[,"PC2"]),
        xlab=sprintf("PC1 (%2.0f%%)", percent.variance["PC1"]), ylab=sprintf("PC2 (%2.0f%%)", percent.variance["PC2"]),
        main=paste("Principal Component Analysis (PCA)\n", title.addition)
    );
    
    # Write the sample labels over the points
    text(x=pca.res$rotation[,"PC1"], y=pca.res$rotation[,"PC2"], cex=0.7, col="black", labels=sample.labels);
    
    return(pca.res)
}
#TODO: Example
