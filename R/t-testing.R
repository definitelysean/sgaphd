#' T-Tests on a Matrix of Values
#'
#' This function is a wrapper for multtest::mt.teststat, which is usually used to perform t-tests
#' on every row of a gene expression matrix (which is stored as [genes, samples]). It returns a dataframe
#' with the t-statistic, p-value, and FDR adjusted p-value for each gene.
#' @param class.labels numeric vector of 0's and 1's identifying which sample (column in matrix) belongs to which of the two groups to be tested
#' @keywords ttest microarray
#' @export
microarray.t.test <- function(class.labels, dataset, test.type="t.equalvar") {
  tval <- multtest::mt.teststat(dataset, classlabel=class.labels, test=test.type, nonpara="n")
  df <- (ncol(dataset) - 2)
  pval <- 2*(1-pt(abs(tval),df))
  fdr <- p.adjust(pval, method="fdr")
  res.all <- data.frame(I(rownames(dataset)), tval, pval, fdr)
  colnames(res.all) <- c("gene.id", "tval", "pval", "fdr")
  return(res.all)
}
#TODO: Example; just generate a small fake gene expression matrix & some class labels.
