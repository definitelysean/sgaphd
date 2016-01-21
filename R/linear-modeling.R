#' Runs a linear model for a single given phenotypic feature
#'
#' This function is a wrapper for limma::lmFit, which is usually used to run linear models for microarray data.
#' It returns a dataframe with the t-statistic, p-value, and FDR adjusted p-value for each gene.
#' @param feature.name A string of the column name in the phenotype data matrix corresponding to the feature of interest
#' @param pheno.data The full phenotypic data matrix (e.g. pData(expression.set))
#' @param dataset the expression matrix of interest. ncol(dataset) should == nrow(pheno.data)
#' @param is.numeric Whether the feature in question is numeric or not. If not numeric, it's assumed to be a factor.
#' @keywords linear model microarray limma clinical feature
#' @export
run.feature.lm = function(feature.name, pheno.data, dataset, is.numeric=TRUE) {
    variable <- pheno.data[, feature.name]  
    if (is.numeric == TRUE) {
        model <- model.matrix(~as.numeric(variable)) 
    } else {
        model <- model.matrix(~as.factor(variable))
    }
    
    fit <- lmFit(dataset, model)
    fit.tval <- (fit$coef / fit$stdev.unscaled / fit$sigma) [,2] #Formula for t-statistic
    fit.pval <- 2*(1-pt(abs(fit.tval), fit$df.residual))
    fit.fdr  <- p.adjust(fit.pval, 'fdr')
    res      <- data.frame(gene.symbols, fit.tval, fit.pval, fit.fdr)
    colnames(res) <- c("gene.symbol", "tval", "pval", "fdr")
    
    return(res)
}

#TODO: Example
#TODO: Error handling
