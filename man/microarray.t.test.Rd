\name{microarray.t.test}
\alias{microarray.t.test}
\title{T-Tests on a Matrix of Values}
\usage{
  microarray.t.test(class.labels, dataset,
    test.type = "t.equalvar")
}
\arguments{
  \item{class.labels}{numeric vector of 0's and 1's
  identifying which sample (column in matrix) belongs to
  which of the two groups to be tested}
}
\description{
  This function is a wrapper for multtest::mt.teststat,
  which is usually used to perform t-tests on every row of
  a gene expression matrix (which is stored as [genes,
  samples]). It returns a dataframe with the t-statistic,
  p-value, and FDR adjusted p-value for each gene.
}
\keyword{microarray}
\keyword{ttest}

