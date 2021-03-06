% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make-PCA.R
\name{make.pca}
\alias{make.pca}
\title{Run a simple Principle Components Analysis}
\usage{
make.pca(expression.matrix, sample.labels, sample.colors = c(),
  title.addition = "")
}
\arguments{
\item{expression.matrix}{a matrix of gene expression values}

\item{sample.labels}{The names which should be drawn over each point in the plot, i.e. the column (sample) names of the expression matrix}

\item{sample.colors}{Colors for each sample (column) to be plotted}

\item{title.addition}{A subtitle to put under the main title ("Principle Components Analysis (PCA)")}
}
\value{
The results from prcomp()
}
\description{
This function is a wrapper for plotting results from prcomp. It takes a matrix of expression values, and some extra
info to color and label the individual points (which are presumed to be samples in a gene expression experiment for
my purposes).
}
\keyword{pca}
\keyword{prcomp}

