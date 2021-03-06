% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{all.msigdb.gene.sets}
\alias{all.msigdb.gene.sets}
\title{All of the Gene Set Collections from the Broad Institutes MSigDB}
\format{A list with 8 elements, whi:
\describe{
  \item{C1}{C1 Collection Gene Sets}
  \item{C2}{C2 Collection Gene Sets}
  \item{C3}{C3 Collection Gene Sets}
  \item{C4}{C4 Collection Gene Sets}
  \item{C5}{C5 Collection Gene Sets}
  \item{C6}{C6 Collection Gene Sets}
  \item{C7}{C7 Collection Gene Sets}
  \item{Hallmark}{Hallmark Collection Gene Sets}
}}
\source{
\url{http://software.broadinstitute.org/gsea/msigdb/}
}
\usage{
all.msigdb.gene.sets
}
\description{
A collated version of all the gene set collections created by the Broad Institute.
Generated by downloading the Entrez .gmt's for each gene set, parsing them with 
roller, and concatenating them al together.
Useful for GSEA and GSVA shotguns!
}
\keyword{datasets}

