% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funcs.R
\name{PCA}
\alias{PCA}
\title{Principal Component Analysis (PCA) method}
\usage{
PCA(bp, dim.biplot = c(2, 1, 3), e.vects = 1:ncol(bp$X),
group.aes = NULL, correlation.biplot = FALSE, ...)
}
\arguments{
\item{bp}{an object of class \code{biplot} obtained from preceding function \code{biplot()}.}

\item{dim.biplot}{dimension of the biplot. Only values 1, 2 and 3 are accepted, with default \code{2}.}

\item{e.vects}{which eigenvectors (principal components) to extract, with default \code{1:dim.biplot}.}

\item{group.aes}{vector of the same length as the number of rows in the data matrix
for differentiated aesthetics for samples.}

\item{correlation.biplot}{logical, if \code{FALSE}, the distances between sample points are
optimally approximated in the biplot. If \code{TRUE}, the correlations between
variables are optimally approximated by the cosine of the angles between
axes. Default is \code{FALSE}.}

\item{...}{additional arguments}
}
\value{
Object of class PCA with the following elements:
\item{X}{matrix of the centered and scaled numeric variables.}
\item{Xcat}{matrix of the categorical variables.}
\item{raw.X}{original data.}
\item{na.action}{vector of observations that have been removed.}
\item{center}{TRUE or FALSE, whether X is centred.}
\item{scaled}{TRUE or FALSE, whether X is scaled.}
\item{means}{mean of each numerical variable.}
\item{sd}{standard deviation of each numerical variable.}
\item{n}{number of observations.}
\item{p}{number of variables.}
\item{group.aes}{vector of the same length as the number of rows in the data matrix for differentiated aesthetics for samples.}
\item{g.names}{descriptive name to be used for group labels.}
\item{g}{number of groups.}
\item{Title}{title of the biplot to be rendered}
\item{Z}{matrix with each row containing the details of the point to be plotted (i.e. coordinates).}
\item{Vr}{matrix consisting of the eigenvectors as columns.}
\item{Xhat}{predictions of the samples.}
\item{ax.one.unit}{one unit in the positive direction of each biplot axis.}
}
\description{
This function produces a list of elements to be used for PCA biplot construction.
}
\examples{
biplot(iris[,1:4]) |> PCA()
}
\references{
Gabriel, K.R. (1971) The biplot graphic display of matrices with application to principal component analysis. \emph{Biometrika.} 58(3):453–467.
}