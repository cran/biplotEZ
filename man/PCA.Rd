% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PCA.R
\name{PCA}
\alias{PCA}
\title{Perform Principal Components Analysis (PCA)}
\usage{
PCA(bp, dim.biplot = c(2, 1, 3), e.vects = 1:ncol(bp$X),
group.aes = NULL, show.class.means = FALSE, correlation.biplot = FALSE)
}
\arguments{
\item{bp}{an object of class \code{biplot} obtained from preceding function \code{biplot()}.}

\item{dim.biplot}{the dimension of the biplot. Only values \code{1}, \code{2} and \code{3} are accepted, with default \code{2}.}

\item{e.vects}{the vector indicating which eigenvectors (principal components) should be plotted in the biplot, with default \code{1:dim.biplot}.}

\item{group.aes}{a vector of the same length as the number of rows in the data matrix
for differentiated aesthetics for samples.}

\item{show.class.means}{a logical value indicating whether group means should be plotted in the biplot.}

\item{correlation.biplot}{a logical value. If \code{FALSE}, the distances between sample points are
optimally approximated in the biplot. If \code{TRUE}, the correlations between
variables are optimally approximated by the cosine of the angles between
axes. Default is \code{FALSE}.}
}
\value{
An object of class PCA with the following elements:
\item{X}{the matrix of the centered and scaled numeric variables.}
\item{Xcat}{the data frame of the categorical variables.}
\item{raw.X}{the original data.}
\item{classes}{the vector of category levels for the class variable. This is to be used for \code{colour}, \code{pch} and \code{cex} specifications.}
\item{na.action}{the vector of observations that have been removed.}
\item{center}{a logical value indicating whether \eqn{\mathbf{X}} is centered.}
\item{scaled}{a logical value indicating whether \eqn{\mathbf{X}} is scaled.}
\item{means}{the vector of means for each numerical variable.}
\item{sd}{the vector of standard deviations for each numerical variable.}
\item{n}{the number of observations.}
\item{p}{the number of variables.}
\item{group.aes}{the vector of category levels for the grouping variable. This is to be used for \code{colour}, \code{pch} and \code{cex} specification.}
\item{g.names}{the descriptive names to be used for group labels.}
\item{g}{the number of groups.}
\item{Title}{the title of the biplot rendered.}
\item{Z}{the matrix with each row containing the details of the points that are plotted (i.e. coordinates).}
\item{Lmat}{the matrix for transformation to the principal components.}
\item{Linv}{the inverse of \eqn{\mathbf{L}}.}
\item{eigenvalues}{the vector of eigenvalues of the covariance matrix of \eqn{\mathbf{X}}.}
\item{ax.one.unit}{one unit in the positive direction of each biplot axis.}
\item{e.vects}{the vector indicating which principal components are plotted in the biplot.}
\item{Vr}{the \code{1:dim.biplot} columns of \eqn{\mathbf{V}}.}
\item{dim.biplot}{the dimension of the biplot.}
\item{class.means}{a logical value indicating whether group means are plotted in the biplot.}
\item{Zmeans}{the matrix of class mean coordinates that are plotted in the biplot.}
}
\description{
This function appends the \code{biplot} object with elements resulting from performing PCA.
}
\examples{
biplot(iris[,1:4]) |> PCA()
# create a PCA biplot
biplot(data = iris) |> PCA() |> plot()

}
\references{
Gabriel, K.R. (1971) The biplot graphic display of matrices with application to principal component analysis. \emph{Biometrika.} 58(3):453–467.
}
\seealso{
\code{\link[=biplot]{biplot()}}
}
