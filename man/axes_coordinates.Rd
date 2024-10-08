% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calibrate_axes.R
\name{axes_coordinates}
\alias{axes_coordinates}
\title{Calibrate Biplot Axes}
\usage{
axes_coordinates(x)
}
\arguments{
\item{x}{an object of class \code{biplot}}
}
\value{
An ordered list containing the coordinates the of tick marks to plotted on the biplot
}
\description{
Convenience function to obtain the coordinates of the calibrated ticks marks on the biplot
}
\examples{
x<-biplot(iris) |> PCA()
coordinates<-axes_coordinates(x)
}
