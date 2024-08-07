% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aesthetics.R
\name{samples}
\alias{samples}
\title{Aesthetics for biplot samples}
\usage{
samples (bp,  which = 1:bp$g, col = ez.col, pch = 16, cex = 1,
label = FALSE, label.name = NULL, label.col=NULL, label.cex = 0.75, 
label.side = "bottom", label.offset = 0.5,
connected=FALSE, connect.col = "black", connect.lty = 1, 
connect.lwd = 1, opacity = 1)
}
\arguments{
\item{bp}{an object of class \code{biplot}.}

\item{which}{vector of which groups of samples to display, with default \code{bp$g}.}

\item{col}{sample colour, with default \code{blue}.}

\item{pch}{sample plotting character, with default \code{+}.}

\item{cex}{sample character expansion, with default \code{1}.}

\item{label}{logical (\code{TRUE}, \code{FALSE}), whether samples should be labelled, with default \code{FALSE}. Alternatively, specify \code{"ggrepel"} for non-overlapping placement of labels.}

\item{label.name}{vector of length number of samples, with default
\code{NULL}, \code{rownames(bp)} are used.}

\item{label.col}{vector of length number of samples with the colour of the labels, defaulting to the
colour of the sample points.}

\item{label.cex}{label text expansion, with default \code{0.75}.}

\item{label.side}{side of the plotting character where label appears, with default \code{bottom}. Note that unlike
the argument \code{pos} in \code{text()}, options are "bottom", "left", "top", "right" and not 1, 2, 3, 4.}

\item{label.offset}{offset of the label from the data point. See ?text for a detailed explanation of the
argument \code{offset}.}

\item{connected}{logical, whether samples are connected in order of rows of data matrix, with default \code{FALSE}.}

\item{connect.col}{colour of the connecting line, with default \code{black}.}

\item{connect.lty}{line type of the connecting line, with default \code{1}.}

\item{connect.lwd}{line width of the connecting line, with default \code{1}.}

\item{opacity}{opacity of sample points, with default 1 (opaque).}
}
\value{
A list with the following components is available:
\item{which}{which means to display.}
\item{col}{colour of the samples.}
\item{pch}{plotting character of the samples.}
\item{cex}{expansion of the plotting character of the samples.}
\item{label}{TRUE or FALSE, whether samples should be labelled, with default
\code{FALSE}.}
\item{label.name}{If \code{NULL}, the row names will be used, with default \code{NULL}. Alternatively, a vector of length \code{n} should be used.}
\item{label.col}{colour of the label.}
\item{label.cex}{expansion of the label.}
\item{label.side}{side at which to plot the label of samples.}
\item{label.offset}{offset of the label from the data point.}
\item{connected}{TRUE or FALSE, whether samples should be connected in row order of X.}
\item{connect.col}{colour of the connecting line.}
\item{connect.lty}{line type of the connecting line.}
\item{connect.lwd}{line width of the connecting line.}
\item{opacity}{opacity of sample points, with default 1 (opaque).}
}
\description{
This function allows formatting changes to samples.
}
\details{
The arguments \code{which}, \code{col}, \code{pch} and \code{cex} are based on the specification of \code{group.aes} or \code{classes}. If no groups
are specified, a single colour, plotting character and / or character expansion is expected. If g groups are
specified, vectors of length g is expected, or values are recycled to length g.

The arguments \code{label}, \code{label.cex}, \code{label.side} and \code{label.offset} are based on the sample size n. A single value
will be recycled n times or a vector of length n is expected.
}
\examples{
biplot(iris[,1:4]) |> PCA() |> samples(col="purple",pch=15, opacity=0.5) |> plot()
biplot(iris[,1:4]) |> PCA() |> 
  samples(col="purple",pch=NA, opacity=0.5, label = TRUE) |> plot()
biplot(iris[,1:4]) |> PCA() |> 
  samples(col="purple",pch=NA, opacity=0.5, label = TRUE, 
          label.name = paste("s:",1:150, sep="")) |> 
  plot()
biplot(iris[,1:4]) |> PCA() |> 
  samples(col="purple",pch=NA, opacity=0.5, label = "ggrepel") |> plot()

}
