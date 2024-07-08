## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  fig.height = 6, fig.width = 7,
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(biplotEZ)

## -----------------------------------------------------------------------------
biplot(rock) |> 
  regress(Z = MASS::sammon(dist(scale(rock), method="manhattan"))$points) |> 
  plot()

## -----------------------------------------------------------------------------
biplot(rock) |> 
  regress(Z = MASS::sammon(dist(scale(rock), method="manhattan"))$points,
          axes = "splines") |> 
  plot()

## -----------------------------------------------------------------------------
biplot(rock, scale = TRUE) |> PCO() |> plot()

## -----------------------------------------------------------------------------
biplot(rock, scale = TRUE) |> PCO(axes = "splines") |> plot()

## -----------------------------------------------------------------------------
Clark.dist <- function(X)
{
  n <- nrow(X)
  p <- ncol(X)
  Dmat <- matrix (0, nrow=n, ncol=n)
  for (i in 1:(n-1))
    for (j in (i+1):n)
      Dmat[i,j] <- sum(((X[i,] - X[j,])/(X[i,] + X[j,]))^2)
  sqrt(Dmat + t(Dmat))
}
my.data <- scale(rock, center=apply(rock,2,min), scale=diff(apply(rock,2,range)))+1
biplot(rock) |> PCO(Dmat = Clark.dist (rock), axes = "splines") |> plot()

## -----------------------------------------------------------------------------
sqrtManhattan
biplot(rock, scaled = TRUE) |> PCO(dist.func = sqrtManhattan) |> plot()

