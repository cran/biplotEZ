## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  fig.height = 6, fig.width = 7,
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include=FALSE-----------------------------------------------------
library(biplotEZ)

## -----------------------------------------------------------------------------
biplot(iris) |> PCA(group.aes = iris$Species,dim.biplot = 1)  |> plot()

## -----------------------------------------------------------------------------
biplot(iris) |> CVA(classes = iris$Species,dim.biplot = 1) |>
  axes(col="black") |> plot()

## -----------------------------------------------------------------------------
bp <- biplot(iris) |> CVA(classes = iris[,5],dim.biplot = 1)|>
  axes(col="black") |> 
  classify(borders = TRUE,opacity = 1)|>plot()

## -----------------------------------------------------------------------------
print(bp)

## -----------------------------------------------------------------------------
biplot(HairEyeColor[,,2], center = FALSE) |> CA(variant = "Princ", 
  dim.biplot=1, lambda.scal = T) |> plot()

## -----------------------------------------------------------------------------
biplot(iris[c(1:50,101:150),1:4])|> PCA(dim.biplot = 1) |> axes(col="black") |> 
  interpolate(newdata = iris[51:100,1:4]) |> newsamples(col="purple") |> plot()

## -----------------------------------------------------------------------------
biplot(iris[,1:3])|> PCA(dim.biplot = 1) |> axes(col="black") |> 
  interpolate(newvariable = iris[,4]) |> 
  newaxes(col="darkred",X.new.names = "Petal.Width") |> plot()

## -----------------------------------------------------------------------------
biplot(iris) |> 
  PCA(group.aes = iris$Species,dim.biplot = 1,show.class.means = TRUE) |> 
  axes(col="black",predict.col = "darkred") |> 
  prediction(predict.samples=100:150) |> plot()

## -----------------------------------------------------------------------------
biplot(iris) |> 
  PCA(group.aes = iris$Species,dim.biplot = 1,show.class.means = TRUE) |> 
  axes(col="black",predict.col = "darkred") |> means(label=TRUE,which=1:3)|>
  prediction(predict.means = 1) |> plot()

## -----------------------------------------------------------------------------
biplot(iris) |> PCA(group.aes = iris[,5],dim.biplot = 1) |> 
  axes(col="black") |> 
  ellipses() |> plot()

## -----------------------------------------------------------------------------
biplot(iris) |> PCA(group.aes = iris[,5],dim.biplot = 1) |> 
  axes(col="black") |> 
  alpha.bags(alpha = 0.7) |> plot()

## -----------------------------------------------------------------------------
biplot(iris) |> PCA(dim.biplot = 1) |> 
  axes(col="black") |>
  density1D() |> plot()

## -----------------------------------------------------------------------------
biplot(iris) |> PCA(dim.biplot = 1) |> axes(col='black') |>
  density1D(h = 0.5 ,kernel = "triangular") |> plot()

## -----------------------------------------------------------------------------
biplot(iris) |> PCA(group.aes = iris[,5],dim.biplot = 1) |> 
  axes(col="black") |>
  density1D() |> plot()

## -----------------------------------------------------------------------------
biplot(iris) |> PCA(group.aes = iris[,5],dim.biplot = 1) |> 
  axes(col="black") |>
  density1D(which = c(2,3)) |> plot()

## -----------------------------------------------------------------------------
biplot(iris) |> PCA(group.aes  = iris[,5],dim.biplot = 1, show.class.means = TRUE) |>
  axes(col="black") |> density1D() |> samples(opacity=0.5)|> alpha.bags()|>
  legend.type(samples = TRUE) |> plot()

## -----------------------------------------------------------------------------
biplot(iris) |> PCA(group.aes  = iris[,5],dim.biplot = 1, show.class.means = TRUE) |>
  axes(col="black") |> density1D() |> samples(opacity=0.5)|> alpha.bags()|>
  legend.type(samples = TRUE,means = TRUE, bags = TRUE) |> plot()

## -----------------------------------------------------------------------------
biplot(iris) |> PCA(group.aes  = iris[,5],dim.biplot = 1, show.class.means = TRUE) |>
  axes(col="black") |> density1D() |> samples(opacity=0.5)|> alpha.bags()|>
  legend.type(samples = TRUE,means = TRUE, bags = TRUE, new=TRUE) |> plot()

## -----------------------------------------------------------------------------
bp <- biplot(iris) |> CVA(classes  = iris[,5],dim.biplot = 1, show.class.means = TRUE) |>
  axes(col="black") |> classify() |> density1D() |> samples(opacity=0.5)|> alpha.bags()|>
  legend.type(samples = TRUE,means = TRUE, bags = TRUE, regions = TRUE, new=TRUE) |> plot() #

## -----------------------------------------------------------------------------
a <- biplot(iris) |> PCA(group.aes = iris[,5],dim.biplot = 1) |> fit.measures()
summary(a)

