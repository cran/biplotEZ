## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  fig.height = 6, fig.width = 7,
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include=FALSE-----------------------------------------------------
library(biplotEZ)
library(rgl)

## -----------------------------------------------------------------------------
biplot(iris) |> PCA(group.aes = iris$Species,dim.biplot = 3) |>
  axes(col="black") |> plot()

## ----echo=FALSE---------------------------------------------------------------
my.plot <- scene3d()
rglwidget(my.plot)

## -----------------------------------------------------------------------------
biplot(iris[1:100,1:4])|> PCA(dim.biplot = 3) |> axes(col="black") |> 
  interpolate(newdata = iris[101:150,1:4]) |> newsamples(col="purple") |> plot()

## ----echo=FALSE---------------------------------------------------------------
my.plot <- scene3d()
rglwidget(my.plot)

## -----------------------------------------------------------------------------
biplot(iris[,1:3])|> PCA(dim.biplot = 3) |> axes(col="black") |> 
  interpolate(newvariable = iris[,4]) |> 
  newaxes(col="darkred",X.new.names = "Petal.Width") |> plot()

## ----echo=FALSE---------------------------------------------------------------
my.plot <- scene3d()
rglwidget(my.plot)

## -----------------------------------------------------------------------------
biplot(iris) |> PCA(group.aes = iris$Species,dim.biplot = 3,show.class.means = TRUE) |> 
  axes(col="black",predict.col = "orange") |>  
  prediction(predict.samples=100:150,which = c(1,4))  |> plot()

## ----echo=FALSE---------------------------------------------------------------
my.plot <- scene3d()
rglwidget(my.plot)

## -----------------------------------------------------------------------------
biplot(iris) |> 
  PCA(group.aes = iris$Species,dim.biplot = 3,show.class.means = TRUE) |> 
  axes(col="black",predict.col = "darkred") |> 
  prediction(predict.means = 1) |> plot()

## ----echo=FALSE---------------------------------------------------------------
my.plot <- scene3d()
rglwidget(my.plot)

## -----------------------------------------------------------------------------
biplot(iris) |> PCA(group.aes = iris[,5],dim.biplot = 3) |> 
  axes(col="black") |> 
  ellipses(kappa = 3,opacity = 0.5) |> plot()

## ----echo=FALSE---------------------------------------------------------------
my.plot <- scene3d()
rglwidget(my.plot)

## -----------------------------------------------------------------------------
biplot(iris) |> CVA(classes = iris$Species,dim.biplot = 3) |>
  axes(col="black") |> plot()

## ----echo=FALSE---------------------------------------------------------------
my.plot <- scene3d()
rglwidget(my.plot)

## -----------------------------------------------------------------------------
biplot(HairEyeColor[,,2], center = FALSE) |> CA(variant = "Symmetric", dim.biplot = 3) |> 
samples(col=c("darkred","forestgreen"), pch=c(15,17), label.col="black") |> plot()

## ----echo=FALSE---------------------------------------------------------------
my.plot <- scene3d()
rglwidget(my.plot)

## -----------------------------------------------------------------------------
SACrime <- matrix(c(1235,432,1824,1322,573,588,624,169,629,34479,16833,46993,30606,13670,
              16849,15861,9898,24915,2160,939,5257,4946,722,1271,881,775,1844,5946,
              4418,15117,10258,5401,4273,4987,1956,10639,29508,15705,62703,37203,
              11857,18855,14722,4924,42376,604,156,7466,3889,203,664,291,5,923,19875,
              19885,57153,29410,11024,12202,10406,5431,32663,7086,4193,22152,9264,3760,
              4752,3863,1337,8578,7929,4525,12348,24174,3198,1770,7004,2201,45985,764,
              427,1501,1197,215,251,345,213,1850,3515,879,3674,4713,696,835,917,422,2836,
              88,59,174,76,31,61,117,32,257,5499,2628,8073,6502,2816,2635,3017,1020,4000,
              8939,4501,50970,24290,2447,5907,5528,1175,14555),nrow=9, ncol=14)
dimnames(SACrime) <- list(paste(c("ECpe", "FrSt", "Gaut", "KZN",  "Limp", "Mpml", "NWst", "NCpe",
                            "WCpe")), paste(c("Arsn", "AGBH", "AtMr", "BNRs", "BRs",  "CrJk",
                                              "CmAs", "CmRb", "DrgR", "InAs", "Mrd", "PubV", 
                                              "Rape", "RAC" )))
names(dimnames(SACrime))[[1]] <- "Provinces"
names(dimnames(SACrime))[[2]] <- "Crimes"

## -----------------------------------------------------------------------------
biplot(SACrime, center = FALSE) |> CA(variant = "Symmetric", dim.biplot = 3) |> 
samples(col=c("royalblue","darkred"), pch=c(15,17), label.col="black") |> plot()

## ----echo=FALSE---------------------------------------------------------------
my.plot <- scene3d()
rglwidget(my.plot)

