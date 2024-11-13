## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  fig.height = 6, fig.width = 7,
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include=FALSE-----------------------------------------------------
library(biplotEZ)

## -----------------------------------------------------------------------------
X <- HairEyeColor[,,2]
X

## -----------------------------------------------------------------------------
N <- sum(X)
N

## -----------------------------------------------------------------------------
P <- X/N
P

## -----------------------------------------------------------------------------
rMass <- rowSums(P)
rMass
cMass <- colSums(P)
cMass

## -----------------------------------------------------------------------------
Dr <- diag(apply(P, 1, sum))
Dr
Dc <- diag(apply(P, 2, sum))
Dc

## -----------------------------------------------------------------------------
Smat <- sqrt(solve(Dr))%*%(P-(Dr %*%matrix(1, nrow = nrow(X), 
    ncol = ncol(X)) %*%  Dc))%*%sqrt(solve(Dc))
svd.out <- svd(Smat)
svd.out

## -----------------------------------------------------------------------------
gamma <- 0
rowcoor <- svd.out[[2]]%*%(diag(svd.out[[1]])^gamma)
colcoor <- svd.out[[3]]%*%(diag(svd.out[[1]])^(1-gamma))

## -----------------------------------------------------------------------------
biplot(HairEyeColor[,,2], center = FALSE) |> CA() |> plot()

## -----------------------------------------------------------------------------
ca.out <- biplot(HairEyeColor[,,2], center = FALSE) |> CA(variant = "Stand") |> 
  plot()

## -----------------------------------------------------------------------------
ca.out <- biplot(HairEyeColor[,,2], center = FALSE) |> 
  CA(variant = "Symmetric") |> plot()
ca.out$lambda.val

## -----------------------------------------------------------------------------
ca.out <- biplot(HairEyeColor[,,2], center = FALSE) |> 
  CA(variant = "Symmetric") |> fit.measures()
print("Quality")
ca.out$quality
print("Adequacy")
ca.out$adequacy
print("Row predictivities")
ca.out$row.predictivities
print("Column predictivities")
ca.out$col.predictivities

## -----------------------------------------------------------------------------
biplot(HairEyeColor[,,2], center = FALSE) |> CA(variant = "Symmetric") |> 
  samples(pch = c(0,2)) |> interpolate(newdata = HairEyeColor[,,1]) |> 
  newsamples(col = c("orange","purple"), pch = c(15,17)) |> plot()      

## -----------------------------------------------------------------------------
biplot(HairEyeColor[,,2], center = FALSE) |> CA(variant = "Princ") |> 
  samples(col = c("cyan","purple"), pch = c(15,17), label.side = c("bottom","top"), 
          label.cex = 1) |> legend.type(samples = TRUE, new = TRUE) |> plot()

## -----------------------------------------------------------------------------
biplot(HairEyeColor[,,2], center = FALSE) |> CA(variant = "Symmetric") |> 
  samples(col = c("forestgreen","magenta"), pch = c(12,17), 
          label.side = c("top","bottom")) |> 
  legend.type(samples = TRUE) |> plot()

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
SACrime

## -----------------------------------------------------------------------------
biplot(SACrime, center = FALSE) |> 
  CA(variant = "Symmetric", lambda.scal = TRUE) |> 
  samples(col = c("cyan","purple"), pch = c(15,17), label.side = c("bottom","top")) |>
  legend.type(samples = TRUE, new = TRUE) |> plot()

