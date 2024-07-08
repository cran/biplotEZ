## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  fig.height = 6, fig.width = 7,
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(biplotEZ)

## -----------------------------------------------------------------------------
biplot(state.x77) |> 
  CVA(classes = state.region) |> 
  plot()

## -----------------------------------------------------------------------------
biplot(state.x77) |> CVA(classes = state.region) |> alpha.bags() |> 
  legend.type (bags = TRUE) |> 
  plot()

## -----------------------------------------------------------------------------
biplot(state.x77, scaled = TRUE) |> 
  CVA(classes = state.division) |> 
  legend.type(means = TRUE) |> 
  plot()

## -----------------------------------------------------------------------------
biplot(state.x77, scaled = TRUE) |> 
  CVA(classes = state.division) |> 
  means(label = TRUE, col = "olivedrab", pch = 15) |> 
  plot()

## -----------------------------------------------------------------------------
biplot(state.x77, scaled = TRUE) |> 
  CVA(classes = state.division) |> 
  means (which = c("West North Central", "West South Central", "East South Central", 
                     "East North Central"), label = TRUE) |>
  plot()

## -----------------------------------------------------------------------------
biplot(state.x77, scaled = TRUE) |> 
  CVA(classes = state.division) |> 
  means (col = "olivedrab", pch = 15, cex = 1.5,
         label = TRUE, label.col = c("blue","green","gold","cyan","magenta",
                                     "black","red","grey","purple")) |>
  plot()

## -----------------------------------------------------------------------------
biplot(state.x77, scaled = TRUE) |> 
  CVA(classes = state.division) |> 
  samples (label = "ggrepel", label.cex=0.65) |> 
  means (label = "ggrepel", label.cex=0.8) |> plot()

## -----------------------------------------------------------------------------
biplot(state.x77, scaled = TRUE) |> 
  CVA(classes = state.division) |>
  classify(classify.regions = TRUE,opacity = 0.2) |> 
  plot()

## -----------------------------------------------------------------------------
obj <- biplot(state.x77, scaled = TRUE) |> 
       CVA(classes = state.division) |> 
       fit.measures() |>
       plot()
summary (obj)

## -----------------------------------------------------------------------------
obj <- biplot(state.x77, scaled = TRUE) |> 
       CVA(classes = state.region) |> 
       fit.measures()
summary (obj, adequacy = FALSE, within.class.axis.predictivity = FALSE,
         within.class.sample.predictivity = FALSE)

## -----------------------------------------------------------------------------
state.2group <- ifelse(state.division == "New England" | 
                       state.division == "Middle Atlantic"  |
                       state.division == "South Atlantic" |
                       state.division == "Pacific",
                       "Coastal", "Central")
biplot (state.x77) |> CVA (state.2group) |> legend.type(means=TRUE) |> plot()

## -----------------------------------------------------------------------------
biplot (state.x77) |> CVA (state.2group, low.dim="Bha") |> legend.type(means=TRUE) |> plot()

## -----------------------------------------------------------------------------
biplot(state.x77, scaled = TRUE) |> AoD(classes = state.region) |> plot()

## -----------------------------------------------------------------------------
biplot(state.x77, scaled = TRUE) |> AoD(classes = state.region, axes = "splines") |> plot()

## -----------------------------------------------------------------------------
biplot(state.x77, scaled = TRUE) |> 
  AoD(classes = state.region, axes = "splines", dist.func=sqrtManhattan) |> plot()

