## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(biplotEZ)

## ---- fig.height = 6, fig.width = 7-------------------------------------------
biplot(state.x77) |> CVA(classes = state.region) |> plot()

## ---- fig.height = 6, fig.width = 7-------------------------------------------
biplot(state.x77) |> CVA(classes = state.region) |> alpha.bags() |> 
  legend.type (bags = TRUE) |> plot()

## ---- fig.height = 6, fig.width = 7-------------------------------------------
biplot(state.x77, scaled = TRUE) |> 
  CVA(classes = state.division) |> 
  legend.type(means = TRUE) |> plot()

## ---- fig.height = 6, fig.width = 7-------------------------------------------
biplot(state.x77, scaled = TRUE) |> 
  CVA(classes = state.division) |> 
  means(label = TRUE, col = "olivedrab", pch = 15) |> plot()

## ---- fig.height = 6, fig.width = 7-------------------------------------------
biplot(state.x77, scaled = TRUE) |> 
  CVA(classes = state.division) |> 
  means (which = c("West North Central", "West South Central", "East South Central", 
                     "East North Central"), label = TRUE) |>
  plot()

## ---- fig.height = 6, fig.width = 7-------------------------------------------
biplot(state.x77, scaled = TRUE) |> 
  CVA(classes = state.division) |> 
  means (col = "olivedrab", pch = 15, cex = 1.5,
         label = TRUE, label.col = c("blue","green","gold","cyan","magenta",
                                     "black","red","grey","purple")) |>
  plot()

## ---- fig.height = 6, fig.width = 7-------------------------------------------
biplot(state.x77, scaled = TRUE) |> 
  CVA(classes = state.division) |> 
  samples (label = "ggrepel", label.cex=0.65) |> 
  means (label = "ggrepel", label.cex=0.8) |> plot()

## ---- fig.height = 6, fig.width = 7-------------------------------------------
obj <- biplot(state.x77, scaled = TRUE) |> 
       CVA(classes = state.division) |> 
       fit.measures() |>
       plot()
summary (obj)

## ---- fig.height = 6, fig.width = 7-------------------------------------------
obj <- biplot(state.x77, scaled = TRUE) |> 
       CVA(classes = state.region) |> 
       fit.measures()
summary (obj, adequacy = FALSE, within.class.axis.predictivity = FALSE,
         within.class.sample.predictivity = FALSE)

