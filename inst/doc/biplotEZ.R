## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  fig.height = 6, fig.width = 7,
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include=FALSE-----------------------------------------------------
library(biplotEZ)

## ----out.width='100%', echo = FALSE, fig.cap="Figure 1: Calibration of biplot axes."----
knitr::include_graphics("axis_calibration.png")

## -----------------------------------------------------------------------------
biplot(rock, scale = TRUE) |> PCA() |> plot()

## -----------------------------------------------------------------------------
state.data <- data.frame (state.region, state.x77)
biplot(state.data)

## -----------------------------------------------------------------------------
biplot(state.x77, classes=state.region)

## -----------------------------------------------------------------------------
biplot(state.x77, group.aes=state.region)

## -----------------------------------------------------------------------------
biplot(state.data)                 # centred, but no scaling
biplot(state.data, scale = TRUE)   # centered and scaled
biplot(state.data, center = FALSE) # no centring (usually not recommended) or scaling

## -----------------------------------------------------------------------------
out <- biplot(state.data)                 # centred, but no scaling
out$center
out$scaled
out$means
out$sd
out <- biplot(state.data, scale = TRUE)   # centered and scaled
out$center
out$scaled
out$means
out$sd
out <- biplot(state.data, center = FALSE) # no centring (usually not recommended) or scaling
out$center
out$scaled
out$means
out$sd

## -----------------------------------------------------------------------------
princomp(state.x77) |> biplot()
out <- prcomp(state.x77, scale.=TRUE) |> biplot()
rbind (head(out$raw.X,3),tail(out$raw.X,3))
rbind (head(out$X,3),tail(out$X,3))
out$center
out$scaled
out$means
out$sd

## -----------------------------------------------------------------------------
biplot(state.x77, scaled = TRUE) |> 
  PCA(group.aes = state.region) |> plot()

## -----------------------------------------------------------------------------
biplot(state.x77, scaled = TRUE) |> 
  PCA(group.aes = state.region) |> 
  legend.type(samples = TRUE) |> plot()

## -----------------------------------------------------------------------------
biplot(state.x77, scaled = TRUE) |> 
  PCA(group.aes = state.region, correlation.biplot = TRUE) |> 
  legend.type(samples = TRUE) |> plot()

## -----------------------------------------------------------------------------
biplot(state.x77, scaled = TRUE) |> 
  PCA(group.aes = state.division) |> 
  legend.type(samples = TRUE) |> plot()

## -----------------------------------------------------------------------------
biplot(state.x77, scaled = TRUE) |> 
  PCA(group.aes = state.division) |> 
  samples (col = c("red", "darkorange", "gold", "chartreuse4", 
                   "green", "salmon", "magenta", "#000000", "blue"),opacity = 0.65,pch=19) |>
  legend.type(samples = TRUE, new = TRUE) |> plot()

## -----------------------------------------------------------------------------
levels (state.division)

## -----------------------------------------------------------------------------
biplot(state.x77, scaled = TRUE) |> 
  PCA(group.aes = state.division) |> 
  samples (col = c("red", "darkorange", "gold", "chartreuse4", 
                   "green", "salmon", "magenta", "black", "blue"),
           pch = c(15, 15, 15, 1, 1, 1, 1, 15, 15)) |>
  legend.type(samples = TRUE, new = TRUE) |> plot()

## -----------------------------------------------------------------------------
biplot(state.x77, scaled = TRUE) |> 
  PCA(group.aes = state.division) |> 
  samples (col = c("red", "darkorange", "gold", "chartreuse4", 
                   "green", "salmon", "magenta", "black", "blue"),
           pch = c(15, 15, 15, 1, 1, 1, 1, 15, 15),
           cex = c(rep(1.5,4), c(1,1.5,1,1.5))) |>
  legend.type(samples = TRUE, new = TRUE)  |> plot()

## -----------------------------------------------------------------------------
biplot(state.x77, scaled = TRUE) |> 
  PCA(group.aes = state.division) |> 
  samples (col = c("red", "darkorange", "gold", "chartreuse4", 
                   "green", "salmon", "magenta", "black", "blue"),
           which = c("West North Central", "West South Central", "East South Central", 
                     "East North Central")) |>
  legend.type(samples = TRUE, new = TRUE)  |> plot()

## -----------------------------------------------------------------------------
biplot(state.x77, scaled = TRUE) |> PCA() |> 
  samples (label = TRUE) |> plot()

## -----------------------------------------------------------------------------
rownames(state.x77)[match(c("Pennsylvania", "New Jersey", "Massachusetts",
                            "Minnesota"), rownames(state.x77))] <- c("PA", "NJ", "MA", "MN")
above <- match(c("Alaska", "California", "Texas", "New York", "Nevada", "Georgia", "Alabama",
                 "North Carolina", "Colorado", "Washington", "Illinois", "Michigan", "Arizon",
                 "Florida", "Ohio", "NJ", "Kansas"), rownames(state.x77))
right.side <- match(c("South Carolina", "Kentucky", "Rhode Island", "New Hampshire", "Virginia",
                      "Missouri", "Delaware", "Hawaii", "Oregon", "PA", "Nebraska", "Montana",
                      "Maryland", "Indiana", "Idaho"), rownames(state.x77))
left.side <- match(c("Wyoming", "Iowa", "MN", "Connecticut"), rownames(state.x77))
label.offset <- rep(0.3, nrow(state.x77))
label.offset[match(c("Colorado", "Kansas", "Idaho"), rownames(state.x77))] <- c(0.8, 0.5, 0.8)
label.side <- rep("bottom", nrow(state.x77))
label.side[above] <- "top"
label.side[right.side] <- "right"
label.side[left.side] <- "left"
biplot (state.x77, scaled=TRUE) |> PCA() |>
  samples (label=TRUE, label.cex=0.6, label.side=label.side, label.offset=label.offset) |>
  plot()

## -----------------------------------------------------------------------------
biplot(state.x77, scaled = TRUE) |> PCA() |> 
  samples (label = "ggrepel") |> plot()

## -----------------------------------------------------------------------------
biplot(state.x77, scaled = TRUE) |> PCA() |> 
  samples (label = "TRUE",label.name=strtrim(row.names(state.x77),3)) |> plot()

## -----------------------------------------------------------------------------
gas.data <- matrix (UKgas, ncol=4, byrow=T)
colnames(gas.data) <- paste("Q", 1:4, sep="")
rownames(gas.data) <- 60:86
even.labels <- rep(c(TRUE, FALSE), 14)
biplot(gas.data, scaled = TRUE) |> PCA() |> 
  samples (connected = TRUE, connect.col="red", label = even.labels, label.cex=0.6) |> 
  plot()

## -----------------------------------------------------------------------------
biplot(rock, scaled = TRUE) |> PCA() |> 
                               axes(which = c("shape","peri"), 
                                    col=c("lightskyblue","slategrey"),
                                    lwd = c(1,2), lty=2) |>
                               plot()

## -----------------------------------------------------------------------------
biplot(rock, scaled = TRUE) |> PCA() |> 
                               axes(col=c("lightskyblue","slategrey","blue"),
                                    label.dir="Hor", label.line=c(0,0.5,1,1.5)) |>
                               plot()

## -----------------------------------------------------------------------------
biplot(rock, scaled = TRUE) |> PCA() |> 
                               axes(label.dir="Paral",
                                    ticks = c(3, 5, 5, 10), tick.label=c(F, F, T, T),
                                    ax.names = c("area", "perimeter", "shape", 
                                                 "permeability in milli-Darcies")) |>
                               plot()

## -----------------------------------------------------------------------------
  obj <- biplot(airquality)
  obj

## -----------------------------------------------------------------------------
  summary(obj)

## -----------------------------------------------------------------------------
obj <- biplot(state.x77, scale = TRUE) |> PCA() |> 
  fit.measures() |> plot()
summary (obj)

## -----------------------------------------------------------------------------
obj <- biplot(state.x77, scale = TRUE) |> PCA() |> 
  fit.measures() 
summary (obj, adequacy = FALSE, sample.predictivity = FALSE)

## -----------------------------------------------------------------------------
biplot(state.x77, scale = TRUE) |> PCA(group.aes = state.region) |>
  samples (which = "South", pch = 15, label = T, label.cex=0.5) |> 
  axes (col = "black") |>
  fit.measures() |> plot (sample.predictivity = TRUE,
                          axis.predictivity = TRUE) 

## -----------------------------------------------------------------------------
biplot(state.x77, scale = TRUE) |> PCA(group.aes = state.region) |>
  axes (vectors = TRUE,unit.circle = TRUE) |>
  fit.measures() |> plot () 

