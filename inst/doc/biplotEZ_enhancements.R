## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  fig.height = 6, fig.width = 7,
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include=FALSE-----------------------------------------------------
library(biplotEZ)

## -----------------------------------------------------------------------------
sunspots <- matrix (sunspot.month[1:(264*12)], ncol = 12, byrow = TRUE)
years <- 1749:2012
rownames(sunspots) <- years
colnames(sunspots) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
century <-paste(floor((years-1)/100)+1, ifelse (floor((years-1)/100)+1<21, "th","st"), sep = "-")
biplot(sunspots, group.aes=century) |> PCA() |>
        axes (label.dir = "Hor", label.line = c(0.8, rep(0,10), 0.8)) |>
        alpha.bags () |> 
        legend.type(bags = TRUE)  |> plot()

## -----------------------------------------------------------------------------
biplot(sunspots, group.aes=century) |> PCA() |>
        axes (label.dir = "Hor", label.line = c(0.8, rep(0,10), 0.8)) |>
        alpha.bags (alpha = c(0.9, 0.95, 0.99), which = c(1,4), opacity = 0) |> 
        legend.type(bags = TRUE)  |> plot()

## -----------------------------------------------------------------------------
biplot(sunspots, group.aes=century) |> PCA() |>
        axes (label.dir = "Hor", label.line = c(0.8, rep(0,10), 0.8)) |>
        samples (which = NULL) |>
        alpha.bags (alpha = c(0.9, 0.95, 0.99), lty = c(1,3,5), opacity=0.05) |> 
        legend.type(bags = TRUE, new = TRUE)  |> plot()

## -----------------------------------------------------------------------------
biplot(sunspots, group.aes=century) |> PCA() |>
        axes (label.dir = "Hor", label.line = c(0.8, rep(0,10), 0.8)) |>
        samples (which = NULL) |>
        alpha.bags (alpha = c(   0.9,   0.95,   0.99,             0.5,         0.6,     0.7), 
                    which = c(     1,      1,      2,               3,           3,      3),
                    col   = c("brown", "red", "gold",  "deepskyblue2", "steelblue3","blue"),
                    lty   = c(     1,      2,     10,               2,           2,      0),
                    lwd   = c(     1,      1,      3,               1,           2,      1),
                    opacity = 0.1) |> plot()

## -----------------------------------------------------------------------------
biplot(sunspots, group.aes=century) |> PCA() |>
        alpha.bags (col   = c("brown", "red", "gold","deepskyblue2"),
                    opacity = 0.1,outlying = TRUE) |> plot()

## -----------------------------------------------------------------------------
biplot(sunspots, group.aes=century) |> PCA() |>
        axes (label.dir = "Hor", label.line = c(0.8, rep(0,10), 0.8)) |>
        samples (which = NULL) |>
        ellipses (alpha = c(0.9, 0.95), lty = c(1,3,5), opacity = 0.1) |> 
        legend.type(ellipses = TRUE)  |> plot()

biplot(sunspots, group.aes=century) |> PCA() |>
        axes (label.dir = "Hor", label.line = c(0.8, rep(0,10), 0.8)) |>
        samples (which = NULL) |>
        ellipses (kappa = 1:2, lty = c(1,3,5), opacity = 0.1) |> 
        legend.type(ellipses = TRUE) |> plot()

## -----------------------------------------------------------------------------
biplot(state.x77,group.aes = state.region,scaled = TRUE) |> PCA() |>
  density2D(which=2,col=c("white","purple","blue","cyan"),contours=TRUE) |> plot()

## -----------------------------------------------------------------------------
biplot(state.x77,scaled = TRUE) |> PCA() |> samples(which=NULL) |>
  density2D(which=1,col=c("white","purple","blue","cyan"),contours = TRUE,cuts = 20) |> plot()

## -----------------------------------------------------------------------------
biplot(rock[1:40,], scale = TRUE) |> PCA() |> 
  interpolate (newdata=rock[41:48,]) |> plot()

## -----------------------------------------------------------------------------
biplot(rock[1:40,], scale = TRUE) |> PCA() |> 
  interpolate (rock[41:48,]) |>
  newsamples (label = TRUE, label.side = "top", col = rainbow(10)) |> plot()

## -----------------------------------------------------------------------------
biplot(rock[,c(1,2,4)], scale = TRUE) |> PCA() |> 
  interpolate (newvariable =rock[,3]) |> plot()

## -----------------------------------------------------------------------------
biplot(rock[,c(1,2,4)], scale = TRUE) |> PCA() |> 
  interpolate (newvariable =rock[,3]) |> 
  newaxes(col="red",ticks = 50,X.new.names = "shape") |> plot()

## -----------------------------------------------------------------------------
biplot(rock[1:40,c(1,2,4)], scale = TRUE) |> PCA() |> 
  interpolate (newdata=rock[41:48,c(1,2,4)],newvariable =rock[1:40,3]) |> 
  newaxes(col="red",ticks = 100,X.new.names = "shape") |> plot()

## -----------------------------------------------------------------------------
out <- biplot(rock, scale = TRUE) |> PCA() |> 
         prediction (predict.samples = TRUE) |> plot()

## -----------------------------------------------------------------------------
summary(out)

## -----------------------------------------------------------------------------
out <- biplot(state.x77, scale = TRUE) |> PCA(group.aes = state.region, show.class.means = TRUE) |> 
         prediction (predict.means = 3:4, which = c("Income","Murder","Population")) |> plot()
summary(out)

## -----------------------------------------------------------------------------
biplot(state.x77, scale = TRUE) |> PCA(group.aes = state.division) |> reflect("x") |> plot()

## -----------------------------------------------------------------------------
biplot(state.x77, scale = TRUE) |> PCA(group.aes = state.division) |> reflect("y") |> plot()

## -----------------------------------------------------------------------------
biplot(state.x77, scale = TRUE) |> PCA(group.aes = state.division) |> rotate(100) |> plot()

## ----eval=FALSE---------------------------------------------------------------
#  biplot(state.x77,scaled = TRUE) |>
#    PCA() |>
#    samples(which=NULL) |>
#    density2D(which=1,col=c("white","purple","blue","cyan"),contours = TRUE,cuts = 20) |>
#    plot(zoom=TRUE)

## ----echo=FALSE---------------------------------------------------------------
biplot(state.x77,scaled = TRUE) |> 
  PCA() |> 
  samples(which=NULL) |>
  density2D(which=1,col=c("white","purple","blue","cyan"),contours = TRUE,cuts = 20) |> 
  plot()


a<-list(x=-2.499021,y=2.92864)
aa<-list(x=-1.486956,y=3.378447)

b<-list(x=1.867849,y=-2.356584)
bb<-list(x=2.767462,y=-2.112939)

text(aa,"Click here",pos=3)
text(bb,"Click here",pos=3)

arrows(aa$x,aa$y,a$x,a$y,length=0.125,lwd=2)
arrows(bb$x,bb$y,b$x,b$y,length=0.125,lwd=2)
polygon(c(a$x,a$x,b$x,b$x),c(a$y,b$y,b$y,a$y),col=adjustcolor("gray",0.6))


## ----echo=FALSE---------------------------------------------------------------
biplot(state.x77,scaled = TRUE) |> 
  PCA() |> 
  samples(which=NULL) |>
  density2D(which=1,col=c("white","purple","blue","cyan"),contours = TRUE,cuts = 20) |> 
  plot(xlim=c(a$x,b$x),ylim=c(b$y,a$y))

## -----------------------------------------------------------------------------
biplot(state.x77,scaled=TRUE) |> 
  PCA() |>
  translate_axes(delta = 0.02) |>
  plot(exp.factor=3)

