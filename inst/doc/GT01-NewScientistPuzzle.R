## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=FALSE--------------------------------------------------------
library("rdecision")
library("grid")

## ----diagram, echo=FALSE, results='hide', fig.keep='last', fig.align='center'----
# new page
grid.newpage()
# functions to transform coordinates and distances in graph space (0:300) 
# to grid space (cm)
fig.size <- dev.size("cm")
scale <- max(300/fig.size[1], 300/fig.size[2])
gx <- function(x) {
  xcm <- fig.size[1]/2 + (x-150)/scale
  return(xcm)
}
gy <- function(y) {
  ycm <- fig.size[2]/2 + (y-150)/scale
  return(ycm)
}
gd <- function(d) {
  dcm <- d/scale
  return(dcm)
}
# grid
for (x in seq(50,250,50)) {
  grid.move.to(x=unit(gx(x),"cm"), y=unit(gy(50),"cm"))
  grid.line.to(x=unit(gx(x),"cm"), y=unit(gy(250),"cm"), gp=gpar(lwd=2))
}
for (y in seq(50,250,50)) {
  grid.move.to(x=unit(gx(50),"cm"), y=unit(gy(y),"cm"))
  grid.line.to(x=unit(gx(250),"cm"), y=unit(gy(y),"cm"), gp=gpar(lwd=2))
}
grid.text(label="A", x=unit(gx(45),"cm"), y=unit(gy(255),"cm"), 
          gp=gpar(fontsize=14))
grid.text(label="B", x=unit(gx(255),"cm"), y=unit(gy(45),"cm"),
          gp=gpar(fontsize=14))
# restaurants
BB <- data.frame(
  x0 = c(150,100,210,160,250,110,50),
  y0 = c(60,110,100,150,160,200,210),
  x1 = c(150,100,240,190,250,140,50),
  y1 = c(90,140,100,150,190,200,240)
)
apply(BB, MARGIN=1, function(r){
  grid.move.to(x=unit(gx(r["x0"]),"cm"), y=unit(gy(r["y0"]),"cm"))
  grid.line.to(x=unit(gx(r["x1"]),"cm"), y=unit(gy(r["y1"]),"cm"), 
               gp=gpar(col="red", lwd=6, lend="square"))
})

## ----construct-graph, echo=TRUE-----------------------------------------------
# create vertices
V <- list()
for (i in 1:5) {
  for (j in 1:5) {
    V <- c(V, Node$new(paste("N",i,j,sep="")))
  }
}
# create edges
E <- list()
for (i in 1:5) {
  for (j in 1:4) {
    E <- c(E, Arrow$new(V[[5*(i-1)+j]], V[[5*(i-1)+j+1]], paste("H",i,j,sep="")))
  }
} 
for (i in 1:4) {
  for (j in 1:5) {
    E <- c(E, Arrow$new(V[[5*(i-1)+j]], V[[5*i+j]], paste("V",i,j,sep="")))
  }
} 
# create graph
G <- Digraph$new(V,E)

## ----findpaths, echo=TRUE-----------------------------------------------------
# get all paths from A to B
A <- V[[1]]
B <- V[[25]]
P <- G$paths(A,B)
# convert paths to walks
W <- lapply(P,function(p){G$walk(p)})
# count and tabulate how many special edges each walk traverses
BB <- c("V11", "H22", "V25", "H33", "V32", "H44", "V43")
nw <- sapply(W, function(w) {
  lv <- sapply(w, function(e) {e$label() %in% BB}) 
  return(sum(lv))
})
# tabulate 
ct <- as.data.frame(table(nw))

## ----echo=FALSE, results='markdown'-------------------------------------------
names(ct) <- c("n", "frequency")
knitr::kable(ct)

