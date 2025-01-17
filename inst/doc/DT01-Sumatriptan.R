## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=FALSE--------------------------------------------------------
library(rdecision)

## ----echo=TRUE----------------------------------------------------------------
# Time horizon
th <- as.difftime(48, units="hours")

# model variables for cost
c.sumatriptan <- 16.10
c.caffeine <- 1.32
c.ED <- 63.16
c.admission <- 1093

# Sumatriptan branch
ta <- LeafNode$new("A", utility=1.0, interval=th)
tb <- LeafNode$new("B", utility=0.9, interval=th)
c3 <- ChanceNode$new()
e1 <- Reaction$new(c3, ta, p=0.594, label="No recurrence")
e2 <- Reaction$new(c3, tb, p=0.406, cost=c.sumatriptan, 
                   label="Relieved 2nd dose")

td <- LeafNode$new("D", utility=0.1, interval=th)
te <- LeafNode$new("E", utility=-0.3, interval=th)
c7 <- ChanceNode$new()
e3 <- Reaction$new(c7, td, p=0.998, label="Relief")
e4 <- Reaction$new(c7, te, p=0.002, cost=c.admission, label="Hospitalization")

tc <- LeafNode$new("C", utility=-0.3, interval=th)
c4 <- ChanceNode$new()
e5 <- Reaction$new(c4, tc, p=0.920, label="Endures attack")
e6 <- Reaction$new(c4, c7, p=0.080, cost=c.ED, label="ED")

c1 <- ChanceNode$new()
e7 <- Reaction$new(c1, c3, p=0.558, label="Relief")
e8 <- Reaction$new(c1, c4, p=0.442, label="No relief")

# Caffeine/Ergotamine branch
tf <- LeafNode$new("F", utility=1.0, interval=th)
tg <- LeafNode$new("G", utility=0.9, interval=th)
c5 <- ChanceNode$new()
e9 <- Reaction$new(c5, tf, p=0.703, label="No recurrence")
e10 <- Reaction$new(c5, tg, p=0.297, cost=c.caffeine, 
                    label="Relieved 2nd dose")

ti <- LeafNode$new("I", utility=0.1, interval=th)
tj <- LeafNode$new("J", utility=-0.3, interval=th)
c8 <- ChanceNode$new()
e11 <- Reaction$new(c8, ti, p=0.998, label="Relief")
e12 <- Reaction$new(c8, tj, p=0.002, cost=c.admission, label="Hospitalization")
 
th <- LeafNode$new("H", utility=-0.3, interval=th)
c6 <- ChanceNode$new()
e13 <- Reaction$new(c6, th, p=0.920, label="Endures attack")
e14 <- Reaction$new(c6, c8, p=0.080, cost=c.ED, label="ED")

c2 <- ChanceNode$new()
e15 <- Reaction$new(c2, c5, p=0.379, label="Relief")
e16 <- Reaction$new(c2, c6, p=0.621, label="No relief")

# decision node
d1 <- DecisionNode$new("d1")
e17 <- Action$new(d1, c1, cost=c.sumatriptan, label="Sumatriptan")
e18 <- Action$new(d1, c2, cost=c.caffeine, label="Caffeine/Ergotamine")
 
# create lists of nodes and edges
V <- list(
  d1, c1, c2, c3, c4, c5, c6, c7, c8,
  ta, tb, tc, td, te, tf, tg, th, ti, tj
)
E <- list(
  e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, 
  e17, e18
)
# tree
DT <- DecisionTree$new(V,E)

## ----caption, echo=FALSE------------------------------------------------------
f1c  <- "Decision tree for the Sumatriptan model"

## ----draw,echo=FALSE,results="hide",fig.keep="all",fig.align="center",fig.cap=f1c----
DT$draw(border=TRUE)

## ----echo=FALSE---------------------------------------------------------------
local({
  RES <- DT$evaluate(by="path")
  RES$Benefit <- NULL
  RES$Run <- NULL
  knitr::kable(RES, row.names=FALSE, digits=c(NA,NA,2,2,2,4))
})

## ----echo=FALSE---------------------------------------------------------------
local({
  SUM <- DT$evaluate()
  SUM$Run <- NULL
  SUM$Probability <- NULL
  SUM$Benefit <- NULL
  knitr::kable(SUM, row.names=FALSE, digits=c(NA,2,5,5))
})

