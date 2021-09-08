## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  echo = FALSE,
  fig.keep = "last",
  fig.align = "center",
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=F------------------------------------------------------------
library("rdecision")
library("knitr")
library("pander")

## ----model-variables, echo=TRUE-----------------------------------------------
# transition counts
nAA <- 1251
nAB <- 350
nAC <- 116
nAD <- 17
nBB <- 731
nBC <- 512
nBD <- 15
nCC <- 1312
nCD <- 437
# Healthcare system costs
dmca <- 1701 # direct medical costs associated with state A
dmcb <- 1774 # direct medical costs associated with state B
dmcc <- 6948 # direct medical costs associated with state C
ccca <- 1055 # Community care costs associated with state A
cccb <- 1278 # Community care costs associated with state B
cccc <- 2059 # Community care costs associated with state C
# Drug costs
cAZT <- 2278 # zidovudine drug cost
cLam <- 2087 # lamivudine drug cost
# Treatment effect
RR <- 0.509 
# Discount rates
cDR <- 6 # annual discount rate, costs (%)
oDR <- 0 # annual discount rate, benefits (%)

## ----model, echo=TRUE---------------------------------------------------------
# create Markov states for monotherapy (zidovudine only)
sAm <- MarkovState$new("A", cost=dmca+ccca+cAZT)
sBm <- MarkovState$new("B", cost=dmcb+cccb+cAZT)
sCm <- MarkovState$new("C", cost=dmcc+cccc+cAZT)
sDm <- MarkovState$new("D", cost=0, utility=0)
# create transitions
tAAm <- Transition$new(sAm, sAm)
tABm <- Transition$new(sAm, sBm)
tACm <- Transition$new(sAm, sCm)
tADm <- Transition$new(sAm, sDm)
tBBm <- Transition$new(sBm, sBm)
tBCm <- Transition$new(sBm, sCm)
tBDm <- Transition$new(sBm, sDm)
tCCm <- Transition$new(sCm, sCm)
tCDm <- Transition$new(sCm, sDm)
tDDm <- Transition$new(sDm, sDm)
# construct the model
m.mono <- SemiMarkovModel$new(
  V = list(sAm, sBm, sCm, sDm),
  E = list(tAAm, tABm, tACm, tADm, tBBm, tBCm, tBDm, tCCm, tCDm, tDDm),
  discount.cost = cDR/100,
  discount.utility = oDR/100
)

## ----setprobs, echo=TRUE------------------------------------------------------
nA <- nAA + nAB + nAC + nAD
nB <- nBB + nBC + nBD
nC <- nCC + nCD
Pt <- matrix(
  c(nAA/nA, nAB/nA, nAC/nA, nAD/nA, 
         0, nBB/nB, nBC/nB, nBD/nB,
         0,      0, nCC/nC, nCD/nC,
         0,      0,      0,      1),
  nrow=4, byrow=TRUE, 
  dimnames=list(source=c("A","B","C","D"), target=c("A","B","C","D"))
)
m.mono$set_probabilities(Pt)

## ----caption, echo=FALSE------------------------------------------------------
f1c  <- paste("Figure 1. Markov model for comparison of HIV therapy.", 
              "A: 200 < cd4 < 500,", "B: cd4 < 200,", "C: AIDS,","D: Death.")

## ----draw,fig.cap=f1c,fig.asp=0.21,fig.keep="all"-----------------------------
#DOT <- m.mono$as_DOT()
#writeLines(DOT, con="mono.gv")
#system2(command="dot", args=c("-Tpng","-o","mono.png","mono.gv"))
knitr::include_graphics(path="mono.png")

## ----echo=FALSE---------------------------------------------------------------
DF <- m.mono$tabulate_states()
pander::pander(DF[,c("Name", "Cost")], justify="lr")
rm(DF)

## ----echo=FALSE---------------------------------------------------------------
TM <- m.mono$transition_probabilities()
pander::pander(TM, emphasize.rownames=FALSE, justify="lcccc")
rm(TM)

## ----monocycle, echo=TRUE-----------------------------------------------------
# create starting populations
N <- 1000
populations <- c(A = N, B = 0, C = 0, D = 0)
m.mono$reset(populations)
# run 20 cycles
MT.mono <- m.mono$cycles(ncycles=20, hcc.pop=FALSE, hcc.cost=FALSE)

## ----print_monocycle----------------------------------------------------------
keep <- c("Years", "A", "B", "C", "D", "Cost", "QALY")
pander::pander(MT.mono[,keep], row.names=FALSE, justify="rrrrrrr", 
               round=c(2,0,0,0,0,0,3))

## ----mon_results, echo=FALSE--------------------------------------------------
el.mono <- sum(MT.mono$QALY)
cost.mono <- sum(MT.mono$Cost)

## ----combo, echo=TRUE---------------------------------------------------------
# annual probabilities modified by treatment effect
pAB <- RR*nAB/nA
pAC <- RR*nAC/nC
pAD <- RR*nAD/nA
pBC <- RR*nBC/nB
pBD <- RR*nBD/nB
pCD <- RR*nCD/nC
# annual transition probability matrix
Ptc <- matrix(
  c(1-pAB-pAC-pAD,         pAB,     pAC, pAD, 
                0, (1-pBC-pBD),     pBC, pBD,
                0,           0, (1-pCD), pCD,
                0,           0,       0,   1),
  nrow=4, byrow=TRUE, 
  dimnames=list(source=c("A","B","C","D"), target=c("A","B","C","D"))
)
# create Markov states for combination therapy
sAc <- MarkovState$new("A", cost=dmca+ccca+cAZT+cLam)
sBc <- MarkovState$new("B", cost=dmcb+cccb+cAZT+cLam)
sCc <- MarkovState$new("C", cost=dmcc+cccc+cAZT+cLam)
sDc <- MarkovState$new("D", cost=0, utility=0)
# create transitions
tAAc <- Transition$new(sAc, sAc)
tABc <- Transition$new(sAc, sBc)
tACc <- Transition$new(sAc, sCc)
tADc <- Transition$new(sAc, sDc)
tBBc <- Transition$new(sBc, sBc)
tBCc <- Transition$new(sBc, sCc)
tBDc <- Transition$new(sBc, sDc)
tCCc <- Transition$new(sCc, sCc)
tCDc <- Transition$new(sCc, sDc)
tDDc <- Transition$new(sDc, sDc)
# construct the model
m.comb <- SemiMarkovModel$new(
  V = list(sAc, sBc, sCc, sDc),
  E = list(tAAc, tABc, tACc, tADc, tBBc, tBCc, tBDc, tCCc, tCDc, tDDc),
  discount.cost = cDR/100,
  discount.utility = oDR/100
)
# set the probabilities
m.comb$set_probabilities(Ptc)

## ----tmcombo, echo=FALSE------------------------------------------------------
TM <- m.comb$transition_probabilities()
pander::pander(TM, emphasize.rownames=FALSE, justify="lcccc")
rm(TM)

## ----combo_run, echo=TRUE-----------------------------------------------------
# run combination therapy model for 2 years
populations <- c('A'=N, 'B'=0, 'C'=0, 'D'=0)
m.comb$reset(populations)
# run 2 cycles
MT.comb <- m.comb$cycles(2, hcc.pop=FALSE, hcc.cost=FALSE)
# feed populations into mono model & reset cycle counter and time
populations <- m.comb$get_populations()
m.mono$reset(
  populations, 
  icycle=as.integer(2), 
  elapsed=as.difftime(365.25*2, units="days")
)
# and run model for next 18 years
MT.comb <- rbind(
  MT.comb, m.mono$cycles(ncycles=18, hcc.pop=FALSE, hcc.cost=FALSE)
)

## ----echo=F-------------------------------------------------------------------
keep <- c("Years", "A", "B", "C", "D", "Cost", "QALY")
pander::pander(MT.comb[,keep], row.names=FALSE, justify="rrrrrrr", 
               round=c(2,0,0,0,0,0,3))

## ----ICER, echo=FALSE---------------------------------------------------------
el.comb <- sum(MT.comb$QALY)
cost.comb <- sum(MT.comb$Cost)
icer <- (cost.comb-cost.mono)/(el.comb-el.mono)

