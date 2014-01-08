# script written in order to look for instances of cohort slope understatement in fertility rates.
# silver bullet for understatement I guess would be a case of a cohort slope being steeper than
# a period slope, yet appearing the other way around
#sqrt(2) # is the exaggeration threshold

# need VV rates and RR rates:
# so, calculate first derivatives for cohorts and periods.
# multiply cohort slopes by sqrt(2) 
# compare in nearby regions.
#library(devtools)
#install_github("DemogBerkeley", subdir = "DemogBerkeley", username = "UCBdemography")
library(DemogBerkeley)
RR <- readHFD("/home/triffe/DATA/HFD/asfrRR.txt")
AC <- readHFD("/home/triffe/DATA/HFD/asfrVH.txt")

# find an approximate year
AC$Year <- AC$Cohort + AC$Age + 1

# that will give us a point on which to center comparisons.
# now we want the first derivative, which we can take as the 
# simple first difference (since we'll do a rolling average in any case):

library(data.table)
AC <- data.table(AC)
AC[,Diff := diff(c(0,ASFR)), by = c("Code","Cohort")]
AC <- as.data.frame(AC)
RR <- data.table(RR)
RR[,Diff := diff(c(0,ASFR)), by = c("Code","Year")]
RR <- as.data.frame(RR)

# append the visual bias: (slopes are decreased because x stretches)
AC$Distort <- AC$Diff / sqrt(2)
#

# OK, now find an extreme example.
CZE <- AC[AC$Code == "CZE", ]
library(reshape2)
Distort <- acast(CZE,Age~Year,value.var = "Distort")
ACdiff  <- acast(CZE,Age~Year,value.var = "Diff")
APdiff  <- acast(RR[RR$Code == "CZE", ],Age~Year,value.var = "Diff")[,-c(1:2)]
ACdiff  <- ACdiff[,colSums(!is.na(ACdiff)) != 0][,-1]
Distort <- Distort[,colSums(!is.na(Distort)) != 0][,-1]
library(LexisUtils)
graphics.off()
dev.new(height=5,width=6)
LexisMap(APdiff,log=FALSE,main="AP",zlim=c(0,.09))
dev.new(height=5,width=6)
LexisMap(ACdiff,log=FALSE,main = "AC",zlim=c(0,.09))
dev.new(height=5,width=6)
LexisMap(Distort,log=FALSE, main = "AC distort",zlim=c(0,.09))

# King avg 
getKingMean <- function(M){
  (M[2:(nrow(M)-1),2:(ncol(M)-1)] + 
      # top
      M[1:(nrow(M)-2),2:(ncol(M)-1)] +
      # bottom
      M[3:nrow(M),2:(ncol(M)-1)] +
      # left 
      M[2:(nrow(M)-1),1:(ncol(M)-2)] +
      # right
      M[2:(nrow(M)-1),3:ncol(M)] +
      # top left
      M[1:(nrow(M)-2),1:(ncol(M)-2)] +
      # bottom left
      M[3:nrow(M),1:(ncol(M)-2)] +
      # top right
      M[1:(nrow(M)-2),3:ncol(M)] +
      # bottom right
      M[3:nrow(M),3:ncol(M)] ) / 9
}


# function to display areas with order-switching

ShowMajorDistortions <- function(Code = "CZE", RR, AC){
  Distort       <- acast(AC[AC$Code == Code, ],Age~Year,value.var = "Distort")
  ACdiff        <- acast(AC[AC$Code == Code, ],Age~Year,value.var = "Diff")
  APdiff        <- acast(RR[RR$Code == Code, ],Age~Year,value.var = "Diff")[,-c(1:2)]
  ACdiff        <- ACdiff[,colSums(!is.na(ACdiff)) != 0][,-1]
  Distort       <- Distort[,colSums(!is.na(Distort)) != 0][,-1]
  APking        <- getKingMean(APdiff)
  ACking        <- getKingMean(ACdiff)
  Distrortking  <- getKingMean(Distort)
  
  G <- ACking > APking & Distrortking < APking
  L <- ACking < APking & Distrortking > APking
  
  LexisMap(G - L, log = FALSE)
  
}


ShowMajorDistortions("CZE",RR, AC)

# well this works fine, now it'll just be a matter of drawing a border around the affected areas.
# this can be done with single segments. Will require matrix trickery and thought. Hmmm.


