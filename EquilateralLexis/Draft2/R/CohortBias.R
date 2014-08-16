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

ShowMajorDistortions <- function(Code = "CZE", RR, AC, plot = TRUE){
  Distort       <- acast(AC[AC$Code == Code, ],Age~Year,value.var = "Distort")
  ACdiff        <- acast(AC[AC$Code == Code, ],Age~Year,value.var = "Diff")
  APdiff        <- acast(RR[RR$Code == Code, ],Age~Year,value.var = "Diff")[,-c(1:2)]
  ACdiff        <- ACdiff[,colSums(!is.na(ACdiff)) != 0][,-1]
  Distort       <- Distort[,colSums(!is.na(Distort)) != 0][,-1]
  APking        <- getKingMean(APdiff)
  ACking        <- getKingMean(ACdiff)
  Distortking  <- getKingMean(Distort)
  
  G <- ACking > APking & Distortking < APking
  L <- ACking < APking & Distortking > APking
  if (plot){
      LexisMap(G - L, log = FALSE)
  }
  
  invisible(list(G=G,L=L))
}

polygon.list <- function(PolyList, ...){
    lapply(PolyList, function(X, ...){
              polygon(x=X[,"x"],y=X[,"y"],...)  
            },...)
    invisible(NULL)
}


DistortedPolygonList <- function(Code = "CZE", RR, AC){
    require(sp)
    require(rgeos)
    
    OrderFlop <- ShowMajorDistortions(Code, RR, AC, FALSE)
    
    MAT     <- OrderFlop$G | OrderFlop$L
    Years   <- as.integer(colnames(MAT))
    Ages    <- as.integer(rownames(MAT))
    x5      <- x4 <- x1 <- col(MAT)[MAT] + min(Years)
    x2      <- x1 - 1
    x3      <- x2
    y5      <- y2 <- y1 <- row(MAT)[MAT] + min(Ages)
    y3      <- y4 <- y1 - 1
    
    x       <- cbind(x1, x2, x3, x4, x5)
    y       <- cbind(y1, y2, y3, y4, y5)
    
    Rects <- SpatialPolygons(lapply(1:nrow(x), function(i,x,y){
                        Polygons(list(Polygon(cbind(x=x[i,],y=y[i,]),hole=FALSE)),i)
                        
                    },x=x,y=y))
    
    PolyList <- lapply(gUnionCascaded(Rects)@polygons[[1]]@Polygons,function(X){
                if (!X@hole)
                    X@coords
            })
    PolyList
}

PolyList <- DistortedPolygonList(Code = "CZE", RR, AC)
APrates <- acast(RR[RR$Code == "CZE", ],Age~Year,value.var = "ASFR")
library(RColorBrewer)
library(grDevices)
yrs   <- as.integer(colnames(APrates))
ages  <- as.integer(rownames(APrates))
image(yrs+.5,ages+.5,t(APrates),
        col = colorRampPalette(rev(brewer.pal(9, "Spectral")), space = "Lab")(100),
        breaks=seq(0,.25,by=.0025),
        ylim=range(ages)+c(0,1),xlim=range(yrs)+c(0,1), asp = 1)
contour(yrs+.5,ages+.5,t(APrates),breaks=seq(0,.25,by=.025),add=TRUE)
polygon.list(PolyList, col = "#55555530",border="magenta")

