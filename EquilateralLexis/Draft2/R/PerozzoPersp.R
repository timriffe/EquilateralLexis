

#install.packages("misc3d")
library(misc3d)
# Gotta find an excuse to implement this in R!
library(reshape2)
library(DemogBerkeley)

# get data matrix of Swedish population, distort dimensions, mark up. 
# Figure out how to superimpose red lines. 
#pass <- userInput()
#user <- userInput()
SWE  <- readHMDweb("SWE","Population5",username = us, password = pw)
B    <- readHMDweb("SWE","Births",username = us, password = pw)
B    <- B[B$Year >= 1750 & B$Year < 1880, ]
BS   <- rev(tapply(B$Male, B$Year - B$Year %% 5, sum))
# population matrix
PM      <- long2mat(SWE[SWE$Year %% 5 == 1 & SWE$Year <= 1876,],"Male1")

# get 0-4 age group
PM[1, ] <- colSums(PM[1:2, ])
PM      <- PM[-c(2,23,24), ]
PM      <- PM[, ncol(PM):1]

#?surfaceTriangles

# equilateral coordinates:
Yr <- min(as.integer(colnames(PM)))
x  <- (col(PM) - 1) * 5 + Yr  - (((row(PM) - 1) * 5) / 2)
y  <- (row(PM) - 1) * 5 * sqrt(3) / 2

# to maintain grid, need to interpolate Z as well, always over the x direction:
# v1, v2 and v3 are the x,y,z coordinates of the verices of each triangle...

# T1 T2 T3 T4 are ll, lr, ur, ul corners of lexis square, for
# recycling nodes and reducing confusion

# lower left
vTx1 <- x[-nrow(PM),-ncol(PM)]
vTy1 <- y[-nrow(PM),-ncol(PM)]
vTz1 <- PM[-nrow(PM),-ncol(PM)]
vT1  <- cbind(c(vTx1),c(vTy1),c(vTz1))

#lower right
vTx2 <- x[-nrow(PM),-1]
vTy2 <- y[-nrow(PM),-1]
vTz2 <- PM[-nrow(PM),-1]
vT2  <- cbind(c(vTx2),c(vTy2),c(vTz2))

# upper right
vTx3 <- x[-1,-1]
vTy3 <- y[-1,-1]
vTz3 <- PM[-1,-1]
vT3  <- cbind(c(vTx3),c(vTy3),c(vTz3))

# upper left
vTx4 <- x[-1,-ncol(PM)]
vTy4 <- y[-1,-ncol(PM)]
vTz4 <- PM[-1,-ncol(PM)]
vT4  <- cbind(c(vTx4),c(vTy4),c(vTz4))

v1 <- rbind(vT1, vT2)
v2 <- rbind(vT2, vT3)
v3 <- rbind(vT4, vT4)

TRI <- makeTriangles(v1, v2, v3, 
        color = "#FFFFCC", 
        color2 = NA, 
        alpha = 1,
        fill = TRUE, col.mesh = NA,
        smooth = 0, material = "default")
# now think through the back panel, connecting
# ages 0-4 with the birth line. It's vertical, at
# a constant y = 0, and needs to be built of triangles.

# lower left

vTx1B <- x[1,-ncol(x)]
vTy1B <- 0
vTz1B <- PM[1,-ncol(PM)]
vT1B  <- cbind(c(vTx1B),c(vTy1B),c(vTz1B))

#lower right
vTx2B <- x[1,-1]
vTy2B <- 0
vTz2B <- PM[1,-1]
vT2B  <- cbind(c(vTx2B),c(vTy2B),c(vTz2B))

# upper right
vTx3B <- x[1,-1] 
vTy3B <- 0
vTz3B <- BS[-1]
vT3B  <- cbind(c(vTx3B),c(vTy3B),c(vTz3B))

# upper left
vTx4B <- x[1,-ncol(x)] 
vTy4B <- 0
vTz4B <- BS[-length(BS)]
vT4B  <- cbind(c(vTx4B),c(vTy4B),c(vTz4B))

Bv1 <- rbind(vT1B, vT1B)
Bv2 <- rbind(vT3B, vT2B)
Bv3 <- rbind(vT4B, vT3B)

BACK <- makeTriangles(Bv1, Bv2, Bv3, 
        color = "#FFFFCC", 
        color2 = NA, 
        alpha = 1,
        fill = TRUE, col.mesh = "gray",
        smooth = 0, material = "default")

# right side flat piece (needs to be edited)

# lower left (looking from side)
x[, 1]
vTx1R <- x[-1, 1]
vTy1R <- y[-1, 1]
vTz1R <- 0
vT1R  <- cbind(c(vTx1R),c(vTy1R),c(vTz1R))

#lower right
vTx2R <- x[-nrow(x), 1]
vTy2R <- y[-nrow(y), 1]
vTz2R <- 0
vT2R  <- cbind(c(vTx2R),c(vTy2R),c(vTz2R))

# upper right
vTx3R <- x[-nrow(x), 1]
vTy3R <- y[-nrow(y), 1]
vTz3R <- PM[-nrow(PM), 1]
vT3R  <- cbind(c(vTx3R),c(vTy3R),c(vTz3R))

# upper left
vTx4R <- x[-1, 1]
vTy4R <- y[-1, 1]
vTz4R <- PM[-1, 1]
vT4R  <- cbind(c(vTx4R),c(vTy4R),c(vTz4R))

Rv1 <- rbind(vT1R, vT2R)
Rv2 <- rbind(vT2R, vT3R)
Rv3 <- rbind(vT4R, vT4R)

Right <- makeTriangles(Rv1, Rv2, Rv3, 
        color = "#FFFFCC", 
        color2 = NA, 
        alpha = 1,
        fill = TRUE, col.mesh = NA,
        smooth = 0, material = "default")

par(mai=c(.1,.1,.1,.1), xaxs = "i", yaxs= "i")
PERR <- drawScene(list(BACK,TRI,Right), 
  screen=list(x=260, y=150, z=-2), 
  perspective = FALSE,
  aspect=c(1/(sqrt(3)/2),1),
  #lighting=perspLighting,
  distance=0)

drawScene(list(BACK,TRI,Right), 
        screen=list(x=265, y=155, z=-2), 
        perspective = TRUE,
        aspect=c(1/(sqrt(3)/2),1),
        distance = 0)

PERR <- drawScene(list(TRI,BACK), 
        screen=list(x=0, y=0, z=0), 
        perspective = FALSE,
        aspect=c(1/(sqrt(3)/2),1),
        #lighting=perspLighting,
        distance=0)
makeSegments <- function(.v1, .v2, col = "black",  ...){
    makeTriangles(v1 = .v2, v2 = .v1, v3 = .v1, col.mesh = col, fill = FALSE, color=NA, color2=NA,...)
}
SEG1 <- makeSegments(v1, v2, col = gray(.2))
SEG2 <- makeSegments(v1, v3, col = gray(.2))
SEG3 <- makeSegments(v3, v2, col = gray(.2))

par(mai=c(.2,.2,.2,.2))
drawScene(list(TRI,SEG1,SEG2),
        screen=list(x=255, y=145, z=0), 
        perspective = FALSE,distance=0)

#rethink segment vertices? reduce redundancies...






