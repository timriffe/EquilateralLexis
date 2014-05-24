



#install.packages("misc3d")
library(misc3d)
# Gotta find an excuse to implement this in R!
library(reshape2)
library(DemogBerkeley)

# get data matrix of Swedish population, distort dimensions, mark up. 
# Figure out how to superimpose red lines. 
pass <- userInput()
user <- userInput()
SWE  <- readHMDweb("SWE","Population5",username = user, password = pass)

# population matrix
PM      <- long2mat(SWE[SWE$Year %% 5 == 0 & SWE$Year <= 1875,],"Total1")

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

vTx1 <- x[-nrow(PM),-ncol(PM)]
vTy1 <- y[-nrow(PM),-ncol(PM)]
vTz1 <- PM[-nrow(PM),-ncol(PM)]
vT1 <- cbind(c(vTx1),c(vTy1),c(vTz1))

vTx2 <- x[-nrow(PM),-1]
vTy2 <- y[-nrow(PM),-1]
vTz2 <- PM[-nrow(PM),-1]
vT2 <- cbind(c(vTx2),c(vTy2),c(vTz2))

vTx3 <- x[-1,-1]
vTy3 <- y[-1,-1]
vTz3 <- PM[-1,-1]

vT3 <- cbind(c(vTx3),c(vTy3),c(vTz3))

#upperTx1 <- x[-nrow(PM),-ncol(PM)]
#upperTy1 <- y[-nrow(PM),-ncol(PM)]
#upperTz1 <- PM[-nrow(PM),-ncol(PM)]
#
#v1U <- cbind(c(upperTx1),c(upperTy1),c(upperTz1))
#
#upperTx2 <- x[-1,-1]
#upperTy2 <- y[-1,-1]
#upperTz2 <- PM[-1,-1]
#v2U <- cbind(c(upperTx2),c(upperTy2),c(upperTz2))

vTx4 <- x[-1,-ncol(PM)]
vTy4 <- y[-1,-ncol(PM)]
vTz4 <- PM[-1,-ncol(PM)]

vT4 <- cbind(c(vTx4),c(vTy4),c(vTz4))

v1 <- rbind(vT1, vT2)
v2 <- rbind(vT2, vT3)
v3 <- rbind(vT4, vT4)

TRI <- makeTriangles(v1, v2, v3, 
        color = "#FFFDD0", 
        color2 = NA, 
        alpha = 1,
        fill = TRUE, col.mesh = "#000000",
        smooth = 0, material = "default")
      
#      for(z in seq(-300,300,by=10)){
#        drawScene(TRI, screen=list(x=160, y=-110, z=z))
#        cat(z,"\n")
#        Sys.sleep(.4)
#      }
drawScene(TRI, 
  screen=list(x=255, y=145, z=0), 
  perspective = FALSE,
  aspect=c(1/(sqrt(3)/2),1),
  #lighting=perspLighting,
  distance=0)

lines3d() # <- use this to add cohort lines!!!
# try to approximate age lines across surface using gemoetry?
# level lines too? hmmm


# use add argument...

?lines3d