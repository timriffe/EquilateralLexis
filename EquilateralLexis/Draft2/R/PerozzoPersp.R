




# Gotta find an excuse to implement this in R!
library(reshape2)
library(DemogBerkeley)

# get data matrix of Swedish population, distort dimensions, mark up. 
# Figure out how to superimpose red lines. 
pass <- userInput()
user <- userInput()
SWE  <- readHMDweb("SWE","Population5",username = user, password = pass)

# population matrix
PM      <- long2mat(SWE[SWE$Year %% 5 == 0,],"Total1")

# get 0-4 age group
PM[1, ] <- colSums(PM[1:2, ])
PM      <- PM[-2, ]
PM      <- rbind(PM,0)

# equilateral coordinates:
Yr <- min(as.integer(colnames(PM)))
x  <- (col(PM) - 1) * 5 + Yr  - (((row(PM) - 1) * 5) / 2)
y  <- (row(PM) - 1) * 5 * sqrt(3) / 2

# to maintain grid, need to interpolate Z as well, always over the x direction:
# v1, v2 and v3 are the x,y,z coordinates of the verices of each triangle...

lowerTx1 <- x[-nrow(PM),-ncol(PM)]
lowerTy1 <- y[-nrow(PM),-ncol(PM)]
lowerTz1 <- PM[-nrow(PM),-ncol(PM)]
v1L <- cbind(c(lowerTx1),c(lowerTy1),c(lowerTz1))

lowerTx2 <- x[-nrow(PM),-1]
lowerTy2 <- y[-nrow(PM),-1]
lowerTz2 <- PM[-nrow(PM),-1]
v2L <- cbind(c(lowerTx2),c(lowerTy2),c(lowerTz2))

lowerTx3 <- x[-1,-1]
lowerTy3 <- y[-1,-1]
lowerTz3 <- PM[-1,-1]
v3L <- cbind(c(lowerTx3),c(lowerTy3),c(lowerTz3))

upperTx1 <- x[-nrow(PM),-ncol(PM)]
upperTy1 <- y[-nrow(PM),-ncol(PM)]
upperTz1 <- PM[-nrow(PM),-ncol(PM)]
v1U <- cbind(c(upperTx1),c(upperTy1),c(upperTz1))

upperTx2 <- x[-1,-1]
upperTy2 <- y[-1,-1]
upperTz2 <- PM[-1,-1]
v2U <- cbind(c(upperTx2),c(upperTy2),c(upperTz2))

upperTx3 <- x[-1,-ncol(PM)]
upperTy3 <- y[-1,-ncol(PM)]
upperTz3 <- PM[-1,-ncol(PM)]
v3U <- cbind(c(upperTx3),c(upperTy3),c(upperTz3))

v1 <- rbind(v1U, v1L)
v2 <- rbind(v2U, v2L)
v3 <- rbind(v3U, v3L)

TRI <- makeTriangles(v1, v2, v3, 
        color = "#FFFDD0", 
        color2 = NA, 
        alpha = 1,
        fill = TRUE, col.mesh = "#000000",
        smooth = 0, material = "default")
drawScene.rgl(TRI, scale = FALSE)

Vall <- rbind(v1,v2,v3)
TRI2 <- surfaceTriangles(Vall[,1], Vall[,3], Vall[,3], color="#FFFDD050")
drawScene.rgl(vtri, scale = FALSE)
vtri <- local({
            z <- 2 * volcano
            x <- 10 * (1:nrow(z))
            y <- 10 * (1:ncol(z))
            surfaceTriangles(x, y, z, color="green3")
        })
drawScene.rgl(vtri, scale = FALSE)
drawScene(vtri, screen=list(x=40, y=-40, z=-135), scale = FALSE)
drawScene(vtri, screen=list(x=-, z=-135), scale = FALSE,
        perspective = TRUE)
drawScene(vtri, screen=list(x=40, y=-40, z=-135), scale = FALSE,
        perspective = TRUE, depth = 0.4)
#install.packages("misc3d")
library(misc3d)
surfaceTriangles(TRI)

x <- seq(-10, 10, length= 30)
y <- x
f <- function(x, y) { r <- sqrt(x^2+y^2); 10 * sin(r)/r }
z <- outer(x, y, f)
z[is.na(z)] <- 1
op <- par(bg = "white")
persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue")
persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue",
        ltheta = 120, shade = 0.75, ticktype = "detailed",
        xlab = "X", ylab = "Y", zlab = "Sinc( r )"
) -> res
round(res, 3)
?persp
# (2) Add to existing persp plot - using trans3d() :

xE <- c(-10,10); xy <- expand.grid(xE, xE)
points(trans3d(xy[,1], xy[,2], 6, pmat = res), col = 2, pch = 16)
lines (trans3d(x, y = 10, z = 6 + sin(x), pmat = res), col = 3)

phi <- seq(0, 2*pi, len = 201)
r1 <- 7.725 # radius of 2nd maximum
xr <- r1 * cos(phi)
yr <- r1 * sin(phi)
lines(trans3d(xr,yr, f(xr,yr), res), col = "pink", lwd = 2)
## (no hidden lines)

# (3) Visualizing a simple DEM model

z <- 2 * volcano        # Exaggerate the relief
x <- 10 * (1:nrow(z))   # 10 meter spacing (S to N)
y <- 10 * (1:ncol(z))   # 10 meter spacing (E to W)
## Don't draw the grid lines :  border = NA
par(bg = "slategray")
persp(x, y, z, theta = 135, phi = 30, col = "green3", scale = FALSE,
        ltheta = -120, shade = 0.75, border = NA, box = FALSE)

# (4) Surface colours corresponding to z-values

par(bg = "white")
x <- seq(-1.95, 1.95, length = 30)
y <- seq(-1.95, 1.95, length = 35)
z <- outer(x, y, function(a, b) a*b^2)
nrz <- nrow(z)
ncz <- ncol(z)
# Create a function interpolating colors in the range of specified colors
jet.colors <- colorRampPalette( c("blue", "green") )
# Generate the desired number of colors from this palette
nbcol <- 100
color <- jet.colors(nbcol)
# Compute the z-value at the facet centres
zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
# Recode facet z-values into color indices
facetcol <- cut(zfacet, nbcol)
persp(x, y, z, col = color[facetcol], phi = 30, theta = -30)

par(op)









