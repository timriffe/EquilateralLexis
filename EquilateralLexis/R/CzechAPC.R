
# Author: triffe
###############################################################################

# load in object DATA, which are all asfrTR rates from the HMD,
# i.e. more countries. 
load("Data/asfrTR.RData")

# DATA contains
# pick out CZE
CZE <- as.matrix(DATA[DATA$Code=="CZE",2:5])

# SqLexis and EqLexis want matrices, all columns already properly labeled.

# I think it will be a good idea to guarantee that these end up
# on the same page, using mfrow=c(2,1)
# otherwise it might be hard to compare them
source("R/EqLexisSpecial.R")
source("R/SqLexisSpecial.R")
library(cairoDevice)
# value breaks
brks <- seq(0,.25,length=251)
colramp <- grDevices:::colorRampPalette(c("white","cadetblue2","blue","lightgreen","green","lawngreen","yellow","orange","red","magenta"))
cols <- colramp(250) # a discrete vector of colors chosen froma  continuous color ramp

# custom legend coords
ys <- seq(0,44,length=251)+57+12
xl <- 1925; xr <- 1926.5

Cairo_pdf("Figs/Figure3.pdf", width = 11, height = 13, pointsize = 8)
#dev.new(height=13,width=11)
par(mar=c(5,8,5,8))
plot(NULL, type = "n", ylim = c(12,110), xlim = c(1930,2010), asp = 1,axes = FALSE, xlab = "",ylab = "")
SqLexisSp(CZE,yshiftforpaper=57,breaks=brks,col=cols,legend = FALSE,cex.ax.lab=1)
rect(1950,69,2010,113) # bounding box for surface region
EqLexisSp(CZE,breaks=brks,col=cols,legend=FALSE,cex.ax.lab=1,c.ax.lab.adj=c(.5,0),y.ax.lab.adj=c(0,-.5))
rect(rep(xl,250),ys[1:250],rep(xr,250),ys[2:251],col=cols,xpd=TRUE,border="transparent") # color bars
rect(xl,min(ys),xr,max(ys)) # legend rect
segments(xr,c(0,.05,.1,.15,.2,.25)*44*4+69,xr+.5,c(0,.05,.1,.15,.2,.25)*44*4+69) # legend ticks
text(xr+.5,c(0,.05,.1,.15,.2,.25)*44*4+69,c("0.00","0.05","0.10","0.15","0.20","0.25"),pos=4) # legend labs
text(xr+.5,115.5,"APCSFR",cex=1.5) # legend title
text(xr+.5,66,"(scale for both plots)") # legend subtitle
text(1950,120,"Standard Fertility Surface",cex=1.5,pos=4) # upper plot title
text(1950,56,"Equilateral Fertility Surface",cex=1.5,pos=4) # lower plot title
dev.off()


