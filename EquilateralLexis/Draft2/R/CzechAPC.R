# This is the syntax behind Figure 2, which makes a standard and an equilateral
# Lexis surface in the same graphics device using Czech data from the HFD.

# data is loaded from the \Data\ folder
# 2 Lexis drawing functions are loaded from the \R\ folder
###############################################################################

# load in object DATA, which are all asfrTR rates from the HFD,
# age column is already numeric.
load("Data/asfrTR.RData")

# pick out CZE
CZE <- as.matrix(DATA[DATA$Code=="CZE",2:5])
# SqLexis and EqLexis want matrices, all columns already properly labeled.

# load in functions
source("R/EqLexisSpecial.R")
source("R/SqLexisSpecial.R")
library(cairoDevice)

# a custom HCL color ramp:
# very high but homogenous chroma value, = flashy enough to compete with HSV, but responsible with perception
# decrease in luminance values
# 

# N = how many intervals between the (6) given hues
myfxHCLramp <- function(H,C=95,L,N=5){
	# H and L must be of equal length
	colsi <- c()
	for (i in 1:(length(H)-1)){
		Hi <- seq(H[i],H[i+1],length=(N+1))
		Hi[Hi<0] <- Hi[Hi<0]+360
		colsi <- c(colsi,hcl(h=Hi,c=C,l=seq(L[i],L[i+1],length=(N+1)))[ifelse(i==1,1,2):(N+1)])
	}
	colsi
}

H <- seq(255,-60,length=6)
L <- seq(75,15,length=6) # L is luminance
L[1] <- 99
# plot(H,L,type='l',main="left side magenta, right side blues",xlab="Hue",ylab="luminance")

# finalize colors and breaks for plot
cols <- myfxHCLramp(H=H,C=98,L=L,N=50)
colsl <- myfxHCLramp(H=H,L=L,N=5)
brksl <- seq(0,.25,length=26)
brks <- seq(0,.25,length=251)

# some markers used to position legend components:
ys <- seq(0,44,length=length(brksl))+12
xl <- 1925; xr <- 1926.5
xlr <- 2010 ; xrr <- 2011.5

# had to make this into a function in order to easily try out different color ramps...
makefig2 <- function(cols,colsl,name){
	savepath <- paste("Figs/Figure2",name,".pdf",sep="")
	Cairo_pdf(savepath, width = 11, height = 13, pointsize = 8)
	#dev.new(height=13,width=11)
	par(mar=c(5,8,5,8))
	# crazy y limits are because the upper (standard) surface is shifted upward
	plot(NULL, type = "n", ylim = c(12,110), xlim = c(1930,2010), asp = 1,axes = FALSE, xlab = "",ylab = "")
	
	# render standard surface
	SqLexisSp(CZE,yshiftforpaper=57,breaks=brks,col=cols,legend = FALSE,cex.ax.lab=1)
	rect(1950,69,2010,113) # bounding box for surface region
	text(1942,57+35,"Age",cex=1.2)
	text(1980,61,"Period",cex=1.2)
	text(2015,100,"Cohort",srt=45,pos=4,xpd=TRUE,cex=1.2)
	
	# render equilateral surface
	EqLexisSp(CZE,breaks=brks,col=cols,legend=FALSE,cex.ax.lab=1,c.ax.lab.adj=c(.5,0),y.ax.lab.adj=c(0,-.5))
	# upper left legend
	rect(rep(xl,length(colsl)),ys[1:length(colsl)]+57,rep(xr,length(colsl)),ys[2:(length(colsl)+1)]+57,col=colsl,xpd=TRUE,border="transparent") # color bars
	rect(xl,min(ys)+57,xr,max(ys)+57) # legend rect
	segments(xr,c(0,.05,.1,.15,.2,.25)*44*4+69,xr+.5,c(0,.05,.1,.15,.2,.25)*44*4+69) # legend ticks
	text(xr+.5,c(0,.05,.1,.15,.2,.25)*44*4+69,c(">0.00","0.05","0.10","0.15","0.20","0.25"),pos=4) # legend labs
	text(xr+.5,115.5,"Fertility Rate",cex=1.2) # legend title
	text(xr+.5,66,"(scales identical)") # legend subtitle
	# lower right legend (identical)
	rect(rep(xlr,length(colsl)),ys[1:length(colsl)]-5,rep(xrr,length(colsl)),ys[2:(length(colsl)+1)]-5,col=colsl,xpd=TRUE,border="transparent") # color bars
	rect(xlr,min(ys)-5,xrr,max(ys)-5) # legend rect
	segments(xrr,c(0,.05,.1,.15,.2,.25)*44*4+7,xrr+.5,c(0,.05,.1,.15,.2,.25)*44*4+7) 
	text(xrr+.5,c(0,.05,.1,.15,.2,.25)*44*4+7,c(">0.00","0.05","0.10","0.15","0.20","0.25"),pos=4,xpd=TRUE) # legend labs
	text(xrr+.5,53.5,"Fertility Rate",cex=1.2) # legend title
	
	# some labels
	text(1950,120,"Standard Fertility Surface",cex=1.5,pos=4) # upper plot title
	text(1950,56,"Equilateral Fertility Surface",cex=1.5,pos=4) # lower plot title
	text(1928,25,"Age",cex=1.2)
	text(1980,3,"Period",srt=-60,xpd=TRUE,cex=1.2)
	text(1996,35,"Cohort",srt=60,pos=4,xpd=TRUE,cex=1.2)
	dev.off()# finalizes pdf
}
# a large number of these figures were produced using various parameterizations
# of RGB, HSV, HCL and Lab color ramps. This was easier due to he makefig2 function
# , which allowed me to experiment more with color.
makefig2(cols,colsl,"Figure2HCLd")

