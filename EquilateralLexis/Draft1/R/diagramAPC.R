# Figure 1 syntax, all curves and angles are hand-made, so to speak, so the code can get sticky:

# this produces two diagrams meant to display the differences in dimensions
# of the right and equilateral triangles:
setwd("E:/CODE/EquilateralLexis")

#dev.off()
#dev.new(height=4,width=8)

# Cairo on Windows, cairoDevice on ubuntu, is the conclusion I came to...
#library(Cairo)
#CairoPDF(height=4,width=8,"Figs//Figure1.pdf")
library(cairoDevice)

Cairo_pdf("Figs/Figure1.pdf",height=4,width=8)
par(mar=c(1,1,1,1),xaxs="i",yaxs="i",mfrow=c(1,2))
plot(NULL,type="n",xlim=c(.6,2.4),ylim=c(.6,2.4),xlab="",ylab="",main="Standard",axes=FALSE,asp=1)
polygon(c(1,2,2),c(1,1,2))
polygon(c(1,2,1),c(1,2,2))
polygon(c(1.9,2,2,1.9),c(1,1,1.1,1.1)) # right angle sign
polygon(c(1,1.1,1.1,1),c(1.9,1.9,2,2)) 

# lower 45 degrees
x <- seq(0,pi/2,length=100)
xvals <- cos(x)*.1+1
yvals <- sin(x)*.1+1
lines(xvals,yvals)

# upper 45 degrees
x <- seq(pi,((3/2)*pi),length=100)
xvals <- cos(x)*.1+2
yvals <- sin(x)*.1+2
lines(xvals,yvals)

# equal angle markers!: (used later too)
rotationmat <- function(theta){
	matrix(c(cos(theta),sin(theta),-sin(theta),cos(theta)),ncol=2)
}
# tick 1
x1y1 <- rotationmat(pi/8) %*% c(.07,0)
x2y2 <- rotationmat(pi/8) %*% c(.13,0)
segments(x1y1[1]+1,x1y1[2]+1,x2y2[1]+1,x2y2[2]+1)
# tick 2
x1y1 <- rotationmat(3*pi/8) %*% c(.07,0)
x2y2 <- rotationmat(3*pi/8) %*% c(.13,0)
segments(x1y1[1]+1,x1y1[2]+1,x2y2[1]+1,x2y2[2]+1)
# tick 3
x1y1 <- rotationmat(9*pi/8) %*% c(.07,0)
x2y2 <- rotationmat(9*pi/8) %*% c(.13,0)
segments(x1y1[1]+2,x1y1[2]+2,x2y2[1]+2,x2y2[2]+2)
# tick 4
x1y1 <- rotationmat(11*pi/8) %*% c(.07,0)
x2y2 <- rotationmat(11*pi/8) %*% c(.13,0)
segments(x1y1[1]+2,x1y1[2]+2,x2y2[1]+2,x2y2[2]+2)
# 45 degrees
text(1.2,1.1,expression(45*degree),cex=.8)

# label sides and points
points(c(1,2,2,1),c(1,1,2,2),pch=19)
text(.95,.95,"A")
text(2.05,.95,"B")
text(2.05,2.05,"C")
text(.95,2.05,"D")
text(1.5,.95,1)
text(1.5,2.05,1)
text(.95,1.5,1)
text(2.05,1.5,1)
text(1.45,1.55,expression(sqrt(2)),srt=45)

# give point coordinates:
text(.95,.85,"(p, a)",cex=.8)
text(2.05,.85,"(p+1, a)",cex=.8)
text(2.05,2.15,"(p+1, a+1)",cex=.8)
text(.95,2.15,"(p, a+1)",cex=.8)
#--------------------------------------


#--------------------------------------
# now equilateral geometry
par(mar=c(1,1,1,1),xaxs="i",yaxs="i")
plot(NULL,type="n",xlim=c(.5,2.3),ylim=c(.45,2.25),xlab="",ylab="",main="Equilateral",axes=FALSE,asp=1)
ysc <- sqrt(3)/2
x1 <- c(1,2,2)-.5*c(0,0,1)
y1 <- ysc*c(1,1,2)
polygon(x1,y1)
x2 <- c(1,2,1)-.5*c(0,1,1)
y2 <- ysc*c(1,2,2)
polygon(x2,y2)
points(c(x1,x2[3]),c(y1,y2[3]),pch=19,xpd=T)
text(x1[1]-.05,y1[1]-.05,"A")
text(x1[2]+.05,y1[2]-.05,"B")
text(x1[3],y1[3]+.08,"C")
text(x2[3],y2[3]+.08,"D",xpd=T)

# label ABCD coords:
text(x1[1]-.05,y1[1]-.25,bquote(bgroup("(",paste("p-",phantom(.), frac(a,2),", ",frac(a*sqrt(3),2),sep=""),")")),cex=.8)
text(x1[2]+.05,y1[2]-.25,bquote(bgroup("(",paste("p+1-",phantom(.), frac(a,2),", ",frac(a*sqrt(3),2),sep=""),")")),xpd=TRUE,cex=.8)
text(x1[2]-.18,y1[3]+.28,bquote(bgroup("(",paste("p+1-",phantom(.), frac(a+1,2),", ",frac(paste("(a+1)")*sqrt(3),2),sep=""),")")),cex=.8)
text(x2[3]+.3,y2[3]+.28,bquote(bgroup("(",paste("p-",phantom(.), frac(a+1,2),", ",frac(paste("(a+1)")*sqrt(3),2),sep=""),")")),xpd=TRUE,cex=.8)


# angle arc segments:
makemeanarc <- function(thetas,xy){
	xys <- matrix(ncol=2,nrow=length(thetas))
	for (i in 1:length(thetas)){
		xys[i,] <- rotationmat(thetas[i])%*%xy
	}
	xys
}
# arc 1
thetas <- seq(0,2*pi/3,length=100)
arc1 <- makemeanarc(thetas,c(.1,0))
lines(arc1[,1]+1,arc1[,2]+ysc)
# arc 2
thetas <- seq(2*pi/3,pi,length=100)
arc1 <- makemeanarc(thetas,c(.1,0))
lines(arc1[,1]+2,arc1[,2]+ysc)
# arc 3
thetas <- seq(pi,5*pi/3,length=100)
arc1 <- makemeanarc(thetas,c(.1,0))
lines(arc1[,1]+1.5,arc1[,2]+2*ysc)
# arc 4
thetas <- seq(5*pi/3,2*pi,length=100)
arc1 <- makemeanarc(thetas,c(.1,0))
lines(arc1[,1]+.5,arc1[,2]+2*ysc,xpd=T)
# equal degree ticks:
# tick 1
x1y1 <- rotationmat(pi/6) %*% c(.07,0)
x2y2 <- rotationmat(pi/6) %*% c(.13,0)
segments(x1y1[1]+1,x1y1[2]+ysc,x2y2[1]+1,x2y2[2]+ysc,lwd=3)
segments(x1y1[1]+1,x1y1[2]+ysc,x2y2[1]+1,x2y2[2]+ysc,col="white")
# tick 2
x1y1 <- rotationmat(pi/2) %*% c(.07,0)
x2y2 <- rotationmat(pi/2) %*% c(.13,0)
segments(x1y1[1]+1,x1y1[2]+ysc,x2y2[1]+1,x2y2[2]+ysc,lwd=3)
segments(x1y1[1]+1,x1y1[2]+ysc,x2y2[1]+1,x2y2[2]+ysc,col="white")
# tick 3
x1y1 <- rotationmat(5*pi/6) %*% c(.07,0)
x2y2 <- rotationmat(5*pi/6) %*% c(.13,0)
segments(x1y1[1]+2,x1y1[2]+ysc,x2y2[1]+2,x2y2[2]+ysc,lwd=3)
segments(x1y1[1]+2,x1y1[2]+ysc,x2y2[1]+2,x2y2[2]+ysc,col="white")
# tick 4
segments(1.5,2*ysc-.07,1.5,2*ysc-.13,lwd=3)
segments(1.5,2*ysc-.07,1.5,2*ysc-.13,col="white")
# tick 5
x1y1 <- rotationmat(7*pi/6) %*% c(.07,0)
x2y2 <- rotationmat(7*pi/6) %*% c(.13,0)
segments(x1y1[1]+1.5,x1y1[2]+2*ysc,x2y2[1]+1.5,x2y2[2]+2*ysc,lwd=3)
segments(x1y1[1]+1.5,x1y1[2]+2*ysc,x2y2[1]+1.5,x2y2[2]+2*ysc,col="white")
# tick 6
x1y1 <- rotationmat(11*pi/6) %*% c(.07,0)
x2y2 <- rotationmat(11*pi/6) %*% c(.13,0)
segments(x1y1[1]+.5,x1y1[2]+2*ysc,x2y2[1]+.5,x2y2[2]+2*ysc,lwd=3)
segments(x1y1[1]+.5,x1y1[2]+2*ysc,x2y2[1]+.5,x2y2[2]+2*ysc,col="white")

# 60 degrees
text(1.2,ysc+.11,expression(60*degree),cex=.8)
# equal segment 1s
text(1.5,ysc-.05,1)
text(1.8,1.5*ysc+.05,1)
text(1.2,1.5*ysc+.05,1)
text(1,2*ysc+.05,1)
text(.7,1.5*ysc-.05,1)

# height sqrt(3)/2
text(1.5,ysc*1.5,expression(frac(sqrt(3),2)),cex=.8)
segments(1.5,1.57,1.5,1.45,lty=3)
segments(1.5,ysc+.02,1.5,1.14,lty=3)
dev.off()

# end of Figure1 code
