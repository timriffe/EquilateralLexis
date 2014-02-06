

# a figure to compare AP and AC in a line graph:

library(DemogBerkeley)

DAT <- readHMDweb("USA","mltper_1x1")

mx <- DAT$mx[DAT$Year == 2000]
xp <- 0:110
xc <- 0:110 * sqrt(2)

pdf("/home/triffe/git/EquilateralLexis/EquilateralLexis/Draft2/Figs/LineGraph.pdf",height=5,width=5)
par(xpd=TRUE,mai=c(.5,.5,.5,.5))
plot(xp, mx, type='l',log="y",axes=FALSE,xlim=c(0,150), col=gray(.2),lwd=3,xlab="",ylab="")
lines(xc, mx, col = "black")
segments(0,1e-4,110,1e-4,lwd=3,col=gray(.2))
segments(xp[xp%%10==0],1e-4,xp[xp%%10==0],.9e-4,col=gray(.2))
text(xp[xp%%10==0],1e-4,xp[xp%%10==0],cex=.7,pos=1)
segments(0,1,max(xc),1)
segments(xc[xp%%10==0],1,xc[xp%%10==0],1.1)
text(xc[xp%%10==0],1,xp[xp%%10==0],cex=.7,pos=3)
text(60,2.2,"Age on cohort axis")
text(60,.5e-4,"Age on period axis")
text(-5,.01,"log(m(x))",srt=90)
dev.off()