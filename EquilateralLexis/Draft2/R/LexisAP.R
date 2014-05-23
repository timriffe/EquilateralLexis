
library(DemogBerkeley)
library(LexisUtils)
library(reshape2)
user <- userInput()
# wait to do above before running next line
pw <- userInput()
mltper <- readHMDweb("USA","mltper_1x1",user,pw)

mx <- acast(mltper,Age~Year,value.var="mx")
LexisMap(mx)
templines <- function(clines,...) {
    lines(clines[[2]], clines[[3]],...)
}
x.5 <- as.integer(colnames(mx))+.5
y.5 <- as.integer(rownames(mx))+.5
levsmain <- c(1e-5,1e-4,1e-3,1e-2,1e-1,1)



levs <- exp(approx(x=1:length(levsmain),y=log(levsmain),xout=seq(1,length(levsmain),by=.25))$y)
line.list <- contourLines(x=x.5, y=y.5, t(mx),levels=levs)

cols <- colorRampPalette(rev(RColorBrewer::brewer.pal(9, "Spectral")), space = "Lab")

image(x.5,y.5,t(mx),xlim=c(1933,2011),ylim=c(0,111), col=cols(length(levs)-1), breaks=levs,useRaster=TRUE)
invisible(lapply(line.list, templines))

mxac <- AP2AC(mx)
range(colnames(mxac))
xcs <- 1822:2009 + .5
line.list <- contourLines(x=xcs, y=y.5, t(mxac),levels=levs)
image(xcs,y.5,t(mxac),xlim=c(1822,2010),ylim=c(0,111), col=cols(length(levs)-1), breaks=levs,useRaster=TRUE)
invisible(lapply(line.list, templines))

# now for the equilateral translation:


x0 <- col(mx) + 1932
y0 <- (row(mx) - 1)
x0 <- c(x0 - .5 * y0)
y0 <- c(y0 * sqrt(3)/2)
lineseq <- contourLines(x=x.5, y=y.5, t(mx),levels=levs)
lineseq <- lapply(lineseq, function(X){
            X$x <- X$x - .5 * X$y
            X$y <- X$y * sqrt(3)/2
            X
        })
x <- c(rbind(x0,x0+1,x0+.5,x0-.5,NA))
y <- c(rbind(y0,y0,y0+sqrt(3)/2,y0+sqrt(3)/2,NA))
colsEq <- c(as.character(cut(mx,breaks=levs,labels=cols(length(levs)-1))))

par(mai=c(0,0,0,0),xaxs="i",yaxs="i")
plot(NULL,type="n",xlim=c(1880,2010),ylim=c(0,111*sqrt(3)/2),axes=FALSE,xlab="",ylab="",asp=1)
polygon(x,y,col=colsEq,border=NA)
invisible(lapply(lineseq, templines,col="#55555540"))











