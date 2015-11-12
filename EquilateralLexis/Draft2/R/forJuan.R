
# Author: tim
###############################################################################
library(RColorBrewer)
library(raster) # will require rgeos!
library(ggplot2)
library(HMDHFDplus)
library(reshape2)
# a handy color ramp function, pulls from Brewer
ramp <- function(N,bp = "YlOrRd",rev.pal=FALSE){
	col.base <- RColorBrewer::brewer.pal(RColorBrewer::brewer.pal.info[bp,]$maxcolors,bp)
	if (rev.pal){
		col.base <- rev(col.base)
	}
	colorRampPalette(col.base,space="Lab")(N)
}
# get test matrix

myfortify <- function(p){
	do.call(rbind,lapply(p@polygons, function(x){
						ID <- x@ID
						xy <- lapply(x@Polygons, function(y){
									XY <- as.data.frame(y@coords)
									XY <- rbind(XY,NA)
									XY$area <- y@area
									XY$hole <- y@hole
									XY$Order <- 1:nrow(XY)
									XY
								})
						for (i in 1:length(xy)){
							xy[[i]]$ID <- ID
							xy[[i]]$group <- paste(ID,i,sep=".")
						}
						do.call(rbind,xy)
					}))
} 
# prelog if necessary, X is an AP matrix of data
LexisPoly <- function(X,breaks=NULL, colramp, equilateral = FALSE){
	if (is.null(breaks)){
		breaks <- pretty(X)
	}
	
	# age/year needed as metadata throughout
	Ages   			<- as.integer(rownames(X))
	Years  			<- as.integer(colnames(X))
	
	# get some contour lines, just in case. May need to do eq transform.
	Lines  			<- contourLines(Years+.5, Ages+.5, t(X), levels = breaks)
	# save, just in case
	if (equilateral){
		Lines <- lapply(Lines, function(x){
						x$x <- x$x - .5 * x$y
						x$y <- x$y * sqrt(3)/2
						x
					})
	}
	
	# turn it into a raster-class object
	r      			<- raster::raster(X[nrow(X):1,],
							xmn = min(Years), xmx = max(Years) + 1, 
							ymn = min(Ages), ymx = max(Ages) + 1)
	# (there is a special method to cut intervals for raster classes)
	z      			<- raster::cut(r, breaks) 
	# now combine cells within-intervals to single polygons
	p      			<- raster::rasterToPolygons(z, dissolve = TRUE)
	
	# now convert back into manageable data.frame (Easier to manipulate than s4 object)
	pl     			<- myfortify(p)
	
	# N colors-- if reverse ramp needed, then make ramp function work that way
	colors 			<- ramp(length(unique(pl$ID)))
	
	# ordering works because polygons formed in order of intervals, given by breaks
	pl$color 		<- colors[as.integer(pl$ID)]
	
    # now get plot order, plot in descending order of area..
	areas 			<- sapply(unique(pl$group), function(gp,pl){
							pl$area[pl$group == gp][1]
						}, pl = pl)
	
	# now we need the sort order, which for some reason doesn't work with order()
	areas 			<- sort(areas, decreasing = TRUE)
	orders 			<- 1:length(areas)
	names(orders) 	<- names(areas)
	# assign as plot order (plot biggest first)
	pl$plotOrder 	<- orders[pl$group]
	
	# order by plotOrder, so all polygons drawn simultaneously (NA-separated)
	pl 				<- pl[with(pl, order(plotOrder, Order)), ]
	
	# remove holes
	pl 				<- pl[!pl$hole, ]
	
	# adjust for equilateral, if necessary
	if (equilateral){
		pl$x <- pl$x - .5 * pl$y
		pl$y <- pl$y * sqrt(3) / 2
	}
	
	LexisP <- list(Polygons = pl, Contours = Lines, equilateral = equilateral, Years = Years, Ages = Ages)
	
	# set to special class for plotting
	class(LexisP) <- "LexisPoly"
	# return plot object
    LexisP
}

plot.LexisPoly <- function(x, contour = FALSE,...){
	equilateral <- x$equilateral
	
	plot(NULL, xlim = range(x$Polygons$x,na.rm=TRUE),ylim=range(x$Polygons$y,na.rm=TRUE),asp=TRUE,axes=FALSE,xlab="",ylab="")
	# color regions
	polygon(x$Polygons$x, x$Polygons$y, col = x$Polygons$color[x$Polygons$Order==1], ...)
	
	# contour lines
	if (contour){
		templines <- function(clines,...) {
			lines(clines[[2]], clines[[3]],...)
		}
		invisible(lapply(x$Contours, templines))
	}
	par(xpd=TRUE)
	# now draw axes. depends on eq
	if (! equilateral){
		# x axis, period
		yrticks <- x$Years - x$Years %% 10
		segments(min(x$Years),x$Ages[1],max(x$Years),x$Ages[1])
		segments(yrticks,x$Ages[1],yrticks,x$Ages[1]-1)
		# y axis, age
		ageticks <- x$Ages - x$Ages %% 10
		segments(min(x$Years),x$Ages[1],max(x$Years),x$Ages[1])
		segments(min(x$Years),ageticks,min(x$Years),ageticks)
	} else {
		
	}

}
names(LexisP)
X <- acast(readHMDweb("USA","mltper_1x1",us,pw),Age~Year,value.var="mx")

breakse10 <- 1/(10^(0:4))
breaksmid <- exp(log(breakse10)[-5] + diff(log(breakse10)) / 2)
breaks    <- sort(c(breaksmid,breakse10))

#pl 			  	<- pl[!pl$hole,]

LexisP <- LexisPoly(X,breaks,ramp,equilateral = FALSE)
plot(LexisP,border=NA,contour=TRUE)

