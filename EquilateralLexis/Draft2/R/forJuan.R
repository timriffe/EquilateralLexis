
# Author: tim
###############################################################################
library(RColorBrewer)
library(raster) # will require rgeos, which is an external program!
library(ggplot2)
library(HMDHFDplus)
library(reshape2)
library(spatstat)
library(colorspace)
# TR
# Many of these functions are recycled/retooled, from stuff used in previous papers and projects.
# I think I have 3 or 4 absolutely different versions of LexRefN()! This one just returns a list.
# with the x and y arguments

#'
#' @title LexRefN gives the x,y coordinates for Lexis reference lines in several configs
#' 
#' @description These coordinates are used for an optional overlay function for Lexis map plots.
#' 
#' @param ages vector of ages, e.g. \code{0:110}. These are y values.
#' @param years vector of years, e.g. \code{1900:2010}. These are x values.
#' @param N. age interval. Default of 5.
#' @param increasing logical. Default \code{TRUE}. \code{TRUE} gives upward diagonals, \code{FALSE} gives downward diagonals.
#' @param equilateral logical. Default \code{FALSE}. Do we want 60 degree or 90/45 degree angles?
#' 
#' @export
#' 

LexRefN <- function(ages, years, N = 5, increasing = TRUE, equilateral = FALSE){
	# vertical
	#par(mai=c(.5, .5, .5, .5), xaxs = "i", yaxs = "i")
	#plot(NULL, xlim = c(1820,2060), ylim = c(0,100), axes = FALSE, ylab = "", xlab = "", asp = 1)
	#LexRefN(0:100, 1900:2000, col = "#A5A5A5", N = 20, xpd=TRUE)
	# ages<-0:100; years <- 1900:2000; col = "#A5A5A5"; N = 20; equilateral <- TRUE; Chrono = TRUE
	# Chrono = FALSE
	minA      <- min(ages)
	maxA      <- max(ages)
	yearshift <- ifelse(equilateral, .5, 0) * ifelse(increasing, 1, -1)
	agemult   <- ifelse(equilateral, sqrt(3) / 2, 1)
	
	# vertical segments:
	Vertical <- list(x1 = years[years %% N == 0] - minA * yearshift,
					y1 = min(ages) * agemult,
					x2 = years[years %% N == 0] - maxA * yearshift,
					y2 = max(ages) * agemult
				)
	
	
	# horizontal lines
	Horizontal <- list(x1=min(years) - ages[ages %% N == 0] * yearshift,
					y1=ages[ages %% N == 0] * agemult,
					x2=max(years) - ages[ages %% N == 0] * yearshift,
					y2=ages[ages %% N == 0] * agemult )
	# diag cohort references, bit more tricky:
	l.years <- (min(years) - ages) 
	coh.ext <- range(c(l.years[l.years %% N == 0], years[years %% N == 0]))
	cohs    <- seq(coh.ext[1], coh.ext[2], by = N)
	
	# bottom, left:
	xl  <- cohs + min(ages)
	yb  <- rep(min(ages), length(cohs))
	yb[xl < min(years)] <- yb[xl < min(years)] + min(years) - xl[xl < min(years)]
	xl[xl < min(years)] <- min(years)
	
	# top, right:
	xr  <- cohs + max(ages)
	yt  <- rep(max(ages), length(cohs))
	yt[xr > max(years)] <- yt[xr > max(years)] - xr[xr > max(years)] + max(years)
	xr[xr > max(years)] <- max(years)
	
	# cut down one last time:
	xr <- xr[yt >= min(ages)]
	xl <- xl[yt >= min(ages)]
	yb <- yb[yt >= min(ages)]
	yt <- yt[yt >= min(ages)]
	
	# draw cohort refs:
	if (increasing){ # upward sloping diagonals
		Diagonal <- list(x1 = xl - yb * yearshift,
					y1 = yb * agemult,
					x2 = xr - yt * yearshift,
					y2 = yt * agemult)
	
	} else { # downward sloping diagonals
		Diagonal <- list(x1 = rev(xr) - yb * yearshift,
					y1 = yb * agemult,
					x2 = rev(xl) - yt * yearshift,
					y2 = yt * agemult)
	}
	# return list
	list(Vertical = Vertical, Horizontal = Horizontal, Diagonal = Diagonal)
}


# a handy color ramp function, pulls from Brewer
#'
#' @title produces a ramp function based on Brewer palettes
#' 
#' @description This takes a vector of colors from one of the \code{RColorBrewer} palettes and gives back a ramp interpolator (Lab space).
#' 
#' @param bp which palette? Make it a continuous one!
#' @param rev.pal logical, should the ordering of the selected palette be reversed?
#' 
#' @value a ramp function.
#' 
#' @importFrom RColorBrewer brewer.pal.info
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices colorRampPalette
#' 
#' @export
ramp <- function(bp = "YlOrRd",rev.pal=FALSE){
	col.base <- brewer.pal(brewer.pal.info[bp,]$maxcolors,bp)
	if (rev.pal){
		col.base <- rev(col.base)
	}
	colorRampPalette(col.base,space="Lab")
}

#'
#' @title like fortify from \code{ggplot2} but it saves the area metadata
#' 
#' @description In order to be able to plot polygons in base that have holes, we need to plot them in order of decreasing size, which means we need to save the areas from the polygon metadata. It gives back a \code{data.frame} with \code{NA}-separated polygons in x,y (rather than lat long). Holes later removed.
#' @param p a \code{SpatialPolygonsDataFrame} or \code{SpatialPolygons} object. 
#' 
#' @value a \code{data.frame} with columns for \code{x}, \code{y}, \code{area} (approx), \code{hole} (logical), \code{Order} (order within-polygon), \code{ID} (the shared ID for like-colored polygons in plot), and \code{group} (the within-polygon ID).
#' 
#' @imports sp
#' 
#' @export

# this is different from ggplot2::fortify because it saves the area. colnames also different.
myfortify <- function(p){
	do.call(rbind,lapply(1:length(p@polygons), function(i,p){
						x <- p@polygons[[i]]
						ID <- p@data[i,"layer"]
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
					},p=p))
} 
# prelog if necessary, X is an AP matrix of data
#colramp <- ramp("BuGn")
LexisPoly <- function(X, breaks = NULL, colramp = ramp(), equilateral = FALSE, N = 10){
	if (is.null(breaks)){
		breaks <- pretty(X)
	}
	# age/year needed as metadata throughout
	Ages   			<- as.integer(rownames(X))
	Years  			<- as.integer(colnames(X))
	# get some contour lines, just in case. May need to do eq transform.
	Lines  			<- contourLines(Years+.5, Ages+.5, t(X), levels = breaks)
	
	#XX <- cut(X,breaks=breaks,labels=colramp(length(breaks)-1))
	#dim(XX) <- dim(X)
	# turn it into a raster-class object
	r      			<- raster(X[nrow(X):1,],
							xmn = min(Years), xmx = max(Years) + 1, 
							ymn = min(Ages), ymx = max(Ages) + 1)
	# (there is a special method to cut intervals for raster classes)
	z      			<- cut(r, breaks = breaks, right = FALSE) 
	
	
	# TR bug here: need to save intervals to be able to assign color properly!
	# either this happens in the cut() or rasterToPolygons() steps... Possibly ask on SO
	# if this turns out to be beyond me. Look at underlaying functions...
	# now combine cells within-intervals to single polygons
	# slower, but gets the IDs right...
	
#	plist <- lapply(unique(z@data@values),function(id,z){
#		pii 	<- rasterToPolygons(z, fun = function(x){x==id}, dissolve = TRUE)
#		rownames(pii@data)
#		# convert to data.frame
#		pli     <- myfortify(pii)
#		pli$ID  <- id
#		pli
#	}, z = z)
## combine to long data.frame
#pl                  <-  do.call(rbind, plist)
    p     <- rasterToPolygons(z, dissolve = TRUE)
	pl    <- myfortify(p)
	# N colors-- if reverse ramp needed, then make ramp function work that way
	colors 			<- colramp(length(unique(pl$ID)))	
	# take a little break here to assign shades of grey to contour lines based on surrounding
	# colors. This is a tricky line of code
	neighb.colors <- colors[cumsum(diff(unlist(lapply(Lines,"[[","level"))) > 0)]
	contour.colors <- ifelse(colorspace::hex2RGB(spatstat::to.grey(neighb.colors))@coords[, 1] < .65,gray(.8),gray(.2))
	for (i in 1:length(contour.colors)){
		Lines[[i]][["col"]] <- contour.colors[i]
	}
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

	# remove holes (works because we plot in decreasing order of polygon size)
	pl 				<- pl[!pl$hole, ]
	# now to include axes, Lexis grids, etc.
	# identify corners for bounding box
	bbox <- list(x = c( min(Years), 
						max(Years)+1, 
						max(Years)+1, 
						min(Years)),
				 y = c( min(Ages), 
						min(Ages), 
						max(Ages)+1, 
						max(Ages)+1)
		          )
	# Age ticks:
    yat <- ylabs   <- Ages[Ages %% N == 0]
	y0  <- min(Years)
	# Year ticks
	xat <- xlabs   <- Years[Years %% N == 0]
	x0 <- min(Ages)
	Ticks <- list(x0 = x0, xat = xat, xlabs = xlabs, 
			      y0 = y0, yat = yat, ylabs = ylabs)
	# Reference lines follow ticks
	Refs  <- LexRefN(c(Ages, max(Ages + 1)), 
			         c(Years, max(Years) + 1), 
					 N, 
					 increasing = TRUE, 
					 equilateral)
	
	# adjust for equilateral, if necessary
	if (equilateral){
		pl$x 		<- pl$x - .5 * pl$y
		pl$y 		<- pl$y * sqrt(3) / 2
		# Ticks adjust
		Ticks$xat 	<- Ticks$xat - .5 * Ticks$x0
		Ticks$x0 	<- Ticks$x0 * sqrt(3) / 2    # this is a y coord
		Ticks$y0 	<- Ticks$y0 - .5 * Ticks$yat # this is an x coord, produces vector because on a slant
		Ticks$yat 	<- Ticks$yat *sqrt(3) / 2
		# Contours
	    Lines <- lapply(Lines, function(x){
				x$x <- x$x - .5 * x$y
				x$y <- x$y * sqrt(3)/2
				x
			})
	    bbox$x <- bbox$x - .5 * bbox$y
		bbox$y <- bbox$y * sqrt(3) / 2
	}
	# object returned has contour lines and metadata too.
	LexisP <- list(
			Polygons = pl, 
			Contours = Lines, 
			equilateral = equilateral, 
			Years = Years, 
			Ages = Ages, 
			Ticks = Ticks, 
			Refs = Refs,
			bbox = bbox)
	# set to special class for plotting
	class(LexisP) <- "LexisPoly"
	# return plot object
    LexisP
}


plot.LexisPoly <- function(x, contour = FALSE,refs = TRUE, border = NA, add = FALSE, ...){
	equilateral <- x$equilateral
	
	if (!add){
		plot(NULL, xlim = range(x$Polygons$x, na.rm = TRUE), 
				   ylim = range(x$Polygons$y, na.rm = TRUE), 
				   asp = TRUE,
			   	   axes = FALSE,
			   	   xlab = "", 
			   	   ylab = "")
    }
	# color regions
	polygon(x$Polygons$x, x$Polygons$y, col = x$Polygons$color[x$Polygons$Order==1], border = border, ...)
	
	# bounding box
	polygon(x$bbox$x,x$bbox$y)
	
	if (refs){
		# Vertical
		segments(x$Refs$Vertical$x1,
				 x$Refs$Vertical$y1,
				 x$Refs$Vertical$x2,
				 x$Refs$Vertical$y2,
				 col = "#55555550")
		# Horizontal
		segments(x$Refs$Horizontal$x1,
				 x$Refs$Horizontal$y1,
				 x$Refs$Horizontal$x2,
				 x$Refs$Horizontal$y2,
				 col = "#55555550") 
		# Diagonal
		segments(x$Refs$Diagonal$x1,
				 x$Refs$Diagonal$y1,
				 x$Refs$Diagonal$x2,
				 x$Refs$Diagonal$y2,
				 col = "#55555550")  
		
	}
	# use refline darkening scheme from HexMex...
	
	
	# contour lines
	if (contour){
		templines <- function(clines,...) {
			lines(clines[[2]], clines[[3]],col=clines$col, ...)
		}
		invisible(lapply(x$Contours, templines))
	}
	par(xpd=TRUE)
	# now draw axes. depends on eq
    segments(x$Ticks$xat,x$Ticks$x0,x$Ticks$xat,x$Ticks$x0-1)
	text(x$Ticks$xat,x$Ticks$x0,x$Ticks$xlabs,pos=1,cex=.8)
	segments(x$Ticks$y0,x$Ticks$yat,x$Ticks$y0-1,x$Ticks$yat)
	text(x$Ticks$y0,x$Ticks$yat,x$Ticks$ylabs,pos=2,cex=.8)
}


names(LexisP)

#pl 			  	<- pl[!pl$hole,]


# a couple mortality surfaces:
mort <- acast(readHMDweb("USA","mltper_1x1",us,pw),Age~Year,value.var="mx")

breakse10 <- 1/(10^(0:4))
breaksmid <- exp(log(breakse10)[-5] + diff(log(breakse10)) / 2)
breaks    <- sort(c(breaksmid,breakse10))

# standard:
mort_rt <- LexisPoly(mort,breaks,ramp(),equilateral = FALSE)
plot(mort_rt,contour=TRUE)
# equilateral:
mort_eq <- LexisPoly(mort,breaks,ramp(),equilateral = TRUE)
plot(mort_eq,contour=TRUE)

# now two fertility surfaces
fert <- acast(readHFDweb("USA","asfrRR",us,pw),Age~Year,value.var="ASFR")
range(fert)
pretty(fert)
breaks <- seq(0,.3,by=.025)

ASFR_rt   <- LexisPoly(fert,breaks,ramp("BuGn"),equilateral = FALSE)
plot(ASFR_rt,contour=TRUE)
plot(p,breaks=breaks,col=colramp(length(breaks)-1))
plot(1:length(breaks),1:length(breaks),pch=19,cex=3,col=(colramp(length(breaks))))