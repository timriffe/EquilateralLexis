
# Author: triffe
###############################################################################

EqLexis <- function(Rates, 
		value = "ASFR", 
		col, 
		breaks, 
		guides = TRUE, 
		guide.int = 5,
		legend = TRUE, 
		lab.breaks = NULL, 
		legend.args = NULL, 
		axis.args = NULL, 
		a.lab=NULL,
		c.lab=NULL,
		y.lab=NULL,
		a.lab.adj=c(0,0),
		c.lab.adj=c(0,0),
		y.lab.adj=c(0,0),
		a.ax.lab.adj=c(0,0),
		c.ax.lab.adj=c(0,0),
		y.ax.lab.adj=c(0,0),
		cex.lab=1,
		cex.ax.lab=.8,
		...){
	# try to coerce to numeric matrix
	if (is.data.frame(Rates)){
		Rates <- as.matrix(Rates[,colnames(Rates) %in% c("Age","Year","Cohort",value)])
	}
	
	# define polygon plot function:
	EQLexTriTiltLeft <- function(x,brks=breaks,cols=col,val=value){
		coli <- cols[(brks-x[val]) >= 0][1]
		ys <- sin(pi/3)
		# lower
		if (diff(c(x["Age"],x["Year"]))==x["Cohort"]){
			xcoord <- c(x["Year"]-.5*x["Age"],
					x["Year"]+1-.5*x["Age"],
					x["Year"]-.5*x["Age"]+.5)
			ycoord <- c(x["Age"]*ys,
					x["Age"]*ys,
					(x["Age"]+1)*ys)
		} else { #upper
			xcoord <- c(x["Year"]-.5*x["Age"],
					(x["Year"]+.5)-.5*x["Age"],
					(x["Year"]-.5)-.5*x["Age"])
			ycoord <- c(x["Age"]*ys,
					(x["Age"]+1)*ys,
					(x["Age"]+1)*ys)
		}
		polygon(xcoord,ycoord,col=coli,border="transparent")
	}	
	
	# default breaks and colors: 
	# if specifying different breaks or colors, remember, as with other
	# image()-like function, 
	if (missing(breaks)) {
		breaks <- approx(x=range(pretty(range(Rates[,value]))),n=201)$y
	}
	if (missing(col)) {
		colramp <- grDevices:::colorRampPalette(c("white","blue","green","yellow","orange"),space = "rgb")
		col <- colramp((length(breaks)-1))
	}
	# allow plotting in margins
	par(xpd=TRUE)
	
	# choose decent x and y ranges
	
	# ysc used to scale y everywhere = sin(pi/3), remember the unit circle!
	ysc <- sqrt(3)/2
	xlim <- c(min(Rates[,"Year"])-2-(diff(range(Rates[,"Age"]))*.5),max(Rates[,"Year"])+1)
	
	ages <- unique(Rates[,"Age"])
	ages <- c(ages,max(ages)+1)
	yl <- range(ages)
	ylim <- ysc*yl
	
	plot(NULL,type="n",ylim=ylim,xlim=xlim,asp=1,axes=FALSE,xlab="",ylab="", ...)
	
	# EQLexTriTiltLeft is the main polygon function
	apply(Rates,1,EQLexTriTiltLeft,cols=col,brks=breaks)
	
	# age references for lines
	agerefs <- sort(ages[ages%%guide.int==0])
	
	# year lines
	yrs <- c(unique(Rates[,"Year"]),max(Rates[,"Year"])+1)
	yrrefs <- sort(yrs[yrs %% guide.int == 0])
	x1 <- yrrefs - yl[1] * .5
	x2 <- yrrefs - yl[2] * .5
	if (guides){
		segments(x1 + .25, ylim[1] - ysc * .5, x2, ylim[2], col = "#00000030")
	} else {
		segments(x1 + .25, ylim[1] - ysc * .5, x1 - .5, ylim[1] + ysc, col = "black")
	}
	text(x1 + 2 + y.ax.lab.adj[1], ylim[1] - 1 + y.ax.lab.adj[2], yrrefs, pos = 1, srt = -60,cex=cex.ax.lab)
	
	# cohort lines to draw 
	cohs <- unique(Rates[,"Cohort"])
	cohrefs <- sort(cohs[cohs%%guide.int==0])
	# on left we want them to stop on the earliest year
	brange 	<- c(min(Rates[,"Year"]),min(Rates[,"Age"]),max(Rates[,"Year"])+1.5,max(Rates[,"Age"])+1.5)
	# shift x position left by .5 for each age
	x1 	<- pmax(cohrefs+brange[2],brange[1])
	x2	<- pmin(cohrefs+brange[4],brange[3])
	# rescale y positions using 'ysc'
	y1 	<- pmax(brange[2],x1-cohrefs)
	y2 	<- pmin(brange[4],x2-cohrefs)
	x1 	<- x1-.5*y1
	x2	<- x2-.5*y2
	y1 	<- y1*ysc
	y2 	<- y2*ysc
	if (guides){
		segments(x1,y1,x2,y2,col="#00000030")
	} else {
		segments(x2-1,y2-ysc*2,x2,y2,col="black")
	}
	text(x2 + 1 + c.ax.lab.adj[1], y2 + 1 + c.ax.lab.adj[2], cohrefs, srt = 60, pos = 3,cex=cex.ax.lab)
	
	# drawing age refs needed some year info
	if (guides) {
		segments(min(yrs)-agerefs*.5-.5,agerefs*ysc,max(xlim)-agerefs*.5,agerefs*ysc,col="#00000030")
	} else {
		segments(min(yrs)-agerefs*.5-.5,agerefs*ysc,min(yrs)-agerefs*.5+.5,agerefs*ysc,col="black")
	}
	text(min(yrs) - agerefs * .5 - .5 + a.ax.lab.adj[1], agerefs * ysc + a.ax.lab.adj[2], labels = agerefs, pos = 2, cex = cex.ax.lab)
	
	text(min(yrs) - 10 - agerefs[4] * .5 + a.lab.adj[1], agerefs[3] + a.lab.adj[2], labels = a.lab, cex = cex.lab)
	text(max(yrrefs) - agerefs[2] * .5 + c.lab.adj[1], agerefs[length(agerefs) - 2] + c.lab.adj[2], labels = c.lab, srt = 60, cex = cex.lab, pos = 3)
	text(median(yrrefs)+y.lab.adj[1],0+y.lab.adj[2],y.lab,cex=cex.lab,srt=-60)
	
	# legend:
	if (legend){
		fields:::image.plot(z = brks, lab.breaks = lab.breaks, col = cols, legend.args = legend.args, axis.args = axis.args, legend.only = TRUE)
	}
	# FINISH bounding box
	# give parallelogram box
	polygon(c(min(Rates[,"Year"])-.5*(max(Rates[,"Age"])+1),
					min(Rates[,"Year"])-.5*min(Rates[,"Age"]),
					max(Rates[,"Year"])+1-.5*min(Rates[,"Age"]),
					max(Rates[,"Year"])+1-.5*(max(Rates[,"Age"])+1)),
			ysc*c(max(Rates[,"Age"])+1,min(Rates[,"Age"]),min(Rates[,"Age"]),max(Rates[,"Age"])+1),
	
	)
	
}


