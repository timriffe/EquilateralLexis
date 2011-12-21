
# Author: triffe
###############################################################################

SqLexis <- function(Rates, 
		value = "ASFR", 
		col, 
		breaks, 
		guides = TRUE, 
		guide.int = 5, 
		legend = TRUE, 
		lab.breaks = NULL, 
		legend.args = NULL, 
		axis.args = NULL, 
		...){
	
	# Standard Lexis coordinates:
	LexTriPeriod <- function(x,brks,cols,value){
		col <- cols[(brks-x[value])>=0][1]
		# lower
		if (diff(c(x["Age"],x["Year"]))==x["Cohort"]){
			xcoord <- c(x["Year"],x["Year"]+1,x["Year"]+1)
			ycoord <- c(x["Age"],x["Age"],x["Age"]+1)
		} else { #upper
			xcoord <- c(x["Year"],x["Year"]+1,x["Year"])
			ycoord <- c(x["Age"],x["Age"]+1,x["Age"]+1)
		}
		polygon(xcoord,ycoord,col=col,border="transparent")
	}
	
	xlim <- c(min(Rates[,"Year"]), max(Rates[,"Year"]) + 1)
	ylim <- c(min(Rates[,"Age"]), max(Rates[,"Age"]) + 1)
	
	# set plot dimensions:
	plot(NULL, type = "n", ylim = ylim, xlim = xlim, asp = 1,axes = FALSE, xlab = "",ylab = "", ...)
	
	# draw triangles:
	apply(Rates,1,LexTriPeriod,cols=cols,brks=brks)
	
	yrs <- sort(unique(Rates[, "Year"]))
	yrs <- yrs[yrs %% guide.int == 0]
	ages <- sort(unique(Rates[, "Age"]))
	ages <- ages[ages %% guide.int == 0]
	
	# period lines, ticks, labels:
	if (guides){
		segments(yrs, min(Rates[, "Age"]) - .5, yrs, max(Rates[, "Age"]) + 1, col = "#00000030")
	} else {
		segments(yrs, min(Rates[, "Age"]), yrs, min(Rates[, "Age"]) - .5, col = "black")
	}
	text(yrs - .5, ylim[1]-3, yrs, pos = 1, srt = 90, xpd = TRUE)
	
	# age lines, ticks, labels
	if (guides){
		segments(xlim[1] - .5, ages, xlim[2], ages, col = "#00000030")
	} else {
		segments(xlim[1] - .5, ages, xlim[1], ages, col = "black")
	}
	text(xlim[1] - 1, ages, labels = ages, pos = 2, xpd = TRUE)
	
	# cohort lines, ticks, labels
	cohorts <- sort(unique(Rates[, "Cohort"]))
	cohorts <- cohorts[cohorts %% guide.int == 0]
	# the x argument is a row of cmat
	brange 	<- c(min(Rates[,"Year"]),min(Rates[,"Age"]),max(Rates[,"Year"])+1.5,max(Rates[,"Age"])+1.5) # to add
	xleft 	<- pmax(cohorts+brange[2],min(Rates[,"Year"]))
	ymin 	<- pmax(brange[2],xleft-cohorts)
	xright	<- pmin(cohorts+brange[4],brange[3])
	ymax 	<- pmin(brange[4],xright-cohorts)
	
	if (guides){
		segments(xleft,ymin,xright,ymax,col="#00000030")
	} else {
		segments(xright,ymax,xright-.5,ymax-.5,col="black")
	}
	text(xright-.5,ymax,cohorts,srt=45,xpd=TRUE,pos=4)
	
	rect(xlim[1],ylim[1],xlim[2],ylim[2])
	if (legend){
		fields:::image.plot(z = brks, lab.breaks = lab.breaks, col = cols, legend.only = TRUE, legend.args = legend.args, axis.args = axis.args)
	}
}

citation("lattice")
