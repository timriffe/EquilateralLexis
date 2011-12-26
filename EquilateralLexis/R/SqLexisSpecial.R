
# Author: triffe
###############################################################################

SqLexisSp <- function(Rates, 
		value = "ASFR", 
		col, 
		breaks, 
		guides = TRUE, 
		guide.int = 5, 
		legend = TRUE, 
		lab.breaks = NULL, 
		legend.args = NULL, 
		axis.args = NULL, 
		cex.ax.lab = .8,
		legend.width = 1.2,
		yshiftforpaper = 45,
		...){
	
	# Standard Lexis coordinates:
	LexTriPeriod <- function(x,Brks,Cols,Value,Yshiftforpaper){
		coli <- Cols[(Brks-x[Value]) >= 0][1]		# lower
		if (diff(c(x["Age"],x["Year"]))==x["Cohort"]){
			xcoord <- c(x["Year"],x["Year"]+1,x["Year"]+1)
			ycoord <- c(x["Age"]+Yshiftforpaper,x["Age"]+Yshiftforpaper,x["Age"]+1+Yshiftforpaper)
		} else { #upper
			xcoord <- c(x["Year"],x["Year"]+1,x["Year"])
			ycoord <- c(x["Age"]+Yshiftforpaper,x["Age"]+1+Yshiftforpaper,x["Age"]+1+Yshiftforpaper)
		}
		polygon(xcoord,ycoord,col=coli,border="transparent")
	}
	
	xlim <- c(min(Rates[,"Year"]), max(Rates[,"Year"]) + 1)
	ylim <- c(min(Rates[,"Age"]), max(Rates[,"Age"]) + 1 + yshiftforpaper)
	
	# set plot dimensions:
	#plot(NULL, type = "n", ylim = ylim, xlim = xlim, asp = 1,axes = FALSE, xlab = "",ylab = "", ...)
	
	# draw triangles:
	apply(Rates,1,LexTriPeriod,Cols=col,Brks=breaks,Yshiftforpaper=yshiftforpaper,Value=value)
	
	yrs <- sort(c(unique(Rates[, "Year"]),max(Rates[,"Year"])+1))
	yrs <- yrs[yrs %% guide.int == 0]
	ages <- sort(unique(Rates[, "Age"]))
	ages <- ages[ages %% guide.int == 0]
	
	# period lines, ticks, labels:
	if (guides){
		segments(yrs, min(Rates[, "Age"]) - .5 + yshiftforpaper, yrs, max(Rates[, "Age"]) + 1 + yshiftforpaper, col = "#00000030")
	} else {
		segments(yrs, min(Rates[, "Age"]) + yshiftforpaper, yrs, min(Rates[, "Age"]) - .5 + yshiftforpaper, col = "black")
	}
	text(yrs - .5, ylim[1]-3 + yshiftforpaper, yrs, pos = 1, srt = 90, xpd = TRUE, cex = cex.ax.lab)
	
	# age lines, ticks, labels
	if (guides){
		segments(xlim[1] - .5, ages + yshiftforpaper, xlim[2], ages + yshiftforpaper, col = "#00000030")
	} else {
		segments(xlim[1] - .5, ages + yshiftforpaper, xlim[1], ages + yshiftforpaper, col = "black")
	}
	text(xlim[1] - 1, ages + yshiftforpaper, labels = ages, pos = 2, xpd = TRUE, cex = cex.ax.lab)
	
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
		segments(xleft,ymin+yshiftforpaper,xright,ymax+yshiftforpaper,col="#00000030")
	} else {
		segments(xright,ymax+yshiftforpaper,xright-.5,ymax-.5+yshiftforpaper,col="black")
	}
	text(xright - .5, ymax+yshiftforpaper, cohorts, srt = 45, xpd = TRUE, pos = 4, cex = cex.ax.lab)
	
	#rect(xlim[1],ylim[1],xlim[2],ylim[2])
	if (legend){
		fields:::image.plot(z = brks, lab.breaks = lab.breaks, col = cols, legend.only = TRUE, legend.width = legend.width, legend.args = legend.args, axis.args = axis.args)
	}
}

