# Author: triffe
###############################################################################

# this is the state of the Lexis() function as of 25-12-2011, some changes were made since the previous release of LexisDiagram when 
# R-2.14 came out. come cleaned up considerably, various other minor changes...
Lexis <-
		function(ages,
				years,
				labs = TRUE,
				col = "black",
				open.device = TRUE,
				mar = c(1,1,1,1),
				age.lab = "Age",
				year.lab = "Year",
				cohort.lab = "Birth Cohort",
				cex.lab = 1,
				cex.ax.lab = 1){
	xrange <- diff(range(years))
	yrange <- diff(range(ages))
	maxrange <- max(c(xrange, yrange))
	xrange <- xrange * (5 / maxrange)
	yrange <- yrange * (5 / maxrange)
	
	# open a device if ecessary, set margins
	if (open.device == TRUE){
		dev.new(width=xrange+2,height=yrange+2)
	}
	# start plotting
	par(xaxs="i",yaxs="i",mar=mar)
	
	if (labs == TRUE){
		# x coordinate for generations
		interval <- 5 / max(c(length(ages), length(years)))
		genx <- max(years) + .25 / interval
		# year labels don't always fit:
		x <- years; y <- ages; xn <- length(x); yn <- length(y)
		
		plot(NA, 
				type = "n",
				xlim = range(x),
				ylim = range(y),
				axes = FALSE, 
				xlab = year.lab,
				ylab = age.lab,
				cex.lab = cex.lab,
				asp = 1,
				col = col)
		segments(x, min(ages), x,max(ages), col = col)
		segments(min(years), y, max(years), y, col = col)
		for (i in 1:(xn - 1)) {
			segments(rep(x[i], (xn-1)), y[-yn], rep(x[i + 1], (xn - 1)), y[-1], col = col)
			axis(1, at = x, labels = x, tick = FALSE, pos = (min(y) - (.1 / interval)), cex = cex.ax.lab)
			axis(2, at = y, labels = y,tick = FALSE, pos = (min(x)), las = 2, cex = cex.ax.lab)
			for (i in 1:(yn-1)){
				text(genx, y[i] + 1, labels = (max(x) - y[i] - 1), srt = 45, xpd = TRUE, cex = cex.ax.lab)
			}
			text(genx + (.5 / interval), mean(ages), cohort.lab, srt = 270, cex = cex.lab, xpd = TRUE)
		}
	}
	if (labs == FALSE){
		x <- years; y <- ages; xn <- length(x); yn <- length(y)
		plot(NA,
				type = "n",
				xlim = range(x),
				ylim = range(y),
				axes = FALSE,
				xlab = "",
				ylab = "",
				cex.lab = cex.lab,
				asp = 1,
				col = col)
		segments(x, min(ages), x, max(ages), col = col)
		segments(min(years), y, max(years), y, col = col)
		for (i in 1:(xn - 1)) {
			segments(rep(x[i], (xn - 1)), y[-yn], rep(x[i + 1], (xn - 1)), y[-1], col = col)
		}
	}
}



