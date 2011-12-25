
# Author: triffe
###############################################################################
#library(LexisDiagram)
source("/home/triffe/git/LexisDiagram/LexisDiagram/R/Lexis.R")
#dev.new(height=7,width=5)
library(cairoDevice)
Cairo_pdf("Figs/Figure1.pdf",height=7,width=5)
Lexis(years = 2008:2012, 
		ages = 0:6, 
		mar = c(3,5,2,5), 
		age.lab = "", 
		year.lab = "", 
		cohort.lab = "",
		open.device = FALSE)
text(2008.5:2011.5, 6.3, 2002:2005, srt = 45, xpd = TRUE)
mtext("Birth cohort", side = 4, line = 4, cex = 1.5)
mtext("Year", side = 1, line = 2, cex = 1.5)
mtext("Age", side = 2,line = 3, cex = 1.5)
dev.off()