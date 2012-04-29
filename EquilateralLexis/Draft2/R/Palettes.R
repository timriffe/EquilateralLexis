# TODO: Add comment
# 
# Author: triffe
###############################################################################


library(RColorBrewer)
display.brewer.all() 
display.brewer.pal("BuGn")
mypalette<-brewer.pal(251,"Greens")
image(1:251,1,as.matrix(1:251),col=mypalette,xlab="Greens (sequential)",
		ylab="",xaxt="n",yaxt="n",bty="n")
brewer.pal.info

hue <- seq(.4,.7,length=20)
plot(NULL,type="n",ylim=c(0,10),xlim=c(0,10))
rect(1,hue[1:99]*10,9,hue[2:100]*10,col=hsv(hue, 1, 1),border=NA)

install.packages("colorspace")

library(colorspace)
x = RGB(runif(1000),runif(1000),runif(1000))
plot(as(x, "LUV"))

# testing sequential palettes
N <- 20
hue <- sequential_hcl(n=N, h = 260, c. = c(80, 0), l = c(30, 90), power = 1.5,gamma = NULL, fixup = TRUE)
x <- seq(from=0,to=1,length.out=N)
plot(NULL,type="n",ylim=c(0,10),xlim=c(0,10))
rect(1,x[1:(N-1)]*10,9,x[2:N]*10,col=hue,border=NA)

# maybe convert initial color vector into HSV, then homogenize S and V
h <- 1:360
for (i in 1:360){
	hue <- sequential_hcl(n=N, h = h[i], c. = c(80, 0), l = c(30, 90), power = 1.5,gamma = NULL, fixup = TRUE)
	plot(NULL,type="n",ylim=c(0,10),xlim=c(0,10))
	rect(1,x[1:(N-1)]*10,9,x[2:N]*10,col=hue,border=NA)
	Sys.sleep(.15)
}

pal <- function(col, border = "light gray"){
	n <- length(col)
	plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")
	rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
}

pal(sequential_hcl(12))
pal(heat_hcl(12, h = c(0, -100), l = c(75, 40), c = c(40, 80), power = 1))
pal(terrain_hcl(12, c = c(65, 0), l = c(45, 95), power = c(1/3, 1.5)))
pal(heat_hcl(12, c = c(80, 30), l = c(30, 90), power = c(1/5, 1.5)))

par(mfrow = c(2, 1))
pal(diverge_hcl(7, c = 100, l = c(50, 90))); pal(diverge_hsv(7))
pal(diverge_hcl(7, h = c(180, 330), c = 59, l = c(75, 95))); pal(cm.colors(7))
pal(heat_hcl(12)); pal(heat.colors(12))
pal(terrain_hcl(12)); pal(terrain.colors(12))


install.packages("IDPmisc")
library(IDPmisc)
pal(bb(n=length(seq(0,.4,by=.01))),border=NA)

seq(0,.4,by=.01)
		
 
# blue to orange
library(colorspace)
H <- 20
S <- seq(0,1,by=.1)
V <- 1
pal(rgb(red=as(HSV(H,S,V),"RGB")@coords))
pal(rgb(red=as(HSV(H,S,V),"RGB")@coords))
HSV(H,S,V)
?hsv
n <- 20;  y <- -sin(3*pi*((1:n)-1/2)/n)
op <- par(mar=rep(1.5,4))
plot(y, axes = FALSE, frame.plot = TRUE,
		xlab = "", ylab = "", pch = 21, cex = 30,
		bg = rainbow(n, start=.85, end=.1),
		main = "Red tones")
par(op)
par(mfrow=c(2,1))
H <- 20
S <- 1
pal(hsv(h=H/360,s=S,v=V,alpha=1),border=NA)
pal(rgb(red=as(HSV(H,S,V),"RGB")@coords),border=NA)

# col H are a vector of hues taking values 0-360
HSVmixpal2 <- function(H,S,V,Ncolors,alpha=.5){
	require(colorspace)
	rgb(red=as(mixcolor(alpha=seq(0,1,length=Ncolors), HSV(H[1], S[1], V[1]), HSV(H[2], S[2], V[2])),"RGB")@coords)
}

pal(HSVmixpal2(H=c(1,360),S=c(1,1),V=c(1,1),Ncolors=25,alpha=.5),border=NA)

FxTestCols <- grDevices:::colorRampPalette(c("white","blue","limegreen","yellow"))
brks <- seq(0,.3,length=31)
FxColVec <- FxTestCols(length(brks)-1)

# now convert to HSV,
HSVmat <- as(hex2RGB(FxColVec),"HSV")

par(mfrow=c(2,1))
pal(FxColVec)
pal(rgb(red=as(HSVmat,"RGB")@coords))
pal(rainbow(10))

brks
R <- rep(0,)
col2rgb("green")

rgbmat <- matrix(0,nrow=3,ncol=(length(brks)-1))
brks
rgbmat[1,] <- c(rep(0,sum(brks<.2)),approx(x=c(.2,.3),y=c(0,255),xout=brks[brks>.2])$y)
rgbmat[2,] <- approx(x=)$y


V <- RateSketch(xlim=c(0,.3),ylim=c(0,1),xnew=brks,compare=cbind(brks[-1],HSVmat@coords[,2],brks[-1],HSVmat@coords[,3]))$SPLINE[,2]
V[V>1] <- 1
V[1:8] <- 1
V[25:31] <- 1

S <- RateSketch(xlim=c(0,.3),ylim=c(0,1),xnew=brks,compare=cbind(brks[-1],HSVmat@coords[,2],brks[-1],V[-1]))$SPLINE[,2]
S[S>1] <- 1
S[16:31] <- 1

HSVmat@coords[,2] <- S[-1]
HSVmat@coords[,3] <- V[-1]

install.packages("colorRamps")
library(colorRamps)

image(matrix(1:400, 20), col = blue2red(400))
image(matrix(1:400, 20), col = blue2green(400))
image(matrix(1:400, 20), col = green2red(400))

pal(blue2yellow(31))

brewer.pal.info
brewer.pal(9,"YlGnBu")

library(RColorBrewer)
library(grDevices)
Fxtest2 <- colorRampPalette(brewer.pal(9,"YlGn"), space = "Lab")
pal(Fxtest2(30))
Fxtest2 <- colorRampPalette(brewer.pal(9,"YlGnBu"), space = "Lab")
pal(Fxtest2(30))
cols <- Fxtest2(30)


# choosing
H <- seq(.6,0.01,length=30)
S <- c(seq(0,1,length=15),rep(1,15))
V <- 1
pal(hsv(H,S,V))

brks <- seq(0,.3,by=.01)
cols <- hsv(H,S,V)

bigleg <- function(cols,brks,ticks=seq(min(brks),max(brks),by=.05)){
	n <- length(col)
	plot(0, 0, type="n", xlim = c(0, 1), ylim = range(brks), axes = FALSE, xlab = "", ylab = "")
	rect(0,brks[1:length(cols)],1,brks[2:length(brks)],col=cols,border="white")
	rect(0,min(brks),1,max(brks))
	segments(0,ticks,-.03,ticks)
	text(-.03,ticks,ticks,xpd=TRUE,pos=2)
	
}

bigleg(cols,brks)

# RGB compare:
hsv(h=c(.6,0),s=c(0,1),v=1)
library(grDevices)
par(mfrow=c(1,4))
bigleg(cols,brks)
bigleg(colorRampPalette(hsv(h=c(.6,0),s=c(0,1),v=1))(30),brks)
bigleg(colorRampPalette(hsv(h=c(.6,0.47793103,0.29482759,0),s=c(0,0.42857143,1,1),v=1))(30),brks)
bigleg(colorRampPalette(hsv(h=c(.6,0.47793103,0.29482759,0.17275862,0),s=c(0,0.42857143,1,1,1),v=1))(30),brks)

par(mfrow=c(1,5))
bigleg(hsv(H,S,V),brks)
bigleg(colorRampPalette(hsv(h=c(.6,0.47793103,0.29482759,0.17275862,0),s=c(0,0.42857143,1,1,1),v=1))(30),brks)
bigleg(colorRampPalette(c(hsv(h=c(.6,0.47793103,0.29482759,0.17275862,0),s=c(0,0.42857143,1,1,1),v=1),"magenta"))(30),brks)
bigleg(colorRampPalette(c(hsv(h=c(.6,0.47793103,0.29482759,0.17275862,24/360,0),s=c(0,0.42857143,1,1,1,1),v=1),"magenta"))(25),brks[1:26])
bigleg(colorRampPalette(c(hsv(h=c(.6,0.47793103,0.29482759,0.17275862,24/360,0),s=c(0,0.42857143,1,1,1,1),v=1),"magenta"))(30),brks)


COLSUNADJ <- colorRampPalette(c(hsv(h=c(.6,0.47793103,0.29482759,0.17275862,24/360,0),s=c(0,0.42857143,1,1,1,1),v=1),"magenta"))(30)

# this is for generalized- more comparable, since very few rates exceed .3

# but there are some surfaces where the max is around .25, so maybe remove orange?

# piecing together explicit color slices?
brks <- seq(0,.3,length=31)
# .01 steps
# white -> cyan (0 -> .05)
WC <- colorRampPalette(c(hsv(.6,0,1),hsv(0.47793103,0.42857143,1)))(6)[-6]
# cyan -> bright green (.05 -> .1)
CG <- colorRampPalette(c(hsv(0.47793103,0.42857143,1),hsv(0.29482759,1,1)))(6)[-6]
# bright green -> yellow (.1 -> .15)
GY <- colorRampPalette(c(hsv(0.29482759,1,1),hsv(0.17275862,1,1)))(6)[-6]
# yellow -> orange( "#FFA812" )(.15 -> .2)
YO <- colorRampPalette(c(hsv(0.17275862,1,1),"#FFA812"))(6)[-6]
# orange -> red (.2 -> .25)
OR <- colorRampPalette(c("#FFA812",hsv(0,1,1)))(6)[-6]
# red -> magenta (.25 -> .3)
RM <- colorRampPalette(c(hsv(0,1,1),"magenta"))(6)[-6]

YR <- colorRampPalette(c(hsv(0.17275862,1,1),hsv(0,1,1)))(6)[-6]

WB <- colorRampPalette(c(hsv(.6,0,1),hsv(260/360,1,1)))(6)[-6]
BG <-  colorRampPalette(c(hsv(260/360,1,1),hsv(0.29482759,1,1)))(6)[-6]

brks30 <- seq(0,.3,length=31)
brks25 <- seq(0,.25,length=26)
brks250 <- seq(0,.25,length=251)

PAL30 <- c(WC,CG,GY,YO,OR,RM) ;brks30 <- seq(0,.3,length=31)
PAL25 <- c(WC,CG,GY,YR,RM) ;brks25 <- seq(0,.25,length=26)
PAL25B <- c(WB,BG,GY,YR,RM)
par(mfrow=c(1,6))
bigleg(PAL30,brks30) 
bigleg(PAL25,brks25)
bigleg(PAL25B,brks25)


# work on BG transition:
BG <-  hsv(seq(260/360,106/360,length=6),1,1)[-6]
BG <-  hsv(seq(260/360,106/360,length=6),c(1,.9,.85,.75,.85,1),1)[-6]
BG <-  hsv(seq(260/360,106/360,length=6),c(1,.9,.85,.75,.75,.9),c(1,1,1,.9,.9,1))[-6]
BG <-  hsv(seq(255/360,106/360,length=6),c(1,.9,.75,.75,.75,.9),c(1,1,1,.9,.9,1))[-6]
BG <-  hsv(seq(255/360,106/360,length=6),c(1,.9,.75,.9,.95,1),c(1,1,1,.8,.8,1))[-6]
BG <-  hsv(c(255/360, 225.2/360, 200.4/360, 142.6/360, 120.8/360, 106/360),c(1,.9,.75,.9,.95,1),c(1,1,.8,.8,.92,1))[-6]

PAL25B <- c(WB,BG,GY,YR,RM)
cols <- colorRampPalette(PAL25B)(250)
colsl <- PAL25B
brksl <- brks25
brks <- brks250

brksfadeout <- c(seq(0,.05,by=.001)[-51],brks25[-c(1:5)])
WBfadeout <- colorRampPalette(c(hsv(.6,0,1),hsv(255/360,1,1)))(51)[-51]
PAL25fadeout <- c(WBfadeout,BG,GY,YR,RM)

brks <- brksfadeout
cols <- PAL25fadeout
# adapted from colorbrewer
WB2 <- c("#F7FBFF","#DEEBF7","#C6DBEF","#9ECAE1","#6BAED6")
BG2 <- colorRampPalette(c("#4292C6",hsv(0.29482759,1,1)))(6)[-6]
PAL25C <- c(WB2,BG2,GY,YR,RM)
bigleg(PAL25C,brks25)

brks <- brksl <- brks25
cols <- colsl <- PAL25C

# won't be printer friendly still, even thoug looks good on palette
PAL25D <- hsv(seq(.6,.05,length=25),s=seq(.2,1,length=25),v=seq(.7,1,length=25))
bigleg(PAL25D,brks25)



# hcl experimentation
bigleg(hcl(seq(255,0,length=25),seq(0,140,length=25),seq(85,30,length=25)),brks25)

pal(hcl(h=seq(300,70,length=25),c=70,l=seq(100,50,length=25)),border="white")
pal(hcl(h=c(1:360),c=80,l=60),border=NA)
pal(hcl(h=120,c=100,l=100),border=NA)


# blue 240
# green h about 120
# yellow about 60
# red about 0
# magenta 300
pal(hcl(h=rep(c(240,120,60,0,300),each=5),c=70,l=seq(100,50,length=25)))

# what about hcl in 5 single-color with progressive shade segments?
st <- c(-12,-6,0,6,12)
st <- c(-14,-7,0,7,14)
st <- c(-16,-8,0,8,16)
pal(hcl(h=c(240+st,120+st,60+st,0+st,300+st),c=70,l=seq(95,40,length=25)))
bigleg(hcl(h=c(240+st,120+st,60+st,0+st,300+st),c=c(seq(5,70,length=5),rep(70,20)),l=seq(90,35,length=25)),brks=brks25)

st <- c(-16,-8,0,8,16)
PAL25D <- hcl(h=c(240+st,120+st,60+st,0+st,300+st),c=c(seq(5,70,length=5),rep(70,20)),l=seq(90,35,length=25))
PAL25E <- hcl(h=c(240+st,120+st,60+st,0+st,300+st),c=70,l=seq(90,35,length=25))
par(mfrow=c(1,2))
bigleg(PAL25D,brks25)
bigleg(PAL25E,brks25)

colsl <- cols <- PAL25D

# HCL run
PAL25_HCL250 <- hcl(h=seq(240,-60,length=250),c=70,l=seq(100,50,length=250))
PAL25_HCL25 <-  hcl(h=seq(240,-60,length=25),c=70,l=seq(100,50,length=25))
cols <- PAL25_HCL250
colsl <- PAL25_HCL25
brks <- seq(0,.25,length=251)
brksl <- seq(0,.25,length=26)

# HSV ramp
H1 <- c(216,172,105,62,24,0)
HSVcols1 <- colorRampPalette(c(hsv(h=H1/260,s=c(0,0.42857143,1,1,1,1),v=1),"magenta"))
PAL25_HSV25_1 <- HSVcols1(25)
PAL25_HSV250_1 <- HSVcols1(250)
cols <- PAL25_HSV250_1
colsl <- PAL25_HSV25_1
brks <- seq(0,.25,length=251)
brksl <- seq(0,.25,length=26)


par(mfrow=c(1,6))
bigleg(PAL25_HCL25,brksl)
bigleg(PAL25_HSV25_1,brksl)

# RGB - b
# 250 colors, 25 legend colors, pieced together
library(grDevices)
brks <- seq(0,.25,length=251)
brksl <- seq(0,.25,length=26)
# white blue (.05) green (.1) yellow (.15) red (.2) magenta (.25+)
PAL25_RGBb <-  c(colorRampPalette(c("white","blue"),space="rgb")(6)[-6],
		colorRampPalette(c("blue","green"),space="rgb")(6)[-6],
		colorRampPalette(c("green","yellow"),space="rgb")(6)[-6],
		colorRampPalette(c("yellow","red"),space="rgb")(6)[-6],
		colorRampPalette(c("red","magenta"),space="rgb")(6)[-6]
		)
PAL250_RGBb <-  c(colorRampPalette(c("white","blue"),space="rgb")(51)[-51],
		colorRampPalette(c("blue","green"),space="rgb")(51)[-51],
		colorRampPalette(c("green","yellow"),space="rgb")(51)[-51],
		colorRampPalette(c("yellow","red"),space="rgb")(51)[-51],
		colorRampPalette(c("red","magenta"),space="rgb")(51)[-51]
		)	
cols <- PAL250_RGBb
colsl <- PAL25_RGBb

# cyan instead of blue
# white blue (.05) green (.1) yellow (.15) red (.2) magenta (.25+)
PAL25_RGBc <-  c(colorRampPalette(c("white","cyan"),space="rgb")(6)[-6],
		colorRampPalette(c("cyan","green"),space="rgb")(6)[-6],
		colorRampPalette(c("green","yellow"),space="rgb")(6)[-6],
		colorRampPalette(c("yellow","red"),space="rgb")(6)[-6],
		colorRampPalette(c("red","magenta"),space="rgb")(6)[-6]
)
PAL250_RGBc <-  c(colorRampPalette(c("white","cyan"),space="rgb")(51)[-51],
		colorRampPalette(c("cyan","green"),space="rgb")(51)[-51],
		colorRampPalette(c("green","yellow"),space="rgb")(51)[-51],
		colorRampPalette(c("yellow","red"),space="rgb")(51)[-51],
		colorRampPalette(c("red","magenta"),space="rgb")(51)[-51]
)	
cols <- PAL250_RGBc
colsl <- PAL25_RGBc

# lab for just GB
# white blue (.05) green (.1) yellow (.15) red (.2) magenta (.25+)
PAL25_RGBd <-  c(colorRampPalette(c("white","blue"),space="rgb")(6)[-6],
		colorRampPalette(c("blue","green"),space="Lab")(6)[-6],
		colorRampPalette(c("green","yellow"),space="rgb")(6)[-6],
		colorRampPalette(c("yellow","red"),space="rgb")(6)[-6],
		colorRampPalette(c("red","magenta"),space="rgb")(6)[-6]
)
PAL250_RGBd <-  c(colorRampPalette(c("white","blue"),space="rgb")(51)[-51],
		colorRampPalette(c("blue","green"),space="Lab")(51)[-51],
		colorRampPalette(c("green","yellow"),space="rgb")(51)[-51],
		colorRampPalette(c("yellow","red"),space="rgb")(51)[-51],
		colorRampPalette(c("red","magenta"),space="rgb")(51)[-51]
)	
cols <- PAL250_RGBd
colsl <- PAL25_RGBd
# Lab space ramp
PAL25_Laba <-  c(colorRampPalette(c("white","blue"),space="Lab")(6)[-6],
		colorRampPalette(c("blue","green"),space="Lab")(6)[-6],
		colorRampPalette(c("green","yellow"),space="Lab")(6)[-6],
		colorRampPalette(c("yellow","red"),space="Lab")(6)[-6],
		colorRampPalette(c("red","magenta"),space="Lab")(6)[-6]
)
PAL250_Laba <-  c(colorRampPalette(c("white","blue"),space="Lab")(51)[-51],
		colorRampPalette(c("blue","green"),space="Lab")(51)[-51],
		colorRampPalette(c("green","yellow"),space="Lab")(51)[-51],
		colorRampPalette(c("yellow","red"),space="Lab")(51)[-51],
		colorRampPalette(c("red","magenta"),space="Lab")(51)[-51]
)	
cols <- PAL250_Laba
colsl <- PAL25_Laba
?colorRampPalette



PAL25_RGBd <-  c(colorRampPalette(c("white","blue"),space="rgb")(6)[-6],
		colorRampPalette(c("blue","green"),space="Lab")(6)[-6],
		colorRampPalette(c("green","yellow"),space="rgb")(6)[-6],
		colorRampPalette(c("yellow","red"),space="rgb")(6)[-6],
		colorRampPalette(c("red","magenta"),space="rgb")(6)[-6]
)
PAL250_RGBd <-  c(colorRampPalette(c("white","blue"),space="rgb")(51)[-51],
		colorRampPalette(c("blue","green"),space="Lab")(51)[-51],
		colorRampPalette(c("green","yellow"),space="rgb")(51)[-51],
		colorRampPalette(c("yellow","red"),space="rgb")(51)[-51],
		colorRampPalette(c("red","magenta"),space="rgb")(51)[-51]
)	
cols <- PAL250_RGBd
colsl <- PAL25_RGBd

# Lab space ramp cyan
PAL25_Labb <-  c(colorRampPalette(c("white","cyan"),space="Lab")(6)[-6],
		colorRampPalette(c("cyan","green"),space="Lab")(6)[-6],
		colorRampPalette(c("green","yellow"),space="Lab")(6)[-6],
		colorRampPalette(c("yellow","red"),space="Lab")(6)[-6],
		colorRampPalette(c("red","magenta"),space="Lab")(6)[-6]
)
PAL250_Labb <-  c(colorRampPalette(c("white","cyan"),space="Lab")(51)[-51],
		colorRampPalette(c("cyan","green"),space="Lab")(51)[-51],
		colorRampPalette(c("green","yellow"),space="Lab")(51)[-51],
		colorRampPalette(c("yellow","red"),space="Lab")(51)[-51],
		colorRampPalette(c("red","magenta"),space="Lab")(51)[-51]
)	
cols <- PAL250_Labb
colsl <- PAL25_Labb


LabRamp3 <- colorRampPalette(c("white","cadetblue","green","yellow","red","magenta"),space="Lab")
bigleg(LabRamp3(26)[-26],brksl)

cols <- LabRamp3(251)[-251]
colsl <- LabRamp3(26)[-26]

LabRamp4 <- colorRampPalette(c("white","turquoise3","green","yellow","red","magenta"),space="Lab")
bigleg(LabRamp4(26)[-26],brksl)

cols <- LabRamp4(251)[-251]
colsl <- LabRamp4(26)[-26]

LabRamp5 <- colorRampPalette(c("white","blue","cyan","green","yellow","red","magenta"),space="Lab")
bigleg(LabRamp5(31)[-31],seq(0,.3,length=31))
brksl <- seq(0,.3,length=31)
brks <- seq(0,.3,length=301)
cols <- LabRamp5(301)[-301]
colsl <- LabRamp5(31)[-31]


# Lab completely different:
brksl <- seq(0,.25,length=26)
brks <- seq(0,.25,length=251)
LabRamp5 <- colorRampPalette(c("white","cyan","blue","green","yellow","red"),space="Lab")
bigleg(LabRamp5(26)[-26],brksl)
cols <- LabRamp5(251)[-251]
colsl <- LabRamp5(26)[-26]


PAL25_Labf <-  c(colorRampPalette(c("white",hcl(h=255,c=50,l=50)),space="Lab")(6)[-6],
		colorRampPalette(c(hcl(h=255,c=60,l=50),"green"),space="Lab")(6)[-6],
		colorRampPalette(c("green","yellow"),space="Lab")(6)[-6],
		colorRampPalette(c("yellow","red"),space="Lab")(6)[-6],
		colorRampPalette(c("red","magenta"),space="Lab")(6)[-6]
)
PAL250_Labf <-  c(colorRampPalette(c("white",hcl(h=255,c=50,l=50)),space="Lab")(51)[-51],
		colorRampPalette(c(hcl(h=255,c=60,l=50),"green"),space="Lab")(51)[-51],
		colorRampPalette(c("green","yellow"),space="Lab")(51)[-51],
		colorRampPalette(c("yellow","red"),space="Lab")(51)[-51],
		colorRampPalette(c("red","magenta"),space="Lab")(51)[-51]
)	
cols <- PAL250_Labf
colsl <- PAL25_Labf
bigleg(colsl,brksl)

# HCL again?
H <- seq(255,-60,length=25)
H[H<0] <- H[H<0]+360
L <- seq(98,30,length=25)
C <- seq(50,70,length=25)
HCL25b <- hcl(h=H,c=70,l=L)

H <- seq(255,-60,length=250)
H[H<0] <- H[H<0]+360
L <- seq(98,30,length=250)
C <- seq(50,70,length=250)
HCL250b <- hcl(h=H,c=70,l=L)

	
cols <- HCL250b
colsl <- HCL25b
bigleg(hcl(h=H,c=70,l=seq(30,90,length=25)),brksl)
# repeat with progressive steps to luminance?
bigleg(hcl(h=H,c=70,l=seq(30,90,length=25)),brksl)

H <- seq(255,-60,length=250)
H[H<0] <- H[H<0]+360
cols <- hcl(h=H,c=70,l=seq(80,20,length=250))

H <- seq(255,-60,length=25)
H[H<0] <- H[H<0]+360
colsl <- hcl(h=H,c=70,l=seq(80,20,length=25))

# step luminance?
brksl <- seq(0,.25,length=26)
brks <- seq(0,.25,length=251)

H <- seq(255,-60,length=250)
H[H<0] <- H[H<0]+360
L <- c(seq(90,80,length=50),seq(75,60,length=50),seq(55,40,length=50),seq(35,20,length=50),seq(20,10,length=50))
cols <- hcl(h=H,c=70,l=L)

H <- seq(255,-60,length=25)
H[H<0] <- H[H<0]+360
L <- c(seq(90,80,length=5),seq(75,60,length=5),seq(55,40,length=5),seq(35,20,length=5),seq(20,10,length=5))
colsl <- hcl(h=H,c=70,l=L)
bigleg(colsl,brksl)


# have new function to make fig3 easy

# Figure3HCLlabrampA
brksl <- seq(0,.25,length=26)
brks <- seq(0,.25,length=251)

library(grDevices)

H <- seq(255,-60,length=6)
H[H<0] <- H[H<0]+360
L <- seq(90,10,length=6)
HCLcols <- hcl(h=H,c=70,l=L)
cols <- colorRampPalette(HCLcols,space="Lab")(251)[-251]
colsl <- colorRampPalette(HCLcols,space="Lab")(26)[-26]
bigleg(colsl,brksl)
makefig3(cols,colsl,"HCLlabrampA")

# would be nice if the middle colors were somehow brighter as well:
H <- seq(255,-60,length=6)
H[H<0] <- H[H<0]+360
L <- seq(80,20,length=6)
HCLcols <- hcl(h=H,c=80,l=L) 
cols <- colorRampPalette(HCLcols,space="Lab")(251)[-251] 
colsl <- colorRampPalette(HCLcols,space="Lab")(26)[-26]
bigleg(colsl,brksl)
makefig3(cols,colsl,"HCLlabrampB")

# what about individual luminance tweak for low vals?- i.e force interpolation
# over larger range of luminance values for low rates:

# my fav so far, admitedly partly on principle...
# to be clear about how this ramp works:
# 6 Evenly spaced hues are selected, given a uniform chorma (90, rather bright, but not HSV bright!),
# and evenly decreasing luminance (ecept for values 0 to .05, which span the range 99-63 i.e. 3-times bigger luminance steps)
# this brings out minute differences in the 0-.05 range a bit better (light blues).
brksl <- seq(0,.25,length=26)
brks <- seq(0,.25,length=251)
H <- seq(255,-60,length=6)
H[H<0] <- H[H<0]+360
L <- seq(75,15,length=6)
L[1] <- 99
HCLcols <- hcl(h=H,c=90,l=L) 
cols <- colorRampPalette(HCLcols,space="Lab")(251)[-251]
colsl <- colorRampPalette(HCLcols,space="Lab")(26)
makefig3(cols,colsl,"HCLlabrampC")

# again, same thing, but all in HCL space?
# a bit more cumbersome... but worth it?
H <- seq(255,-60,length=6)
colsi <- c()
N <- 5
for (i in 1:5){
	Hi <- seq(H[i],H[i+1],length=(N+1))
	Hi[Hi<0] <- Hi[Hi<0]+360
	colsi <- c(colsi,hcl(h=Hi,c=95,l=seq(L[i],L[i+1],length=(N+1)))[ifelse(i==1,1,2):(N+1)])
}
colsl <- colsi
cols  <- colsi

# generalized, because this is now my fav
myfxHCLramp <- function(H,C=95,L,N=5){
	# H and L must be of equal length
	colsi <- c()
	for (i in 1:(length(H)-1)){
		Hi <- seq(H[i],H[i+1],length=(N+1))
		Hi[Hi<0] <- Hi[Hi<0]+360
		colsi <- c(colsi,hcl(h=Hi,c=C,l=seq(L[i],L[i+1],length=(N+1)))[ifelse(i==1,1,2):(N+1)])
	}
	colsi
}
brksl <- seq(0,.25,length=26)
brks <- seq(0,.25,length=251)
H <- seq(255,-60,length=6)
L <- seq(75,15,length=6)
L[1] <- 99
makefig2(myfxHCLramp(H=H,C=98,L=L,N=50),myfxHCLramp(H=H,L=L,N=5),"HCLd")

L <- seq(40,100,length=26)
bigleg(myfxHCLramp(H=H,C=98,L=L,N=5),brks=seq(0,.26,by=.01))
makefig2(myfxHCLramp(H=H,C=98,L=L,N=50),myfxHCLramp(H=H,L=L,N=5),"HCLd")

myfxHCLramp2 <- function(H,C=95,L,N=5,alpha){
	if (missing(alpha)){
		alpha <- rep(1,length(H))
	}
	if (length(C)==1){
		C <- rep(C,length(H))
	}
	# H and L must be of equal length
	colsi <- c()
	for (i in 1:(length(H)-1)){
		Hi <- seq(H[i],H[i+1],length=(N+1))
		Hi[Hi<0] <- Hi[Hi<0]+360
		colsi <- c(colsi,hcl(h=Hi,c=seq(C[i],C[i+1],length=(N+1)),l=seq(L[i],L[i+1],length=(N+1)),alpha=seq(alpha[i],alpha[i+1],length=(N+1)))[ifelse(i==1,1,2):(N+1)])
	}
	colsi
}
L <- seq(75,15,length=6) # L is luminance
L[1] <- 99
C<- seq(50,100,length=6)

par(mfrow=c(1,4))
bigleg(myfxHCLramp2(H=H,C=98,L=L,N=5),brks=seq(0,.26,by=.01))
bigleg(myfxHCLramp2(H=H,C=98,L=L,N=5,alpha=c(.3,1,1,1,1,1)),brks=seq(0,.26,by=.01))
bigleg(myfxHCLramp2(H=H,C=C,L=L,N=5,alpha=c(.3,1,1,1,1,1)),brks=seq(0,.26,by=.01))

makefig2(myfxHCLramp2(H=H,C=98,L=L,N=50),myfxHCLramp2(H=H,C=C,L=L,N=5),"HCLe")
makefig2(myfxHCLramp2(H=H,C=98,L=L,N=50,alpha=c(.3,1,1,1,1,1)),myfxHCLramp2(H=H,C=98,L=L,N=5,alpha=c(.3,1,1,1,1,1)),"HCLf")
makefig2(myfxHCLramp2(H=H,C=C,L=L,N=50,alpha=c(.3,1,1,1,1,1)),myfxHCLramp2(H=H,C=C,L=L,N=5,alpha=c(.3,1,1,1,1,1)),"HCLg")
