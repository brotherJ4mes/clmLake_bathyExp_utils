#!/bin/Rscript

ctl <- read.table('sfc_T/ctl.csv', sep=',', head=T, colClasses=c('POSIXct',rep('numeric',19)))
bi0 <- read.table('sfc_T/bi0m.csv', sep=',', head=T, colClasses=c('POSIXct',rep('numeric',19)))
bi2 <- read.table('sfc_T/bi2m.csv', sep=',', head=T, colClasses=c('POSIXct',rep('numeric',19)))

bad_lks <- which(is.na(ctl[1,])) # get columns of bad_lks

dts <- as.POSIXct(row.names(ctl), tz='z')

t0 <- 1 
tf <- 1460

dts <- dts[t0:tf]
ctl <- ctl[t0:tf,-bad_lks]-273.15
bi0 <- bi0[t0:tf,-bad_lks]-273.15
bi2 <- bi2[t0:tf,-bad_lks]-273.15
lks <- colnames(ctl)

#png('temp_diff.png', w=2400,h=1200)
#x11(w=14,h=7)
#layout(matrix(1:15,3,5))
#par(mar=c(1,4,1,1), oma=c(5,0,4,0))
#for (i in 1:15){ 
#	print(i)
#	#ylim <- range(ctl[,i],bi0[,],bi2[,i], finite=T)
#	#ylim <- c(250,305)
#	#ylim <- c(270,290)
#	ylim <- c(-20,40)
#	plot(dts, ctl[,i],'l', ylab=NA, ylim=ylim, xlab=NA, yaxt='n'); 
#	mtext(lks[i], 2, line=.5)
#	axis(4)
#	lines(dts, bi0[,i], col='red'); 
#	lines(dts, bi2[,i], col='blue'); 
#}
#
#mtext(text=c('control','bilinear','bilinear_2m'), cex=2, font=2, side=3, outer=T, col=c('black','red','blue'), at=c(.4,.5,.6))
#mtext('2018', side=1, outer=T, cex=1.5, line=2.5)


#x11(w=14,h=7)
layout(matrix(1:15,3,5))
par(mar=c(1,4,1,1), oma=c(5,0,4,0))
for (i in 1:15){ 
	print(i)
	ylim <- range(ctl[,i],bi0[,i],bi2[,i], finite=T)
	print(ylim)
	#ylim <- c(250,305)
	#ylim <- c(270,290)
	ylim <- c(-10,35)
	bi0[,i]-ctl[,i],'l', col='darkgreen',ylab=NA, ylim=ylim, xlab=NA, yaxt='n'); 
	mtext(lks[i], 2, line=.5)
	abline(h=0,lty=2,col='black')
	axis(4)
}


mtext('bilinear - control', cex=2, font=2, side=3, outer=T)
