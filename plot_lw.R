#!/bin/Rscript
qlibrary(viridis)
qlibrary(stringr)
qlibrary(fields)
graphics.off()


outdir <- '/home/kessler/work/jtti/65e0d491f698c7b0fdfee2b7/figures/'
rng <- list(min=-25, max=50)

lkmeta <- read.table('txt/lake_ids.txt', head=T)

min_frac <- .3
ctlcol <- 'brown'
glocol <- 'orange'

filter_rng <- function(data){
	sellow <- which(data < rng$min, arr.ind=T)
	selhig <- which(data > rng$max, arr.ind=T)
	data[sellow] <- NA
	data[selhig] <- NA
	#if(length(sellow) > 0) cat(sprintf('threw out %i low vals\n', length(sellow)))
	#if(length(selhig) > 0) cat(sprintf('threw out %i high vals\n', length(selhig)))
	return(data)
}

obs <- read.table('txt/temp_out_mq3.txt')
frac <- read.table('txt/frac_out_mq3.txt')
obs[frac < min_frac] <- NA

dts_obs <- as.Date(row.names(obs), format='%Y%m%d')
ctl <- read.table('csv/ctl_T3D.csv', row.names=1)
glo <- read.table('csv/bi0m_T3D.csv', row.names=1)
glo2 <- read.table('csv/bi2m_T3D.csv', row.names=1)
dts_mod <- as.Date(row.names(ctl))
#glo2 <- read.table('bi2m_T3D.csv')


# filter for wild values
ctl <- filter_rng(ctl)
glo <- filter_rng(glo)
glo2 <- filter_rng(glo2)
obs <- filter_rng(obs)


# rename and re-order lakes
renamorder <- function(dat){ 
	dat <- dat[,order(names(dat), decreasing=T)]
	names(dat) <- str_to_title(gsub('_',' ',names(dat)))
	names(dat)[names(dat)=='Mcconaugh'] <- 'McConaugh'
	names(dat) <- gsub(' Lake','', names(dat))
	return(dat)
}


ctl <- renamorder(ctl)
glo <- renamorder(glo)
glo2 <- renamorder(glo2)
obs <- renamorder(obs)
lks <- names(ctl)
max_diff <- abs(max(range(glo-ctl, na.rm=T)))



dts_mod <- dts_mod[1:420]
ctl <- ctl[1:420,]
glo <- glo[1:420,]


# temp diff plot
if(T){
	pdf(file=sprintf('%s/lakewide_tempdiff.pdf', outdir), w=20)
	par(mar=c(4,9,4,2), cex.axis=1.25)
	image.plot(x=dts_mod, y=1:ncol(ctl), z=as.matrix(glo-ctl), col=hcl.colors(100,'Blue-Red 3'), xaxt='n', yaxt='n', 
			   main='Lake-wide Temperature Difference (GLOBathy - Flatbottom)', ylab=NA, xlab=NA, legend.lab='degrees C', zlim=c(-max_diff, max_diff))
	axis(2, at=1:ncol(ctl), lab=names(ctl), las=2)
	#axis.Date(side=1, x=dts_mod, at=seq(dts_mod[1], rev(dts_mod)[1], by='month', format='%b'), format='%b \'%y')
	axis.Date(side=1, x=dts_mod, at=seq(dts_mod[1], rev(dts_mod)[1], by='month', format='%b'), format='%b')
}


# INTERSECT DATES for OBS and MODEL 
ctl <-  ctl[!dts_mod < min(dts_obs),]
glo <-  glo[!dts_mod < min(dts_obs),]
glo2 <-  glo2[!dts_mod < min(dts_obs),]


# loop to draw individual plots
if(F){
	graphics.off()
	for (lk in rev(lks)){
		if(all(is.na(obs[,lk]))) next
		x11(w=14)
		plot(dts_obs, obs[,lk], main=lk, pch=20, ylab='surface temp (deg C)', xlab=NA)
		lines(dts, ctl[,lk], col='blue', lwd=2)
		lines(dts, glo[,lk], col='red', lwd=2)
	}
}

dts_obs <- dts_obs[1:359]
obs <- obs[1:359,]
dts <- dts_obs


# loop to panel plot
# PART 1:  PLOT AND SKILL ASSESS FULL TIME SERIES
if(T){
pdf(file=sprintf('%s/remote_val.pdf', outdir), w=17, h=10)
layout(matrix(1:20, 5,4)); par(mar=c(0,0,0,0), oma=c(3,5,3,3))
for (lk in rev(lks)){
	if(all(is.na(obs[,lk]))) next
		lkname <- gsub(' Lake','',lk)
		lkname <- paste('  ',lkname, sep='')
		plot(dts_obs, obs[,lk], pch=NA, ylab=NA, xlab=NA, xaxt='n', yaxt='n', ylim=c(-10,35))
		lines(dts, ctl[,lk], col=ctlcol, lwd=2)
		lines(dts, glo[,lk], col=glocol, lwd=2)
		points(dts_obs, obs[,lk], pch=5, cex=1, col='black', lwd=.75)
		#points(dts_obs, obs[,lk], pch=18, cex=1, col='black')
		mtext(side=3, adj=0, line=-2, text=lkname, cex=1.25)

		yi <- par('mfg')[1]
		xi <- par('mfg')[2]
		if(xi==1) axis(2)
		if(xi==4) axis(4)
		if(yi==1) axis.Date(3, x=dts, at=seq(dts[1], rev(dts)[1], by='month'), format='%b')
		if(yi==5) axis.Date(1, x=dts, at=seq(dts[1], rev(dts)[1], by='month'), format='%b')
	}

}
axis.Date(1, x=dts, at=seq(dts[1], rev(dts)[1], by='month'), format='%b')
plot.new()
legend('center', legend=c('Flatbottom', 'GLOBathy','Satellite Data'), 
	   col=c(ctlcol, glocol, 'black'), lwd=c(2, 2, NA), pch=c(NA,NA,5), cex=1.75)

# PART 2:  PLOT MONTHLY AVERAGES
if(T){ # MANUAL SWITCH TO PLOT
mons <- list(substr(rownames(obs), 5, 6))
obs <- aggregate(obs, by=mons, mean, na.rm=T)[,-1]
ctl <- aggregate(ctl, by=mons, mean, na.rm=T)[,-1]
glo <- aggregate(glo, by=mons, mean, na.rm=T)[,-1]
dts <- as.Date(sprintf('2019-%02i-01', 1:12))
pdf(file=sprintf('%s/mon_validate.pdf', outdir), w=20, h=14)
layout(matrix(1:20, 5,4)); par(mar=c(0,0,0,0), oma=c(3,5,3,3), cex.axis=1.5)
for (lk in rev(lks)){
	if(all(is.na(obs[,lk]))) next
		plot(dts, obs[,lk], pch=NA, ylab=NA, xlab=NA, xaxt='n', yaxt='n', ylim=c(-25,35))
		points(dts, obs[,lk], pch=15, cex=2.5, col='black')
		points(dts, ctl[,lk], col=ctlcol, pch=19, cex=2)
		points(dts, glo[,lk], col=glocol, pch=17, cex=2)
		mtext(side=3, adj=0, line=-2.5, text=sprintf(' %s', lk), cex=1.5)

		par(new=T)
		plot(dts, ctl[,lk]-obs[,lk], 'h', col=ctlcol, lwd=5, ylim=c(-10,40), lend=1, yaxt='n', xaxt='n')
		lines(dts+5, glo[,lk]-obs[,lk], 'h', col=glocol, lwd=5, ylim=c(-10,40), lend=1)
		abline(h=0, lwd=1)

		yi <- par('mfg')[1]
		xi <- par('mfg')[2]

		# yaxes
		if(xi==1 && yi%%2==0) axis(2, at=seq(-10,30,by=10), lwd=0)
		if(xi==1) axis(2, at=seq(0,30,by=10), lab=NA)
		if(xi==4 && yi%%2==1) axis(4, at=seq(-10,30,by=10), lwd=0)
		if(xi==4) axis(4, at=seq(0,30,by=10), lab=NA)

		# xaxes 
		if(yi==1) axis.Date(3,at=dts, lab=NA)
		if(yi==5 || lk=='nipigon') axis.Date(1,at=dts, lab=substr(month.abb,1,1), line=0, lwd=0)
	}
plot.new()
#legend('center', legend=c('Flatbottom', 'GLOBathy','Satellite Data'))
legend('center', legend=c('Flatbottom', 'GLOBathy','Satellite Data'), col=c(ctlcol, glocol, 'black'), pch=c(19,17,15), cex=3)
#legend('center', legend=c('Flatbottom', 'GLOBathy','Satellite Data'), col=c(ctlcol, glocol, 'black'), lty=NA, pch=c(19,17,15))#, cex=2.5, pt.cex=c(3,3,3))
mtext(side=2, outer=T, text='Lake-wide surface temperature (deg C)', cex=1.25, line=3)
}


pdf(file=sprintf('%s/mon_diff.pdf', outdir), w=12, h=10)
par(mar=c(4,10,4,1))
lk_names <- str_to_title(gsub('_',' ',names(ctl)))

image.plot(x=dts, y=1:ncol(ctl), z=as.matrix(glo-ctl), col=hcl.colors(100,'Blue-Red 3') , xaxt='n', yaxt='n',
		   main='Lake-wide Sfc Temp Difference (GLOBathy - Flatbottom)', ylab=NA, xlab=NA, legend.lab='degrees C')
axis(2, at=1:ncol(ctl), lab=lk_names, las=2, cex.axis=1.5)
axis.Date(1,at=dts, lab=substr(month.abb,1,1), line=0, lwd=1, cex.axis=1.25)
dev.off()










ctl_ice <- read.table('csv/ctl_ICE3D.csv', row.names=1)
glo_ice <- read.table('csv/bi0m_ICE3D.csv', row.names=1)
dts_ice <- as.Date(row.names(ctl_ice))

# clip no ice lakes (based on ctl runs)
has_ice <- apply(ctl_ice, 2, max, na.rm=T)>0
ctl_ice <- ctl_ice[,has_ice]
glo_ice <- glo_ice[,has_ice]

ctl_ice <- renamorder(ctl_ice)
glo_ice <- renamorder(glo_ice)

#
dts_ice <- dts_ice[which(!is.na(glo_ice[,1]))]
ctl_ice <- na.exclude(ctl_ice)
glo_ice <- na.exclude(glo_ice)


# plot suspect ice lakes
if(F){
	x11()
	matplot(dts_ice, ctl_ice[,c('Sebago','Goose','Oneida')], 'l', ylim=c(0,1), lty=1, ylab='FLATBOTTOM ice cover', lwd=2)
	legend('topright', legend=c('Sebago','Goose','Oneida'), text.col=1:3, cex=2)
	x11(w=10)
	matplot(dts_ice, glo_ice[,c('Sebago','Goose','Oneida')], 'l', ylim=c(0,1), lty=1, ylab='GLOBATHY ice cover', lwd=2)
	legend('topright', legend=c('Sebago','Goose','Oneida'), text.col=1:3, cex=2)
}

# clean up sticky ice pixel (persists 12/13% in globathy run)
#glo_ice[glo_ice[,'Sebago']==.13,'Sebago'] <- 0
glo_ice[glo_ice[,'Goose']==.05,'Goose'] <- 0
ctl_ice[ctl_ice[,'Goose']==.12,'Goose'] <- 0

#instead just entirely exclude sebago and goose
selbadice <- grep('Oneida|Sebago', names(ctl_ice))
glo_ice <- glo_ice[,-selbadice]
ctl_ice <- ctl_ice[,-selbadice]


# ICE DIFF PLOT
if(T){
	pdf(file=sprintf('%s/lakewide_icediff.pdf', outdir), w=20)
	par(mar=c(4,9,4,2), cex.axis=1.25)
	image.plot(x=dts_ice, y=1:ncol(ctl_ice), z=as.matrix(glo_ice-ctl_ice), col=hcl.colors(100, 'Purple-Green'), zlim=c(-1,1), 
			   xaxt='n', yaxt='n', main='Lake-wide Ice Cover Difference (GLOBathy - Flatbottom)', 
			ylab=NA, xlab=NA, legend.lab='fractional ice cover', legend.line=2.5)
	axis(2, at=1:ncol(ctl_ice), lab=names(ctl_ice), las=2)
	axis.Date(side=1, x=dts_ice, at=seq(dts_ice[1], rev(dts_ice)[1], by='month', format='%b'))
}


thresh <- .9

print('Winnipesaukee: Jan-14 Apr-24 ')
#print('Sebago:    NA        Apr-18' ) # sebago was a bad lake
for (lk in c('Winnipesaukee')){
	ctl_dts <- range(dts_ice[ctl_ice[,lk] > thresh])
	glo_dts <- range(dts_ice[glo_ice[,lk] > thresh])
	cat(sprintf('flatbottom: %s %s \n', format(ctl_dts[1], '%b-%d'), format(ctl_dts[2], '%b-%d')))
	cat(sprintf('GLOBathy: %s %s \n', format(glo_dts[1], '%b-%d'),format(ctl_dts[2], '%b-%d')))
}




