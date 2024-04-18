#!/bin/Rscript
qlibrary(viridis)
qlibrary(stringr)
qlibrary(fields)

outdir <- '/home/kessler/work/jtti/65e0d491f698c7b0fdfee2b7/figures/'

rng <- list(min=-25, max=50)

plt <- F
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

# plot for amount of lake valid
#barplot(apply(frac, 2, function(x) sum(x>=.5, na.rm=T)/365), ylim=c(0,1), las=2)

dts_obs <- as.Date(row.names(obs), format='%Y%m%d')

ctl <-  read.table('csv/ctl_T3D.csv', row.names=1)
glo <-  read.table('csv/bi0m_T3D.csv', row.names=1)
glo2 <- read.table('csv/bi2m_T3D.csv', row.names=1)
dts_mod <- as.Date(row.names(ctl))
#glo2 <- read.table('bi2m_T3D.csv')


# filter for wild values
ctl <- filter_rng(ctl)
glo <- filter_rng(glo)
glo2 <- filter_rng(glo2)
obs <- filter_rng(obs)





lks <- names(obs)

ctl <-  ctl[!dts_mod < min(dts_obs),]
glo <-  glo[!dts_mod < min(dts_obs),]
glo2 <-  glo2[!dts_mod < min(dts_obs),]
dts <- dts_obs




# calculate monthly data
mons <- list(substr(rownames(obs), 5, 6))
obs <- aggregate(obs, by=mons, mean, na.rm=T)[,-1]
ctl <- aggregate(ctl, by=mons, mean, na.rm=T)[,-1]
glo <- aggregate(glo, by=mons, mean, na.rm=T)[,-1]
dts <- as.Date(sprintf('2019-%02i-01', 1:12))

# PLOT MONTHLY AVERAGES compared to obs
pdf(file=sprintf('%s/mon_validate.pdf', outdir), w=20)
layout(matrix(1:20, 5,4)); par(mar=c(0,0,0,0), oma=c(3,5,3,3), cex.axis=1.5)
for (lk in lks){
	if(all(is.na(obs[,lk]))) next
	if(T){ # MANUAL SWITCH TO PLOT
		plot(dts, obs[,lk], pch=NA, ylab=NA, xlab=NA, xaxt='n', yaxt='n', ylim=c(-15,35))
		points(dts, obs[,lk], pch=15, cex=2.5, col='black')
		points(dts, ctl[,lk], col=ctlcol, pch=19, cex=2)
		points(dts, glo[,lk], col=glocol, pch=17, cex=2)
		mtext(side=3, adj=0, line=-2.5, text=sprintf(' %s',str_to_title(gsub('_',' ',lk))),  cex=1.5)

		yi <- par('mfg')[1]
		xi <- par('mfg')[2]

		# yaxes
		if(xi==1 && yi%%2==0) axis(2, at=seq(-10,30,by=10), lwd=0)
		if(xi==1) axis(2, at=seq(-10,30,by=10), lab=NA)
		if(xi==4 && yi%%2==1) axis(4, at=seq(-10,30,by=10), lwd=0)
		if(xi==4) axis(4, at=seq(-10,30,by=10), lab=NA)

		# xaxes 
		if(yi==1) axis.Date(3,at=dts, lab=NA)
		if(yi==5 || lk=='nipigon') axis.Date(1,at=dts, lab=substr(month.abb,1,1), line=0, lwd=0)
	}
}
plot.new()
#legend('center', legend=c('Flatbottom', 'GLOBathy','Satellite Data'))
legend('center', legend=c('Flatbottom', 'GLOBathy','Satellite Data'), col=c(ctlcol, glocol, 'black'), pch=c(19,17,15), cex=3)
#legend('center', legend=c('Flatbottom', 'GLOBathy','Satellite Data'), col=c(ctlcol, glocol, 'black'), lty=NA, pch=c(19,17,15))#, cex=2.5, pt.cex=c(3,3,3))
mtext(side=2, outer=T, text='Lake-wide surface temperature (deg C)', cex=1.25, line=3)


graphics.off()

#x11()
#par(mar=c(4,8,4,1))
#lk_names <- str_to_title(gsub('_',' ',names(ctl)))
#
#image.plot(x=dts, y=1:ncol(ctl), z=as.matrix(glo-ctl), col=hcl.colors(100,'Blue-Red 3') , xaxt='n', yaxt='n',
#		   main='Lake-wide Sfc Temp Difference (GLOBathy - Flatbottom)', ylab=NA, xlab=NA, legend.lab='degrees C')
#axis(2, at=1:ncol(ctl), lab=lk_names, las=2)
#axis.Date(1,at=dts, lab=substr(month.abb,1,1), line=0, lwd=1, cex.axis=1.25)

