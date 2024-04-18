################################################################################
############################ User Controls #####################################
################################################################################

# load array z and dz, dims are sensor, column layer, model version
source('get_z_depths.R')

# read in obs and obs metadata
meta    = read.csv('csv/all_metadata_adjusted.csv')
obs     = read.csv('csv/all_data_hourly.csv')[-8761,]
column_temps_dir = 'csv/Column_temps'

# many locations have very low temperatures (around -20), which throws off the color scale
# change all values below lower_lim to equal lower_lim?

# select locations of interest (multiple depths or no)
locs = names(table(substr(names(obs)[-1],1,5)))
dts <- seq.POSIXt(as.POSIXct('2019-01-01 00:00'), by='hour', length=8760)

mypal = function(n) hcl.colors(n, "YlOrRd", rev = TRUE)


for (loc in locs){


	# read 
	ctl = as.matrix(read.csv(sprintf('%s/%s/%s_00.csv', column_temps_dir, 'ctl', loc))[,-1])
	z_ctl = z[sprintf('%s_00',loc),,'ctl']
	glo = as.matrix(read.csv(sprintf('%s/%s/%s_00.csv', column_temps_dir, 'bi0m', loc))[,-1])
	z_glo = z[sprintf('%s_00',loc),,'bi0m']
	if(all(is.na(ctl)) | all(is.na(glo))) next

	zmax <- max(z_ctl, z_glo)
	tmax <- max(ctl, glo)
	if(tmax > 100){ print(tmax); next }
	tmin <- min(ctl, glo) 
	levs <- pretty(0:tmax, n=20)
	ctl[ctl < 0] <- 0
	glo[glo < 0] <- 0
	nlevs <- length(levs)
	cmap <- mypal(nlevs-1)
	

	print(loc)

	# plot
	png(sprintf('figs/%s.png',loc), width=1500, h=600)
	#png('test.png', width=1500, h=600)
	#pdf('test.pdf', width=22, h=10, pointsize=22)
	#pdf('test.pdf', width=12, h=6)
	layout(matrix(c(1,3,2,3), 2,2), heights=c(.8,.2)); #png
	#layout(matrix(c(1,3,2,3), 2,2), heights=c(.85,.15)); #pdf
	par(mar=c(0,0.5,0,0.5), oma=c(0,6,4,2), cex.axis=1.75, cex.lab=1.75)

	frame(); plot.window(xlim=range(dts), y=c(zmax,0), xaxs='i', yaxs='i')
	.filled.contour(x=dts, y=z_ctl, z=ctl, levels=levs, col=cmap); box()
	axis.POSIXct(1,at=seq.POSIXt(dts[1], rev(dts)[1], by='month'), lab=substr(month.abb,1,1))
	axis(2); axis(4, lab=NA, tcl=-.25); 
	mtext('depth (m)', 2, line=4, cex=1.75); mtext('flatbottom', 3, cex=1.75, line=.5)

	frame(); plot.window(xlim=range(dts), y=c(zmax,0), xaxs='i', yaxs='i')
	.filled.contour(x=dts, y=z_glo, z=glo, levels=levs, col=cmap); box()
	axis.POSIXct(1,at=seq.POSIXt(dts[1], rev(dts)[1], by='month'), lab=substr(month.abb,1,1))
	axis(2, lab=NA, tcl=-.25);	axis(4, lab=NA); mtext('GLOBathy', 3, cex=1.75, line=.5)

	mtext(side=3, outer=T, loc, cex=2,line=2)

	par(mar=c(2,40,5.5,40)) # png
#	par(mar=c(1,30,5,30))  #pdf
	plot.new()
	plot.window(ylim=c(0,1), xlim=c(0,nlevs))
	rect(0:(nlevs-2), 0, 1:(nlevs-1),1, col=cmap, border='white')
	axis(1, at=(1:nlevs)[c(T,F,F)]-0.5, lab=levs[c(T,F,F)], lwd=0, line=-.5)
	mtext('temperature (deg C)', 3, cex=1.75)


	dev.off()
}


