#!/bin/Rscript
library(fields)


lk <- 'manitoba'


# read surface
#ncid <- nc_open(sprintf('%s_surf_exp.nc', lk))
ncid <- nc_open(sprintf('%s_surf.nc', lk))
tsk <- ncvar_get(ncid,'TSK')-273.15
d <- ncvar_get(ncid,'LAKE_DEPTH')[,,1]
nc_close(ncid)


# read 3D
#ncid <- nc_open(sprintf('%s_3D_exp.nc', lk))
ncid <- nc_open(sprintf('%s_3D.nc', lk))
t3 <- ncvar_get(ncid, 'T_LAKE3D')-273.15    # dims [Y, X, t, z]???
#nc_close(ncid)



# locate largest north-south/east-west transect of lake
i <- which.max(apply(!is.na(d), 2, sum))
j <- which.max(apply(!is.na(d), 1, sum))


X <- 1:nrow(t3)
Y <- 1:ncol(t3)
Z <- 1:dim(t3)[4]

Tlim <- c(16,22)

cmap <- viridis(20)

t <- 1

for (t in 1:12){
	print(t)
	#png(sprintf('%s_exp/%03i.png', lk, t), w=1900, h=900, pointsize=20)
	png(sprintf('%s_ctl_cbar/%03i.png', lk, t), w=1900, h=900, pointsize=20)
	layout(rbind(1:3))
	par(mar=c(5,1,4,1), oma=c(5,0,2,0))

	#surface plot with transect lines
	image(x=X, y=Y, tsk[,,t], col=cmap, ylim=c(max(Y),1), xaxt='n', yaxt='n', xlab=NA, ylab=NA, zlim=Tlim, main='Surface Temp')
	abline(h=i,v=j, lty=2, lwd=3, col='red')

	# zonal plot
	#image.plot(x=X, y=1:max(d[j,], na.rm=T), t3[,j,t,], xaxt='n', ylim=c(max(d, na.rm=T),0))

	image(x=X, y=Z, t3[,i,t,], xaxt='n', col=cmap, zlim=Tlim, ylim=rev(range(Z)), main='Zonal Transect', xlab=NA)

	image(x=Y, y=Z, t3[j,,t,], xaxt='n', col=cmap, zlim=Tlim, ylim=rev(range(Z)), main='Meridional Transect', xlab=NA)

	mtext(sprintf('\ttime=%i',t), side=3, outer=T, adj=0, cex=2)

	par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), mar=c(0, 0, 0, 0), new=TRUE)
	image.plot(tsk[,,t], legend.only=T, horizontal=T, col=cmap, zlim=Tlim, legend.args=list(text='water temp (deg C)', line=1))
	#mtext(sprintf('time=%i',t), side=1)

	dev.off()
}
