#!/sw/pkgs/arc/stacks/gcc/10.3.0/R/4.2.0/bin/Rscript
qlibrary(rgdal)
qlibrary(raster)
qlibrary(ncdf4)


read_T <- function(fn, selx, sely){
	ncid <- nc_open(fn)
	T3D <- ncvar_get(ncid,'LAKE_T3D')-273.15  # native dims (y, z, x)  WHY??? lol
	T2D <- matrix(NA, 10, length(selx)) # saved output for this timestep
	for (z in 1:10) T2D[z,] <- T3D[,z,][cbind(selx,sely)] 
	nc_close(ncid)
	return(T2D)
}

#read_meta <- function(meta_fn='/nfs/turbo/seas-drewgron/in_situ/final_data/sensor_metadata.csv'){
#	dat <- read.csv(meta_fn)
#	df <- data.frame(name=dat$name, depth=dat$depth, lake=dat$lake)
#	pts_ll <- SpatialPointsDataFrame(dat[c('lon','lat')], proj4string=CRS('+proj=longlat +datum=WGS84'), data=df)
#	pts <- spTransform(pts_ll, '+proj=lcc +lat_0=38.5 +lon_0=-97.5 +lat_1=38.5 +lat_2=38.5 +x_0=0 +y_0=0 +R=6370000 +units=m +no_defs +type=crs')
#}


read_lks <- function()	return(readOGR('export/my_lks.shp', v=F))

read_grid <- function() return(raster('export/wrf_grid.tif'))


rastT <- function(fn, varstr){
	grd <- read_grid()
	T <- ncvar_get(nc_open(fn), varstr)[,1,]
#	T <- ncvar_get(nc_open(fn), varstr)
	values(grd) <- t(T)[1059:1,]
	return(grd)
}


