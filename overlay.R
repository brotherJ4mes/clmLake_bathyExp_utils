#!/bin/Rscript
library(rgdal)
library(raster)

var <- 'LAKE_T3D'
grid_file <- 'wrf_grid.tif'
lk_shp_fn <- 'my_lks.shp'
#dir_in <- 'ctl'

fn <- 'sub_first4.nc'

# =====================================================================================
# =====================================================================================
# =====================================================================================

grd <- raster(grid_file)
grd <- raster(grd) # drop the actual variable values create a template to use later)
lks <- readOGR(lk_shp_fn, v=F)


var_brk <- raster(grd)

ncid <- nc_open(fn)
var_mat <- t(ncvar_get(ncid, var, start=c(1,1,1,1), count=c(-1,1,-1,1)))
#nc_close(ncid)
var_mat <- var_mat[nrow(var_mat):1,]
values(var_brk) <- var_mat


pdf('first_hour_shapes.pdf', w=30, h=18)
plot(var_brk,ax=T)
raster::plot
plot(lks,add=T)
text(x=coordinates(lks)[,1], y=coordinates(lks)[,2]-1e5, labels=lks$NAME, col='blue')
dev.off()

