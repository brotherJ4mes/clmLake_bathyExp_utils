#!/bin/Rscript
library(rgdal)
library(raster)
library(maps)

area_thresh=10

all_lks <- readOGR('~/work/geospatial_data/GloBathy/conus/conus_lakes.shp') # read in ~48K lakes in CONUS

# omit smaller lakes for plotting 
all_lks <- all_lks[all_lks$AREA_KM2 > area_thresh,]

# read in table and create from it a spatialpts data frame
lks <- read.table('lakes_to_extract.tsv', head=T)
lk_pts <- SpatialPointsDataFrame(lks[,c('lon','lat')], data=lks, proj4string=crs(proj4string(all_lks))) 


plot(all_lks, ax=T, col='lightblue', border=NA)
plot(lk_pts, cex=.5, pch=25, bg='red', add=T)
text(lk_pts, lab=lk_pts$name, pos=1)
map('usa', add=T)



