### draw world map using package maps
library(maps)
library(ncdf4)
## want to add grid cells of model

basename="tas_Amon_GFDL-CM3_rcp26_r1i1p1_"
combo="GFDL CM3"
base=rep(basename,10)  ### each model contains 5 years, so total=10
end=c(rep("200601-201012.nc",5),rep("201101-201512.nc",5))
files=paste(base,end,sep="")

nc=nc_open(files[1]) 
lat=ncvar_get(nc, "lat")
lon=ncvar_get(nc, "lon")

### convert lon to -180-180\
lon2=lon
for (i in 1:length(lon)) {
    if (lon[i]>180) {lon2[i]=lon[i]-360} else {lon2[i]=lon[i]}
}

lon=lon2
rm(lon2)


quilt.plot()

plot(x=-180,y=90,col="white",xlim=c(-180,180), ylim=c(-90, 90),xlab="Lon",ylab="Lat",main="Locations of RCM Output")
map("world",lwd=2, add=T)

quilt.plot(expand.grid(lon, lat),c(tas1[,,1]))
map("world",lwd=2,add=T)


### add model grid cells
for(i in 1:length(lon)){
  for(j in 1:length(lat)){
    points(lon[i],lat[j],pch=1)
  }
}


library(fields)

points(-100,30,col="red",pch=16,cex=2)

