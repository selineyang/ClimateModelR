library(ncdf4)
library(abind)
library(chron)
library(RColorBrewer)
library(lattice)
library(maps)
library(mapdata)
library(fields)
library(ggplot2)

### create file names (just read 2 models for example)
basename="tas_Amon_GFDL-CM3_rcp26_r1i1p1_"
combo="GFDL CM3"
base=rep(basename,10)  ### each model contains 5 years, so total=10
end=c("200601-201012.nc","201101-201512.nc","201601-202012.nc", "202101-202512.nc","202601-203012.nc",
      "203101-203512.nc","203601-204012.nc", "204101-204512.nc","204601-205012.nc","205101-205512.nc",
      "205601-206012.nc","206101-206512.nc","206601-207012.nc","207101-207512.nc","207601-208012.nc",
      "208101-208512.nc","208601-209012.nc","209101-209512.nc","209601-210012.nc")
files=paste(base,end,sep="")
nc=list()
tas=list()
#combine data??
for (i in 1:length(end)){
  nc[[i]]<-nc_open(files[i])
  tas[[i]]<-ncvar_get(nc[[i]],"tas")
} 

tasM=tas[[1]]
for (i in 2:length(end)){
  tasM<-abind(tasM,tas[[i]])
}

dim(tasM)
#change kelvin to celsius
celsius<-tasM-273.15

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

#temp in Jan
quilt.plot(expand.grid(lon, lat),c(celsius[,,1]))
map("world",lwd=2,add=T)

#90 years after , temperature in the US
quilt.plot(expand.grid(lon, lat),c(celsius[,,(90*12 + 1)]),xlim=c(-130,-60),ylim=c(25,50))
map("state",lwd=2,add=T)
plot(expand.grid(lon,lat), xlim=c(-128,-66),ylim=c(25,50),xlab="lon", ylab="lat")
map("state", add=T)

#North Carolina
plot(expand.grid(lon,lat), xlim=c(-85,-75),ylim=c(32,38),xlab="lon", ylab="lat")
map("state",regions = "North Carolina", add=T)
points(x=-80.2442, y=36.2442, col="red",pch=16)#Winston Salem

#time series
plot(celsius[90,40,], type="l") 

## which grid cell covers W-S?  plug in for first 2 coordinates, make time plot
plot(celsius[112,63,], type="l", main = "Temperature of Winston-Salem")


#task2/14/2018:
#make time series plot for all grid  cell in NC , in CO!!
#draw map of NC, plot points at the lon lat us grid cells!!!
#
par(mfrow=c(4,2))
plot(celsius[112,64,], type="l",main = "1") 
plot(celsius[113,64,], type="l",main = "2") 
plot(celsius[114,64,], type="l",main = "3") 
plot(celsius[111,63,], type="l",main = "4") 
plot(celsius[112,63,], type="l",main = "5") 
plot(celsius[113,63,], type="l",main = "6") 
plot(celsius[114,63,], type="l",main = "7") 

dev.off()
#Colorado
plot(expand.grid(lon,lat), xlim=c(-110,-100),ylim=c(35,42),xlab="lon", ylab="lat", main="Colorado")
map("state",regions = "Colorado", add=T)

par(mfrow=c(3,3))
for (i in 64:66){
  for (j in 101:103){
    plot(celsius[j,i,], type="l")
  }
}

dev.off()
#find the index for certain bound
which(lon > -120 & lon < -80)

#nc=nc_open(files[1]) 
### can type nc in console to see the model

#tas1<-ncvar_get(nc,"tas") ### can get a subset of all data (start=(xc,yc,timec)...)
#rm(nc) ###just save space

### new model
#nc=nc_open(files[6]) 
#tas2<-ncvar_get(nc,"tas")
#rm(nc)

## combine by time
#tas<-abind(tas1,tas2,along = 3)


#expand(lon,lat)
#plot(expand.grid(lon,lat))

glbmatrix=as.data.frame(expand.grid(lon,lat))
plot(glbmatrix)
dim(glbmatrix)
M<-Matrix(0,nrow = length(lat),ncol = length(lon))
M
dim(M)

#Extract US from the matrix
#find the bound of US and label all in the bound as 1
for(row in 59:70) {
  for(col in 95:117) {
    M[row, col] = 1
  }
}
#print out the rectangel region
M[c(59:70),c(95:117)]

#label those not in US as 0 
M[70,c(108:117)] = 0
M[69,c(111:116)] = 0
M[68,c(112:114)] = 0
M[67,c(112,117)] = 0
M[66,117] = 0
M[65,c(115:117)] = 0
M[64,c(95,115:117)] = 0
M[63,c(95,115:117)] = 0
M[62,c(95:97,114:117)] = 0
M[61,c(95:99,113:117)] = 0
M[60,c(95:102,110,111,113:117)] = 0
M[59,c(95:104,106:111,113:117)] = 0

#print out new matrix 
M[c(59:70),c(95:117)]



#US map in grid plot
plot(expand.grid(lon,lat), xlim=c(-128,-66),ylim=c(25,50),xlab="lon", ylab="lat")
map("usa", add=T)

which(lon>=-124.68 & lon<=-67.01)
which(lat>=25.13 & lat<=49.38)

#color the bundary of US
for (i in 95:117){
  for (j in 59:70){
    points(x=lon[i],y=lat[j],col="red",pch=16)
  }
}


##-----------------------------------------

plot(expand.grid(lon,lat), xlim=c(-128,-66),ylim=c(25,50),xlab="lon", ylab="lat")
map("usa", add=T)

#find x and y for M where the value equal 1
oneM<-which(M==1, arr.ind = T)

#plot the us region "green" using matrix oneM
points(x=lon[oneM[,2]],y=lat[oneM[,1]],col="green",pch=16)


# this one should work, but somthing wrong 
#function(find_1) =find_1(x)

##-------------------------------------------
usa<-map_data("usa")
head(usa)
length(usa$long)
summary(usa)

##----------------------------------------------------------------
#find average temperature time series of the US 
dim(celsius)
oneM[2,1]
oneM[2,2]
dim(oneM)
#celsius[lon, lat, month]
#lon=oneM[,2], lat=oneM[,1]
plot(celsius[oneM[,2][1],oneM[,1][1],], type="l")

#find the average temp for month1
for (i in 1:length(oneM[,2])){
  for (j in 1:length(oneM[,1])){
    month1[i]<-celsius[oneM[,2][i], oneM[,1][j],1]
  }
}
month1
mean(month1)

#now the problem is to how to loop through 1140 month, 3 for loop? It took a while though 
store<-array(0,c(1140,190))

for (i in 1:length(oneM[,2])){
  for (j in 1:length(oneM[,1])){
    store[1,i]<-cbind(celsius[oneM[,2][i], oneM[,1][j],1])
  }
}

#store all the temperature into matrix 'store'
for (h in 1:1140){
  for (i in 1:length(oneM[,2])){
    for (j in 1:length(oneM[,1])){
      store[h,i]<-cbind(celsius[oneM[,2][i], oneM[,1][j],h])
    }
  }
}

#find the mean temperature for each month
avgtemp=vector()
for (i in 1:1140){
  avgtemp[i]<-mean(store[i,])
}
avgtemp
#time series for average temperature
plot(avgtemp, type="l")




#sum=0
#for (i in 1:190){
  #sum=sum+month1[i]0
#}
dim(oneM)
length(oneM[,1])
oneM
