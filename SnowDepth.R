library(raster)
library(maptools)
library(ggplot2)
library(lidR)


## Get snow depth measured points
SEED <- raster("E:/SnowForestRelationships/RuddTank_SEED-DEM_50cm.tif")
# SNWDEP.poly <- readShapePoly("E:/SnowForestRelationships/SNWDEP-ALL2_0p5BUFFER.shp")
SNWDEP.poly <- readShapePoly("E:/SnowForestRelationships/SNWDEP-ALL2_1mBUFFER.shp")
crs(SNWDEP.poly) <- proj4string(SEED)
SNWDEP.poly@data <- SNWDEP.poly@data[,2:3]

SNWDEP.poly@data$gnd.min <- NA
SNWDEP.poly@data$gnd.25 <- NA
SNWDEP.poly@data$gnd.50 <- NA
SNWDEP.poly@data$gnd.75 <- NA
SNWDEP.poly@data$gnd.max <- NA
SNWDEP.poly@data$gnd.ave <- NA
SNWDEP.poly@data$gnd.cnt <- NA

SNWDEP.poly@data$velo.min <- NA
SNWDEP.poly@data$velo.05 <- NA
SNWDEP.poly@data$velo.10 <- NA
SNWDEP.poly@data$velo.25 <- NA
SNWDEP.poly@data$velo.50 <- NA
SNWDEP.poly@data$velo.75 <- NA
SNWDEP.poly@data$velo.max <- NA
SNWDEP.poly@data$velo.ave <- NA
SNWDEP.poly@data$velo.cnt <- NA

## lidar
gnd <- readLAS("E:/Velodyne_Kaarta/Ground_Only/SEED_1m-ground.las")

las <- readLAS("E:/Velodyne_Kaarta/Ground_Only/2017-01-26_08m-ground_ref40.las")
for(s in which(SNWDEP.poly@data$DATE=="2017-01-26")){
  gnd.class <- gnd
  lasclassify(gnd, SNWDEP.poly[s,], field="poly")
  gnd.poly = lasfilter(gnd.class, poly==TRUE)
  # hist(gnd.poly@data$Z, breaks=50)
  if(is.null(gnd.poly)==TRUE){
    SNWDEP.poly@data[s,3:9] <- rep(NA,7)}else{SNWDEP.poly@data[s,3:9] <- c(quantile(gnd.poly@data$Z, probs=c(0,.25,.5,.75,1)),mean(gnd.poly@data$Z),length(gnd.poly@data$Z))}
  # fivenum(gnd.poly@data$Z[which(gnd.poly@data$Z < 2258)])
  
  las.class <- las 
  lasclassify(las.class, SNWDEP.poly[s,], field="poly")
  velo.poly = lasfilter(las.class, poly==TRUE)
  # hist(poly@data$Z, breaks=10)
  if(is.null(velo.poly)==TRUE){
    SNWDEP.poly@data[s,10:18] <- rep(NA,9)}else{SNWDEP.poly@data[s,10:18] <- c(quantile(velo.poly@data$Z, probs=c(0,.05,.1,.25,.5,.75,1)),mean(velo.poly@data$Z),length(velo.poly@data$Z))}
  # fivenum(poly@data$Z[which(velo.poly@data$Z < 2258)])
}
las <- readLAS("E:/Velodyne_Kaarta/Ground_Only/2017-02-03_08m-ground_ref40.las")
for(s in which(SNWDEP.poly@data$DATE=="2017-02-03")){
  gnd.class <- gnd
  lasclassify(gnd, SNWDEP.poly[s,], field="poly")
  gnd.poly = lasfilter(gnd.class, poly==TRUE)
  # hist(gnd.poly@data$Z, breaks=50)
  if(is.null(gnd.poly)==TRUE){
    SNWDEP.poly@data[s,3:9] <- rep(NA,7)}else{SNWDEP.poly@data[s,3:9] <- c(quantile(gnd.poly@data$Z, probs=c(0,.25,.5,.75,1)),mean(gnd.poly@data$Z),length(gnd.poly@data$Z))}
  # fivenum(gnd.poly@data$Z[which(gnd.poly@data$Z < 2258)])
  
  las.class <- las 
  lasclassify(las.class, SNWDEP.poly[s,], field="poly")
  velo.poly = lasfilter(las.class, poly==TRUE)
  # hist(poly@data$Z, breaks=10)
  if(is.null(velo.poly)==TRUE){
    SNWDEP.poly@data[s,10:18] <- rep(NA,9)}else{SNWDEP.poly@data[s,10:18] <- c(quantile(velo.poly@data$Z, probs=c(0,.05,.1,.25,.5,.75,1)),mean(velo.poly@data$Z),length(velo.poly@data$Z))}
  # fivenum(poly@data$Z[which(velo.poly@data$Z < 2258)])
}
las <- readLAS("E:/Velodyne_Kaarta/Ground_Only/2017-02-08_08m-ground_ref40.las")
for(s in which(SNWDEP.poly@data$DATE=="2017-02-08")){
  gnd.class <- gnd
  lasclassify(gnd, SNWDEP.poly[s,], field="poly")
  gnd.poly = lasfilter(gnd.class, poly==TRUE)
  # hist(gnd.poly@data$Z, breaks=50)
  if(is.null(gnd.poly)==TRUE){
    SNWDEP.poly@data[s,3:9] <- rep(NA,7)}else{SNWDEP.poly@data[s,3:9] <- c(quantile(gnd.poly@data$Z, probs=c(0,.25,.5,.75,1)),mean(gnd.poly@data$Z),length(gnd.poly@data$Z))}
  # fivenum(gnd.poly@data$Z[which(gnd.poly@data$Z < 2258)])
  
  las.class <- las 
  lasclassify(las.class, SNWDEP.poly[s,], field="poly")
  velo.poly = lasfilter(las.class, poly==TRUE)
  # hist(poly@data$Z, breaks=10)
  if(is.null(velo.poly)==TRUE){
    SNWDEP.poly@data[s,10:18] <- rep(NA,9)}else{SNWDEP.poly@data[s,10:18] <- c(quantile(velo.poly@data$Z, probs=c(0,.05,.1,.25,.5,.75,1)),mean(velo.poly@data$Z),length(velo.poly@data$Z))}
  # fivenum(poly@data$Z[which(velo.poly@data$Z < 2258)])
}
las <- readLAS("E:/Velodyne_Kaarta/Ground_Only/2017-02-17_08m-ground_ref40.las")
for(s in which(SNWDEP.poly@data$DATE=="2017-02-17")){
  gnd.class <- gnd
  lasclassify(gnd, SNWDEP.poly[s,], field="poly")
  gnd.poly = lasfilter(gnd.class, poly==TRUE)
  # hist(gnd.poly@data$Z, breaks=50)
  if(is.null(gnd.poly)==TRUE){
    SNWDEP.poly@data[s,3:9] <- rep(NA,7)}else{SNWDEP.poly@data[s,3:9] <- c(quantile(gnd.poly@data$Z, probs=c(0,.25,.5,.75,1)),mean(gnd.poly@data$Z),length(gnd.poly@data$Z))}
  # fivenum(gnd.poly@data$Z[which(gnd.poly@data$Z < 2258)])
  
  las.class <- las 
  lasclassify(las.class, SNWDEP.poly[s,], field="poly")
  velo.poly = lasfilter(las.class, poly==TRUE)
  # hist(poly@data$Z, breaks=10)
  if(is.null(velo.poly)==TRUE){
    SNWDEP.poly@data[s,10:18] <- rep(NA,9)}else{SNWDEP.poly@data[s,10:18] <- c(quantile(velo.poly@data$Z, probs=c(0,.05,.1,.25,.5,.75,1)),mean(velo.poly@data$Z),length(velo.poly@data$Z))}
  # fivenum(poly@data$Z[which(velo.poly@data$Z < 2258)])
}

SNWDEP.poly@data$DEM.ave <- NA
SNWDEP.poly@data$DEM.med <- NA
SNWDEP.poly@data$DEM.min <- NA
SNWDEP.poly@data$DEM.max <- NA
for(s in 1:nrow(SNWDEP.poly@data)){
  SNWDEP.poly@data$DEM.ave[s] <- extract(SEED, SNWDEP.poly[s,], fun=mean)
  SNWDEP.poly@data$DEM.med[s] <- extract(SEED, SNWDEP.poly[s,], fun=median)
  SNWDEP.poly@data$DEM.min[s] <- extract(SEED, SNWDEP.poly[s,], fun=min)
  SNWDEP.poly@data$DEM.max[s] <- extract(SEED, SNWDEP.poly[s,], fun=max)
}

### Finally, write this shit.
write.csv(SNWDEP.poly@data, "E:/SnowForestRelationships/SNWDEP_1mGround_1mBUFFER_DATA_IntensityFilter.csv")

# dat <- read.csv("E:/SnowForestRelationships/SNWDEP-ALL2_1mBUFFER_DATA.csv", header=T)
# dat$med.dif <- dat$velo.med - dat$gnd.med
# 
# ggplot(dat, aes(x=DEPTH, y=velo.med,col=as.factor(DATE)))+
#   geom_point(aes())+
#   geom_abline(intercept=0,slope=1)


## Interpolating SNow Surfaces
library(raster)
library(lidR)
SEED <- raster("E:/SnowForestRelationships/RuddTank_SEED-DEM_1m.tif")

ref <- SEED
ref[] <- 1:ncell(SEED)

velo1 <- SEED
velo1[] <- NA

las <- readLAS("E:/Velodyne_Kaarta/Ground_Only/2017-01-26_08m-ground_ref40.las")
lasclassify(las,ref, field="ref")
velo1.stack <- stack(velo1, velo1, velo1, velo1, velo1, velo1, velo1, velo1)

library(dplyr)
gnd <- las@data %>% group_by(ref) %>% summarize(min=base::summary(Z)[1],
                                                q1=base::summary(Z)[2],
                                                med=base::summary(Z)[3],
                                                mean=base::summary(Z)[4],
                                                q3=base::summary(Z)[5],
                                                max=base::summary(Z)[6],
                                                sd=sd(Z), count=n())
gnd <- data.frame(gnd)

velo1.stack[gnd$ref] = as.matrix(gnd[,2:9])
writeRaster(velo1.stack, "E:/SnowForestRelationships/2017-01-26_1mGroundProbs_IntensFilter.tif")

las <- readLAS("E:/Velodyne_Kaarta/Ground_Only/2017-02-08_08m-ground_ref40.las")
lasclassify(las,ref, field="ref")
velo1.stack <- stack(velo1, velo1, velo1, velo1, velo1, velo1, velo1, velo1)

library(dplyr)
gnd <- las@data %>% group_by(ref) %>% summarize(min=base::summary(Z)[1],
                                                q1=base::summary(Z)[2],
                                                med=base::summary(Z)[3],
                                                mean=base::summary(Z)[4],
                                                q3=base::summary(Z)[5],
                                                max=base::summary(Z)[6],
                                                sd=sd(Z), count=n())
gnd <- data.frame(gnd)

velo1.stack[gnd$ref] = as.matrix(gnd[,2:9])
writeRaster(velo1.stack, "E:/SnowForestRelationships/2017-02-08_1mGroundProbs_IntensFilter.tif")

las <- readLAS("E:/Velodyne_Kaarta/Ground_Only/2017-02-17_08m-ground_ref40.las")
lasclassify(las,ref, field="ref")
velo1.stack <- stack(velo1, velo1, velo1, velo1, velo1, velo1, velo1, velo1)

library(dplyr)
gnd <- las@data %>% group_by(ref) %>% summarize(min=base::summary(Z)[1],
                                                q1=base::summary(Z)[2],
                                                med=base::summary(Z)[3],
                                                mean=base::summary(Z)[4],
                                                q3=base::summary(Z)[5],
                                                max=base::summary(Z)[6],
                                                sd=sd(Z), count=n())
gnd <- data.frame(gnd)

velo1.stack[gnd$ref] = as.matrix(gnd[,2:9])
writeRaster(velo1.stack, "E:/SnowForestRelationships/2017-02-17_1mGroundProbs_IntensFilter.tif")

las <- readLAS("E:/Velodyne_Kaarta/Ground_Only/2017-02-03_08m-ground_ref40.las")
lasclassify(las,ref, field="ref")
velo1.stack <- stack(velo1, velo1, velo1, velo1, velo1, velo1, velo1, velo1)

library(dplyr)
gnd <- las@data %>% group_by(ref) %>% summarize(min=base::summary(Z)[1],
                                                q1=base::summary(Z)[2],
                                                med=base::summary(Z)[3],
                                                mean=base::summary(Z)[4],
                                                q3=base::summary(Z)[5],
                                                max=base::summary(Z)[6],
                                                sd=sd(Z), count=n())
gnd <- data.frame(gnd)

velo1.stack[gnd$ref] = as.matrix(gnd[,2:9])
writeRaster(velo1.stack, "E:/SnowForestRelationships/2017-02-03_1mGroundProbs_IntensFilter.tif")

#########################################################
