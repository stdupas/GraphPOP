######### Landscape############
r1<- raster(ncol=2, nrow=1)
r1[] <- rep(1,2:2)
s <- stack(x=c(r1))
r0<- raster(ncol=0, nrow=0)
s <- stack(x=c(r0))
s<-1
r2<- raster(ncol=2, nrow=1)
r2[] <- rep(1,2:2)
s<- stack(x=c(r1,r2))

p<-c(as.Date("2000-01-10"),as.Date("2000-01-11"))
p<-"2000-01-11"
p<-as.Date("2000-01-11")
p<-c(as.Date("2000-01-11"),as.Date("2000-01-11"))
p<-c(as.Date("2000-01-11"),as.Date("2000-01-10"))
p<-c(as.Date("2000-01-11"),as.Date("2000-01-11"),as.Date("2000-01-11"))

v<-"a"
v<-c("a","b")
v<-c("layer.1","layer.2")
v<-1

lands<-Landscape(rasterstack = s,period = p,vars = v)

######### LandscapeHistory ################
r1<- raster(ncol=2, nrow=1)
r1[] <- rep(1,2:2)
r2<- raster(ncol=2, nrow=1)
r2[] <- rep(2,2:2)
s<- stack(x=c(r1,r2))
p1<-c(as.Date("2000-01-11"),as.Date("2000-01-12"))
p2<-c(as.Date("2000-01-13"),as.Date("2000-01-13"))
lands<-Landscape(rasterstack = s,period = p1,vars = v)
lands2<-Landscape(rasterstack = s,period = p2,vars = v)
lLands<-list(lands,lands2)
lLands<-c(lands2,lands)
lLands<-c(1,lands)
lLands<-c(lands,1)
lLands<-list(lands,lands)
landsH<-LandscapeHistory(lLands)
landsH[[1]]["period"]
#
######### NicheModel ###################
vari<-c("l")
vari<-"l"
vari<-c(1)
vari<-c("l",1)
vari<-c(1,"l")
vari<-c("l","t")
vari<-c(NA)
vari<-c(NULL)

para<-list(1)
para<-list(c(1,5))
para<-list(1,c(1,5))
para<-list(1,2)
para<-list(c(2,3),c(1,5))
para<-list(1,c(1,5))

rea<-c(l="constant")
rea<-c(l="constant",t="envelin")
rea<-c(l="constant",t="enveline")
rea<-c(l="constant",t="envelin",p="enveloppe")

model<-NicheModel(variables=vari,parameterList=para,reactNorms=rea)
model



######### MigrationModel ###################
migraShape<-"fat_tail1"
migraShape<-"gaussian"
migraShape<-"exponential"
migraShape<-"contiguous"
migraShape<-"contiguous8"
migraShape<-"island"
migraShape<-"fat_tail2"
migraShape<-"contiguous_long_dist_mixt"
migraShape<-"gaussian_long_dist_mixt"
migraShape<-"constant"
migraShape<-"yo"
migraShape<-c("fat_tail1","gaussian")
migraShape<-1

migrapDisp<-1
migrapDisp<-c(1)
migrapDisp<-c(1,2)
migrapDisp<-"1"
migrapDisp<-1.2
migrapDisp<-c(1.2)
migrapDisp<-c(1,"1")


migramodel<-MigrationModel(shape = migraShape,param = migrapDisp)


######### EnvDinModel ###################
vari<-c("l","t")

para<-list(1,c(1,5))
para<-list(1,2)
para<-list(c(2,3),c(1,5))

rea<-c(l="constant",t="envelin")

migrapDisp<-c(1,2)
migraShape<-"fat_tail1"

nicheK<-NicheModel(variables = vari, parameterList = para,reactNorms = rea)
nicheK<-1

nicheR<-NicheModel(variables = vari, parameterList = para,reactNorms = rea)
nicheR<-"2"

migramodel<-MigrationModel(shape = migraShape,param = migrapDisp)
migramodel<-NicheModel(variables = vari, parameterList = para,reactNorms = rea)
env1<-EnvDinModel(K=nicheK,R=nicheR,migration = migramodel)
######### test fonction ###################
r1<- raster(ncol=2, nrow=1)
r1[] <- rep(1,2:2)
r2<- raster(ncol=2, nrow=1)
r2[] <- rep(1,2:2)
s<- stack(x=c(r1,r2))

p1<-as.Date("2000-01-11")
v<-c("l","p")
lands<-Landscape(rasterstack = s,period = p1,vars = v)
lands<-Landscape(rasterstack = s2,period = p1,vars = v)
