############## CREATION OF TransitionMatrix #######################################
R
#library(GraphPOP)
#library(Essai)
#source("./Essai/R/Landscape.R")
#source("./Essai/R/TransitionBackward.R")
source("/home/dupas/GraphPOP/GraphPOP/R/Landscape.R")
source("/home/dupas/GraphPOP/GraphPOP/R/TransitionBackward.R")
r1<- raster(ncol=2, nrow=2)
r1[] <- rep(2:5,1)
r2<- raster(ncol=2, nrow=2)
r2[] <- rep(2,2:2)
s<- stack(x=c(r1,r2))
p1<-as.Date("2000-01-11")
vari<-c("l","t")
paraK<-list(c(0,5),2)
paraR<-list(2,2)
reaK<-c(l="envelin",t="constant")
reaR<-c(l="constant",t="constant")
extent(s)<-c(0,2,0,2)
lscp1<-Landscape(rasterstack = s,period=p1,vars=vari)
modelK<-NicheModel(variables=vari,parameterList=paraK,reactNorms=reaK)
modelR<-NicheModel(variables=vari,parameterList=paraR,reactNorms=reaR)
m<-MigrationModel(shape="gaussian",param = (1/1.96))
edm1<-EnvDinModel(K=modelK,R=modelR,migration = m)
demo1<-createDemographic(lscp1,edm1)
############## manipulation #################
yo<-compare(demo1,lscp1,FALSE,50)
heatmap(demo1["TransiBackw"])
heatmap(demo1["TransiForw"])
Collisionijk(hitting_time_digraph(demo1["TransiBackw"]))
par(mfrow=c(2,2))
for(i in 1:4){
  a<-raster(yo[[i]])
  plot(a,main=title(switch(EXPR=as.character(i),
                          "1"="Simul_coalescent_X200",
                          "2"="linearizedFstDigraph",
                          "3"="linearizedFstUnDigraph",
                          "4"="Stepping_Stone"))
  )
}

## Complex landscape
lscp<-Landscape(stack(
      raster(vals=2000,nrow=9,ncol=19),
      raster(vals=2E5*matrix(dnorm(1:9,5,2.5),ncol=1)%*%matrix(dnorm(c(1:10,1:9),5,2.5),nrow=1),ncol=19, nrow=9)),   
      period=as.Date("2017-08-03"),
      vars=c("constant","elevation")
)

modelK<-NicheModel(variables=c("constant"),parameterList=c(2000),reactNorms=reaK)
modelR<-NicheModel(variables=vari,parameterList=paraR,reactNorms=reaR)
m<-MigrationModel(shape="gaussian",param = (1/1.96))
edm1<-EnvDinModel(K=modelK,R=modelR,migration = m)
demo1<-createDemographic(lscp1,edm1)

