############## CREATION OF TransitionMatrix #######################################

#library(GraphPOP)
#library(Essai)
#source("./Essai/R/Landscape.R")
#source("./Essai/R/TransitionBackward.R")
# source("/home/dupas/GraphPOP/R/graphPOP.R")
# source("/home/dupas/GraphPOPR/TransitionBackward.R")
# r1<- raster(ncol=2, nrow=2)
# r1[] <- rep(2:5,1)
# r2<- raster(ncol=2, nrow=2)
# r2[] <- rep(2,2:2)
# s<- stack(x=c(r1,r2))
# extent(s)<-c(0,2,0,2)
# p1<-as.Date("2000-01-11")
# vari<-c("l","t")
# paraK<-list(c(0,5),2)
# paraR<-list(2,2)
# reaK<-c(l="envelin",t="constant")
# reaR<-c(l="constant",t="constant")
# lscp1<-landscape(rasterstack = stack(r1=raster(matrix(rep(2:5,1),ncol=2,nrow=2)),r2=raster(matrix(rep(2,2:2),ncol=2,nrow=2)),r2=raster(matrix(0,1,0,1,ncol=2,nrow=2))),period=p1,vars=vari)
# modelK<-nicheModel(variables=vari,parameterList=paraK,reactNorms=reaK)
# modelR<-nicheModel(variables=vari,parameterList=paraR,reactNorms=reaR)
# m<-migrationModel(shape="gaussian",param = (1/1.96))
# edm1<-envDynModel(K=modelK,R=modelR,migration = m)
# envDynLand <- runEnvDinModel(object = lscp1,model = edm1)
# cellNumberofNodes<-sampleLandscape(demographic=edm1, sampleSize=20)
# demo1<-createDemographic(envDynLand,cellNumberOfNodes)
# sample <-
# ############## manipulation #################
# yo<-compare(demo1,lscp1,FALSE,50,"")
# 
# heatmap(demo1["TransiBackw"])
# heatmap(demo1["TransiForw"])
# Collisionijk(hitting_time_digraph(demo1["TransiBackw"]))
# par(mfrow=c(2,2))
# for(i in 1:4){
#   a<-raster(yo[[i]])
#   plot(a,main=title(switch(EXPR=as.character(i),
#                           "1"="Simul_coalescent_X200",
#                           "2"="linearizedFstDigraph",
#                           "3"="linearizedFstUnDigraph",
#                           "4"="Stepping_Stone"))
#   )
# }
# 
# # larger landacape
# 
# extent(s)<-c(0,7,0,3)
# p1<-as.Date("2000-01-11")
# vari<-c("alt","c")
# paraK<-list(c(0,5),2)
# paraR<-list(2,2)
# reaK<-c(t="envelin",c="constant")
# reaR<-c(t="constant",c="constant")
# lscp1<-Landscape(stack(
#     raster(vals=2E4*matrix(dnorm(1:3,2,1),ncol=1)%*%matrix(dnorm(c(1:4,1:3),2,1),nrow=1),ncol=7, nrow=3,xmn=0,xmx=7,ymn=0,ymx=3),
#     raster(vals=2000,nrow=3,ncol=7,xmn=0,xmx=7,ymn=0,ymx=3)
#   ),period=p1,vars=vari)
# modelK<-NicheModel(variables=vari,parameterList=paraK,reactNorms=reaK)
# modelR<-NicheModel(variables=vari,parameterList=paraR,reactNorms=reaR)
# m<-MigrationModel(shape="gaussian",param = (1/1.96))
# edm1<-EnvDinModel(K=modelK,R=modelR,migration = m)
# demo1<-createDemographic(lscp1,edm1)
# ############## manipulation #################
# yo<-compare(demo1,lscp1,FALSE,50,"")
# 
# 
# r1<- raster(ncol=3, nrow=3)
# r1[] <- rep(c(2,4,6,1,1,1,6,4,2),1)
# r2<- raster(ncol=3, nrow=3)
# r2[] <- rep(2,9)
# s<- stack(x=c(r1,r2))
# extent(s)<-c(0,3,0,3)
# p1<-as.Date("2000-01-11")
# vari<-c("t","c")
# paraK<-list(c(0,5),10)
# paraR<-list(2,2)
# reaK<-c(t="envelin",c="constant")
# reaR<-c(t="constant",c="constant")
# lscp1<-Landscape(rasterstack = s,period=p1,vars=vari)
# modelK<-NicheModel(variables=vari,parameterList=paraK,reactNorms=reaK)
# modelR<-NicheModel(variables=vari,parameterList=paraR,reactNorms=reaR)
# m<-MigrationModel(shape="gaussian",param = (1/1.96))
# edm1<-EnvDinModel(K=modelK,R=modelR,migration = m)
# demo1<-createDemographic(lscp1,edm1)
# ############## manipulation #################
# yo<-compare(demo1,lscp1,FALSE,50,"")
# 
# coalescent<-simul_multi_coal(demo1,FALSE,50)
# lcoal<-lapply(1:50,function(n){
#   coal_2<-coalescent_2_newick(coalescent[[n]][[1]])
#   cat(coal_2, file = "ex.tre", sep = "\n")
#   tree<-read.tree("ex.tre")
#   cophenetic(tree)
# })
# a<-matrix(data = apply(sapply(lcoal,as.vector),1,mean),nrow = nrow(lcoal[[1]]),ncol = ncol(lcoal[[1]]))
# log10timescale = ceiling(log10(max(a))) 
# # we calculate the time scale of the distance matrix 
# # as the number of digit the maximum time distance 
# # do not reach
# ascaled = a/10^log10timescale
# b<-linearizedFstDigraph(demo1["TransiBackw"],lscp1)
# c<-linearizedFstUndigraph(demo1["TransiBackw"],lscp1)
# d<-apply(lscp1["distanceMatrix"],c(1,2),log)
# mat<-list(ascaled,b,c,d)
# par(mfrow=c(2,2))
# if (fname!="") pdf(fname)
# for(i in 1:4){
#   plot(bionj(mat[[i]]),main=title(switch(EXPR=as.character(i),
#                                          "1"="Simul_coalescent",
#                                          "2"="linearizedFstDigraph",
#                                          "3"="linearizedFstUnDigraph",
#                                          "4"="Stepping_Stone"))
#   )
# }
# mat
# 
# 
# heatmap(demo1["TransiBackw"])
# heatmap(demo1["TransiForw"])
# Collisionijk(hitting_time_digraph(demo1["TransiBackw"]))
# par(mfrow=c(2,2))
# for(i in 1:4){
#   a<-raster(yo[[i]])
#   plot(a,main=title(switch(EXPR=as.character(i),
#                           "1"="Simul_coalescent_X200",
#                           "2"="linearizedFstDigraph",
#                           "3"="linearizedFstUnDigraph",
#                           "4"="Stepping_Stone"))
#   )
# }
# 
# 
# ## Complex landscape
# lscp2<-Landscape(stack(
#       raster(vals=2000,nrow=9,ncol=19,xmn=0,xmx=19,ymn=0,ymx=9),
#       raster(vals=2E5*matrix(dnorm(1:9,5,2.5),ncol=1)%*%matrix(dnorm(c(1:10,1:9),5,2.5),nrow=1),ncol=19, nrow=9,xmn=0,xmx=19,ymn=0,ymx=9)),   
#       period=as.Date("2017-08-03"),
#       vars=c("constant","elevation")
# )
# 
# vari = c("constant","elevation")
# paraK<-list(10, c(0,5))
# paraR<-list(2)
# reaK<-c(constant="constant",elevation="envelin")
# reaR<-c(constant="constant")
# 
# modelK<-NicheModel(variables=vari,parameterList=paraK,reactNorms=reaK)
# modelR<-NicheModel(variables=vari[[1]],parameterList=paraR,reactNorms=reaR)
# 
# m<-MigrationModel(shape="gaussian",param = (1/1.96))
# edm2<-EnvDinModel(K=modelK,R=modelR,migration = m)
# demo2<-createDemographic(lscp2,edm2)
# 
# ############## manipulation #################
# yo<-compare(demo2,lscp2,FALSE,50,"")
# heatmap(demo1["TransiBackw"])
# heatmap(demo1["TransiForw"])
# Collisionijk(hitting_time_digraph(demo1["TransiBackw"]))
# par(mfrow=c(2,2))
# for(i in 1:4){
#   a<-raster(yo[[i]])
#   plot(a,main=title(switch(EXPR=as.character(i),
#                            "1"="Simul_coalescent_X200",
#                            "2"="linearizedFstDigraph",
#                            "3"="linearizedFstUnDigraph",
#                            "4"="Stepping_Stone"))
#   )
# }
