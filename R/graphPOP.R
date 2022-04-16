library(ape)
library(stringr)
library(markovchain)
library(matrixcalc)
library(MASS)
library(raster)


############## CLASS AND VALIDITY ####


setClass("envDynData",
         # envDynData are raster layers containing information about environmental variables
         # and or connectivity variables that will allow to buil niche models and friction models
         # the layers that contain environmental variable have a connectionType = FALSE
         # the layers that contain connected type variable have a connectionType = TRUE
         # connected class variables are coded as follows; when cell value is :
         # - 0 : the cell is not connected to any other cell 
         # - n!=0 : the cell is connected to the cells having the same value
         contains = "RasterStack",
         representation (connectionType="character"),
         prototype({r1<- raster(ncol=2, nrow=2)
         r1[] <- rep(2:5,1)
         r2<- raster(ncol=2, nrow=2)
         r2[] <- c(TRUE,FALSE,TRUE,FALSE)
         s<- stack(x=c(r1,r2))
         names(s)<-c("t","con")
         extent(s)<-c(0,2,0,2)
         s},connectionType=c("raster","groups"))
)


setValidity("envDynData",
            function(object){
              if (length(object@connectionType)!=length(oject)) return("the length of connectionType slot informing the type of connection data and the length of rasters stack differ")
              if (!all(object@connectionType%in%c("raster","vector","groups"))) return("connectionType argument character value was not any of 'raster', 'vector', or 'group'")
                return(TRUE)
            })

envDynData<-function(rasterstack=rasterstack,connectionType=typeCon){
  if(class(rasterstack)!="RasterStack")stop("error in Landscape rasterstack : rasterstack just accept RasterStack!")
  if (length(rasterstack@layers)==0)stop("there is no raster in the stack")
  if(class(connectionType)!="logical")stop("the slot connectionType should contain FALSE/TRUE values")
  if(length(connectionType)!=length(rasterstack@layers))stop("the length of rasterstack and slot connectionType should be identical")
  new("landscape",rasterstack,connectionType=typeCon)
}

setClass("envDynHistory",
         slots = c(envDynHist="list",dates="Date"),
         validity = function(object){
           if (any(lapply(object@envDynHist,FUN = function(x) class(x)!="environment"))) stop("the list envDynList has to be of class envDynData")
         })

reactionNorm = c("constant","enveloppe","envelin","conQuadratic","conQuadraticSkw")

npNiche <- function(x) {unlist(lapply(x,function(x) switch(x[],
                                                           constant=1,
                                                           #                                                         proportional=1,
                                                           enveloppe=2,
                                                           envelin=2,
                                                           #                                                         quadratic=4,
                                                           conQuadratic=2,
                                                           conQuadraticSkw=2)
))
}

setClass("nicheModel",
         slots = c(var="character",reactNorms="character",parameterList="list"),
         # defines the reaction norm for each variable, without interaction in this version
         # var : the variable names to which the niche model applies
         # reactNorms : the reaction norm for the variable (there should be one reaction norm per variable)
         # parameterList: the list of numerical parameters vectors for each reaction norm, there should be a parameter vector for each reaction norm
         validity=function(object){
           if(class(object@var)!="character")stop("error in NicheModel variables : variables just accept character!")
           if(!is.list(object@parameterList))stop("error in NicheModel parameterList : parameterList just accept list!")
           if (!all(object@reactNorms)%in%reactionNorm)
           if(FALSE%in%lapply(object@parameterList,is.numeric))stop("error in NicheModel parameter list : Parameter list just accept numeric!")
           if(!all(object@var)%in%names(object@reactNorms))stop("error in NicheModel : number of variables and number of reaction norms do not correspond")
           notMatching <- (unlist(lapply(1:length(object@parameterList),function(x) nbpar(object@reactNorms[x]) != length(object@parameterList[[x]]))))
           if (any(notMatching)) stop(paste("error in NicheModel : number of paremeters and reactionNorm do not match for variable ",which(notMatching),". ",sep=""))
           TRUE
         },
         prototype(
           vari<-c("temp")
           paraK<-list(c(0,5),2)
           paraR<-list(2,2)
           reaK<-c(temp="envelin",temp="constant")
           reaR<-c(temp="constant",temp="constant")
         )
)

prototype({
  r1<- raster(ncol=2, nrow=2)
  r1[] <- rep(2:5,1)
  r2<- raster(ncol=2, nrow=2)
  r2[] <- c(TRUE,FALSE,TRUE,FALSE)
  s<- stack(x=c(r1,r2))
  names(s)<-c("temp","connect")
  extent(s)<-c(0,2,0,2)
  s
},connectionType=c("raster","groups"))

nicheModel<-function(var=characterVector1,reactNorms=characterVector2,parameterList=listOfNumeric){#,form=formul){
  names(parameterList)=var
  names(reactNorms)=var
  new("nicheModel",var=var,reactNorms=reactNorms,parameterList=parameterList)#,form=form)
}

migrationShape<- c("classes","routes","fat_tail1","gaussian","exponential","contiguous","contiguous8","island","fat_tail2","contiguous_long_dist_mixt","gaussian_long_dist_mixt")

npMig <- function(x) {unlist(lapply(x,function(x) switch(x[],
                                                         fat_tail1=2,
                                                         gaussian=1,
                                                         exponential=1,
                                                         contiguous=1,
                                                         contiguous8=1,
                                                         island=1,
                                                         fat_tail2=2,
                                                         contiguous_long_dist_mixt=2,
                                                         gaussian_long_dist_mixt=2)
))
}

validitymigrationModel=function(object){
  if(!is.character(object@shapeDisp))stop("error in  migrationModel shapeDisp : ShapeDisp just accept character!")
  if(length(object@shapeDisp)!=1)stop("error in  migrationModel shapeDisp : ShapeDisp is  not valid because it contains more or less than one shape!")
  if(!object@shapeDisp%in%listOfMigrationShape)stop("error in  migrationModel shapeDisp : the given shape not exist for migrationModel !")
  if(FALSE%in%lapply(object@pDisp,is.numeric))stop("error in migrationModel pDisp : pDisp just accept numeric!")
  if(nbpar(object@shapeDisp)!=length(object@pDisp))stop("error in migrationModel : number of paremeters and shapeDisp do not match for variable")
  TRUE
}

setClass("migrationModel",
         slots = c(var="character",shapeDisp="migrationShape",pDisp="list"),
         validity = validitymigrationModel
)



migrationModel<-function(shape=character,param=p){
  new("migrationModel",shapeDisp=shape,pDisp=param)
}

setClass("envDynModel",
         slots=c(K="nicheModel",R="nicheModel",mig="migrationModel")
         #         validity=function(object){
         #           if(class(object@K)!="NicheModel")stop("Error in envDynModel K : K just accept NicheModel !")
         #           if(class(object@R)!="NicheModel")stop("Error in envDynModel R : R just accept NicheModel !")
         #           if(class(object@migration)!="migrationModel")stop("Error in envDynModel migration : migration just accept migrationModel !")
         #         }
)

envDynModel<-function(K,R,migration){
  new("envDynModel",K=K,R=R,migration=migration)
}

setClass("envDynLandscape",
         contains=c(envDynModel="envDynModel"),
         slots = c(K="raster",R="raster",m="matrix",transFor="transitionMatrixForward",transBack="transitionMatrixBackward",environment="landscape"),
#         validity=function(object){
#           if(class(object@K)!="raster")stop("Error in envDynLandscape: K should be of class raster")
#           if(class(object@R)!="raster")stop("Error in envDynLandscape: R should be of class raster")
#           if(class(object@migration)!="matrix")stop("Error in envDynLandscape: migration should be of class matrix")
#         }
)

envDynLandscape<-function(K,R,migMat,transForMat,transBackMat,environment,envDynModel){
  new("envDynLandscape",K=K,R=R,m=migMat,transFor=transForMat,transBack=transBackMat,environment=environment,envDynModel)
}

setClass("TransitionBackward",
         contains = "matrix",
         validity = function(object){
           if (all(nrow(object)==0))stop("The matrix is empty.")
           if (nrow(object)!=ncol(object))stop("The matrix is not square")
           if (!all(rowSums(object)>0.999999999 && rowSums(object)<1.000000001))stop("The sum of probabilities in each row is not 1")
         }
)


TransitionBackward<- function(matrix){
  if (nrow(matrix)!=ncol(matrix))stop("The matrix is not square")
  if(class(rownames(matrix)[1])!="character"){
    lname <- c(1:nrow(matrix))
    rownames(matrix) <- lname
    colnames(matrix) <- lname
  }
  new(Class="TransitionBackward",matrix)
}


setClass("TransitionForward",
         contains = "matrix",
         validity = function(object){
                        if (all(nrow(object)==0))stop("The matrix is empty.")
                        if (nrow(object)!=ncol(object))stop("The matrix is not square")
                      }
)

Demographic<-setClass("Demographic",
                      contains = "envDynLandscape",
                      slots = c(sampleCells="integer"),
                      validity = function(object){
                        if(any(object@K<0))stop("K is negative")
                        if(any(object@R<0))stop("R is negative")
                        if(any(object@sampleCells>nCellA(object)))stop("Sample cell number outside the range")
                      }
)# Demographic contains all the informacion to run a coalescent and calculate probabilities and graph statistics


############## METHODS #####

setGeneric(
  name = "transitionMatrixBackward",
  def=function(K,R,mig){return(standardGeneric("transitionMatrixBackward"))}
)

setGeneric(
  name = "sampleLandscape",
  def=function(demographic, sampleSize,xy, option){return(standardGeneric("sampleLandscape"))}
)

setGeneric(
  name = "transitionMatrixForward",
  def=function(K,R,mig,meth){return(standardGeneric("transitionMatrixForward"))}
)

setGeneric(
  name = "laplaceMatrix",
  def=function(object){return(standardGeneric("laplaceMatrix"))}
)

setGeneric(
  name = "ordinary_laplacian",
  def=function(object){return(standardGeneric("ordinary_laplacian"))}
)


setGeneric(
  name = "hitting_time_digraph",
  def=function(object){return(standardGeneric("hitting_time_digraph"))}
)

setGeneric(
  name = "commute_time_digraph",
  def=function(object){return(standardGeneric("commute_time_digraph"))}
)

setGeneric(
  name = "simul_coalescent",
  def=function(demographic,printCoal){return(standardGeneric("simul_coalescent"))}
)

setGeneric(
  name = "simul_multi_coal",
  def=function(demographic,printCoal,iteration){return(standardGeneric("simul_multi_coal"))}
)

setGeneric(
  name = "compare",
  def=function(demographic,popSize,printCoal,iteration,fname){return(standardGeneric("compare"))}
)

setGeneric(
  name = "Collisionijk",
  def=function(Hitting_mat){return(standardGeneric("Collisionijk"))}
)

setGeneric(
  name = "linearizedFstDigraph",
  def=function(transition, popSize){return(standardGeneric("linearizedFstDigraph"))}
)

setGeneric(
  name = "coalescent_2_newick",
  def=function(coalescent){return(standardGeneric("coalescent_2_newick"))}
)

setGeneric(
  name = "linearizedFstUndigraph",
  def=function(transition, popSize){return(standardGeneric("linearizedFstUndigraph"))}
)

setGeneric(
  name = "commute_time_undigraph",
  def=function(object){return(standardGeneric("commute_time_undigraph"))}
)


setMethod(
  f ="[",
  signature = c(x="Demographic" ,i="character",j="missing"),
  definition = function (x ,i ,j , drop ){
    switch ( EXPR =i,
             "K" ={return(x@K)} ,
             "R" ={return(x@R)} ,
             "TransiBackw" ={return(x@TransiBackw)} ,
             "TransiForw" = {return(x@TransiForw)},
             stop("This slots doesn't exist!")
    )
  }
)

setMethod(
  f ="[",
  signature = c(x="envDynModel" ,i="character",j="missing"),
  definition = function (x ,i ,j , drop ){
    switch ( EXPR =i,
             "K" ={return(x@K)} ,
             "R" ={return(x@R)} ,
             "migration" ={return(x@migration)} ,
             stop("This slots doesn't exist!")
    )
  }
)

setMethod(
  f ="[",
  signature = c(x="migrationModel" ,i="character",j="missing"),
  definition = function (x ,i ,j , drop ){
    switch ( EXPR =i,
             "pDisp" ={return(x@pDisp)} ,
             "shapeDisp" ={return(x@shapeDisp)} ,
             stop("This slots doesn't exist!")
    )
  }
)

setMethod(
  f ="[",
  signature = c(x="landscape" ,i="character",j="missing"),
  definition = function (x ,i ,j , drop ){
    switch ( EXPR =i,
             "period" ={return(x@period)} ,
             "distanceMatrix" ={return(x@distanceMatrix)} ,
             "vars" ={return(x@vars)} ,
             stop("This slots doesn't exist!")
    )
  }
)

setMethod("runNicheModel",
          signature=c("landscape","NicheModel"),
          definition = function(object,model){                  #X=object, p=,shape=
            Y=lapply(model@variables,function(x){
              switch(model@reactNorms[[x]],
                     constant={setValues(object[[x]],rep(model@parameterList[[x]],ncell(object[[x]])))},
                     #proportional = {values(object[[x]])=object[[x]]*model@parameterList[[x]]},
                     enveloppe = {object[[x]]=enveloppe(object[[x]],model@parameterList[[x]])},
                     envelin={object[[x]]=envelinear(object[[x]],model@parameterList[[x]])},
                     conQuadratic={object[[x]]=conQuadratic(object[[x]],model@parameterList[[x]])},
                     conQuadraticSkw={object[[x]]=conQuadraticSkw(object[[x]],model@parameterList[[x]])},#conquadraticskewed=conquadraticskewed(object[,,(model@variables==x)],p),
                     #conquadraticsq=conquadraticsq(object[,,(model@variables==x)],p),
                     #conquadraticskewedsq=conquadraticskewedsq(object[,,(model@variables==x)],p)
                     stop("This variable does not exist for NicheModel !")
              )
            }
            )
            Y=prod(stack(Y))
          }
)



enveloppe <- function(X,p){
  if(length(p)!=2)stop("The parameter of envelope must have two dimensions")
  else X>=p[1]&X<=p[2]
}

envelinear <- function(X, p) {
  if(length(p)!=2)stop("The parameter of envelinear must have two dimensions, p[1] is the value at Y = 0, p[2] is the value of X at Y = 1")
  else (X-p[1])/(p[2]-p[1])*enveloppe(X,p)
}

constant <- function(X,p){X[]<-p}

conQuadratic <- function(X,p)
{
  if(length(p)!=2)stop("The parameter is  not valid because it contains more or less than two values")
  else -4*(X-p[2])*(X-p[1])/((p[2]-p[1])^2)*enveloppe(X,p)
}

conQuadraticSkw <- function(X,p){
  conQuadratic(X,p)*envelinear(X,p)
  X
}

######### CREER TRANSITION MATRIX ###############################################################################
setMethod(f="runEnvDynModel",
          signature=c("landscape","envDynModel"),
          definition=function(object,model){
            R<-runNicheModel(object,model["R"])
            K<-runNicheModel(object,model["K"])
            migrationMat<-migrationMatrix(object,model["migration"])
            b<-transitionMatrixBackward(K=values(K),R=values(R),mig=migrationMat)      
            f<-transitionMatrixForward(K=values(K),R=values(R),mig=migrationMat,meth = "non_overlap") # creates the forward transition matrix between cells
            envDynLandscape(K=K,R=R,migration=migrationMat,transForMat = f,transBackMat = b)
          }
)

setMethod(f="runEnvDynModel",
          signature=c("enDynLandscape"),
          definition=function(object){
            R<-runNicheModel(object["R"])
            K<-runNich
            m<-migrationMatrix(object,model["m"])
            f <-transitionMatrixBackward(K=K,R=R,mig=m)      
            b <-transitionMatrixForward(K=K,R=R,mig=m,meth = "non_overlap") # creates the forward transition matrix between cells
            envDynLandscape(K=K,R=R,migration=m,transForMat = f,transBackMat = b)
          }
)


setMethod(
  f="migrationMatrix",
  signature=c("landscape","migrationModel"),
  definition=function(object,model)
  {
    Ndim = 1+all(ncell(object)!=dim(object)[1:2])
    #if model["shapeDisp"]=="contiguous" matrix()
    migration = apply(object["distanceMatrix"], c(1,2),
                      function(x)(switch(model["shapeDisp"],
                                         fat_tail1 = 1/(1+x^model["pDisp"][2]/model["pDisp"][1]),
                                         gaussian = (dnorm(x, mean = 0, sd = model["pDisp"][1], log = FALSE)),
                                         exponential = (dexp(x, rate = 1/model["pDisp"][1], log = FALSE)),
                                         contiguous = (x==0)*(1-model["pDisp"][1])+((x>0)-(x>1.4*res(object)[1]))*(model["pDisp"][1]/(2*Ndim)),
                                         contiguous8 = (x==0)*(1-object["pDisp"][1])+((x>0)-(x>2*res(object)[1]))*(model["pDisp"][1]/(4*Ndim)),
                                         island = (x==0)*(1-model["pDisp"][1])+(x>0)*(model["pDisp"][1]),
                                         fat_tail2 = x^model["pDisp"][2]*exp(-2*x/(model["pDisp"][1]^0.5)),
                                         contiguous_long_dist_mixt = model["pDisp"]["plongdist"]/ncellA(object)+(x==0)*(1-model["pDisp"]["pcontiguous"]-model["pDisp"]["plongdist"])+((x>0)-(x>1.4*res(object)[1]))*(model["pDisp"]["pcontiguous"]/2),
                                         gaussian_long_dist_mixt = model["pDisp"][2]/ncellA(object) + (dnorm(x, mean = 0, sd = model["pDisp"][1], log = FALSE))
                      )))
    return(migration)
  }
)
setMethod(f="transitionMatrixBackward",
          signature=c("numeric","numeric","matrix"),
          definition=function(R,K,mig){
            if ((length(R)==1)&(length(K)==1)){transition = R * K * t(mig)}
            if ((length(R)>1)&(length(K)==1)){transition = t(matrix(R,nrow=length(R),ncol=length(R))) * K * t(mig)}
            if ((length(R)==1)&(length(K)>1)){transition = R * t(matrix(K,nrow=length(K),ncol=length(K))) * t(mig)}
            if ((length(R)>1)&(length(K)==1)){transition = t(matrix(R,nrow=length(R),ncol=length(R))) * lpar$K * t(mig)}
            if ((length(R)>1)&(length(K)>1)) {transition = t(matrix(R,nrow=length(R),ncol=length(R))) * t(matrix(K,nrow=length(K),ncol=length(K))) * t(mig)}
            t<-transition/t(sapply(rowSums(transition),function(x)rep(x,ncol(transition))))
            TransitionBackward(t)
          }
)


setMethod(
  f="transitionMatrixForward",
  signature=c("numeric","numeric","matrix","character"),
  definition=function(R,K,mig,meth)
  {
    rs = matrix(R,nrow=length(R),ncol=length(R))
    Ku = t(matrix(K,nrow=length(K),ncol=length(K)))
    leave = mig*(1+rs)*t(Ku); leave = leave - diag(leave)
    tMF<-switch (meth,
            non_overlap = mig * rs * Ku / colSums(rs * t(Ku) * mig),
            overlap = mig * (1+rs) * Ku / (colSums((1+rs) * t(Ku) * mig - t(leave))),
            stop("error in creation of transitionMatrixForward : the method does not exist !")
    )
    new(Class = "TransitionForward",tMF)
  }
)


setMethod(f="sampleLandscape",
          signature = c("Demographic","numeric","data.frame","character"),
          # if length(sampleCells)==1 a sample of length equals to sampleCells is taken at random 
          # from the landscape cells with probability proportional to the cells's K value
          # if length(sampleCells)>1, sampleCells contains the cell number of the sample to at to 
          # the demographic object to create
          # creates the environmental dynamic model with the landscape and parameters
          # creates the backward transition matrix between cells to run the coalescent
          # creates the environmental dynamic model with the landscape and parameters
          definition = function(demographic, sampleSize,xy=NULL, option="randomfromK"){
            if (!(option%in%c("randomefromK","fromCoords","K"))) stop("wrong option used in sampleLandscape funcion call")
            if (option=="fromCoords") if (any(colnames(xy)!=c("x","y"))) stop("colnames of xy should include 'x' and 'y', for longitude and latitude)")
            tmp <- switch (option,
                           fromCoords = {
                             sampleCells=as.vector(cellFromXY(landscape,yx[,c("x","y")]))
                             names(sampleCells)=1:length(sampleCells)
                             sampleCells
                           },
                           randomfromK = {
                             sampleNo=as.vector(rmultinom(1,sampleSize,demographic$K/sum(landscapePopSize)))
                             sampleCells=rep(1:length(sampleNo),sampleNo) # it creates a vector of the cells number where the sample occur
                             names(sampleCells)=1:length(sampleCells)
                             sampleCells
                           },
                           K ={
                             sampleCells=rep(1:length(sampleNo),landscape$K) # it creates a vector of the cells number where the sample occur
                             names(sampleCells)=1:length(sampleCells)
                             sampleCells
                           }
            )
          }
)


setMethod(f="demographic",
          signature=c("envDynLandscape","integer"),
          definition=function(envDynLand,sampleCells){
            #lpar<-runEnvDynModel(object,model)             # creates the environmental dynamic model with the landscape and parameters
            #b<-transitionMatrixBackward(object,lpar)      
            #f<-transitionMatrixForward(lpar,"non_overlap") # creates the forward transition matrix between cells
            #sample according to envDynModel@K 
            new(Class = "Demographic",object=envDynLand,sampleCells=sampleCells)
          }
)

setMethod(
  f = "nCellA",
  signature = "Demographic",
  definition = function(object){
    nCellA(object[[1]])
  }
)


setMethod(
  f = "laplaceMatrix",
  signature = "TransitionBackward",
  definition = function(object){
    matrixD = diag(rep(1,dim(object)[1])) # diagonal equals to 1
    laplacianMatrix = matrixD - object
    laplacianMatrix[is.na(laplacianMatrix)]<-0 # replace NA by 0
    #cat("laplacian",laplacianMatrix)
    return(laplacianMatrix)
  }
)

setMethod(
  f="ordinary_laplacian",
  signature = "TransitionBackward",
  definition = function(object){
    markovB<-new("markovchain", states=dimnames(transition)[[1]], transitionMatrix=transition)
    PI<-diag(steadyStates(markovB)[1,])
    PI - PI%*%transition
  }
)

setMethod(
  f="hitting_time_digraph",
  signature = "TransitionBackward",
  definition = function(object){
    Ones <- rep(1,dim(object)[1])
    markovB<-new("markovchain", states=dimnames(object)[[1]], transitionMatrix=object)
    pi_<-steadyStates(markovB)[1,]
    PI <- diag(pi_)
    L <- PI - PI%*%object
    Z <- ginv(L + pi_%*%t(pi_))
    H <- Ones%*%t(diag(Z))-Z
    H
  }
)


setMethod(
  f="commute_time_digraph",
  signature = "TransitionBackward",
  definition = function(object){
    mat<-hitting_time_digraph(object)
    sapply(1:ncol(mat),function(x)sapply(1:nrow(mat),function(y)mat[x,y]+mat[y,x]))
  }
)

############################################
setMethod(
  f="simul_coalescent", ## simulates a coalescent 
  signature=c("Demographic","logical"),
  definition=function(demographic,printCoal)
  {
    prob_forward=NA
    N <- round(demographic["K"]);#N[N==0]<-1
    coalescent = list() #
    cell_number_of_nodes <- parent_cell_number_of_nodes <- democraphic@sampleCells
    nodes_remaining_by_cell = list()
    time=0
    single_coalescence_events=0
    single_and_multiple_coalescence_events=0
    for (cell in 1:nCellA(demographic))
    {
      nodes_remaining_by_cell[[cell]] <- which(cell_number_of_nodes==cell)
    }
    while (length(unlist(nodes_remaining_by_cell))>1)
    {
      for (node in 1:length(parent_cell_number_of_nodes))
      {
        parent_cell_number_of_nodes[node] = sample(nCellA(demographic),size=1,prob=c(demographic["TransiBackw"][cell_number_of_nodes[node],]))
      }
      prob_forward[time] = sum(log(demographic["TransiForw"][parent_cell_number_of_nodes,cell_number_of_nodes]))
      time=time+1; if(printCoal==TRUE){if (round(time/10)*10==time) {print(time)}}
      for (cell in 1:nCellA(demographic))
      {
        nodes_remaining_in_the_cell = nodes_remaining_by_cell[[cell]] <- as.numeric(names(which(parent_cell_number_of_nodes==cell)))
      }
      prob_forward[time] = sum(log(demographic["TransiForw"][parent_cell_number_of_nodes,cell_number_of_nodes]))
      time=time+1;  if(printCoal==TRUE){if (round(time/10)*10==time) {print(time)}}
      for (cell in 1:nCellA(demographic))
      {
        nodes_remaining_in_the_cell = nodes_remaining_by_cell[[cell]] <- as.numeric(names(which(parent_cell_number_of_nodes==cell)))
        if (length(nodes_remaining_in_the_cell)>1)
        {
          nbgenesremaining=length(nodes_remaining_in_the_cell)
          smp = sample(N[cell],length(nodes_remaining_in_the_cell),replace=TRUE)
          parentoffspringmatrix <- matrix(smp,nrow=nbgenesremaining,ncol=N[cell])==matrix(1:N[cell],nrow=nbgenesremaining,ncol=N[cell],byrow=TRUE)
          rownames(parentoffspringmatrix) <- nodes_remaining_in_the_cell
          if (any(colSums(parentoffspringmatrix)>1) )
          {
            for (multiple in which(colSums(parentoffspringmatrix)>1))
            {
              single_coalescence_events = single_coalescence_events +1
              nodes_that_coalesce = names(which(parentoffspringmatrix[,multiple]))
              new_node <- max(nodes)+1;nodes=nodes[!(names(nodes)%in%nodes_that_coalesce)];nodes=append(nodes,new_node);names(nodes)[length(nodes)]=new_node
              parent_cell_number_of_nodes <- append(parent_cell_number_of_nodes[!(names(parent_cell_number_of_nodes)%in%nodes_that_coalesce)],cell);names(parent_cell_number_of_nodes)[length(parent_cell_number_of_nodes)]<-new_node
              coalescent[[single_coalescence_events]] <- list(time=time,coalescing=as.numeric(nodes_that_coalesce),new_node=new_node)
              nodes_remaining_in_the_cell = nodes_remaining_by_cell[[cell]] <- append(nodes_remaining_in_the_cell[!nodes_remaining_in_the_cell %in% nodes_that_coalesce],new_node)
              single_and_multiple_coalescence_events = single_and_multiple_coalescence_events + length(nodes_that_coalesce) - 1
            }
          }
        }
      }
      cell_number_of_nodes = parent_cell_number_of_nodes
    }
    tips = NULL
    internals = NULL
    nodes = NULL
    times = NULL
    for (i in 1:length(coalescent))#i=1;i=2
    {
      nodes = append(nodes,c(coalescent[[i]]$coalescing,coalescent[[i]]$new_node))
      internals = append(internals,coalescent[[i]]$new_node)
      times = append(times,coalescent[[i]]$time)
    }
    nodes = as.numeric(levels(as.factor(c(nodes,internals))));nodes = nodes[order(nodes)]
    tips = nodes[!((nodes)%in%(internals))]
    # getting the branch length of each coalescing node
    for (i in 1:length(coalescent))#i=1
    {
      for (coalescing in coalescent[[i]]$coalescing)# coalescing = coalescent[[i]]$coalescing[1]
      {
        if (coalescing %in% tips) {coalescent[[i]]$br_length <- append(coalescent[[i]]$br_length,coalescent[[i]]$time)
        } else {
          coalescent[[i]]$br_length <- append(coalescent[[i]]$br_length,coalescent[[i]]$time-times[which(internals==coalescing)])
        }
      }
    }
    list(coalescent=coalescent,prob_forward=sum(prob_forward))
  }
)

setMethod(
  f="simul_multi_coal",
  signature=c("Demographic","logical","numeric"),
  definition=function(demographic,printCoal,iteration){
    lapply(1:iteration,function(x)simul_coalescent(demographic,printCoal))
  }
)

setMethod(
  f="compare",
  signature=c("Demographic","Landscape","logical","numeric","character"),
  definition=function(demographic,popSize,printCoal,iteration,fname){
    coalescent<-simul_multi_coal(demographic,printCoal,iteration)
    lcoal<-lapply(1:iteration,function(n){
      coal_2<-coalescent_2_newick(coalescent[[n]][[1]])
      cat(coal_2, file = "ex.tre", sep = "\n")
      tree<-read.tree("ex.tre")
      cophenetic(tree)
    })
    a<-matrix(data = apply(sapply(lcoal,as.vector),1,mean),nrow = nrow(lcoal[[1]]),ncol = ncol(lcoal[[1]]))
    log10timescale = ceiling(log10(max(a))) 
    # we calculate the time scale of the distance matrix 
    # as the number of digit the maximum time distance 
    # do not reach
    ascaled = a/10^log10timescale
    # a is scaled to bep plotted by NJ
    b<-linearizedFstDigraph(demographic["TransiBackw"],popSize)
    c<-linearizedFstUndigraph(demographic["TransiBackw"],popSize)
    d<-apply(popSize["distanceMatrix"],c(1,2),log)
    mat<-list(ascaled,b,c,d)
    par(mfrow=c(2,2))
    if (fname!="") pdf(fname)
    for(i in 1:4){
      plot(bionj(mat[[i]]),main=title(switch(EXPR=as.character(i),
                                        "1"=paste("Simul_coalescent, t/10^",log10timescale),,
                                        "2"="linearizedFstDigraph",
                                        "3"="linearizedFstUnDigraph",
                                        "4"="Stepping_Stone"))
           )
    }
    mat
  }
)

setMethod(
  f="Collisionijk",
  signature="matrix",
  definition=function(Hitting_mat)
  {
    Tijk=array(NA,dim=c(dim(Hitting_mat)[1],dim(Hitting_mat)[2],dim(Hitting_mat)[1]))
    for (k in 1:dim(Hitting_mat)[1]){
      for (i in 1:dim(Hitting_mat)[1]){
        for (j in 1:dim(Hitting_mat)[2]){
          Tijk[i,j,k] <- max(Hitting_mat[i,k],Hitting_mat[j,k])
        }
      }
    }
    Tijk
  }
)

############################################
setMethod(
  f = "valuesA",
  signature = "rasterstack",
  definition = function(object){
    x=na.omit(values(object))
    colnames(x)=names(x)
    rownames(x) <- cellNumA(object)
    x
  }
)

setMethod(
  f="linearizedFstDigraph",
  signature=c("TransitionBackward","Landscape"),
  definition=function(transition, popSize)#popSize is raster class
  {
    H <- hitting_time_digraph(transition)
    dim2 <- dim(H);dim2[[3]]=2
    H2 <- array(c(H,t(H)),dim=dim2)
    MaxH <- apply(H2,c(1,2),max)
    genetic_dist = MaxH / (8*sum(valuesA(popSize))*nCellA(popSize))
    genetic_dist
  }
)

setMethod(
  f="coalescent_2_newick",
  signature="list",
  definition=function(coalescent)
  {
    tree=paste(" ",coalescent[[length(coalescent)]]$new_node," ",sep="")
    for (i in length(coalescent):1)
    {
      Time = coalescent[[i]]$time
      coalesc <- as.character(coalescent[[i]]$coalescing)
      tree <- str_replace(tree,paste(" ",as.character(coalescent[[i]]$new_node)," ",sep=""),paste(" ( ",paste(" ",coalesc," :",coalescent[[i]]$br_length,collapse=" ,",sep=""),") ",sep=""))
    }
    tree <- gsub(" ","",paste(tree,";",sep=""))
    tree
  }
)

setMethod(
  f="linearizedFstUndigraph",
  signature=c("TransitionBackward","Landscape"),
  definition=function(transition, popSize)
  {
    commute_time <- commute_time_undigraph(transition)
    linearizedFst = commute_time / (16*sum(valuesA(popSize))*nCellA(popSize))
    linearizedFst
  }
)

setMethod(
  f="commute_time_undigraph",
  signature = "TransitionBackward",
  definition = function(object){
    laplacian = laplaceMatrix(object)
    inverseMP = ginv(laplacian) # generalized inverse matrix  (Moore Penrose)
    diag = diag(inverseMP) # get diagonal of the inverse matrix
    mii = matrix(diag, nrow =dim(inverseMP), ncol = dim(inverseMP))
    mjj = t(mii)
    mij = inverseMP
    #mji = t(mij)
    commute_time = mii + mjj - 2*mij #- mji
    commute_time
  }
)
