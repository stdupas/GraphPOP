library(ape)
library(stringr)
library(markovchain)
library(matrixcalc)
library(MASS)
library(raster)


############## CLASS AND VALIDITY ####

### Generics for handling landscapes removing NA cells

setClass("geoEnvData",
         contains = "RasterStack",
         prototype=prototype(stack(stack(x=c(temp=raster(matrix(c(5,3,2,3,2,3,2,3,5),nrow=3),xmn=0,xmx=3,ymn=0,ymx=3,crs=crs("+proj=longlat")),pops=raster(matrix(rep(1:3,3),nrow=3),xmn=0,xmx=3,ymn=0,ymx=3,crs=crs("+proj=longlat"))))))
)
new("geoEnvData",stack(x=c(temp=raster(matrix(c(5,3,2,3,2,3,2,3,5),nrow=3),xmn=0,xmx=3,ymn=0,ymx=3,crs=crs("+proj=longlat")),pops=raster(matrix(rep(1:3,3),nrow=3),xmn=0,xmx=3,ymn=0,ymx=3))))

geoEnvData <- function(rasterStack=NULL,Array=array(c(c(5,3,2,3,2,3,2,3,5),rep(1:3,3)),dim=c(3,3,2),dimnames = list(1:3,1:3,c("temp","pops"))),CRS="+proj=longlat",xmn=0,xmx=3,ymn=0,ymx=3){
  if (is.null(rasterStack)) rasterStack=stack(apply(Array,3,function(x) raster(matrix(x,nrow = dim(Array)[1]),xmn=xmn,xmx=xmx,ymn=ymn,ymx=ymx,crs=CRS)))
  new("geoEnvData",rasterStack)
}

setGeneric(name="NAcells",
           def=function(object){
             return(standardGeneric("NAcells"))
           })

setGeneric(
  name = "Acells",
  def=function(object){return(standardGeneric("Acells"))}
)

setGeneric(
  name = "xyA",
  def=function(object){return(standardGeneric("xyA"))}
)

setGeneric(
  name = "nCellA",
  def=function(object){return(standardGeneric("nCellA"))}
)

setGeneric(
  name = "valuesA",
  def=function(object){return(standardGeneric("valuesA"))}
)

#### Methods

setMethod("NAcells",
          signature=c("geoEnvData"),
          definition = function(object){
            which(is.na(values(object[[1]])))
          }
)
setMethod("Acells",
          signature=c("geoEnvData"),
          definition = function(object){
            which(!is.na(values(object[[1]])))
          }
)


setMethod("xyA",
          signature = "geoEnvData",
          definition = function(object){
            df=xyFromCell(object,Acells(object))
            rownames(df) <- Acells(object)
            df
          }
)

setMethod(
  f = "nCellA",
  signature = "geoEnvData",
  definition = function(object){
    length(Acells(object))
  }
)

setMethod(
  f = "valuesA",
  signature = "geoEnvData",
  definition = function(object){
    select <- Acells(object)
    x=values(object)[select]
    names(x) <- select
    x
  }
)

setClass("socioecoGroupData",
         contains = "RasterStack",
         representation(categories="character")
         )

validitysocioecoGroupData=function(object){
  #if (ncell(object)!=dim(object)[2]) stop("the socioecoClass must be one dimentional rasterStack : ncategories cols and 1 row")
  if (ncell(object)!=length(object@categories)) stop("the number of cells and the number of categories should not differ in socioecoGroupData")
  return(TRUE)
}


setValidity("socioecoGroupData",validitysocioecoGroupData)

a=new("socioecoGroupData",stack(raster(matrix(1:4,nrow=1))),categories=c("susa","bogota","villapinzon","lacalera"))

socioecoGroupData<-function(categories=c("group1","group2"),Values=c(1:4), Nlayers= 2, layerNames=c("cuidado","alimentación"),Array=NULL,rasterStack=NULL){
  if (is.null(rasterStack)) {
    if (is.null(Array)) Array = array(Values,dim = c(1,length(categories),Nlayers),dimnames = list(1:1,categories,layerNames))
    rasterStack = stack(apply(Array,3,function(x){raster(x)}))
  }
  new("socioecoGroupData",rasterStack,categories=categories)
}

setMethod("show",
          "socioecoGroupData",
          function(object) {
            cat("class\t\t: socioecoGroupData\n")
            cat("# of categ.\t:",ncell(object), "\n")
            cat("# of layers\t:",nlayers(object),"\n")
            cat("categories\t:",object@categories,"\n")
            cat("var names\t:",names(object),"\n")}
          
)

setMethod("Acells",
          signature=c("socioecoGroupData"),
          definition = function(object){
            which(!is.na(values(object[[1]])))
          }
)

setMethod(
  f = "valuesA",
  signature = "socioecoGroupData",
  definition = function(object){
    select <- Acells(object)
    x=values(object)[select]
    names(x) <- select
    x
  }
)

setMethod(
  f = "nCellA",
  signature = "socioecoGroupData",
  definition = function(object){
    length(Acells(object))
  }
)

setClass("socioecoGroupsData",
         contains = "list",
         prototype = prototype(list(mercado=new("socioecoGroupData",stack(list(probabilidadEnfermedad=raster(matrix(1:2,nrow=1)))),categories=c("Corrabastos","Semillas identidad")),
           sistema=new("socioecoGroupData",stack(list(pesticidas=raster(matrix(c(0,2,0),nrow=1)),abono=raster(matrix(c(0,2,2),nrow=1)))),categories=c("ageoeco","convencional","pequeño"))
              )
         )
        )

validitysocioecoGroupsData=function(object){
if (any(unlist(lapply(object,FUN = function(x) (class(x)!="socioecoGroupData"))))) stop("socioecoGroupsData is a list of object of class socioecoGroupData")
  return(TRUE)
}


setValidity("socioecoGroupsData",validitysocioecoGroupsData)
#listofsocioecoGroupData=NULL,listofCategories=list(c("Corrabastos","Semillas_identidad"),c("ageoeco","convencional","pequeño")),
#Names=c("mercado","sistema"),listofValues=list(c(1,2),matrix(c(0,2,0,0,2,2),ncol=2)), Nlayers= c(1,2), listofLayerNames=list("probabilidadEnfermedad","pesticidas","abono"),listofArray=NULL,listofRasterStack=NULL)

a=new("socioecoGroupsData")
     
      
socioecoGroupsData<-function(listofsocioecoGroupData=NULL,listofCategories=list(c("Corrabastos","Semillas_identidad"),c("ageoeco","convencional","pequeño")),
                              Names=c("mercado","sistema"),listofValues=list(c(1,2),matrix(c(0,2,0,0,2,2),ncol=2)), Nlayers= c(1,2), listofLayerNames=list("probabilidadEnfermedad",c("pesticidas","abono")),listofArray=NULL,listofRasterStack=NULL)
  {
  if (is.null(listofsocioecoGroupData)) {
    if (is.null(listofRasterStack)) {
      if (is.null(listofArray)) listofArray = lapply(1:length(Nlayers),function(i) array(data = listofValues[[i]],dim=c(1,length(listofCategories[[i]]),Nlayers[i]) , dimnames= list(1,listofCategories[[i]],listofLayerNames[[i]])))
    }
    listofRasterStack = lapply(1:length(listofArray),function(i) {stack(apply(listofArray[[i]],3,function(x){raster(x)}))})
  }
  listofsocioecoGroupData = lapply(1:length(listofRasterStack),function(i) socioecoGroupData(categories = listofCategories[[i]],rasterStack = listofRasterStack[[i]]))
  names(listofsocioecoGroupData)=Names
  new("socioecoGroupsData",listofsocioecoGroupData)
}

setMethod("show",
          "socioecoGroupsData",
          function(object) {
            cat("class\t\t: socioecoGroupsData\n")
            cat("# of elements\t:",length(object), "\n\n")
            for (i in 1:length(object))
              {
              cat("element",i,"\t:",names(object)[i],"\n")
              cat("class\t\t:",class(object[[i]]),"\n")
              cat("# of categ.\t:",ncell(object[[i]]), "\n")
              cat("# of layers\t:",nlayers(object[[i]]),"\n")
              cat("categories\t:",object[[i]]@categories,"\n")
              cat("var names\t:",names(object[[i]]),"\n\n")}
          }
)

setMethod("Acells",
          signature=c("socioecoGroupsData"),
          definition = function(object){
            lapply(object, function(x){Acells(x)})
          }
)

setMethod("nCellA",
          signature=c("socioecoGroupsData"),
          definition = function(object){
            lapply(object, function(x){nCellA(x)})
          }
)

setGeneric("categories",
           def=function(object){return(standardGeneric("categories"))})

setMethod("categories",
          "socioecoGroupData",
          function(object){list(socioeco=object@categories)}
)
setMethod("categories",
          "socioecoGroupsData",
          function(object) {lapply(object, function(x){categories(x)})}
)

setMethod("categories",
          "geoEnvData",
          #function(object) {apply(xyFromCell(object,1:nCellA(object)),1,function(x) paste(x,collapse ="_"))}
          function(object) Acells(object)
)


setMethod("variable.names",
          "socioecoGroupData",
          function(object) {names(object)})


setMethod("variable.names",
          "socioecoGroupsData",
          function(object) {lapply(object, function(x){names(x)})}
)


#stackConnectionType
setClass("socioecoGeoData",
         # socioecoGeoData are raster layers containing information about environmental variables
         # and or connectivity variables that will allow to build niche models and friction models
         # the layers that contain geographic information related to geographic coordinates are called connectionType=geographic
         # the layers that contain other type of information characterizing ecological populations by groups (ethnic groups, market exchanges, plant varieties for pests and diseases) have a connectionType = grouping
         # connected class variables are coded as follows; when cell value is :
         # - 0 : the cell is not connected to any other cell 
         # - n!=0 : the cell is connected to the cells having the same value
         representation("geoEnvData","socioecoGroupsData"),
         prototype = prototype(geoEnvData=geoEnvData(),socioecoData=socioecoGroupsData())
)



connectionTypes=c("geographic","grouping","routes")

#setValidity("socioecoGeoData",
 #           function(object){
  #            if (length(object@stackConnectionType)!=nlayers(object)) return("the length of stackConnectionType slot informing the type of connection data and the number of layers of stack differ")
   #           if (!all(object@stackConnectionType%in%connectionTypes)) return("stackConnectionType argument character value was not any of 'raster', 'vector', or 'group'")
    #            return(TRUE)
     #       })

#
# socioecoGeoData has 2 components
# a geographic and/or a socioeconomic that permits to generate 
# transition matrix individuals among demes defined 
# by geographic and socioeconomic variables

socioecoGeoData<-function(x=NULL,socioecoList=NULL)
{
  #
  # socioecoGeoData has 2 components
  # a geographic and/or a socioeconomic that permits to generate 
  # transition matrix individuals among demes defined 
  # by geographic and socioeconomic variables
  if (is.null(x)) geo=new("geoEnvData") else {
      if (class(x)=="array") {
        if (is.null(stackConnectionType)) stackConnectionType=rep("geographic",dim(x)[3]) 
          geo=new("geoEnvData",
          {Stack=stack(sapply(1:dim(x)[3],function(i) raster(x[,,i])),layers=envLayerNames)
          names(Stack)=envLayerNames
          extent(Stack)=Extent
          crs(Stack) <- Crs
          Stack})}
   else if (class(x)=="RasterStack") {
    geo=new("geoEnvData",rasterstack)} else if (class(x)=="geoEnvData") {geo=x} else stop("x should be raster, RasterStack, array or empty")
  }
  if (is.null(socioecoList)) socioecoList=socioecoGroupsData()
  new("socioecoGeoData",geo,socioecoList)
}


setMethod("variable.names",
          "socioecoGeoData",
          function(object) {list(geo=names(object),socioeco=variable.names(object@socioecoData))  
          }
)
setMethod("show",
          "socioecoGeoData",
          function(object) {
            cat("An object of class 'socioecoGeoData'\n")
            cat("- geoEnvData inherited class:\n")
            cat("dimensions\t:",object@nrows,",",object@ncols,",",nCellA(object)[1],",",dim(object)[3],"(nrow, ncol, ncell, layers)"," \n")
            cat("resolution\t:",res(object)[1],",",res(object)[2]," (x, y)")
            cat("\nextent\t\t:",extent(object@geoEnvData)[1],",", extent(object@geoEnvData)[2], "," , extent(object@geoEnvData)[3], ",", extent(object@geoEnvData)[4], " (xmin, xmax, ymin, ymax)")
            cat("\ncrs\t\t:",as.character(crs(object)))
            cat("\nnames\t\t: ")
            cat(names(object),sep = ", ")
            cat("\n\n- socioecoGroupsData slot:\n")
            cat(show(object@socioecoData))
            }
          )

setMethod("nCellA",
          signature = "socioecoGeoData",
          function(object) {
            return(c(geoCells=nCellA(object@geoEnvData),socioCells=sapply(object@socioecoData,FUN = function(x) nCellA(x))))
            }
          )

socioecoGeoData(x = geoEnvData(),socioecoList=socioecoGroupsData())

reactionNorm = c("scaling","enveloppe","envelin","conQuadratic","conQuadraticSkw")

npNiche <- function(x) {unlist(lapply(x,function(x) switch(x[],
                                                           scaling=1,
                                                           enveloppe=2,
                                                           envelin=2,
                                                           conQuadratic=2,
                                                           conQuadraticSkw=2)
))
}

validityNicheModel=function(object){
  if(class(object@varNiche)!="character")stop("error in NicheModel variables : variables just accept character!")
  if(!is.list(object@pNiche))stop("error in NicheModel pNiche : pNiche just accept list!")
  if (!all(object@reactNorms%in%reactionNorm))stop(paste("reaction norm should be one of the following :",paste(reactionNorm,collapse = ", ")))
  if(FALSE%in%lapply(object@pNiche,is.numeric))stop("error in NicheModel parameter list : Parameter list just accept numeric!")
#  if(!all(names(object@reactNorms)%in%object@varNiche))stop("error in NicheModel : names of reactionNorm slot are not all included in var slot")
  notMatching <- (unlist(lapply(1:length(object@pNiche),function(x) npNiche(object@reactNorms[x]) != length(object@pNiche[[x]]))))
  if (any(notMatching)) stop(paste("
                                            error in number of parameter given in nicheModel pNiche 
                                            according to reactionNorm:
                                            scaling=1,
                                            enveloppe=2,
                                            envelin=2,
                                            conQuadratic=2,
                                            conQuadraticSkw=2
                                            number of paremeters and reactionNorm do not match for variable ",which(notMatching),". ",sep=""))
  if (length(object@varNiche)!=length(object@reactNorms)) stop("there should be the same number of elements in reactNorms and varNiche since reactNorms[i] is applied to varNiche[i]")
  if (length(object@reactNorms)!=length(object@pNiche)) stop("there should be the same number of elements in pNiche and reactNorms since pNiche[[i]] is used in reactNorms[i] function")
#  if (length(object@varNiche)>=2 stop("niche must be shape x scale")
      TRUE
}

setClass("nicheModel",
         representation(varNiche="character",reactNorms="character",pNiche="list"),
         # defines the reaction norm for each variable, without interaction in this version
         # var : the variable names to which the niche model applies
         # reactNorms : the reaction norms for the variables as a vector. The names of the entries correspond to the variables the reaction norm uses, and the value corresponds to the type of reaction norm
         # pNiche: the list of numerical parameters vectors for each reaction norm, there should be a parameter vector for each reaction norm. The names of the vector components correspond to the values of the reaction norm slot
         prototype(varNiche=c("temp","temp"),reactNorms=c("envelin","scaling"),pNiche=list(envelin=c(2,4),scaling=100))
)

setValidity("nicheModel", validityNicheModel)

nicheModel<-function(varNiche=c("temp","temp"),reactNorms=c("envelin","scaling"),pNiche=list(c(3,4),100)){#,form=formul){
  names(pNiche)=reactNorms
  new("nicheModel",varNiche=varNiche,reactNorms=reactNorms,pNiche=pNiche)#,form=form)
}

npMig <- function(x) {unlist(lapply(x,function(x) switch(x[],
                                                         popSep=0,
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

migrationShapes<-c("popSep","fat_tail1","gaussian","exponential","contiguous","contiguous8", "island","fat_tail2","contiguous_long_dist_mixt","gaussian_long_dist_mixt")
# popSep : dispersion within groups is panmictic

validitygeoMigrationModel=function(object){
  #if(!is.character(object@shapeMig))stop("error in  migrationModel shapeMig : shapeMig just accept character!")
  whichSlotHasDifferentSize = sapply(c("varMig","shapeMig","pMig"),FUN = function(x) {length(slot(object,x))!=length(object@modelConnectionType)})
  if (sum(object@pMixt)!=1) warning("pMixt parameter should sum to 1")
  if(any(whichSlotHasDifferentSize)) stop(paste("error in  migrationModel; the slot(s)",
                                               paste(c("varMig","shapeMig","pMig","pMixt")[whichSlotHasDifferentSize],collapse = " and "), 
                                               " is not valid because it has different length than modelConnectionType slot, which is ",
                                               length(object@modelConnectionType),sep = ""))
  whichIsNotShapeMig = sapply(1:length(object@modelConnectionType),FUN = function(i) {
    !object@shapeMig[i]%in%migrationShapes})
  if(any(whichIsNotShapeMig)) {
    stop(paste("the shapeMig '",paste(object@shapeMig[whichIsNotShapeMig],collapse="' and '"),
               "' is not a valid shapeMig",sep=""))
    }
  if(!any(object@shapeMig%in%migrationShapes))stop(paste("error in  migrationModel : the parameter shapeMig must have one of the following values: '",paste(migrationShapes,collapse=", "),"'",sep=""))
  if(FALSE%in%lapply(object@pMig,is.numeric))stop("error in migrationModel pMig : pMig just accept numeric vectors")
  for (i in 1:length(object@varMig)) {# checks for correct number of migration parameters
    if(npMig(object@shapeMig[i])!=length(object@pMig[[i]])) {
      stop(paste("error in migrationModel : number of paremeters and shapeMig do not match for varMig",object@shapeMig[i]))}
  }
  TRUE
}
#proportion of individual produced in each attibuted cell that migrates to each attributed cell of the raster
setClass("geoMigrationModel",
         representation(modelConnectionType="character",varMig="character",shapeMig="character",pMig="list",pMixt="numeric"),
         # describes migration model : depending on distance or grouping
         # pMixt : a proportion of the individual migrates according to each model
         # models are described in successive components of modelConnectionType, varMig, shapeMig, pMig and pMixt slots
         # if modelConnectionType = dist and shapeMig=island the model is an island model in which each cell is an island and a proportion of 
         #                          propagules migrates to the pool with a proportion given by the parameter
         # if modelConnectionType = group and shapeMig=island the model is an island model in which each group is an island and a proportion of 
         #                          propagules migrates to the pool with a proportion given by the parameter there is a possiblity of dispersion models within groups according to the shapeDisp
         prototype(modelConnectionType=c("geographic","grouping"),varMig=c("temp","pops"),shapeMig=c("gaussian","popSep"),pMig=list(1/1.96,numeric(0)),pMixt=c(.5,.5)),
         # this prototype, migrationModel is a mixture of two components : migration related to distance in the first component and migration related to groups in the second component. Probability of migration between two points is a mixture of probablity given by the dist gaussian model and the group popSep model. 
         # the group popSep model means island model where cells of the raster belong to pops according to their group variable and mix panmictically within groups according to the popSep model
         # the dist gaussian model means dispersion between two points in the raster is given by gaussian distribution
)
# Para especies que tiene hierarchia en su estructura genetica puede tener interes considerar un island a nivel
# de celda (modelConnectionType= dist, and shapeMig=island) y un island anivel de grupo geográfico (modelConnectionType= group, and shapeMig=island)

setValidity("geoMigrationModel", validitygeoMigrationModel)


geoMigrationModel<-function(modelConnectionType=c("geographic","grouping"),varMig=c("temp","pops"),shapeMig=c("gaussian","island"),pMig=list(gaussian=1/1.96,island=.2),pMixt=c(0.5,0.5)){
  #if(length(pMixt)==length(shapeMig)) pMixt=(pMixt/sum(pMixt))[1:(length(pMixt)-1)] # pMixt requires length(shapeMig)-1 parameters only, since the sum of mixture parameters =1
  new("geoMigrationModel",modelConnectionType=modelConnectionType,varMig=varMig,shapeMig=shapeMig,pMig=pMig,pMixt=pMixt)
}

socioecoMigrationShapes=c("euclideanInverse") # Invers

validitysocioecoMigrationModel=function(object){
  #if(!is.character(object@shapeMig))stop("error in  migrationModel shapeMig : shapeMig just accept character!")
  if (length(object@varMig)!=length(object@pMig)) stop("length of pMig list is different from length of varMig list")
  if (!(object@shapeMig%in%socioecoMigrationShapes)) stop("migrationShape must be one of the following(s): ",socioecoMigrationShapes,collapse = ", ")
  if (any(names(object@pMig)!=object@varMig)) stop("names of pMig list should equal varMig")
}

#proportion of individual produced in each attibuted cell that migrates to each attributed cell of the raster
setClass("socioecoMigrationModel",
         representation(weight="numeric",varMig="character",shapeMig="character",pMig="list"),
         # describes migration model in multiple dimensions given by the variables of the socioecoGroupData
         # 
         # these socioecoGroupData provide distance measure between socioecoCategories
         # for instance 
         # weigth represents the coefficient given to socioecoMigration Matrix relative to geoMigration matrix (has a fixed weight of 1)
         # varMig represents the names of the socioecoGroupsData used to calculate migration rates
         # shapeMig represents the model used to transform varMig into migration rates
         # pMig represent the parameters to transform varMig into distance
         prototype(weight=1,varMig=c("Español","Chibcha","Corrabastos","Semillas_Comerciales","Guardianes_de_semilla"),shapeMig=c("euclideanInverse"),pMig=list(Español=1,Chibcha=1,Corrabastos=1,Semillas_Comerciales=1,Guardianes_de_semilla=1)),
         # this prototype, migrationModel calculates migration rates using InverseEuclidean as the inverse of multidimentional euclidean distance on the 5 dimensions of varMig
)

setValidity("socioecoMigrationModel", validitysocioecoMigrationModel)


socioecoMigrationModel<-function(weight=1,varMig=c("Español","Chibcha","Corrabastos","Semillas_Comerciales","Guardianes_de_semilla"),shapeMig=c("euclideanInverse"),pMig=list(Español=1,Chibcha=1,Corrabastos=1,Semillas_Comerciales=1,Guardianes_de_semilla=1)){
  new("socioecoMigrationModel",weight=weight,varMig=varMig,shapeMig=shapeMig,pMig=pMig)
}

setMethod("variable.names",
          "socioecoMigrationModel",
          function(object){
            object@varMig
          }
          )

setMethod("variable.names",
          "geoMigrationModel",
          function(object){
            object@varMig
          }
)

setMethod("variable.names",
          "nicheModel",
          function(object){
            object@varNiche
          }
)

setMethod("model.extract",
          "nicheModel",
          function(frame,component="reactNorms"){
            switch(component,
                   varNiche=frame@varNiche,
                   reactNorms=frame@reactNorms)
          }
)

setMethod("model.extract",
          "socioecoMigrationModel",
          function(frame,component="shapeMig"){
            switch(component,
                   shapeMig=frame@shapeMig,
                   weight=frame@weight,
                   varMig=frame@varMig,
                   pMig=frame@pMig)
          }
)

setGeneric("model",
  def=function(object){
    return(standardGeneric("model"))
  })


setMethod("model",
           "nicheModel",
           function(object){
             list(reactNorms=object@reactNorms,varNiche=object@varNiche)
           })

a=socioecoGeoData(x = geoEnvData(),socioecoList=socioecoGroupsData())
b=nicheModel(varNiche=c("temp","temp"),reactNorms=c(temp="envelin",temp="scaling"),pNiche=list(envelin=c(3,4),scaling=100))
c=geoMigrationModel(modelConnectionType=c("geographic","grouping"),varMig=c("temp","pops"),shapeMig=c("gaussian","popSep"),pMig=list(gaussian=1/1.96,popSep=numeric(0)),pMixt=c(.5,.5))
d=socioecoMigrationModel()
#stack(x=c(temp=raster(matrix(2:5,nrow=2),xmn=0,xmx=2,ymn=0,ymx=2),pops=raster(matrix(rep(1:2,2),nrow=2),xmn=0,xmx=2,ymn=0,ymx=2)))

#connectionType=c("geographic","grouping") # two types of connection geo is related to geographic distance, grouping


setClass("socioecoGeoDataHistory",
         # to represent environmental dynamic data history
         # inherits from socioecoData as the present socioecogeodata
         # includes a past socioecogeodata list with parsing times
         # the last past socioecogeodata in the list goes from the last parsing time to minus infinite
         contains="socioecoGeoData",
         representation(pastSocioecoGeoData="list",parsingTimes="numeric",timeUnit="character",zeroTime="POSIXlt"),
         prototype(new("socioecoGeoData"),pastSocioecoGeoData=list(new("socioecoGeoData"),new("socioecoGeoData"),new("socioecoGeoData")),parsingTimes=c(0,-200,-5000,-20000),timeUnit="days",zeroTime=as.POSIXlt('2005-4-19 7:01:00'))
)

validitysocioecoGeoDataHistory = function(object){
  if (any(object@parsingTimes>0)) stop("the pastStartingTimes should be negative")
  if (length(object@parsingTimes)!=(length(object))) stop("slot pastStartingTimes should include all the starting dates between period, which is length(object)")
  if (length(object@parsingTimes)>1) for (i in 2:length(object@pastStartingTimes)) {if (object@pastStartingTimes[i]>=object@pastStartingTimes[i-1]) stop("the socioecoGeoDataList should order from recent to past")}
}

setValidity("socioecoGeoDataHistory", validitysocioecoGeoDataHistory)

socioecoGeoDataHistory <- function(SocioecoGeoData=socioecoGeoData(),PastSocioecoGeoData=list(socioecGaoData(),socioecGaoData(),socioecGaoData()),ParsingTimes=c(0,200,500,200),TimeUnit="days",zeroTime=as.POSIXlt('2005-4-19 7:01:00')){
  new("socioecoGeoDataHistory",SocioecoGeoData,pastSocioecoGeodata=PastSocioecoGeodata,parsingTimes=ParsingTimes,timeUnit=TimeUnit,zeroTime=ZeroTime)
}

  a=new("socioecoGeoDataHistory")

#socioecoGeoDataHistory <- function(socioecoGeoData)

setMethod("show",
          "socioecoGeoDataHistory",
          function(object){
            cat("An object of class 'socioecoGeoDataHistory':\n\n")
            if (length(object@parsingTimes)==2) {
              cat("unique socioecoGeoData :")
              cat("(Time frame from", as.character(object@zeroTime),"to",object@parsingTimes[2],object@timeUnit,")")
              cat("\nTime units\t:",object@timeUnit)
            }
            
            if (length(object@parsingTimes)==0) {
              cat("unique socioecoGeoData (no time frame) :\n")
            }

            if (length(object@parsingTimes)>2) {
              cat("most recent socioecoGeoData  :\n")
              cat("(Time frame from", as.character(object@zeroTime),"to",object@parsingTimes[2], object@timeUnit,")")
              cat("\nTime units\t:",object@timeUnit,"\n")
            }
            
            cat("(Inherited class):\n")
            cat("\nAn object of class 'socioecoGeoData'\n")
            cat("- geoEnvData inherited class:\n")
            cat("dimensions\t:",object@nrows,",",object@ncols,",",nCellA(object)[1],",",dim(object)[3],"(nrow, ncol, ncell, layers)"," \n")
            cat("resolution\t:",res(object)[1],",",res(object)[2]," (x, y)")
            cat("\ngeographic extent\t\t:",object@extent[1],",",object@extent[2],",",object@extent[3],",",object@extent[4],", (xmin, xmax, ymin, ymax)")
            cat("\ncrs\t\t:",as.character(crs(object)))
            cat("\nnames\t\t: ")
            cat(names(object),sep = ", ")
            cat("\n\n- socioecoGroupsData slot:\n")
            cat(show(object@socioecoData)) 
            if (length(object@parsingTimes)>2) {for(i in 1:length(object@pastSocioecoGeoData)){
              cat("\n\nPast socioecoGeoData:\n")
              cat("Past period #:\t",i,"\nsocioecoGeoData From", object@parsingTimes[i+1],"to",object@parsingTimes[i+2],"time units)\n")
              cat("(Time units\t: ",object@timeUnit," )\n",sep="")
              show(object@pastSocioecoGeoData[[i]])
            }}
          }
          )


validitysocioecoGeoDataModel=function(object){
  if(class(object@Kmodel)!="nicheModel")stop("Error in socioecoGeoDataModel Kmodel : Kmodel just accept NicheModel !")
  if(class(object@Rmodel)!="nicheModel")stop("Error in socioecoGeoDataModel Rmodel : Rmodel just accept NicheModel !")
  if(class(object@geoMigModel)!="geoMigrationModel")stop("Error in socioecoGeoDataModel migration : migration just accept migrationModel !")
  TRUE
}


setClass("socioecoGeoDataModel",
         contains = "socioecoGeoDataHistory",
         representation(Kmodel="nicheModel",Rmodel="nicheModel",geoMigModel="geoMigrationModel",socioecoMigModel="socioecoMigrationModel"),
         validity=validitysocioecoGeoDataModel,
         prototype(new("socioecoGeoDataHistory"),Kmodel=new("nicheModel"),Rmodel=new("nicheModel"),geoMigModel=new("geoMigrationModel"),socioecoMigModel=new("socioecoMigrationModel"))
      
)
a=new("socioecoGeoDataModel")

socioecoGeoDataModel<-function(socioecoGeoDataHistory=NULL,
                               SocioecoGeoData=socioecoGeoData(),PastSocioecoGeoData=list(socioecoGeoData(),socioecoGeoData(),socioecoGeoData()),
                               ParsingTimes=c(0,200,500,200),TimeUnit="days",zeroTime=as.POSIXlt('2005-4-19 7:01:00'),
                               nicheK=NULL,nicheR=NULL,migModel=NULL,
                               EnvStack=stack(x=c(temp=raster(matrix(c(5,4,2,4,2,4,2,4,5),nrow=3),xmn=0,xmx=3,ymn=0,ymx=3,crs="+proj=longlat"),pops=raster(matrix(c(1,2,2,1,1,2,1,1,1),nrow=3),xmn=0,xmx=3,ymn=0,ymx=3))),
                               stackConnectionType=c("geographic","grouping"),envLayerNames=NULL,Extent=NULL,
                               varNicheK="temp",reactNormsK=c(temp="scaling"),pNicheK=list(scalingK=100),
                               varNicheR=c("temp","temp"),reactNormsR=c(temp="envelin",temp="scaling"),pNicheR=list(envelin=c(1,4),scalingR=10),
                               modelConnectionType=c("geographic","grouping"),varMig=c("temp","pops"),shapeMig=c("gaussian","popSep"),pMig=list(1.10574E5/1.96,numeric(0)),pMixt=c(.5,.5))
  
{
  if (is.null(socioecoGeoDataHistory)) socioecoGeoDataHistory=socioecoGeoDataHistory(SocioecoGeoData,PastSocioecoGeoData,ParsingTimes,TimeUnit,zeroTime)
  if (is.null(nicheK)) nicheK=nicheModel(varNiche = varNicheK,reactNorms = reactNormsK, pNiche = pNicheK)
  if (is.null(nicheR)) nicheR=nicheModel(varNiche = varNicheR,reactNorms = reactNormsR, pNiche = pNicheR)
  if (is.null(migModel)) migModel= migrationModel(modelConnectionType = modelConnectionType,varMig = varMig,shapeMig = shapeMig,pMig = pMig,pMixt = pMixt)
  new("socioecoGeoDataModel",socioecoGeoDataList,Kmodel=nicheK,Rmodel=nicheR,migModel=migModel)
}

setValidity("socioecoGeoDataModel", validitysocioecoGeoDataModel)

setMethod("show",
          "nicheModel",
          function(object) {
            cat("class\t\t: nicheModel\n")
            cat("varNiche\t: ")
            cat(object@varNiche,sep = ", ")
            cat("\nreactNorms\t: ")
            cat(object@reactNorms,sep=", ")
            cat("\n")
            for (i in 1:length(object@pNiche)) {
              cat("pNiche [[",i,"]]\t: ",sep="") 
              cat(object@pNiche[[i]],sep = ", ")
              cat("\n")
            }
          }
)


setMethod("show",
          "geoMigrationModel",
          function(object) {
            cat("class\t\t: geoMigrationModel\n")
            cat("connection Type\t: ")
            cat(object@modelConnectionType,sep="\t")
            cat("\ndata variables\t: ")
            cat(object@varMig,sep="\t")
            cat("\nshapeMig\t: ")
            cat(object@shapeMig,sep="\t")
            cat("\n")
            for (i in 1:length(object@pMig)){
              cat("pMig [[",i,"]]\t: ", sep="")
              cat(object@pMig[[i]],"\n")
            }
            cat("mixture param \t: ")
            cat(object@pMixt,sep="\t")
          })

setMethod("show",
          "socioecoGeoDataModel",
          function(object) {
            cat("class\t\t: socioecoGeoDataModel\n\n")
            cat("Data (Inherited)\n")
            cat("class\t\t: envData\n")
            cat("dimension\t:",dim(object), "(nrow, ncol, nlayers) \n")
            cat("number of cells\t: ")
            cat(ncell(object),nCellA(object),sep=", ")
            cat(" (total, attributed) \n")
            cat("resolution\t: ")
            cat(c(object@extent[2]-object@extent[1],object@extent[4]-object@extent[3])/dim(object)[1:2],sep=", ")
            cat(" (x, y)\n")
            cat("extent\t\t: ")
            cat(object@extent[1:4],sep=", ")
            cat("\ncrs\t\t:",as.character(crs(object)),"\n")
            cat("names\t\t:",names(object),"\n")
            cat("min values\t: ")
            cat(apply(values(object),MARGIN = 2, FUN = min),sep=", ")
            cat("\nmax values\t: ")
            cat(apply(values(object),MARGIN = 2, FUN = max),sep=", ")
            cat("\n\nKmodel\n")
            show(object@Kmodel)
            cat("\nRmodel\n")
            show(object@Rmodel)
            cat("\nmigModel\n")
            show(object@geoMigModel)
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

scaling <- function(X,p){X[]<-p
X
}

conQuadratic <- function(X,p)
{
  if(length(p)!=2)stop("The parameter is  not valid because it contains more or less than two values")
  else -4*(X-p[2])*(X-p[1])/((p[2]-p[1])^2)*enveloppe(X,p)
}

conQuadraticSkw <- function(X,p){
  conQuadratic(X,p)*envelinear(X,p)
  X
}

setGeneric(
  name = "buildRKlandscape",
  def=function(object){return(standardGeneric("buildRKlandscape"))}
)

setMethod("buildRKlandscape",
          signature=c("socioecoGeoDataModel"),
          definition = function(object){                  #X=object, p=,shape=
            Ki=lapply(1:length(object@Kmodel@varNiche),function(i){
              switch(object@Kmodel@reactNorms[[i]],
                     scaling=setValues(object[[i]],object@Kmodel@pNiche[[i]]),
                     enveloppe = enveloppe(object[[object@Kmodel@varNiche[i]]],object@Kmodel@pNiche[[i]]),
                     envelin=envelinear(object[[i]],object@Kmodel@pNiche[[i]]),
                     conQuadratic=conQuadratic(object[[i]],object@Kmodel@pNiche[[i]]),
                     conQuadraticSkw=conQuadraticSkw(object[[i]],object@Kmodel@pNiche[[i]]),
                     stop("This reaction norm does not exist for a niche object !")
              )})
            Ri=lapply(1:length(object@Rmodel@varNiche),function(i){
              switch(object@Rmodel@reactNorms[[i]],
                       scaling=setValues(object[[i]],object@Rmodel@pNiche[[i]]),
                       enveloppe = enveloppe(object[[object@Rmodel@varNiche[i]]],object@Rmodel@pNiche[[i]]),
                       envelin=envelinear(object[[i]],object@Rmodel@pNiche[[i]]),
                       conQuadratic=conQuadratic(object[[i]],object@Rmodel@pNiche[[i]]),
                       conQuadraticSkw=conQuadraticSkw(object[[i]],object@Rmodel@pNiche[[i]]),
                       stop("This variable does not exist for Nicheobject !")
              )})
            if (length(Ri)==1) R=Ri[[1]] else R=prod(stack(Ri))
            if (length(Ki)==1) K=Ki[[1]] else K=prod(stack(Ki))
            crs(R)<-crs(object)
            extent(R)<-object@extent
            result=stack(R,K)
            names(result)<-c("R","K")
            result
            # the niche function makes the product of the different layers results
            # typically the scaling times the shape (enveloppe, envelin, ocnQuadratic or conQuadraticSkw)
          }
)

setGeneric(
  name = "buildGeodist",
  def=function(object){return(standardGeneric("buildGeodist"))}
)

setMethod(
  f="buildGeodist",
  signature=c("socioecoGeoDataModel"),
  definition=function(object)
  {
    Ndim = 1+all(ncell(object)!=dim(object)[1:2]) # if the landscape is a line one cell width Ndim=1, otherwise Ndim=2
    #if model["shapeMig"]=="contiguous" matrix()
    geoDist = unlist(apply(xyA(object),1,
                           function(x){
                             values(distanceFromPoints(object,x))
                             }))[Acells(object),]
    geoDist[which(geoDist==0)]<-sqrt(2*min(geoDist[which(geoDist!=0)])^2)/3
    # The distance between points within the same cell is one third 
    #
    return(geoDist)
  }
)

setGeneric(
  name = "buildMigrationMatrix",
  def=function(object){return(standardGeneric("buildMigrationMatrix"))}
)


setMethod(
  f="buildMigrationMatrix",
  signature=c("socioecoGeoDataModel"),
  definition=function(object)
  {
    Ndim = 1+all(ncell(object)!=dim(object)[1:2]) # if the landscape is a line one cell width Ndim=1, otherwise Ndim=2
    #if model["shapeMig"]=="contiguous" matrix()
    migration=list()
    for (i in which(object@migModel@modelConnectionType=="geographic"))
    {
      migration[[i]] = apply(buildGeodist(object), c(1,2),
                             function(x)(switch(object@migModel@shapeMig[i],
                                                gaussian = dnorm(x, mean = 0, sd = object@migModel@pMig[[i]][1], log = FALSE),
                                                exponential = (dexp(x, rate = 1/object@migModel@pMig[[i]][1], log = FALSE)),
                                                contiguous = apply(xyA(object),1,function(x){((abs(xyA(object)[,"x"]-x["x"])==res(object)[1])&(xyA(object)[,"y"]==x["y"])|(abs(xyA(object)[,"y"]-x["y"])==res(object)[1])&(xyA(object)[,"x"]==x["x"]))*object@migModel@pMig[[1]][1]/4})+diag(nCellA(object))*(1-object@migModel@pMig[[1]][1]),
                                                contiguous8 = apply(xyA(object),1,function(x){((abs(xyA(object)[,"x"]-x["x"])==res(object)[1])&(xyA(object)[,"y"]==x["y"])|(abs(xyA(object)[,"y"]-x["y"])==res(object)[1])&(xyA(object)[,"x"]==x["x"]|(abs(xyA(object)[,"x"]-x["x"])==res(object)[1])))*object@migModel@pMig[[1]][1]/8})+diag(nCellA(object))*(1-object@migModel@pMig[[1]][1]),
                                                island = diag(nCellA(object))*(1-object@migModel@pMig[[i]][1])+(1-diag(nCellA(object)))*(object@migModel@pMig[[i]][1])/(nCellA(object)-1),
                                                fat_tail2 = x^object@migModel@pMig[[i]][2]*exp(-2*x/(object@migModel@pMig[[i]][1]^0.5))
                                                #contiguous_long_dist_mixt = model["pMig"]["plongdist"]/nCellA(object)+(x==0)*(1-model["pMig"]["pcontiguous"]-model["pMig"]["plongdist"])+((x>0)-(x>1.4*res(object)[1]))*(model["pMig"]["pcontiguous"]/2),
                                                #gaussian_long_dist_mixt = model["pMig"][2]/nCellA(object) + (dnorm(x, mean = 0, sd = object@migModel@pMig[[i]][1], log = FALSE))
                             )))
      migration[[i]]<-migration[[i]]/sum(migration[[i]])
    }
    for (i in which(object@migModel@modelConnectionType=="grouping"))
    {
      migration[[i]] = switch(object@migModel@shapeMig[i],
                                                popSep = sapply(valuesA(object)[,i],function(x) {x==valuesA(object)[,i]})
                             )
      migration[[i]]<-migration[[i]]/sum(migration[[i]])
    }
    return(apply(array(unlist(migration), dim = c(nCellA(object),nCellA(object), length(migration))),c(1,2),function(x){sum(x*object@migModel@pMixt)}))
  }
)

a=socioecoGeoDataModel(EnvStack = stack(x=c(temp=raster(matrix(c(5,4,2,4,2,4,2,4,5),nrow=3),xmn=0,xmx=3,ymn=0,ymx=3,crs="+proj=longlat"),pops=raster(matrix(c(1,2,2,1,1,2,1,1,1),nrow=3),xmn=0,xmx=3,ymn=0,ymx=3))),pMig=list(1.10574E5/1.96,numeric(0)),pMixt=c(.5,.5))
b=buildMigrationMatrix(a)

setClass("envDynLandscape",
         contains=c(socioecoGeoDataModel="socioecoGeoDataModel"),
         slots = c(RKlandscape="RasterStack",geoDist="matrix",migrationMatrix="matrix",transitionForward="matrix",transitionBackward="matrix"),
)

envDynLandscape<-function(socioecoGeoDataModel=NULL,RKlandscape=NULL,geoDist=NULL,migrationMatrix=NULL,transitionForward=NULL,transitionBackward=NULL,
                          envData=NULL,
                          EnvStack=stack(x=c(temp=raster(matrix(c(5,4,2,4,2,4,2,4,5),nrow=3),xmn=0,xmx=3,ymn=0,ymx=3,crs="+proj=longlat"),pops=raster(matrix(c(1,2,2,1,1,2,1,1,1),nrow=3),xmn=0,xmx=3,ymn=0,ymx=3))),
                          stackConnectionType=c("geographic","grouping"),envLayerNames=NULL,Extent=NULL,
                          varNicheK="temp",reactNormsK=c(temp="scaling"),pNicheK=list(scalingK=100),
                          varNicheR=c("temp","temp"),reactNormsR=c(temp="envelin",temp="scaling"),pNicheR=list(envelin=c(1,4),scalingR=10),
                          modelConnectionType=c("geographic","grouping"),varMig=c("temp","pops"),shapeMig=c("gaussian","popSep"),pMig=list(1.10574E5/1.96,numeric(0)),pMixt=c(.5,.5)){
  if (is.null(socioecoGeoDataModel)) {
    if (is.null(envData)) envData=new("envData",EnvStack,stackConnectionType=stackConnectionType)
    socioecoGeoDataModel=socioecoGeoDataModel(envData = envData,nicheK = nicheModel(varNiche = varNicheK,reactNorms = reactNormsK, pNiche = pNicheK),nicheR = nicheModel(varNiche = varNicheR,reactNorms = reactNormsR, pNiche = pNicheR), migModel = migrationModel(modelConnectionType = modelConnectionType,varMig = varMig,shapeMig = shapeMig,pMig = pMig,pMixt = pMixt))}
  if (is.null(RKlandscape)) RKlandscape=buildRKLandscape(socioecoGeoDataModel)
  if (is.null(geoDist)) geoDist=buildGeodist(socioecoGeoDataModel)
  if (is.null(migrationMatrix)) RKlandscape=buildMigrationMatrix(socioecoGeoDataModel)
  if (is.null(transitionForward)) transitionForward=buildTransitionForward(socioecoGeoDataModel)
  if (is.null(transitionBackward)) transitionBackward=buildTransitionBackward(socioecoGeoDataModel)
  new("envDynLandscape",socioecoGeoDataModel,RKlandscape=RKlandscape,migrationMatrix=migrationMatrix,transitionForwar=transitionForward,transitionBackward=transitionBackward)
}

setGeneric(
  name = "transitionBackward",
  def=function(K,R,mig){return(standardGeneric("getTransitionBackward"))}
)


setMethod(f="transitionBackward",
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



##,transFor="transitionMatrixForward",transBack="getTransitionBackward",environment="landscape"


######################### |          | ############################### 
######################### | To TRASH | ############################### 
######################### V          V ############################### 

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
  name = "getTransitionBackward",
  def=function(K,R,mig){return(standardGeneric("getTransitionBackward"))}
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
  name = "getLaplacian",
  def=function(object){return(standardGeneric("getLaplacian"))}
)

setGeneric(
  name = "getOrdinaryLaplacian",
  def=function(object){return(standardGeneric("getOrdinaryLaplacian"))}
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
  name = "simulCoal",
  def=function(demographic,printCoal){return(standardGeneric("simulCoal"))}
)

setGeneric(
  name = "simulMultiCoal",
  def=function(demographic,printCoal,iteration){return(standardGeneric("simulMultiCoal"))}
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
  signature = c(x="socioecoGeoDataModel" ,i="character",j="missing"),
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
             "pMig" ={return(x@pMig)} ,
             "shapeMig" ={return(x@shapeMig)} ,
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

setMethod("buildRKLandscape",
          signature=c("landscape","NicheModel"),
          definition = function(object,model){                  #X=object, p=,shape=
            Y=lapply(model@variables,function(x){
              switch(model@reactNorms[[x]],
                     scaling={setValues(object[[x]],rep(model@pNiche[[x]],ncell(object[[x]])))},
                     #proportional = {values(object[[x]])=object[[x]]*model@pNiche[[x]]},
                     enveloppe = {object[[x]]=enveloppe(object[[x]],model@pNiche[[x]])},
                     envelin={object[[x]]=envelinear(object[[x]],model@pNiche[[x]])},
                     conQuadratic={object[[x]]=conQuadratic(object[[x]],model@pNiche[[x]])},
                     conQuadraticSkw={object[[x]]=conQuadraticSkw(object[[x]],model@pNiche[[x]])},#conquadraticskewed=conquadraticskewed(object[,,(model@variables==x)],p),
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

scaling <- function(X,p){X[]<-p}

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
setMethod(f="runsocioecoGeoDataModel",
          signature=c("landscape","socioecoGeoDataModel"),
          definition=function(object,model){
            R<-buildRKLandscape(object,model["R"])
            K<-buildRKLandscape(object,model["K"])
            migrationMat<-migrationMatrix(object,model["migration"])
            b<-getTransitionBackward(K=values(K),R=values(R),mig=migrationMat)      
            f<-transitionMatrixForward(K=values(K),R=values(R),mig=migrationMat,meth = "non_overlap") # creates the forward transition matrix between cells
            envDynLandscape(K=K,R=R,migration=migrationMat,transForMat = f,transBackMat = b)
          }
)

setMethod(f="runsocioecoGeoDataModel",
          signature=c("enDynLandscape"),
          definition=function(object){
            R<-buildRKLandscape(object["R"])
            K<-runNich
            m<-migrationMatrix(object,model["m"])
            f <-getTransitionBackward(K=K,R=R,mig=m)      
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
    #if model["shapeMig"]=="contiguous" matrix()
    migration = apply(object["distanceMatrix"], c(1,2),
                      function(x)(switch(model["shapeMig"],
                                         fat_tail1 = 1/(1+x^model["pMig"][2]/model["pMig"][1]),
                                         gaussian = (dnorm(x, mean = 0, sd = model["pMig"][1], log = FALSE)),
                                         exponential = (dexp(x, rate = 1/model["pMig"][1], log = FALSE)),
                                         contiguous = (x==0)*(1-model["pMig"][1])+((x>0)-(x>1.4*res(object)[1]))*(model["pMig"][1]/(2*Ndim)),
                                         contiguous8 = (x==0)*(1-object@migModel@pMig[[i]][1])+((x>0)-(x>2*res(object)[1]))*(model["pMig"][1]/(4*Ndim)),
                                         island = (x==0)*(1-model["pMig"][1])+(x>0)*(model["pMig"][1]),
                                         fat_tail2 = x^object@migModel@pMig[[i]][2]*exp(-2*x/(model["pMig"][1]^0.5)),
                                         contiguous_long_dist_mixt = model["pMig"]["plongdist"]/nCellA(object)+(x==0)*(1-model["pMig"]["pcontiguous"]-model["pMig"]["plongdist"])+((x>0)-(x>1.4*res(object)[1]))*(model["pMig"]["pcontiguous"]/2),
                                         gaussian_long_dist_mixt = object@migModel@pMig[[i]][2]/nCellA(object) + (dnorm(x, mean = 0, sd = model["pMig"][1], log = FALSE))
                      )))
    return(migration)
  }
)
setMethod(f="getTransitionBackward",
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
            #lpar<-runsocioecoGeoDataModel(object,model)             # creates the environmental dynamic model with the landscape and parameters
            #b<-getTransitionBackward(object,lpar)      
            #f<-transitionMatrixForward(lpar,"non_overlap") # creates the forward transition matrix between cells
            #sample according to socioecoGeoDataModel@K 
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
  f = "getLaplacian",
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
  f="getOrdinaryLaplacian",
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
  f="simulCoal", ## simulates a coalescent 
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
  f="simulMultiCoal",
  signature=c("Demographic","logical","numeric"),
  definition=function(demographic,printCoal,iteration){
    lapply(1:iteration,function(x)simulCoal(demographic,printCoal))
  }
)

setMethod(
  f="compare",
  signature=c("Demographic","Landscape","logical","numeric","character"),
  definition=function(demographic,popSize,printCoal,iteration,fname){
    coalescent<-simulMultiCoal(demographic,printCoal,iteration)
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
                                        "1"=paste("simulCoal, t/10^",log10timescale),,
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
    rownames(x) <- Acells(object)
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
    laplacian = getLaplacian(object)
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

 
## removed > Trash
setValidity("envDynHistory",validityEnvDynHistory)

setMethod("show",
          "envDynHistory",
          function(object){
            cat("An object of class \"envDynHistory\":\nenvData ordered from most recent to most remote in the past\n\n", sep="")
            cat("ending date \t:", object@endingDate,"\n")
            cat("time unit\t:",object@timeUnit,"\n")
            for (i in 1:length(object)) {
              cat("\n",i,") Period -",i,":\n\n",sep="")           
              cat("starting in time units at\t: ",object@pastStartingTimes[i],"\n\n",sep="")
              show(object[[i]])
            }
          }
)

new("envDynHistory")
