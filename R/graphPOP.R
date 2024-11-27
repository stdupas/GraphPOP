#library(ape)
#library(stringr)
#library(markovchain)
#library(matrixcalc)
#library(MASS)
#library(raster)


############## CLASS AND VALIDITY ####

### Generics for handling landscapes removing NA cells

#' Class representing geographic and environmental data
#' 
#' @slot layerConnectionTypes character vector establishing the types of connection types of each layer. The amount of connection types should be the same as the amount of layers.
#' @importClassesFrom raster RasterStack
#' @inherit raster::raster description
#' @export

setClass("geoEnvData",
         contains = "RasterStack",
         representation(layerConnectionTypes="character"),
         prototype=prototype(
           raster::stack(x=c(temp= raster::raster(matrix(c(5,3,2,3,2,3,2,3,5),nrow=3),xmn=0,xmx=3,ymn=0,ymx=3,crs=crs("+proj=longlat")),
                     pops= raster::raster(matrix(rep(1:3,3),nrow=3),xmn=0,xmx=3,ymn=0,ymx=3,crs=crs("+proj=longlat")))),
           layerConnectionTypes=c("geographic","grouping")
           )
         )

#' Identify NA values
#' 
#' @description
#' `NAcells` returns the cells with NA values in a raster object.
#' 
#' @param object a raster object to evaluate the values.
#' @returns vector of cells which value is NA.

setGeneric(name="NAcells",
           def=function(object){
             return(standardGeneric("NAcells"))
           })

#' Identify non-NA values in a raster
#' 
#' @description
#' `Acells` returns the cells that don't have NA values in a raster object.
#' @param object raster object.
#' @returns vector of cells which values aren't NA.

setGeneric(
  name = "Acells",
  def=function(object){return(standardGeneric("Acells"))}
)

#' Coordinate difference from each cell
#' 
#' @description
#' Gets the coordinate distance from each non-NA cell to each other in the raster.
#' @param object raster object.
#' @importFrom raster xyFromCell
#' @returns data.frame of the coordinate distance from each non-NA cell to every other. 

setGeneric(
  name = "xyA",
  def=function(object){return(standardGeneric("xyA"))}
)

#' Counts the number of non-NA cells
#' @description
#' This function returns the number of cells in a raster containing non-NA values.
#' 
#' @param object raster object.
#' @returns integer counting the amount of non-NA cells.

setGeneric(
  name = "nCellA",
  def=function(object){return(standardGeneric("nCellA"))}
)

#' Values of the cells in a raster.
#' @description
#' This function returns the value of the cells in a raster layer.
#' 
#' @param object raster object.
#' @returns vector containing the values of the cells in a raster layer.

setGeneric(
  name = "valuesA",
  def=function(object){return(standardGeneric("valuesA"))}
)

#' NA cells.
#' 
#' @name NAcells
#' @docType methods
#' @rdname NAcells-methods
#' @aliases NAcells,geoEnvData

setMethod("NAcells",
          signature=c("geoEnvData"),
          definition = function(object){
            which(is.na(values(object[[1]])))
          }
)

#' A cells.
#' 
#' @name Acells
#' @docType methods
#' @rdname NAcells-methods
#' @aliases Acells,geoEnvData

setMethod("Acells",
          signature=c("geoEnvData"),
          definition = function(object){
            which(!is.na(values(object[[1]])))
          }
)

#' xyA.
#' 
#' @name xyA
#' @docType methods
#' @rdname xyA-methods
#' @aliases xyA,geoEnvData
#' @importFrom raster xyFromCell

setMethod("xyA",
          signature = "geoEnvData",
          definition = function(object){
            df= raster::xyFromCell(object,Acells(object))
            rownames(df) <- Acells(object)
            df
          }
)

#' n Cells A.
#' 
#' @name nCellsA
#' @docType methods
#' @rdname nCellsA-methods
#' @aliases nCellsA,geoEnvData

setMethod(
  f = "nCellA",
  signature = "geoEnvData",
  definition = function(object){
    length(Acells(object))
  }
)

#' values A.
#' 
#' @name valuesA
#' @docType methods
#' @rdname valuesA-methods
#' @aliases valuesA,geoEnvData
#' @importFrom raster values

setMethod(
  f = "valuesA",
  signature = "geoEnvData",
  definition = function(object){
    select <- Acells(object)
    x=raster::values(object)[select]
    names(x) <- select
    x
  }
)

#' Builder function for geoEnvData objects
#' 
#' @description
#' Builder function to create geoEnvData objects. 
#' 
#' @param rasterStack a RasterStack object.
#' @param Array An array containing the values for the RasterStack layers.
#' @importFrom raster stack
#' @importFrom raster raster
#' @export

geoEnvData <- function(rasterStack=NULL,Array=array(c(c(5,3,2,3,2,3,2,3,5),rep(1:3,3)),dim=c(3,3,2),dimnames = list(1:3,1:3,c("temp","pops"))),CRS="+proj=longlat",xmn=0,xmx=3,ymn=0,ymx=3,layerConnectionTypes=c("geographic","grouping")){
  if (is.null(rasterStack)) rasterStack= raster::stack(apply(Array,3,function(x) raster::raster(matrix(x,nrow = dim(Array)[1]),xmn=xmn,xmx=xmx,ymn=ymn,ymx=ymx,crs=CRS)))
  new("geoEnvData",rasterStack,layerConnectionTypes=layerConnectionTypes)
}

#' Show method for geoEnvData.
#' 
#' @name show
#' @docType methods
#' @rdname show-methods
#' @aliases show,geoEnvData
#' @importFrom raster res
#' @importFrom raster extent

setMethod("show",
          "geoEnvData",
          function(object) {
            cat("Class\t\t: geoEnvData\n")
            cat("geo dimensions\t: ",nrow(object),", ",ncol(object),", ",raster::nlayers(object),", ",raster::ncell(object),", ",nCellA(object)," (nrow, ncol, nlayers, ncell, ncellA)\n",sep="")
            cat("layerConnectionTypes\t:",paste(object@layerConnectionTypes,sep=", "),"\n")
            cat("resolution\t: ",raster::res(object)[1],", ",raster::res(object)[2],"\n",sep="")
            cat("extent\t\t: ",paste(raster::extent(object),sep=", "),"(xmin, xmax, ymin, ymax)\n",sep="")
            cat("crs\t\t: ",as.character(crs(object)))}
)

#' Class representing the socio-economic grouping data
#' 
#' @slot layerConnectionTypes 
#' @importClassesFrom raster RasterStack
#' @inherit raster::raster description
#' @slot categories character. Vector establishing the types of connection types of each layer. The amount of connection types should be the same as the amount of layers.
#' @export

setClass("socioecoGroupData",
         contains = "RasterStack",
         representation(categories="character")
         )

#' Validity function for socioecoGroupData
#' @description
#' Validates that the number of cells and the number of categories are equal in socioecoGroupData object.
#' @param object socioecoGroupData object.
#' @importFrom raster ncell

validitysocioecoGroupData=function(object){
  #if (ncell(object)!=dim(object)[2]) stop("the socioecoClass must be one dimentional rasterStack : ncategories cols and 1 row")
  if (raster::ncell(object)!=length(object@categories)) stop("the number of cells and the number of categories should not differ in socioecoGroupData")
  return(TRUE)
}

setValidity("socioecoGroupData",validitysocioecoGroupData)

#' Creates a socioecoGroupData object
#' @description
#' This function creates a socioecoGroupData object.
#' @param categories character. The categories of the socio-ecological groups.
#' @param Values numeric. Values for the rasterStack.
#' @param Array array. Array with the values of the socio-ecological groups per cell.
#' @param rasterStack rasterStack object. This object contains the ordered values within a rasterStack object.
#' @returns socioecoGroupData object.
#' @importFrom raster stack
#' @export

socioecoGroupData<-function(categories=c("group1","group2"),Values=c(1:4), Nlayers= 2, layerNames=c("cuidado","alimentación"),Array=NULL,rasterStack=NULL){
  if (is.null(rasterStack)) {
    if (is.null(Array)) Array = array(Values,dim = c(1,length(categories),Nlayers),dimnames = list(1:1,categories,layerNames))
    rasterStack = raster::stack(apply(Array,3,function(x){raster(x)}))
  }
  new("socioecoGroupData",rasterStack,categories=categories)
}

#' Show method for socioecoGroupData.
#' 
#' @name show
#' @docType methods
#' @rdname show-methods
#' @aliases show,socioecoGroupData
#' @importFrom raster ncell
#' @importFrom raster nlayers

setMethod("show",
          "socioecoGroupData",
          function(object) {
            cat("class\t\t: socioecoGroupData\n")
            cat("# of categ.\t:",raster::ncell(object), "\n")
            cat("# of layers\t:",raster::nlayers(object),"\n")
            cat("categories\t:",object@categories,"\n")
            cat("var names\t:",names(object),"\n")}
)

#' Acells method for socioecoGroupData.
#' 
#' @name Acells
#' @docType methods
#' @rdname Acells-methods
#' @aliases Acells,socioecoGroupData

setMethod("Acells",
          signature=c("socioecoGroupData"),
          definition = function(object){
            which(!is.na(values(object[[1]])))
          }
)

#' valuesA method for socioecoGroupData.
#' 
#' @name valuesA
#' @docType methods
#' @rdname valuesA-methods
#' @aliases valuesA,geoEnvData
#' @importFrom raster values

setMethod(
  f = "valuesA",
  signature = "socioecoGroupData",
  definition = function(object){
    select <- Acells(object)
    x=raster::values(object)[select]
    names(x) <- select
    x
  }
)

#' nCellA method for socioecoGroupData.
#' 
#' @name nCellA
#' @docType methods
#' @rdname nCellA-methods
#' @aliases nCellA,socioecoGroupData

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

#' show method for socioecoGroupsData.
#' 
#' @name show
#' @docType methods
#' @rdname show-methods
#' @aliases show,socioecoGroupsData
#' @importFrom raster ncell
#' @importFrom raster nlayers
#' @export

setMethod("show",
          "socioecoGroupsData",
          function(object) {
            cat("class\t\t: socioecoGroupsData\n")
            cat("# of elements\t:",length(object), "\n\n")
            for (i in 1:length(object))
              {
              cat("element",i,"\t:",names(object)[i],"\n")
              cat("class\t\t:",class(object[[i]]),"\n")
              cat("# of categ.\t:",raster::ncell(object[[i]]), "\n")
              cat("# of layers\t:",raster::nlayers(object[[i]]),"\n")
              cat("categories\t:",object[[i]]@categories,"\n")
              cat("var names\t:",names(object[[i]]),"\n\n")}
          }
)

#' Acells method for socioecoGroupsData.
#' 
#' @name Acells
#' @docType methods
#' @rdname Acells-methods
#' @aliases Acells,socioecoGroupsData

setMethod("Acells",
          signature=c("socioecoGroupsData"),
          definition = function(object){
            lapply(object, function(x){Acells(x)})
          }
)

#' nCellA method for socioecoGroupsData.
#' 
#' @name nCellA
#' @docType methods
#' @rdname nCellA-methods
#' @aliases nCellA,socioecoGroupsData

setMethod("nCellA",
          signature=c("socioecoGroupsData"),
          definition = function(object){
            lapply(object, function(x){nCellA(x)})
          }
)

#' Method to obtain the category names of the connection types.
#' 
#' @description
#' Method to obtain the category names of the non-geographic connection types.
#' @param object Any object with a `categories` slot.
#' @returns names of the values of the `categories` slot.
#' @export

setGeneric("categories",
           def=function(object){return(standardGeneric("categories"))})

#' Categories method for socioecoGroupData.
#' 
#' @name categories
#' @docType methods
#' @rdname categories-methods
#' @aliases categories,socioecoGroupData

setMethod("categories",
          "socioecoGroupData",
          function(object){list(socioeco=object@categories)}
)

#' Categories method for socioecoGroupsData.
#' 
#' @name categories
#' @docType methods
#' @rdname categories-methods
#' @aliases categories,socioecoGroupsData

setMethod("categories",
          "socioecoGroupsData",
          function(object) {lapply(object, function(x){categories(x)})}
)

#' Categories method for geoEnvData objects.
#' 
#' @name categories
#' @docType methods
#' @rdname categories-methods
#' @aliases categories,geoEnvData

setMethod("categories",
          "geoEnvData",
          #function(object) {apply(xyFromCell(object,1:nCellA(object)),1,function(x) paste(x,collapse ="_"))}
          function(object) Acells(object)
)

#' Variable.names method for socioecoGroupData.
#' 
#' @name variable.names
#' @docType methods
#' @rdname variable.names-methods
#' @aliases variable.names,socioecoGroupData

setMethod("variable.names",
          "socioecoGroupData",
          function(object) {names(object)})

#' Variable.names method for socioecoGroupsData.
#' 
#' @name variable.names
#' @docType methods
#' @rdname variable.names-methods
#' @aliases variable.names,socioecoGroupData

setMethod("variable.names",
          "socioecoGroupsData",
          function(object) {lapply(object, function(x){names(x)})}
)

#' Class to contain the socio-ecological and the geographical data.
#' 
#' @description
#' socioecoGeoData are raster layers containing information about environmental variables and or connectivity variables that will allow to build niche models and friction models
#' the layers that contain geographic information related to geographic coordinates are called connectionType = geographic
#' the layers that contain other type of information characterizing ecological populations by groups (ethnic groups, market exchanges, plant varieties for pests and diseases) have a connectionType = grouping
#' connected class variables are coded as follows; when cell value is:
#' - 0 : the cell is not connected to any other cell 
#' - n!=0 : the cell is connected to the cells having the same value
#' @slot geoEnvData geoEnvData object. This object contains the geographical data.
#' @slot socioecoData socioecoGroupsData object. This object contains the data for the socio-ecological grouping data.
#' @export

setClass("socioecoGeoData",
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

#' Creates a socioecoGeoData object
#' 
#' @description
#' Function to construct a socioecoGeoData object.
#' 
#' @param x Either a geoEnvData or a rasterStack object containing the geographical information.
#' @param socioecoList socioecoGroupsData object containing the groups and connection types.
#' @param stackConnectionType character. The name of the stack connection type. Default value `geographic`.
#' @returns socioecoGeoData object.
#' @importFrom raster stack
#' @importFrom raster raster
#' @importFrom raster extent
#' @importFrom raster crs
#' @export

socioecoGeoData<-function(x=NULL,socioecoList=NULL,stackConnectionType=NULL,envLayerNames=NULL)
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
          {Stack= raster::stack(sapply(1:dim(x)[3],function(i) raster::raster(x[,,i])),layers=envLayerNames)
          names(Stack)=envLayerNames
          raster::extent(Stack)=Extent
          raster::crs(Stack) <- Crs
          Stack})}
   else if (class(x)=="RasterStack") {
     if (is.null(stackConnectionType)) stackConnectionType=rep("geographic",dim(x)[3]) 
     geo=new("geoEnvData",rasterstack,stackConnectionType)} else if (class(x)=="geoEnvData") {geo=x} else stop("x should be raster, RasterStack, array or empty")
  }
  if (is.null(socioecoList)) socioecoList=socioecoGroupsData()
  new("socioecoGeoData",geo,socioecoList)
}

#' Variable.names method for socioecoGeoData.
#' 
#' @name variable.names
#' @docType methods
#' @rdname variable.names-methods
#' @aliases variable.names,socioecoGeoData
#' @importFrom stats variable.names

setMethod("variable.names",
          "socioecoGeoData",
          function(object) {list(geo=names(object),socioeco=stats::variable.names(object@socioecoData))  
          }
)

#cat("geo dimensions\t: ",nrow(object),", ",ncol(object),", ",nlayers(object),", ",ncell(object),", ",nCellA(object)," (nrow, ncol, nlayers, ncell, ncellA)\n",sep="")
#cat("layerConnectionTypes\t:",paste(object@layerConnectionTypes,sep=", "),"\n")
#cat("resolution\t: ",res(object)[1],", ",res(object)[2],"\n",sep="")
#cat("extent\t\t: ",paste(extent(object),sep=", "),"(xmin, xmax, ymin, ymax)\n",sep="")
#cat("crs\t\t: ",as.character(crs(object)))}

#' Show method for socioecoGeoData.
#' 
#' @name show
#' @docType methods
#' @rdname show-methods
#' @aliases show,socioecoGeoData
#' @importFrom raster res
#' @importFrom raster extent
#' @importFrom raster crs
#' @export

setMethod("show",
          "socioecoGeoData",
          function(object) {
            cat("An object of class 'socioecoGeoData'\n")
            cat("- geoEnvData inherited class:\n")
            cat("dimensions\t:",object@nrows,",",object@ncols,",",nCellA(object)[1],",",dim(object)[3],"(nrow, ncol, ncell, layers)"," \n")
            cat("resolution\t:",raster::res(object)[1],",",raster::res(object)[2]," (x, y)")
            cat("\nlayerConnectionTypes\t:",paste(object@layerConnectionTypes,sep=", "))
            cat("\nextent\t\t:",raster::extent(object@geoEnvData)[1],",", raster::extent(object@geoEnvData)[2], "," , raster::extent(object@geoEnvData)[3], ",", raster::extent(object@geoEnvData)[4], " (xmin, xmax, ymin, ymax)")
            cat("\ncrs\t\t:",as.character(raster::crs(object)))
            cat("\nnames\t\t: ")
            cat(names(object),sep = ", ")
            cat("\n\n- socioecoGroupsData slot:\n")
            cat(show(object@socioecoData))
            }
          )

#' nCellA method for socioecoGeoData.
#' 
#' @name nCellA
#' @docType methods
#' @rdname nCellA-methods
#' @aliases nCellA,socioecoGroupData

setMethod("nCellA",
          signature = "socioecoGeoData",
          function(object) {
            return(c(geoCells=nCellA(object@geoEnvData),socioCells=sapply(object@socioecoData,FUN = function(x) nCellA(x))))
            }
          )

reactionNorm = c("scaling","enveloppe","envelin","conQuadratic","conQuadraticSkw")

#' Function for the amount of parameters of a niche model.
#' 
#' @description
#' Returns the amount of parameters required for determined niche model.
#' 
#' @param x Character. The type of the niche model.
#' @returns Int. Number of parameters for the niche model.

npNiche <- function(x) {unlist(lapply(x,function(x) switch(x[],
                                                           scaling=1,
                                                           enveloppe=2,
                                                           envelin=2,
                                                           conQuadratic=2,
                                                           conQuadraticSkw=2)
))
}

#' Validity function for nicheModel object
#' 
#' @description
#' Validity control for the nicheModel objects.
#' @param object nicheModel object.
#' @returns boolean.

validityNicheModel=function(object){
  if(!is(object@varNiche,"character"))stop("error in NicheModel variables : variables just accept character!")
  if(!is.list(object@pNiche))stop("error in NicheModel pNiche : pNiche just accept list!")
  if (!all(object@reactNorms%in%reactionNorm))stop(paste("reaction norm should be one of the following :",paste(reactionNorm,collapse = ", ")))
  if(FALSE%in%lapply(object@pNiche,is.numeric))stop("error in NicheModel parameter list : Parameter list just accept numeric!")
# if(!all(names(object@reactNorms)%in%object@varNiche))stop("error in NicheModel : names of reactionNorm slot are not all included in var slot")
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

#' nicheModel class.
#' 
#' @description
#' Defines the reaction norm for each variable, without interaction in this version.
#' 
#' @slot varNiche Character. The variable names to which the niche model applies.
#' @slot reactNorms Character. The reaction norms for the variables as a vector. The names of the entries correspond to the variables the reaction norm uses, and the value corresponds to the type of reaction norm.
#' @slot pNiche List. The list of numerical parameters vectors for each reaction norm, there should be a parameter vector for each reaction norm. The names of the vector components correspond to the values of the reaction norm slot.
#' @export

setClass("nicheModel",
         representation(varNiche="character",reactNorms="character",pNiche="list"),
         prototype(varNiche=c("temp","temp"),reactNorms=c("envelin","scaling"),pNiche=list(envelin=c(2,4),scaling=100))
)

setValidity("nicheModel", validityNicheModel)

#' nicheModel builder function
#' 
#' @description
#' Builder function for nicheModel objects.
#' 
#' @param varNiche Character. The variable names to which the niche model applies.
#' @param reactNorms Character. The reaction norms for the variables as a vector. The names of the entries correspond to the variables the reaction norm uses, and the value corresponds to the type of reaction norm.
#' @param pNiche List. The list of numerical parameters vectors for each reaction norm, there should be a parameter vector for each reaction norm. The names of the vector components correspond to the values of the reaction norm slot.
#' @returns an object of nicheModel class.
#' @export

nicheModel<-function(varNiche=c("temp","temp"),reactNorms=c("envelin","scaling"),pNiche=list(c(3,4),100)){#,form=formul){
  names(pNiche)=reactNorms
  new("nicheModel",varNiche=varNiche,reactNorms=reactNorms,pNiche=pNiche)#,form=form)
}

#' Function to return the amount of parameters for a migration kernel.
#' 
#' @description
#' Function used to return the amount of parameters of a migration probability distribution. 
#' 
#' @param x Character. The name of the migration probability distribution.
#' @returns Int. Returns the amount of parameters for the input probability distribution.

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

#' Validity function for geoMigrationModel objects.
#' 
#' @description
#' Validity function for geoMigrationModel objects. 
#' 
#' @param object geoMigrationModel object.
#' @returns boolean.

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

#' Migration model based on the geographical distance or grouping variables.
#' @description
#' Proportion of individual produced in each attributed cell that migrates to each attributed cell of the raster.
#' models are described in successive components of modelConnectionType, varMig, shapeMig, pMig and pMixt slots
#' if modelConnectionType = dist and shapeMig=island the model is an island model in which each cell is an island and a proportion of propagules migrates to the pool with a proportion given by the parameter.
#' if modelConnectionType = group and shapeMig=island the model is an island model in which each group is an island and a proportion of propagules migrates to the pool with a proportion given by the parameter there is a possibility of dispersion models within groups according to the shapeDisp.
#' Models are described in successive components of modelConnectionType, varMig, shapeMig, pMig and pMixt slots.
#' 
#' @slot modelConnectionType Character. describes migration model: depending on distance or grouping.
#' @slot varMig Variable according to which the individual migrates.
#' @slot shapeMig Character. Type of probability distribution for the dispersal. The popSep model means island model where cells of the raster belong to pops according to their group variable and mix panmictically within groups according to the popSep model.
#' @slot pMixt Numeric. A proportion of the individual migrates according to each model.
#' @slot pMig List. Parameters of the corresponding probability distribution.
#' @export 
 
setClass("geoMigrationModel",
         representation(modelConnectionType="character",varMig="character",shapeMig="character",pMig="list",pMixt="numeric"),         #
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

#' Builder function for geoMigrationModel objects.
#' 
#' @description
#' Builder function for objects of the geoMigrationModel class.
#' 
#' @param modelConnectionType Character. describes migration model: depending on distance or grouping.
#' @param varMig Variable according to which the individual migrates.
#' @param shapeMig Character. Type of probability distribution for the dispersal. The popSep model means island model where cells of the raster belong to pops according to their group variable and mix panmictically within groups according to the popSep model.
#' @param pMixt Numeric. A proportion of the individual migrates according to each model.
#' @param pMig List. Parameters of the corresponding probability distribution.
#' @returns Object of the class geoMigrationModel.
#' @export 

geoMigrationModel<-function(modelConnectionType=c("geographic","grouping"),varMig=c("temp","pops"),shapeMig=c("gaussian","island"),pMig=list(gaussian=1/1.96,island=.2),pMixt=c(0.5,0.5)){
  #if(length(pMixt)==length(shapeMig)) pMixt=(pMixt/sum(pMixt))[1:(length(pMixt)-1)] # pMixt requires length(shapeMig)-1 parameters only, since the sum of mixture parameters =1
  new("geoMigrationModel",modelConnectionType=modelConnectionType,varMig=varMig,shapeMig=shapeMig,pMig=pMig,pMixt=pMixt)
}

socioecoMigrationShapes=c("euclideanInverse") # Invers

#' Validity for socioecoMigrationModel objects.
#' @description
#' Validates socioecoMigrationModel objects. 
#' Checks if the length of pMig is different from length of varMig, if the migrationShape is one of the socioecoMigrationShapes elements, and if the names of pMig list are equal to the names of varMig.
#' @param object socioecoMigrationModel object.
#' @returns boolean.

validitysocioecoMigrationModel=function(object){
  #if(!is.character(object@shapeMig))stop("error in  migrationModel shapeMig : shapeMig just accept character!")
  if (length(object@varMig)!=length(object@pMig)) stop("length of pMig list is different from length of varMig list")
  if (!(object@shapeMig%in%socioecoMigrationShapes)) stop("migrationShape must be one of the following(s): ",socioecoMigrationShapes,collapse = ", ")
  if (any(names(object@pMig)!=object@varMig)) stop("names of pMig list should equal varMig")
}

#' Class to describe a migration model given by the socioecological variables.
#' @description
#' Proportion of individual produced in each attributed cell that migrates to each attributed cell of the raster.
#' 
#' @slot weight Numeric. represents the coefficient given to socioecoMigration Matrix relative to geoMigration matrix (has a fixed weight of 1).
#' @slot varMig Character. represents the names of the socioecoGroupsData used to calculate migration rates.
#' @slot shapeMig represents the model used to transform varMig into migration rates.
#' @slot pMig represent the parameters to transform varMig into distance.
#' @export

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

#' Builder function for a socioecoMigrationModel object.
#' 
#' @description
#' Proportion of individual produced in each attributed cell that migrates to each attributed cell of the raster.
#' 
#' @param weight Numeric. represents the coefficient given to socioecoMigration Matrix relative to geoMigration matrix (has a fixed weight of 1).
#' @param varMig Character. represents the names of the socioecoGroupsData used to calculate migration rates.
#' @param shapeMig represents the model used to transform varMig into migration rates.
#' @param pMig represent the parameters to transform varMig into distance.
#' @returns An object of the socioecoMigrationModel class.
#' @export

socioecoMigrationModel<-function(weight=1,varMig=c("Español","Chibcha","Corrabastos","Semillas_Comerciales","Guardianes_de_semilla"),shapeMig=c("euclideanInverse"),pMig=list(Español=1,Chibcha=1,Corrabastos=1,Semillas_Comerciales=1,Guardianes_de_semilla=1)){
  new("socioecoMigrationModel",weight=weight,varMig=varMig,shapeMig=shapeMig,pMig=pMig)
}

#' variable.names method for socioecoMigrationModel.
#' 
#' @name variable.names
#' @docType methods
#' @rdname variable.names-methods
#' @aliases variable.names,socioecoMigrationModel

setMethod("variable.names",
          "socioecoMigrationModel",
          function(object){
            object@varMig
          }
          )

#' variable.names method for geoMigrationModel.
#' 
#' @name variable.names
#' @docType methods
#' @rdname variable.names-methods
#' @aliases variable.names,socioecoMigrationModel

setMethod("variable.names",
          "geoMigrationModel",
          function(object){
            object@varMig
          }
)

#' variable.names method for nicheModel.
#' 
#' @name variable.names
#' @docType methods
#' @rdname variable.names-methods
#' @aliases variable.names,nicheModel

setMethod("variable.names",
          "nicheModel",
          function(object){
            object@varNiche
          }
)

#' model.extract method for nicheModel objects.
#' 
#' @name model.extract
#' @docType methods
#' @rdname model.extract-methods
#' @aliases model.extract,nicheModel

setMethod("model.extract",
          "nicheModel",
          function(frame,component="reactNorms"){
            switch(component,
                   varNiche=frame@varNiche,
                   reactNorms=frame@reactNorms)
          }
)

#' model.extract method for socioecoMigrationModel objects.
#' 
#' @name model.extract
#' @docType methods
#' @rdname model.extract-methods
#' @aliases model.extract,socioecoMigrationModel

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

#' Obtain the niche model of an object.
#' @description
#' This function obtains the niche model of an object.
#' 
#' @param object Object to extract the model.
#' @returns nicheModel object.

setGeneric("model",
  def=function(object){
    return(standardGeneric("model"))
  })

#' model method for nicheModel objects.
#' 
#' @name model
#' @docType methods
#' @rdname model-methods
#' @aliases model,nicheModel

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

#' locus class to contain individual locus information
#' @description
#' The `locus` class is used to store the information of a particular locus. It contains the locus name and the allele values for either microsatellite markers or presence/absence.
#' @slot name Character. The name of the locus.
#' @slot alleles Numeric. Vector that contains the allele values.
#' @export

setClass("locus",
         representation(name="character", alleles="integer"),
         prototype(name="SSR",alleles=as.integer(runif(2,min = 100, max = 200)))
         )

#' Validity function for locus class
#' @description
#' This function tests the validity of the locus class objects.
#' @param object locus object.
#' @returns boolean. 

validityLocus<-function(object) {
  if(!is.vector(object@alleles)) stop("The alleles must be given in the form of a numeric vector")
}

setValidity("locus", validityLocus)

#' Create a locus object
#' @description
#' This function creates a locus object.
#' @param locusName Character. The name of the marker.
#' @param mValues Numeric. The value(s) of the marker alleles.
#' @returns a locus object.
#' @export 

locus <- function(locusName=NULL, mValues=NULL) {
  if(is.null(locusName)) { locusName <- "SSR" }
  if(is.null(mValues)) { mValues <- runif(2, min = 100, max = 250) }
  new("locus", name = locusName, alleles = as.integer(mValues))
}

#' Generic method for to obtain the ploidy of a genotype.
#' @description
#' This method returns the ploidy for a marker or a genotype.
#' 
#' @param object object with genetic markers.
#' @returns Int. Ploidy level of the marker(s).
#' @export

setGeneric("getPloidy",
           def=function(object){
             return(standardGeneric("getPloidy"))
           })

#' getPloidy method for locus objects.
#' 
#' @name getPloidy
#' @docType methods
#' @rdname getPloidy-methods
#' @description
#' This method returns the ploidy level for the objects of class locus.
#' @aliases getPloidy,locus

setMethod("getPloidy",
          "locus",
          function(object){
            length(object@alleles)
          })

#' show method for locus objects.
#' 
#' @name show
#' @docType methods
#' @rdname show-methods
#' @aliases show,locus

setMethod("show",
          "locus",
          function(object){
            cat("Locus",object@name,"\n")
            cat(object@alleles)
          })

#' genotype class to hold the genetic information of individual samples
#' @description
#' This object holds the collection of loci that represents an individual.
#' 
#' @slot loci list of locus objects. This list contains the information of all the loci in the sample.
#' @export

setClass("genotype",
         representation(loci="list"),
         prototype(loci=list(locus("SSR1",c(125,127)),locus("SSR2", c(127,132)), locus("SSR3",c(200,188)))))

validityGenotype <- function(object){
  if(!any(sapply(object@loci,FUN= is, class2 = "locus"))) { stop("All objects in the genotype must be locus objects!")}
  if(anyDuplicated(sapply(object@loci,FUN = function(x) x@name))) { stop("The names of the markers cannot be repeated!") }
}

setValidity("genotype", validityGenotype)

#' getPloidy method for genotype objects.
#' 
#' @name getPloidy
#' @docType methods
#' @rdname getPloidy-methods
#' @description
#' This method returns the ploidy level for each of the markers in a genotype object.
#' @aliases getPloidy,genotype

setMethod("getPloidy",
          "genotype",
          function(object){
            return(sapply(object@loci, FUN = getPloidy))
          })

#' Show method for genotype objects.
#' @name show
#' @docType methods
#' @rdname show-methods
#' @aliases show,genotype

setMethod("show",
          "genotype",
          function(object) {
            cat("Genotype object:\n\n")
            for(i in 1:length(object@loci)) {
              cat(i,". ")
              show(object@loci[[i]])
              cat("\n")
            }
          })

#' Creates a genotype object.
#' @description
#' This function creates an object of genotype class.
#' @param samLoci List of `locus` objects. All the objects on this list should be of the class `locus`.
#' @returns Object of the class `genotype`.
#' @export

genotype <- function(samLoci=NULL) {
  if(is.null(samLoci)) {
    samLoci <- list(locus("SSR1", as.integer(runif(2,min = 100,max = 200))),locus("SSR2",as.integer(runif(2,min = 100,max = 200))), locus("SSR3", as.integer(runif(2,min = 100,max = 200))))
  }
  new("genotype", loci = samLoci)
}

#' Class that contains a single haplotype
#' @description
#' An object of this class contains represents a single haplotype. For this reason it cannot have more than one allele at each locus.
#' @slot .Data Genotype object. This is a class that inherits from the `genotype` with the constrain that it can only have one allele per locus.
#' @export

setClass("haplotype",
         contains = "genotype",
         representation(loci="list"),
         prototype(loci=list(locus("SSR1",128), locus("SSR2", 102), locus("SSR3",115))))

validityHaplotype <- function(object) {
  if(any(getPloidy(object) != 1)) {stop("All markers must be haploid in a haplotype!")}
}

setValidity("haplotype",validityHaplotype)

#' Creates a haplotype object
#' @description
#' This function creates a haplotype object.
#' @param samLoci List of `locus` objects. All the objects on this list should be of the class `locus`. For `haplotype` objects there must be only one allele per locus.
#' @returns Object of the haplotype class.
#' @export

haplotype <- function(samLoci=NULL){
  if(is.null(samLoci)) {
    samLoci <- list(locus("SSR1",128), locus("SSR2", 102), locus("SSR3",115))
  }
  new("haplotype",loci = samLoci)
}

#' Show method for haplotype objects.
#' @name show
#' @docType methods
#' @rdname show-methods
#' @aliases show,haplotype

setMethod("show","haplotype",function(object) {
  cat("Haplotype object:\n\n")
  for(i in 1:length(object@loci)) {
    cat(i,". ")
    show(object@loci[[i]])
    cat("\n")
  }
})

#' Generic method setName
#' @description
#' Sets the genetic marker name of a locus.
#' @param object `locus` class object. This method changes the name of a marker in a locus.
#' @returns locus object
#' @export

setGeneric("setName",
           def=function(object, newName = NULL){
             return(standardGeneric("setName"))
           })

#' setName method for locus objects.
#' @name setName
#' @docType methods
#' @rdname setName-methods
#' @aliases setName,locus

setMethod("setName", "locus", function(object, newName = NULL){
  if(is.null(newName)) { stop("You must provide a name for the marker!")}
  object@name <- newName
  return(object)
})

#' Generic method getAlleles
#' @description
#' This method obtains all the alleles in a genotype or haplotype.
#' @param object `genotype` or `haplotype` class object. This method returns the allele values for each marker in the genotype or haplotype.
#' @returns Numeric list.
#' @export

setGeneric("getAlleles",
           def=function(object){
             return(standardGeneric("getAlleles"))
           })

#' getAlleles method for genotype objects.
#' @name getAlleles
#' @docType methods
#' @rdname getAlleles-methods
#' @aliases getAlleles,genotype

setMethod("getAlleles", "genotype", function(object){
  c(sapply(object@loci,FUN = function(x) paste(x@name,".",x@alleles,sep="")))
})

#' Generic method markerNames
#' @description
#' This method obtains all the marker names in a genotype or haplotype.
#' @param object `genotype` or `haplotype` class object. This method returns the names for each marker in the genotype or haplotype.
#' @returns Character containing the names of the markers.
#' @export

setGeneric("markerNames", #This could be deprecated
           def=function(object){
             return(standardGeneric("markerNames"))
           })

#' markerNames method for genotype objects.
#' @name markerNames
#' @docType methods
#' @rdname markerNames-methods
#' @aliases markerNames,genotype

setMethod("markerNames", "genotype", function(object){
  sapply(object@loci,FUN = function(x) x@name)
})

#' Generic method getHaplotypes
#' @description
#' This method obtains all or the specified number of possible haplotypes that a genotype could produce.
#' @param object `genotype` class object. From this object the haplotypes are sampled.
#' @returns haplotype objects.
#' @export

setGeneric("getHaplotypes",
           def=function(object, num = NULL){
             return(standardGeneric("getHaplotypes"))
           })

#' getHaplotypes method for genotype objects.
#' @name getHaplotypes
#' @docType methods
#' @rdname getHaplotypes-methods
#' @aliases getHaplotypes,genotype

setMethod("getHaplotypes", "genotype", function(object, num = NULL) {
  posHap <- unique(expand.grid(getAlleles(object)))
  lociTab <- apply(posHap, MARGIN = c(1,2), locus, locusName = "Locus")
  
  for(i in 1:dim(posHap)[2]) {
    lociTab[,i] <- lapply(lociTab[,i], setName, newName = markerNames(object)[i])
  }
  
  if(is.null(num)){
    hapList <- apply(lociTab, MARGIN = 1, haplotype)
  }
  else if(is.numeric(num)) {
    num <- as.integer(num)
    numVect <- sample(dim(lociTab)[1], size = num, replace = TRUE)
    hapList <- lapply(numVect, FUN = function(x) haplotype(lociTab[x,]))
  }
  else {
    stop("The number of haplotypes must be a number!")
  }
  return(hapList)
})

#' Class that contains the information of the sampled cells.
#' 
#' @description
#' This class holds the geographical (coordinates), temporal (times of collection), and socioecological characteristics of the samples taken.
#' @importClassesFrom sp SpatialPoints
#' @slot geoCoordinates SpatialPoints object. Coordinates for the samples.
#' @slot sampleCell Int. Cells where the samples are located
#' @slot sampleTime Int. Times when the samples where collected. These times are used to integrate the past samples into the coalescence simulations.
#' @slot socioecoCoordinates Character list. Socioecological coordinates for the samples.
#' @importFrom sp SpatialPoints
#' @export

setClass("samplePoints",
         representation(geoCoordinates = "SpatialPoints",sampleCell = "numeric", sampleTime ="numeric", socioecoCoordinates="list"),
         prototype(geoCoordinates = sp::SpatialPoints(data.frame(Lat = c(0 ,0 ,1 ,1 ,1 ,3 ,3 ,0, 1, 0, 2, 0, 2, 0, 1, 0), Long = c(2, 3, 2, 1, 0, 2, 2, 0, 3, 3, 1, 0, 3, 0, 2, 1)), proj4string = CRS(as.character("+proj=longlat +datum=WGS84 +no_defs"))), 
                   sampleCell = NULL, 
                   sampleTime = c(0,0,0,0,0,-1,-3,-5,-15,-35,-50,-50,-72,-90,-90,-110)
                   )
         )

#' Validity function for samplePoints objects.
#' 
#' @description
#' Validity verification for samplePoints objects. It checks that the samples, the sample times and the coordinates are the same length.
#' 
#' @param object The samplePoints object.
#' @returns Boolean. 

validitysamplePoints <- function(object) {
  if(!is(object@geoCoordinates,"SpatialPoints")) stop("The geoCoordinates slot must be an object of the SpatialPoints class")
  if(!is(object@sampleTime,"numeric")) stop("The sampling times must be numeric")
}

setValidity("samplePoints", validitysamplePoints)

#' show method for samplePoints objects.
#' 
#' @name show
#' @docType methods
#' @rdname show-methods
#' @aliases show,samplePoints

setMethod("show", "samplePoints", function(object) {
  cat("An object of class 'samplePoints':\n\n")
  cat("Sample number:\n")
  cat(1:length(object@sampleTime),"\n")
  cat("Sampling times:\n")
  cat(object@sampleTime, "\n")
  cat("Summary of sample coordinates:\n")
  show(object@geoCoordinates)
})

#' Create objects of class samplePoints
#' 
#' @description
#' Creates an object of the class samplePoints
#' @param sCoordinates SpatialPoints, matrix or data.frame object. Coordinates where the samples were collected. In case of not being a `SpatialPoints` object, the data.frame or matrix should have only two columns, for Latitude and Longitude respectively.
#' @param sTimes Sampling times. 
#' @param proj4 Projection string of class \link[sp]{CRS}. Default value `NULL`. If NULL, `+proj=longlat +datum=WGS84 +no_defs` is used.
#' @returns samplePoints object.
#' @export

samplePoints <- function(sCoordinates, sTimes, proj4 = NULL) {
  
  if(is.null(proj4)) { proj4 <- sp::CRS(as.character("+proj=longlat +datum=WGS84 +no_defs")) }
  
  if(is(sCoordinates, "SpatialPoints")) { sampleCoords = sCoordinates }
  else { tryCatch(
                  {sampleCoords = sp::SpatialPoints(sCoordinates, proj4string = proj4)},
                  error = function(cond) {
                      message("There is a problem with the format of your coordinates")
                      message("Here is the original error message:")
                      message(conditionMessage(cond))
                  }
                )
        }
  
  new("samplePoints", geoCoordinates = sampleCoords,sampleCell = 0, sampleTime = sTimes)
}

#' Class that contains the genetic data of the samples
#' @description
#' This class contains the spatial and temporal information (inherited from [spatialPoints] object) as well as the genetic data of the samples.
#' 
#' @slot .Data spatialPoints object. 
#' @slot genetData list of `genotype` objects. This list contains the information for the genetic markers of each of the sampled individuals.
#' @slot markerType character. The t
#' @slot recombDist matrix. This matrix contains the recombination probability between the studied markers. All marker names in the genotypes should also be in this recombination matrix.
#' @export

setClass("genetSample",
         contains="samplePoints",
         representation(genetData = "list", markerType = "character" ,recombDist = "matrix"),
         )

validityGenetSample <- function(object) {
  if(length(object@geoCoordinates) != length(object@genetData)) {stop("All sampled points should have its corresponding genetic data")}
  if(!(object@markerType %in% c("microsatellite", "snp"))) {stop("The marker is not within the accepted values: 'microsatellite' or 'snp'")}
}

setValidity("genetSample", validityGenetSample)

#' Creates a genetSample object.
#' @description
#' This function creates a genetSample object.
#' @param sampledPoints samplePoints object containing the spatial information of the samples.
#' @param samGenotypes list of genotype objects corresponding to the sampled organisms.
#' @param mType character. Type of markers used. `microsatellite` is the name used for microsatellite markers and `snp` for SNPs.
#' @param recDist either a matrix or a data.frame containing the distance or the recombination probability between the markers.
#' @param mDist boolean. This indicates if the recombination matrix (or data.frame) contains recombination probabilities (`FALSE`) or distances in cM (`TRUE`). Defaults to `FALSE`.
#' @returns genetSample object.
#' @export

genetSample <- function(sampledPoints=NULL, samGenotypes=NULL, mType = NULL, recDist=NULL, mDist = TRUE) {
  if(is.null(sampledPoints)) {sampledPoints <- new("samplePoints", sampleCell = 0)}
  if(is.null(samGenotypes)) {
    genList <- list()
    for(i in 1:length(sampledPoints@geoCoordinates)) {genList <- append(genList,genotype())}
    samGenotypes <- genList
  }
  if(is.null(recDist)) {
    markers <- unique(c(sapply(samGenotypes,markerNames)))
    recDist <- matrix(0.5,ncol = length(markers), nrow = length(markers),dimnames = list(markers,markers))
    diag(recDist) <- 0
  }
  if(!mDist) { recDist <- (1-exp(-(2*recDist)/100))/2 }
  if(is.null(mType)) {mType <- "microsatellite"}
  
  new("genetSample", sampledPoints, genetData = samGenotypes, markerType = mType, recombDist = recDist)
}

#' show method for genetSample objects.
#' 
#' @name show
#' @docType methods
#' @rdname show-methods
#' @aliases show,genetSample

setMethod("show", "genetSample", function(object){
  cat("An object of the class 'genetSample':\n\n")
  cat("Sample number:\n")
  cat(1:length(object@sampleTime),"\n")
  cat("Sampling times:\n")
  cat(object@sampleTime, "\n")
  cat("Summary of sample coordinates:\n")
  show(object@geoCoordinates)
  cat("\n")
  cat(length(object@genetData)," genotypes\n")
  cat("Characterized by:\t", unique(c(sapply(object@genetData, markerNames))), " markers\n")
  cat("Ploidy levels:\t", unique(c(sapply(object@genetData, getPloidy))),"\n")
})

#' mutationModel class
#' @description
#' This class holds the mutation model used for calculating the genetic transition matrix. This only provides some barriers to coerce the possible values of the mutation models.
#' @export

setClass("mutationModel",
         contains="character")

validityMutationModel <- function(object){
  if(!(object %in% c("simple","geometric"))) {stop("The mutation model should be either 'simple' or 'geometric")}
  if(length(object) > 1) {stop("The mutation model should contain only one value")}
}

setValidity("mutationModel", validityMutationModel)

#' Creates a mutationModel object
#' @description
#' This function creates a mutationModel object.
#' @param x character string indicating the mutation models. Currently only `simple` and `geometric` are supported.
#' @returns mutationModel object.
#' @export

mutationModel <- function(x) {
  new("mutationModel", x)
}

#' show method for mutationModel
#' @name show
#' @docType methods
#' @rdname show-methods
#' @aliases show,mutationModel

setMethod("show","mutationModel",function(object){
  cat("Mutation model:\n")
  cat(object)
})

#' Method to generate a genetic allele matrix
#' @description
#' This method generates a genetic allele matrix from an object containing genotypes.
#' @param object Object that contains the genotypes of the samples.
#' @returns Matrix of the probabilities of the alleles in a sample of genotypes.
#' @export

setGeneric("genetMatrix",
           def = function(object) {
             standardGeneric("genetMatrix")
           })

#' genetMatrix method for genotype objects.
#' 
#' @name genetMatrix
#' @docType methods
#' @rdname genetMatrix-methods
#' @aliases genetMatrix,genotype

setMethod("genetMatrix", "genotype", function(object) {
  tempAll <- sapply(unique(getAlleles(object)), FUN = function(x) length(grep(x,getAlleles(object))))
  
  resAll <- sapply(names(tempAll), FUN = function(y) {
    #tempAll[y]/sum(tempAll[grep(sub("\\..*",x=y,replacement = ""),names(tempAll))]) #Done so the sum of probabilities for each locus was 1.
    tempAll[y]/sum(tempAll)
  })
  
  return(matrix(resAll, nrow = 1, dimnames = list("",names(tempAll))))
})

#' genetMatrix method for genetSample objects.
#' 
#' @name genetMatrix
#' @docType methods
#' @rdname genetMatrix-methods
#' @aliases genetMatrix,genetSample

setMethod("genetMatrix","genetSample", function(object){
  samList <- lapply(object@genetData, FUN = genetMatrix)
  markLabels <- sort(unique(unlist(lapply(samList,colnames))))
  tempMat <- matrix(data = 0, nrow = length(samList), ncol = length(markLabels), dimnames = list(1:length(samList),markLabels))
  
  for(i in 1:length(samList)) {
    genepos <- sapply(colnames(samList[[i]]), FUN = function(x) {grep(x,colnames(tempMat))})
    tempMat[i,genepos] <- tempMat[i,genepos] + samList[[i]]
  }
  
  return(tempMat)
})

#' Calculates the genetic transition probability between two markers
#' @description
#' This function calculates the transition probability between two markers according to the type of marker and the distance metric selected.
#' @param x First marker to compare.
#' @param y Second marker to compare.
#' @param mark Character. Marker type. It can only be `microsatellite`.
#' @param dist Character. Genetic distance metric. Can be either `simple` or `geometric`.
#' @returns Double. Transition probability between two markers.

geneticDistance <- function(x,y,mark,dist) {
  
  P <- 0.1
  Mu <- 1e-2
  
  switch(mark,
         microsatellite=switch(dist,
                               simple = (Mu)^abs(x-y),
                               geometric = (Mu)*(1-P)*((P)^(abs(x-y)-1))),
         snp = abs((1-abs(x-y))-Mu)
  )
}

#' Compare genetic markers
#' @description
#' This function compares genetic markers after the genetic matrix is done.
#' @param x the markers to be compared.
#' @param marker the type of marker used.
#' @param distMet the distance metric used.
#' @returns a probability transition matrix.

markerCompare <- function(x, marker, distMet){
  if(gsub("\\..*",x=x[1],replacement = "") != gsub("\\..*",x=x[2],replacement = "")) {
    0
  }
  else {
    geneticDistance(as.numeric(gsub("^.*?\\.",x=x[1],replacement = "")),as.numeric(gsub("^.*?\\.",x=x[2],replacement = "")),mark = marker, dist = distMet)
  }
}

#' Class that includes genetSample, the mutation model and the gene distance matrix.
#' @description
#' This class includes inherits from genetSample the spatial and genetic data of the samples, and contains also the mutation model for the markers and the distance between the loci.
#' @slot .Data genetSample object that contains the genetic and spatial information.
#' @slot mutationModel Character. One of the implemented mutation models used to calculate the probability of coalescence. 
#' @export

setClass("genetSet",
         contains="genetSample",
         representation(sampleMatrix = "matrix",mutationModel = "character", transitionMatrix = "matrix"))

validityGenetSet <- function(object) {
  if(any(colnames(object@sampleMatrix) != colnames(object@transitionMatrix))) {stop("The same marker-allele combinations should be both on the sample matrix and the transition matrix")}
  if(any(colnames(object@sampleMatrix) != rownames(object@transitionMatrix))) {stop("The same marker-allele combinations should be both on the sample matrix and the transition matrix")}
  if(any(rowSums(object@transitionMatrix) < 0.9999)) {stop("The sum of probabilities should be 1 for all rows")}
  if(any(colSums(object@transitionMatrix) < 0.9999)) {stop("The sum of probabilities should be 1 for all columns")}
}

setValidity("genetSet", validityGenetSet)

#' Creates a genetSet object
#' @description
#' This function creates a genetSet object. This uses a genetSample object and a mutation model object to calculate the genetic transition matrix.
#' @param genData genetSample object. This object should contain the genetic and spatial information of the sampled individuals.
#' @param mutModel mutationModel object. This specifies the mutation model selected for the calculation of the genetic transition probabilities.
#' @returns genetSet object.
#' @export

genetSet <- function(genData = NULL, mutModel = NULL) {
  if(is.null(genData)) {genData <- genetSample()}
  if(is.null(mutModel)) {mutModel <- "geometric"}
  
  samMatrix <- genetMatrix(genData)
  geneLabs <- colnames(samMatrix)
  transMat <- matrix(data = 0, ncol = length(geneLabs), nrow = length(geneLabs), dimnames = list(geneLabs,geneLabs))
  
  triang <- combn(geneLabs,2, FUN = markerCompare, marker=genData@markerType, distMet = mutModel)
  transMat[lower.tri(transMat)] <- triang
  transMat <- t(transMat)
  transMat[lower.tri(transMat)] <- triang
  diag(transMat) <- 1 - colSums(transMat)
  new("genetSet", genData, sampleMatrix = samMatrix, mutationModel = mutModel, transitionMatrix = transMat)
}

#' show method for genetSet objects
#' @name show
#' @docType methods
#' @rdname show-methods
#' @aliases show,genetSet

setMethod("show", "genetSet", function(object){
  cat("Object of the class: \tgenetSet\n\n")
  cat("Sample number:\n")
  cat(1:length(object@sampleTime),"\n")
  cat("Sampling times:\n")
  cat(object@sampleTime, "\n")
  cat("Sample cells:\n")
  cat(object@sampleCell)
  cat("\n")
  cat("Summary of sample coordinates:\n")
  show(object@geoCoordinates)
  cat("\n")
  cat(length(object@genetData)," genotypes\n")
  cat("Characterized by:\t", unique(c(sapply(object@genetData, markerNames))), " markers\n")
  cat("Ploidy levels:\t", unique(c(sapply(object@genetData, getPloidy))),"\n")
  cat("Marker type:\t")
  cat(object@markerType)
  cat("\nMutation model:\t")
  cat(object@mutationModel)
})

#' Class that reunites the present and past socioecoData.
#' 
#' @description
#' Class to represent environmental dynamic data history. It inherits from socioecoData as the present socioecoGeoData. Includes a past socioecoGeoData list with parsing times.
#' The last past socioecoGeoData in the list goes from the last parsing time to minus infinite.
#' @slot .Data socioecoGeoData object that contains the present data.
#' @slot pastSocioecoGeoData List of socioecoGeoData representing the past socio-ecological and geographical data.
#' @slot parsingTimes Numeric. List of the parsing times.
#' @slot timeUnit Character. Specifies the time unit.
#' @slot zeroTime POSIXlt. The present time.
#' @export

setClass("socioecoGeoDataHistory",
         contains="socioecoGeoData",
         representation(pastSocioecoGeoData="list",parsingTimes="numeric",timeUnit="character",zeroTime="POSIXlt"),
         prototype(new("socioecoGeoData"),pastSocioecoGeoData=list(new("socioecoGeoData"),new("socioecoGeoData"),new("socioecoGeoData")),parsingTimes=c(0,-200,-5000,-20000),timeUnit="days",zeroTime=as.POSIXlt('2005-4-19 7:01:00'))
)

validitysocioecoGeoDataHistory = function(object){
  if (any(object@parsingTimes>0)) stop("the pastStartingTimes should be negative")
  if ((length(object@parsingTimes)-1)!=(length(object@pastSocioecoGeoData))) stop("slot pastStartingTimes should include all the starting dates between period, which is length(object)")
  if (length(object@parsingTimes)>1) for (i in 2:length(object@parsingTimes)) {if (object@parsingTimes[i]>=object@parsingTimes[i-1]) stop("the socioecoGeoDataList should order from recent to past")}
}

setValidity("socioecoGeoDataHistory", validitysocioecoGeoDataHistory)

#' Creates an object of the class socioecoGeoDataHistory.
#' 
#' @description
#' Class to represent environmental dynamic data history. It inherits from socioecoData as the present socioecogeodata. Includes a past socioecogeodata list with parsing times.
#' The last past socioecogeodata in the list goes from the last parsing time to minus infinite.
#' @param socioecoGeoData socioecoGeoData object that contains the present data.
#' @param PastSocioecoGeoData List of socioecoGeoData representing the past socio-ecological and geographical data.
#' @param ParsingTimes Numeric. List of the parsing times.
#' @param TimeUnit Character. Specifies the time unit. Defaults to "days".
#' @param ZeroTime POSIXlt. The present time.
#' @returns An object of the socioecoGeoDataHistory class.
#' @export

socioecoGeoDataHistory <- function(SocioecoGeoData=socioecoGeoData(),PastSocioecoGeoData=list(socioecoGeoData(),socioecoGeoData(),socioecoGeoData()),ParsingTimes=c(0,-200,-500,-2000),TimeUnit="days",ZeroTime=as.POSIXlt('2005-4-19 7:01:00')) {
  new("socioecoGeoDataHistory",SocioecoGeoData,pastSocioecoGeoData=PastSocioecoGeoData,parsingTimes=ParsingTimes,timeUnit=TimeUnit,zeroTime=ZeroTime)
}

#' show method for socioecoGeoDataHistory objects.
#' 
#' @name show
#' @docType methods
#' @rdname show-methods
#' @aliases show,socioecoGeoDataHistory

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
            cat("\nlayerConnectionTypes\t:",paste(object@layerConnectionTypes,sep=", "))
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
            cat(show(object@samplePoints))
          }
)

#' Validity function for objects of the socioecoGeoDataModel class.
#' @description
#' This function validates that the R and K models are `nicheModel` objects and that the geo-migration model is a `geoMigrationModel` object.
#' @param object The evaluated socioecoGeoDataModel object.
#' @returns Boolean.

validitysocioecoGeoDataModel=function(object){
  if(!is(object@Kmodel,"nicheModel"))stop("Error in socioecoGeoDataModel Kmodel: Kmodel only accepts NicheModel !")
  if(!is(object@Rmodel,"nicheModel"))stop("Error in socioecoGeoDataModel Rmodel: Rmodel only accepts NicheModel !")
  if(!is(object@geoMigModel,"geoMigrationModel"))stop("Error in socioecoGeoDataModel migration: migration only accepts migrationModel !")
}

#' Class to contain the socioecoGeoDataModel built on the historical information
#' 
#' @description
#' This class builds the migration models using the historical and present socioecological and geographical information. 
#' 
#' @slot .Data socioecoGeoDataHistory object containing all the historical and present socioecological and geographical information.
#' @slot SocioecoGeoData socioecoGeoData object.
#' @slot Kmodel nicheModel object. Carrying capacity niche model.
#' @slot Rmodel nicheModel object. Reproductive potential niche model.
#' @slot geoMigModel geoMigrationModel object. Contains the geographic migration model information.
#' @slot socioecoMigModel socioecoMigrationModel object. Contains the socioecological migration model information.
#' @export

setClass("socioecoGeoDataModel",
         contains = "socioecoGeoDataHistory",
         representation(SocioecoGeoData = "socioecoGeoData",Kmodel="nicheModel",Rmodel="nicheModel",geoMigModel="geoMigrationModel",socioecoMigModel="socioecoMigrationModel"),
         validity=validitysocioecoGeoDataModel,
         prototype(new("socioecoGeoDataHistory"),Kmodel=new("nicheModel"),Rmodel=new("nicheModel"),geoMigModel=new("geoMigrationModel"),socioecoMigModel=new("socioecoMigrationModel"))
      
)

#' Create a socioecoGeoDataModel object.
#' @description
#' Create a socioecoGeoDataModel object.
#' 
#' @param socioecoGeoDataHistory socioecoGeoDataHistory object. This object contains the socio-ecological, geographical and historical data for the studied population. Default value `NULL`.
#' @param SocioecoGeoData socioecoGeoData object. If the socioecoGeoDataHistory object is missing, this parameter contains the present socio-ecological and geographical data.
#' @param PastSocioecoGeoData List of socioecoGeoData objects. If the socioecoGeoDataHistory object is missing, this parameter contains the past socio-ecological and geographical data.
#' @param ParsingTimes Numeric. Parsing times for the present and past data.
#' @param TimeUnit Character. The time unit for the Parsing Times.
#' @param ZeroTime POSIXlt object. The zero or present time.
#' @param nicheK nicheModel object. Niche model for the carrying capacity.
#' @param nicheR nicheModel object. Niche model for the reproductive capacity.
#' @param migModel migrationModel object. The migration model calculated for the studied landscape.
#' @param varNicheK Character. The variable name of which the carrying capacity K depends.
#' @param reactNormsK Character. The type of reaction norm for the listed variable for the carrying capacity.
#' @param varNicheR Character. The variable name of which the reproductive potential R depends.
#' @param reactNormsR Character. The type of reaction norm for the listed variable for the reproductive potential.
#' @param pNicheR Numeric. The parameter(s) for the reaction norm of the reproductive potential (R).
#' @param pNicheK Numeric. The parameter(s) for the reaction norm of the carrying capacity (K).
#' @param modelConnectionType Character. The types of connection types in the model.
#' @param varMig Character. The variables involved in the migration model calculation.
#' @param shapeMig Character. The shape of the migration probability distribution.
#' @param pMig Numeric. Migration probability.
#' @param pMixt Numeric. Mixture probability.
#' @export
#' @importFrom raster stack
#' @importFrom raster raster
#' @returns socioecoGeoDataModel object.

socioecoGeoDataModel<-function(socioecoGeoDataHistory=NULL,
                               SocioecoGeoData=socioecoGeoData(),PastSocioecoGeoData=list(socioecoGeoData(),socioecoGeoData(),socioecoGeoData()),
                               ParsingTimes=c(0,-200,-500,-2000),TimeUnit="days",ZeroTime=as.POSIXlt('2005-4-19 7:01:00'),
                               nicheK=NULL,nicheR=NULL,migModel=NULL,
                               varNicheK="temp",reactNormsK=c(temp="scaling"),pNicheK=list(scalingK=100),
                               varNicheR=c("temp","temp"),reactNormsR=c(temp="envelin",temp="scaling"),pNicheR=list(envelin=c(1,4),scalingR=10),
                               modelConnectionType=c("geographic","grouping"),varMig=c("temp","pops"),shapeMig=c("gaussian","popSep"),pMig=list(1.10574E5/1.96,numeric(0)),pMixt=c(.5,.5))
  
{
  if (is.null(socioecoGeoDataHistory)) socioecoGeoDataHistory=socioecoGeoDataHistory(SocioecoGeoData,PastSocioecoGeoData,ParsingTimes,TimeUnit,ZeroTime)
  if (is.null(nicheK)) nicheK=nicheModel(varNiche = varNicheK,reactNorms = reactNormsK, pNiche = pNicheK)
  if (is.null(nicheR)) nicheR=nicheModel(varNiche = varNicheR,reactNorms = reactNormsR, pNiche = pNicheR)
  if (is.null(migModel)) migModel= geoMigrationModel(modelConnectionType = modelConnectionType,varMig = varMig,shapeMig = shapeMig,pMig = pMig,pMixt = pMixt)
  
  new("socioecoGeoDataModel",socioecoGeoDataHistory,Kmodel=nicheK,Rmodel=nicheR,geoMigModel=migModel)
}

setValidity("socioecoGeoDataModel", validitysocioecoGeoDataModel)

#' show method for nicheModel objects.
#' 
#' @name show
#' @docType methods
#' @rdname show-methods
#' @aliases show,nicheModel

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

#' show method for geoMigrationModel objects.
#' 
#' @name show
#' @docType methods
#' @rdname show-methods
#' @aliases show,geoMigrationModel

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

#' show method for socioecoGeoDataModel objects.
#' 
#' @name show
#' @docType methods
#' @rdname show-methods
#' @aliases show,socioecoGeoDataModel

setMethod("show",
          "socioecoGeoDataModel",
          function(object) {
            cat("class\t\t: socioecoGeoDataModel\n\n")
            cat("Data (Inherited)\t\t: socioecoGeoDataHistory\n")
            show(object@.Data)
            cat("\nslot\t\t: socioecoGeoData\n")
            show(object@SocioecoGeoData)
            cat("\nslot\t\t: pastsocioecoGeoData \n")
            show(object@pastSocioecoGeoData)
            cat("\nslot\t\t: Kmodel \n")
            show(object@Kmodel)
            cat("\nslot\t\t: Rmodel \n")
            show(object@Rmodel)
            cat("\nslot\t\t: geoMigModel \n")
            show(object@geoMigModel)
            cat("\nslot\t\t: socioecoMigModel \n")
            show(object@socioecoMigModel)
          }
) 

#' Envelope reaction norm
#' @description
#' An envelope reaction norm. Its value is 1 when `X` is within the values of the `p` parameter, and 0 when `X` is above or below the `p` values.
#' @param X Numeric. Present value for the parameter.
#' @param p Numeric. Lower and higher limit for the reaction norm. `p` must have two values.
#' @returns Numeric. 1 for values within the lower and upper limits, and 0 for values outside.

enveloppe <- function(X,p){
  if(length(p)!=2)stop("The parameter of envelope must have two dimensions")
  else X>=p[1]&X<=p[2]
}

#' Envelope linear reaction norm
#' @description
#' An envelope linear reaction norm. Its value is 0 when `X` its above or below the range marked by `p`. The result value is 0 when `X = p[1]` and 1 when `X = p[2]`, and the resulting diagonal straight line between `(p[1],0)` and `(p[2],1)`.
#' 
#' @param X Numeric. Present value for the parameter.
#' @param p Numeric. Lower and higher limit for the reaction norm. `p` must have two values.
#' @returns Numeric. Value between 0 and 1.

envelinear <- function(X, p) {
  if(length(p)!=2)stop("The parameter of envelinear must have two dimensions, p[1] is the value at Y = 0, p[2] is the value of X at Y = 1")
  else (X-p[1])/(p[2]-p[1])*enveloppe(X,p)
}

#' Scaling function
#' @description
#' Function used to scale the reaction norms.
#' 
#' @param X Numeric. Reaction norm value.
#' @param p Numeric. Scaling parameter.
#' @returns Scaled value.

scaling <- function(X,p){X[]<-p
X
}

#' conQuadratic reaction norm
#' @description
#' Quadratic reaction norm between the `p` parameter values and 0 outside.
#' 
#' @param X Numeric. Present value for the parameter.
#' @param p Numeric. Lower and higher limit for the reaction norm. `p` must have two values.
#' @returns Numeric. Value between 0 and 1.

conQuadratic <- function(X,p)
{
  if(length(p)!=2)stop("The parameter is  not valid because it contains more or less than two values")
  else -4*(X-p[2])*(X-p[1])/((p[2]-p[1])^2)*enveloppe(X,p)
}

#' conQuadraticSkw reaction norm
#' @description
#' Skewed quadratic reaction norm between the `p` parameter values and 0 outside. In this case the quadratic reaction is multiplied by an envelope linear norm to obtain a skewed quadratic reaction norm.
#' 
#' @param X Numeric. Present value for the parameter.
#' @param p Numeric. Lower and higher limit for the reaction norm. `p` must have two values.
#' @returns Numeric. Value between 0 and 1.

conQuadraticSkw <- function(X,p){
  conQuadratic(X,p)*envelinear(X,p)
  X
}

#' Build an R/K landscape
#' @description
#' The niche function makes the product of the different layers results
# typically the scaling times the shape (enveloppe, envelin, ocnQuadratic or conQuadraticSkw).
#' @param object Object containing the information required to build the R/K landscape.
#' @returns rasterStack object containing the R lanscape in a layer and the K landscape in another layer.
#' @export

setGeneric(
  name = "buildRKlandscape",
  def=function(object){return(standardGeneric("buildRKlandscape"))}
)

#' buildRKlandscape method for socioecoGeoDataModel objects.
#' 
#' @name buildRKlandscape
#' @docType methods
#' @rdname buildRKlandscape-methods
#' @importFrom raster crs
#' @importFrom raster stack
#' @importFrom raster extent
#' @aliases buildRKlandscape,socioecoGeoDataModel

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
            raster::crs(R)<-raster::crs(object)
            raster::extent(R)<-object@extent
            result=raster::stack(R,K)
            names(result)<-c("R","K")
            result
            # the niche function makes the product of the different layers results
            # typically the scaling times the shape (enveloppe, envelin, ocnQuadratic or conQuadraticSkw)
          }
)

#' Build geographic distance matrix
#' @description
#' Builds a geographic distance matrix from raster data.
#' 
#' @param object Object containing raster data to calculate the geographical distance.
#' @returns Matrix of dimension n^2 (being n the number of cells in the raster) with the geographic distance among cells.
#' @export

setGeneric(
  name = "buildGeodist",
  def=function(object){return(standardGeneric("buildGeodist"))}
)

#' buildGeodist method for socioecoGeoDataModel objects.
#' 
#' @name buildGeodist
#' @docType methods
#' @rdname buildGeodist-methods
#' @importFrom raster distanceFromPoints
#' @aliases buildGeodist,socioecoGeoDataModel

setMethod(
  f="buildGeodist",
  signature=c("socioecoGeoDataModel"),
  definition=function(object)
  {
    Ndim = 1+all(ncell(object)!=dim(object)[1:2]) # if the landscape is a line one cell width Ndim=1, otherwise Ndim=2
    #if model["shapeMig"]=="contiguous" matrix()
    geoDist = unlist(apply(xyA(object),1,
                           function(x){
                             values(raster::distanceFromPoints(object@geoEnvData,x))
                             }))[Acells(object),]
    geoDist[which(geoDist==0)]<-sqrt(2*min(geoDist[which(geoDist!=0)])^2)/3
    # The distance between points within the same cell is one third 
    #
    return(geoDist)
  }
)

#' Build Migration Matrix
#' @description
#' Build a migration probability matrix for the landscape described in a raster.
#' 
#' @param object Object containing raster data to build the migration probability matrix,
#' @returns Matrix of size n^2 * n^2 (being n the number of cells) containing the transition probability among cells.
#' @export

setGeneric(
  name = "buildMigrationMatrix",
  def=function(object){return(standardGeneric("buildMigrationMatrix"))}
)

#' buildMigrationMatrix method for socioecoGeoDataModel objects.
#' 
#' @name buildMigrationMatrix
#' @docType methods
#' @rdname buildMigrationMatrix-methods
#' @aliases buildMigrationMatrix,socioecoGeoDataModel

setMethod(
  f="buildMigrationMatrix",
  signature=c("socioecoGeoDataModel"),
  definition=function(object)
  {
    Ndim = 1+all(ncell(object)!=dim(object)[1:2]) # if the landscape is a line one cell width Ndim=1, otherwise Ndim=2
    #if model["shapeMig"]=="contiguous" matrix()
    migration=list()
    for (i in which(object@geoMigModel@modelConnectionType=="geographic"))
    {
      migration[[i]] = apply(buildGeodist(object), c(1,2),
                             function(x)(switch(object@geoMigModel@shapeMig[i],
                                                gaussian = dnorm(x, mean = 0, sd = object@geoMigModel@pMig[[i]][1], log = FALSE),
                                                exponential = (dexp(x, rate = 1/object@geoMigModel@pMig[[i]][1], log = FALSE)),
                                                contiguous = apply(xyA(object),1,function(x){((abs(xyA(object)[,"x"]-x["x"])==res(object)[1])&(xyA(object)[,"y"]==x["y"])|(abs(xyA(object)[,"y"]-x["y"])==res(object)[1])&(xyA(object)[,"x"]==x["x"]))*object@geoMigModel@pMig[[1]][1]/4})+diag(nCellA(object))*(1-object@geoMigModel@pMig[[1]][1]),
                                                contiguous8 = apply(xyA(object),1,function(x){((abs(xyA(object)[,"x"]-x["x"])==res(object)[1])&(xyA(object)[,"y"]==x["y"])|(abs(xyA(object)[,"y"]-x["y"])==res(object)[1])&(xyA(object)[,"x"]==x["x"]|(abs(xyA(object)[,"x"]-x["x"])==res(object)[1])))*object@geoMigModel@pMig[[1]][1]/8})+diag(nCellA(object))*(1-object@geoMigModel@pMig[[1]][1]),
                                                island = diag(nCellA(object))*(1-object@geoMigModel@pMig[[i]][1])+(1-diag(nCellA(object)))*(object@geoMigModel@pMig[[i]][1])/(nCellA(object)-1),
                                                fat_tail2 = x^object@geoMigModel@pMig[[i]][2]*exp(-2*x/(object@geoMigModel@pMig[[i]][1]^0.5))
                                                #contiguous_long_dist_mixt = model["pMig"]["plongdist"]/nCellA(object)+(x==0)*(1-model["pMig"]["pcontiguous"]-model["pMig"]["plongdist"])+((x>0)-(x>1.4*res(object)[1]))*(model["pMig"]["pcontiguous"]/2),
                                                #gaussian_long_dist_mixt = model["pMig"][2]/nCellA(object) + (dnorm(x, mean = 0, sd = object@geoMigModel@pMig[[i]][1], log = FALSE))
                             )))
      migration[[i]]<-migration[[i]]/sum(migration[[i]])
    }
    for (i in which(object@geoMigModel@modelConnectionType=="grouping"))
    {
      migration[[i]] = switch(object@geoMigModel@shapeMig[i],
                                                popSep = sapply(valuesA(object)[i],function(x) {x==valuesA(object)[i]})
                             )
      migration[[i]]<-migration[[i]]/sum(migration[[i]])
    }
    return(apply(array(unlist(migration), dim = c(nCellA(object)[1],nCellA(object)[1], length(migration))),c(1,2),function(x){sum(x*object@geoMigModel@pMixt)}))
  }
)

#' Backward transition probabilities matrix
#' 
#' @description
#' Square matrix containing the backward transition probabilities of a defined landscape.
#' @export

setClass("TransitionBackward",
         contains = "matrix",
         validity = function(object){
           if (all(nrow(object)==0))stop("The matrix is empty.")
           if (nrow(object)!=ncol(object))stop("The matrix is not square")
           if (!all(rowSums(object)>0.999999999) && !all(rowSums(object)<1.000000001)) {stop("The sum of probabilities in each row is not 1")}
         }
)

#' Create a TransitionBackward object
#' @description
#' This function creates a TransitionBackward object.
#' @param matrix Matrix containing the backwards transition probability.
#' @returns TransitionBackward object.
#' @export

TransitionBackward<- function(matrix){
  if (nrow(matrix)!=ncol(matrix))stop("The matrix is not square")
  if(class(rownames(matrix)[1])!="character"){
    lname <- c(1:nrow(matrix))
    rownames(matrix) <- lname
    colnames(matrix) <- lname
  }
  new(Class="TransitionBackward",matrix)
}

#' Forward transition probabilities matrix
#' 
#' @description
#' Square matrix containing the forward transition probabilities of a defined landscape.
#' @export

setClass("TransitionForward",
         contains = "matrix",
         validity = function(object){
           if (all(nrow(object)==0))stop("The matrix is empty.")
           if (nrow(object)!=ncol(object))stop("The matrix is not square")
         }
)

#' Build a backwards transition matrix
#' @description
#' Builds a backwards transition matrix from landscape and niche information.
#' 
#' @param object Object containing the landscape and niche information.
#' @returns Backwards transition matrix.
#' @export

setGeneric(
  name = "buildTransitionBackward",
  def=function(object){return(standardGeneric("buildTransitionBackward"))}
)

#' buildTransitionBackward method for socioecoGeoDataModel objects.
#' 
#' @name buildTransitionBackward
#' @docType methods
#' @rdname buildTransitionBackward-methods
#' @aliases buildTransitionBackward,socioecoGeoDataModel
#' @importFrom raster values

setMethod(f="buildTransitionBackward",
          signature=c("socioecoGeoDataModel"),
          definition=function(object){
            RKland <- buildRKlandscape(object)
            R <- raster::values(RKland)[,1]
            K <- raster::values(RKland)[,2]
            #R <- object@Rmodel@pNiche[[1]]
            #K <- object@Kmodel@pNiche[[1]]
            mig <- buildMigrationMatrix(object)
            if ((length(R)==1)&(length(K)==1)){transition = R * K * t(mig)}
            if ((length(R)>1)&(length(K)==1)){transition = t(matrix(R,nrow=length(R),ncol=length(R))) * K * t(mig)}
            if ((length(R)==1)&(length(K)>1)){transition = R * t(matrix(K,nrow=length(K),ncol=length(K))) * t(mig)}
            if ((length(R)>1)&(length(K)==1)){transition = t(matrix(R,nrow=length(R),ncol=length(R))) * lpar$K * t(mig)}
            if ((length(R)>1)&(length(K)>1)) {transition = t(matrix(R,nrow=length(R),ncol=length(R))) * t(matrix(K,nrow=length(K),ncol=length(K))) * t(mig)}
            t<-transition/t(sapply(rowSums(transition),function(x)rep(x,ncol(transition))))
            TransitionBackward(t)
          }
)

#setMethod(f="transitionBackward",
#          signature=c("socioecoGeoDataModel"),
#          definition=function(object){
#            if ((length(R)==1)&(length(K)==1)){transition = R * K * t(mig)}
#            if ((length(R)>1)&(length(K)==1)){transition = t(matrix(R,nrow=length(R),ncol=length(R))) * K * t(mig)}
#            if ((length(R)==1)&(length(K)>1)){transition = R * t(matrix(K,nrow=length(K),ncol=length(K))) * t(mig)}
#            if ((length(R)>1)&(length(K)==1)){transition = t(matrix(R,nrow=length(R),ncol=length(R))) * lpar$K * t(mig)}
#            if ((length(R)>1)&(length(K)>1)) {transition = t(matrix(R,nrow=length(R),ncol=length(R))) * t(matrix(K,nrow=length(K),ncol=length(K))) * t(mig)}
#            t<-transition/t(sapply(rowSums(transition),function(x)rep(x,ncol(transition))))
#            TransitionBackward(t)
#          }
#)

#' Build a forward transition matrix
#' @description
#' Builds a forward transition matrix from landscape and niche information.
#' 
#' @param object Object containing the landscape and niche information.
#' @returns Forward transition matrix.
#' @export

setGeneric(
  name = "buildTransitionForward", 
  def = function(object,meth){return(standardGeneric("buildTransitionForward"))}
)

#' buildTransitionForward method for socioecoGeoDataModel objects.
#' 
#' @name buildTransitionForward
#' @docType methods
#' @rdname buildTransitionForward-methods
#' @aliases buildTransitionForward,socioecoGeoDataModel,character
#' @importFrom raster values

setMethod(
  f="buildTransitionForward",
  signature=c("socioecoGeoDataModel","character"),
  definition=function(object,meth)
  {
    RKland <- buildRKlandscape(object)
    R <- raster::values(RKland)[,1]
    K <- raster::values(RKland)[,2]
    mig <- buildMigrationMatrix(object)
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

#' envDynSet class
#' 
#' @description
#' This class calculates the forward and backwards socioecological transition matrices for the coalescence simulations starting from on the information present in the socioecoGeoDataModel object.
#' @slot .Data socioecoGeoDataModel object containing the socioecological and geographical information of the studied area.
#' @slot RKlandscape RasterStack object. R and K landscape calculated using the nicheModel objects corresponding to each environmental variable.
#' @slot geoDist matrix. Distance matrix from each individual cell to each other in the raster.
#' @slot migrationMatrix matrix. Calculated spatial migration matrix.
#' @slot transitionForward matrix. Calculated forward transition matrix.
#' @slot transitionBackward matrix. Calculated backwards transition matrix.
#' @export

setClass("envDynSet",
         contains = "socioecoGeoDataModel",
         representation(RKlandscape="RasterStack",geoDist="matrix",migrationMatrix="matrix",transitionForward="matrix",transitionBackward="matrix"),
         prototype(socioecoGeoDataModel(),
                   RKlandscape=buildRKlandscape(socioecoGeoDataModel()),
                   geoDist=buildGeodist(socioecoGeoDataModel()),
                   migrationMatrix=buildMigrationMatrix(socioecoGeoDataModel()),
                   transitionForward=buildTransitionForward(socioecoGeoDataModel(), "non_overlap"),
                   transitionBackward=buildTransitionBackward(socioecoGeoDataModel())),
         validity=validitysocioecoGeoDataModel
)

#prototype(new("socioecoGeoDataHistory"),Kmodel=new("nicheModel"),Rmodel=new("nicheModel"),geoMigModel=new("geoMigrationModel"),socioecoMigModel=new("socioecoMigrationModel"))
#representation(Kmodel="nicheModel",Rmodel="nicheModel",geoMigModel="geoMigrationModel",socioecoMigModel="socioecoMigrationModel")

#' Creates an envDynSet object.
#' @description
#' This function creates an envDynSet object. It can either be created from a socioecoGeoDataModel object, or the information contained in that object separately.
#' @param socioecoGeoDataModel socioecoGeoDataModel object containing the socioecological and geographical information of the studied area.
#' @param RKlandscape RasterLayer object. This object will be calculated either from the data in the socioecoGeoDataModel or from the data provided directly to this function.
#' @param geoDist matrix. Geographical distance calculated from each cell to each other.
#' @param migrationMatrix matrix. Migration transition matrix calculated considering the socioecological and spatial variables.
#' @param transitionForward matrix. Calculated transition forward matrix that considers the socioecological and spatial variables.
#' @param transitionBackward matrix. Calculated transition backwards matrix that considers the socioecological and spatial variables.
#' @param envData envData object. If the socioecoGeoDataModel is not provided, the spatial data must be provided as an [envData] object.
#' @param EnvStack RasterStack object containing the environmental conditions of the studied area.
#' @importFrom raster raster
#' @importFrom raster stack
#' @param stackConnectionType Character. The type of connection in the different raster layers. Can be either geographic or grouping.
#' @param envLayerNames Character. Names of the different environmental layers of the raster.
#' @param varNicheK Character. Names of the variables considered for the K niche model.
#' @param varNicheR Character. Names of the variables considered for the R niche model.
#' @param reactNormsK Character. Reaction norms for the K niche model.
#' @param pNicheK Numeric list. Parameters for the K niche model calculation.
#' @param pNicheR Numeric list. Parameters for the R niche model calculation
#' @param reactNormsR Character. Reaction norms for the R niche model.
#' @param modelConnectionType Character. Type of connection of the migration model.
#' @param varMig Character. Variables involved in the calculation of the migration matrix. These are variables that should affect either the K or R values for a population.
#' @param pMig Numeric list. Parameters for the migration model.
#' @param pMixt Numeric. Parameters for the mixture model.
#' @returns envDynSet object.
#' @export

envDynSet<-function(socioecoGeoDataModel=NULL,RKlandscape=NULL,geoDist=NULL,migrationMatrix=NULL,transitionForward=NULL,transitionBackward=NULL,
                          envData=NULL,
                          EnvStack=raster::stack(x=c(temp=raster::raster(matrix(c(5,4,2,4,2,4,2,4,5),nrow=3),xmn=0,xmx=3,ymn=0,ymx=3,crs="+proj=longlat"),pops=raster::raster(matrix(c(1,2,2,1,1,2,1,1,1),nrow=3),xmn=0,xmx=3,ymn=0,ymx=3))),
                          stackConnectionType=c("geographic","grouping"),envLayerNames=NULL,Extent=NULL,
                          varNicheK="temp",reactNormsK=c(temp="scaling"),pNicheK=list(scalingK=100),
                          varNicheR=c("temp","temp"),reactNormsR=c(temp="envelin",temp="scaling"),pNicheR=list(envelin=c(1,4),scalingR=10),
                          modelConnectionType=c("geographic","grouping"),varMig=c("temp","pops"),shapeMig=c("gaussian","popSep"),pMig=list(1.10574E5/1.96,numeric(0)),pMixt=c(.5,.5)){
  if (is.null(socioecoGeoDataModel)) {
    if (is.null(envData)) envData= geoEnvData(rasterStack = EnvStack, layerConnectionTypes = stackConnectionType)
    socioecoGeoDataModel=socioecoGeoDataModel(SocioecoGeoData = envData,nicheK = nicheModel(varNiche = varNicheK,reactNorms = reactNormsK, pNiche = pNicheK),nicheR = nicheModel(varNiche = varNicheR,reactNorms = reactNormsR, pNiche = pNicheR), migModel = geoMigrationModel(modelConnectionType = modelConnectionType,varMig = varMig,shapeMig = shapeMig,pMig = pMig,pMixt = pMixt))}
  if (is.null(RKlandscape)) RKlandscape=buildRKlandscape(socioecoGeoDataModel)
  if (is.null(geoDist)) geoDist=buildGeodist(socioecoGeoDataModel)
  if (is.null(migrationMatrix)) migrationMatrix=buildMigrationMatrix(socioecoGeoDataModel)
  if (is.null(transitionForward)) transitionForward=buildTransitionForward(socioecoGeoDataModel, "non_overlap")
  if (is.null(transitionBackward)) transitionBackward=buildTransitionBackward(socioecoGeoDataModel)
  new("envDynSet",socioecoGeoDataModel,RKlandscape=RKlandscape,migrationMatrix=migrationMatrix,transitionForward=transitionForward,transitionBackward=transitionBackward)
}

#' ecoGenetSet class
#' @description
#' This class reunites the genetic information of the samples contained in a genetSet object together with the spatial, demographic and social information contained in envDynSet.
#'  @slot .Data envDynSet object containing all socioecological and demographic information.
#'  @slot genetSample genetSet object containing all genetic and spatial information for the samples collected.
#'  @export

setClass("ecoGenetSet", 
         contains = "envDynSet",
         representation(genetSample = "genetSet"),
         prototype(envDynSet(), genetSample = genetSet()))

#' Creates an ecoGenetSet object
#' @description
#' This function creates an ecoGenetSet object from an envDynSet and a genetSet objects.
#' This is the preferred method for creating an ecoGenetSet object as opposed to `new("ecoGenetSet")` as this way some important calculations would not be performed.
#' @param envDyn envDynSet object. The object containing the spatial, social and demographic information of the region under study.
#' @param genetData genetSet object. The object containing the genetic, temporal and location information of the collected samples.
#' @returns ecoGenetSet object.
#' @importFrom raster cellFromXY
#' @export

ecoGenetSet <- function(envDyn = NULL, genetData = NULL) {
  if(is.null(envDyn)) {envDyn <- envDynSet()}
  if(is.null(genetData)) {genetData <- genetSet()}
  genetData@sampleCell <- setNames(raster::cellFromXY(envDyn,genetData@geoCoordinates),1:length(genetData@genetData))
  new("ecoGenetSet",envDyn, genetSample = genetData)
}

#' Extracts data from ecoGenetSet objects

setMethod(
  f ="[",
  signature = c(x="ecoGenetSet" ,i="character",j="missing"),
  definition = function (x ,i ,j , drop ){
    switch ( EXPR =i,
             "K" ={return(values(x@RKlandscape$K))} ,
             "R" ={return(values(x@RKlandscape$R))} ,
             "TransiBackw" ={return(x@transitionBackward)},
             "TransiForw" = {return(x@transitionForward)},
             "Present" = {return(x@genetSample@sampleCell[which(x@genetSample@sampleTime == 0)])},
             "Past" = {return(x@genetSample@sampleCell[which(x@genetSample@sampleTime != 0)])},
             "Times" = return(x@genetSample@sampleTime),
             stop("This slot doesn't exist!")
    )
  }
)

#' genealSimProb class
#' @description
#' This class contains the ecoGenetSet information and a list of different coalescence simulations with its respective demographic and genetic probabilities.
#' @slot .Data ecoGenetSet object containing the demographic and genetic information.
#' @slot genealogies list of coalSim objects (simulated genealogies) with its corresponding probabilities.
#' @export

setClass("genealSimProb",
         contains="ecoGenetSet",
         representation(genealogies = "list"),
         prototype(ecoGenetSet(),genealogies = list())
         )

#' Creates a genealSimProb object
#' @description
#' This function creates a genealSimProb object which contains the environmental, demographic, genetic information and the coalescent simulations together with its demographic and genetic probabilities.
#' @param ecoGenetSet ecoGenetSet object. This contains all the environmental, demographic and genetic information required.
#' @param geneal list of coalSim simulations of the corresponding ecoGenetSet object. If `NULL`, an empty list will be created to store the coalescent simulations made from the ecoGenetSet object.
#' @returns genealSimProb
#' @export

genealSimProb <- function(ecoGenetSet = NULL, geneal = NULL) {
  if(is.null(ecoGenetSet)) { ecoGenetSet <- ecoGenetSet() }
  genealogies <- list()
  if(!is.null(geneal)) { genealogies <- append(genealogies,geneal) }
  new("genealSimProb", ecoGenetSet, genealogies = genealogies)
}

###################Coalescence simulation methods#########################

#' coalSim class
#' @description
#' This class is a container for the coalescence simulations.
#' @slot coalescent list. This list stores the coalescent events and times from a coalescent calculated by the `simulCoal` method.
#' @slot probForward numeric. Forward demographic probability of the coalescent simulation.
#' @export

setClass("coalSim",
         representation(coalescent = "list", probForward = "numeric", genetProb = "numeric"))

#' show method for coalSim objects.
#' 
#' @name show
#' @docType methods
#' @rdname show-methods
#' @aliases show,coalSim

setMethod("show", "coalSim", function(object){
  cat("Object of the class coalescent:\n")
  cat("Forward probability:\n")
  cat(object@probForward)
  cat("\nGenetic probability:\n")
  cat(object@genetProb)
  # cat("\nCoalescent:\n")
  # show(object@coalescent)
})

#' Genetic probability calculation for a genealogy
#' @description
#' This function calculates the probability of a genealogy given the genetic data of a sample. This function requires a coalescent (simulated genealogy) and a ecoGenetSet object containing the marker information for the samples as well as the genetic transition matrix.
#' @param coalescent coalSim object. This object should contain the simulated genealogy.
#' @param ecoGenetData ecoGenetSet object. This object should contain the genetic information of the samples.
#' @importFrom matrixcalc matrix.power
#' @returns numeric. The calculated probability of the genealogy given the genetic characterization of the samples.

geneticProb<-function(coalescent, ecoGenetData) {
  probability <- 1
  #Ne <- mean(ecoGenetData["K"])
  genTransMat <- ecoGenetData@genetSample@transitionMatrix
  sampleMat <- ecoGenetData@genetSample@sampleMatrix
  for(tc in coalescent) {
    tempProb <- t(matrixcalc::matrix.power(genTransMat,(tc$br_length[1]-1)) %*% sampleMat[as.character(tc$coalescing[1]),]) %*% (matrixcalc::matrix.power(genTransMat,(tc$br_length[2]-1)) %*% sampleMat[as.character(tc$coalescing[2]),])
    probability <- probability * tempProb
    
    newNode <- apply(rbind(sampleMat[as.character(tc$coalescing[1]),],sampleMat[as.character(tc$coalescing[2]),]), MARGIN = 2, FUN = mean)
    
    sampleMat <- rbind(sampleMat, newNode)
    rownames(sampleMat)[nrow(sampleMat)] <- tc$new_node
    
  }
  return(as.numeric(probability))
}

#' Coalescence simulation from socioecological and geographical data.
#' @description
#' Simulates a coalescence from socioecological and geographical data.
#' @param ecoGenetSet ecoGenetSet object. This object must contain all the information for the coalescence simulation.
#' @param printCoal Boolean. If `TRUE` it prints the time elapsed during the simulations.
#' @returns List. The first element is the coalescent, and the second is the summed forward probability. 
#' @export

setGeneric(
  name = "simulCoal",
  def=function(ecoGenetSet,printCoal){return(standardGeneric("simulCoal"))}
)

#' simulCoal method for ecoGenetSet objects.
#' 
#' @name simulCoal
#' @docType methods
#' @rdname ecoGenetSet-methods
#' @aliases simulCoal,ecoGenetSet,boolean
#' #' @description
#' Simulates a coalescence from socioecological and geographical data.
#' @param ecoGenetSet ecoGenetSet object. This object must contain all the information for the coalescence simulation.
#' @param printCoal Boolean. If `TRUE` it prints the time elapsed during the simulations.
#' @returns List. The first element is the coalescent, and the second is the summed forward probability. 

setMethod(
  f="simulCoal", ##Simulates a coalescent
  signature=c("ecoGenetSet","logical"),
  definition=function(ecoGenetSet,printCoal)
  {
    #Initialization of the objects that will store the information about the coalescent
    prob_forward=NA
    N <- round(ecoGenetSet["K"]);#N[N==0]<-1
    coalescent = list() #
    cell_number_of_nodes <- parent_cell_number_of_nodes <- ecoGenetSet["Present"]
    nodes_remaining_by_cell = list()
    nodes_remaining <- as.numeric(names(ecoGenetSet@genetSample@sampleCell))
    time=0
    single_coalescence_events=0
    single_and_multiple_coalescence_events=0
    
    #Adds the remaining nodes per cell to the nodes_remaining_by_cell list for the coalescent simulation
    for (cell in 1:nCellA(ecoGenetSet)[1])
    {
      nodes_remaining_by_cell[[cell]] <- which(cell_number_of_nodes==cell)
    }
    
    #Main cycle for the coalescent simulation. It will run until there is only one remaining node, including the nodes samples in the past
    while (length(nodes_remaining)>1) 
    {
      #Adding the nodes sampled in the past if there is any
      if(time > 0 && any(abs(ecoGenetSet["Times"]) == time)) 
      {
        past_nodes_remaining <- ecoGenetSet@genetSample@sampleCell[which(abs(ecoGenetSet["Times"]) == time)]
        
        #This cycle adds the past nodes present in each of the cells. It cycles only through the cells where the past nodes are
        for(cell in unique(past_nodes_remaining)) 
        {
          nodes_remaining_by_cell[[cell]] <- append(nodes_remaining_by_cell[[cell]], as.numeric(names(which(past_nodes_remaining == cell))))
          num_nodes_to_add <- length(which(past_nodes_remaining == cell))
          final_pos_parent_cell <- length(parent_cell_number_of_nodes)
          parent_cell_number_of_nodes <- append(parent_cell_number_of_nodes,rep(cell, num_nodes_to_add))
          names(parent_cell_number_of_nodes)[(final_pos_parent_cell+1):(final_pos_parent_cell+num_nodes_to_add)] <- as.numeric(names(which(past_nodes_remaining == cell)))
          cell_number_of_nodes <- parent_cell_number_of_nodes
        }
      }
      
      #Spatial transition backwards of each node with a probability given by the Backwards Transition matrix of the envDynSet object
      for (node in 1:length(parent_cell_number_of_nodes))
      {
        parent_cell_number_of_nodes[node] = sample(x=nCellA(ecoGenetSet)[1],size=1,prob=c(ecoGenetSet["TransiBackw"][cell_number_of_nodes[node],]))
      }
      
      #After the spatial transition backwards, the nodes newly positioned in the cells are accounted for in this cycle
      for (cell in 1:nCellA(ecoGenetSet)[1])
      {
        nodes_remaining_by_cell[[cell]] <- as.numeric(names(which(parent_cell_number_of_nodes==cell)))
      }
      
      #Calculation of the Forward transition probability of each iteration
      prob_forward[time] = sum(log(ecoGenetSet["TransiForw"][parent_cell_number_of_nodes,cell_number_of_nodes]))
      time=time+1;  if(printCoal==TRUE){if (round(time/10)*10==time) {print(time)}}
      
      #The coalescent events are calculated in this cycle accounting for the new cell position of each node
      for (cell in 1:nCellA(ecoGenetSet)[1])
      {
        nodes_remaining_in_the_cell = nodes_remaining_by_cell[[cell]] <- as.numeric(names(which(parent_cell_number_of_nodes==cell)))
        #nodes_remaining_in_the_cell = nodes_remaining_by_cell[[cell]] <- as.numeric(which(parent_cell_number_of_nodes==cell))
        #The coalescence of the nodes in each cell is calculated if there is more than one node in the cell, otherwise all nodes in the cell have already coalesced
        if (length(nodes_remaining_in_the_cell)>1)
        {
          #Amount of nodes remaining in the cell
          nbgenesremaining=length(nodes_remaining_in_the_cell)
          smp = sample(N[cell],length(nodes_remaining_in_the_cell),replace=TRUE)
          #A matrix of the random coalescent events is generated. The row names of this matrix correspond to the nodes in the current cell
          parentoffspringmatrix <- matrix(smp,nrow=nbgenesremaining,ncol=N[cell])==matrix(1:N[cell],nrow=nbgenesremaining,ncol=N[cell],byrow=TRUE)
          rownames(parentoffspringmatrix) <- nodes_remaining_in_the_cell
          #When the column sum of any of the columns of the parentoffspringmatrix object are more than 1, it means there is a coalescent event between the nodes indicated in the rownames
          if (any(colSums(parentoffspringmatrix)>1) )
          {
            for (multiple in which(colSums(parentoffspringmatrix)>1))
            {
              #The counter for single coalescent events is increased by 1
              single_coalescence_events = single_coalescence_events +1
              #The new nodes that formed by the coalescing of other nodes are given a name and added to the list of nodes in the current cell
              nodes_that_coalesce = names(which(parentoffspringmatrix[,multiple]))
              nodes <- unlist(nodes_remaining_by_cell) 
              new_node <- max(unique(c(nodes,nodes_remaining)))+1; nodes = nodes[!(nodes %in% nodes_that_coalesce)]; nodes=append(nodes,new_node) 
              #new_node <- max(nodes)+1;nodes=nodes[!(names(nodes)%in%nodes_that_coalesce)];nodes=append(nodes,new_node);names(nodes)[length(nodes)]=new_node ##Legacy code
              #The parent cell of the nodes that coalesced are removed from this list and the parent cell of the newly formed nodes are added
              parent_cell_number_of_nodes <- append(parent_cell_number_of_nodes[!(names(parent_cell_number_of_nodes)%in%nodes_that_coalesce)],cell);names(parent_cell_number_of_nodes)[length(parent_cell_number_of_nodes)]<-new_node
              #The coalescent events are accounted for
              coalescent[[single_coalescence_events]] <- list(time=time,coalescing=as.numeric(nodes_that_coalesce),new_node=new_node)
              #The coalesced nodes are removed from the nodes remaining and the newly formed nodes are added
              nodes_remaining_in_the_cell = nodes_remaining_by_cell[[cell]] <- append(nodes_remaining_in_the_cell[!nodes_remaining_in_the_cell %in% nodes_that_coalesce],new_node)
              #The coalesced nodes are removed from the list containing all nodes (present and past) to avoid overwriting node names
              nodes_remaining <- append(nodes_remaining[!(nodes_remaining %in% nodes_that_coalesce)], new_node)
              #The single and multiple coalescent events are counted by adding the number of nodes that coalesced minus 1
              single_and_multiple_coalescence_events = single_and_multiple_coalescence_events + length(nodes_that_coalesce) - 1
            }
          }
        }
      }
      #Before the following iteration, the cell number of nodes is assigned the value of the parent cell number of nodes
      cell_number_of_nodes = parent_cell_number_of_nodes
    }
    tips = NULL
    internals = NULL
    nodes = NULL
    times = NULL
    #Collect the data of nodes, internals and times in the corresponding lists
    for (i in 1:length(coalescent))#i=1;i=2
    {
      nodes = append(nodes,c(coalescent[[i]]$coalescing,coalescent[[i]]$new_node))
      internals = append(internals,coalescent[[i]]$new_node)
      times = append(times,coalescent[[i]]$time)
    }
    nodes = as.numeric(levels(as.factor(c(nodes,internals))));nodes = nodes[order(nodes)]
    tips = nodes[!((nodes)%in%(internals))]
    #Getting the branch length of each coalescing node
    for (i in 1:length(coalescent))#i=1
    {
      for (coalescing in coalescent[[i]]$coalescing)# coalescing = coalescent[[i]]$coalescing[1]
      {
        #The branch length of the nodes is calculated. The sampling time is subtracted from the tips
        if (coalescing %in% tips) {coalescent[[i]]$br_length <- append(coalescent[[i]]$br_length,(coalescent[[i]]$time - abs(ecoGenetSet@genetSample@sampleTime[coalescing])))
        } else {
          coalescent[[i]]$br_length <- append(coalescent[[i]]$br_length,coalescent[[i]]$time-times[which(internals==coalescing)])
        }
      }
    }
    
    #Return a list with the coalescent and the total probability forward
    new("coalSim",coalescent=coalescent,probForward=sum(prob_forward), genetProb = geneticProb(coalescent, ecoGenetSet))
  }
)

#' Method to simulate multiple coalSim.
#' @description
#' This method is used to simulate multiple coalescents for parameter inference.
#' @param ecoGenetSet ecoGenetSet object that contains the spatial, demographic and genetic information.
#' @param printCoal boolean. `TRUE` if the time of each simulation should be displayed.
#' @param iteration int. The amount of simulations to perform.
#' @param multiCore boolean. If `TRUE` 
#' @returns list of coalSim objects.
#' @export

setGeneric(
  name = "simulMultiCoal",
  def=function(ecoGenetSet,printCoal,iteration,multiCore=FALSE,cores=NULL){return(standardGeneric("simulMultiCoal"))}
)

#' simulMultiCoal method for ecoGenetData objects.
#' 
#' @name simulMultiCoal
#' @docType methods
#' @rdname simulMultiCoal-methods
#' @aliases simulMultiCoal,ecoGenetSet,logical,numeric
#' @importFrom parallel detectCores
#' @importFrom parallel mclapply

setMethod(
  f="simulMultiCoal",
  signature=c("ecoGenetSet","logical","numeric","logical","numeric"),
  definition=function(ecoGenetSet,printCoal,iteration,multiCore=FALSE,cores = NULL){
    if(multiCore){
      if(is.null(cores)) { cores <- parallel::detectCores() }
      parallel::mclapply(1:iteration,function(x) simulCoal(ecoGenetSet, FALSE), mc.cores = cores)
    }
    else {
      lapply(1:iteration,function(x)simulCoal(ecoGenetSet,printCoal))
    }
  }
)

#' simulMultiCoal method for ecoGenetData objects.
#' 
#' @name simulMultiCoal
#' @docType methods
#' @rdname simulMultiCoal-methods
#' @aliases simulMultiCoal,ecoGenetSet,logical,numeric,missing
#' @importFrom parallel detectCores
#' @importFrom parallel mclapply

setMethod(
  f="simulMultiCoal",
  signature=c("ecoGenetSet","logical","numeric","logical","missing"),
  definition=function(ecoGenetSet,printCoal,iteration,multiCore=FALSE,cores = NULL){
    if(multiCore){
      cores <- parallel::detectCores()
      parallel::mclapply(1:iteration,function(x) simulCoal(ecoGenetSet, FALSE), mc.cores = cores)
    }
    else {
      lapply(1:iteration,function(x)simulCoal(ecoGenetSet,printCoal))
    }
  }
)

#' simulMultiCoal method for ecoGenetData objects.
#' 
#' @name simulMultiCoal
#' @docType methods
#' @rdname simulMultiCoal-methods
#' @aliases simulMultiCoal,ecoGenetSet,logical,numeric,missing,ANY
#' @importFrom parallel detectCores
#' @importFrom parallel mclapply

setMethod(
  f="simulMultiCoal",
  signature=c("ecoGenetSet","logical","numeric","missing","ANY"),
  definition=function(ecoGenetSet,printCoal,iteration,multiCore=FALSE,cores = NULL){
    lapply(1:iteration,function(x)simulCoal(ecoGenetSet,printCoal))
  }
)

#' Print the demographic and genetic probabilities of an object
#' @description
#' This function returns a data frame with the demographic and genetic probabilities for the genealigy simulations contained in an object.
#' @param object the object containing the simulations.
#' @returns data frame with the demographic and genetic probabilities.
#' @export

setGeneric(
  name = "getGenealProb",
  def=function(object){return(standardGeneric("getGenealProb"))}
)

#' getGenealProb method for genealSimProb objects.
#' 
#' @name getGenealProb
#' @docType methods
#' @rdname getGenealProb-methods
#' @aliases getGenealProb,genealSimProb

setMethod(
  f="getGenealProb",
  signature = "genealSimProb",
  definition = function(object){
    if(length(object@genealogies) < 1) {stop("There are no genealogies in this object!")}
    matrix(c(sapply(object@genealogies, FUN = function(x){return(x@probForward)}), sapply(object@genealogies, FUN = function(x) {return(x@genetProb)})), nrow = 2, byrow = TRUE, dimnames = list(c("Demographic","Genetic"), 1:length(object@genealogies)))
  })

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

#' Converts coalSim objects into Newick trees.
#' @description
#' This method converts coalSim objects from a coalescence simulation into a Newick-formatted tree.
#' @param coalescent coalSim object. This object is generated by a coalescent simulation (implemented in the `simulCoal` method).
#' @returns Newick format tree.
#' @export

setGeneric(
  name = "coalescent_2_newick",
  def=function(object){return(standardGeneric("coalescent_2_newick"))}
)

setGeneric(
  name = "linearizedFstUndigraph",
  def=function(transition, popSize){return(standardGeneric("linearizedFstUndigraph"))}
)

setGeneric(
  name = "commute_time_undigraph",
  def=function(object){return(standardGeneric("commute_time_undigraph"))}
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



setMethod(
  f="compare",
  signature=c("ecoGenetSet","numeric","logical","numeric","character"),
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
                                             "1"=paste("simulCoal, t/10^",log10timescale),
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

setMethod(
  f="linearizedFstDigraph",
  signature=c("TransitionBackward","numeric"),
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

#' coalescent_2_newick method for coalSim objects.
#' 
#' @name coalescent_2_newick
#' @docType methods
#' @rdname coalescent_2_newick-methods
#' @aliases coalescent_2_newick,coalSim

setMethod(
  f="coalescent_2_newick",
  signature="coalSim",
  definition=function(object)
  {
    tree=paste(" ",object@coalescent[[length(object@coalescent)]]$new_node," ",sep="")
    for (i in length(object@coalescent):1)
    {
      Time = object@coalescent[[i]]$time
      coalesc <- as.character(object@coalescent[[i]]$coalescing)
      tree <- str_replace(tree,paste(" ",as.character(object@coalescent[[i]]$new_node)," ",sep=""),paste(" ( ",paste(" ",coalesc," :",object@coalescent[[i]]$br_length,collapse=" ,",sep=""),") ",sep=""))
    }
    tree <- gsub(" ","",paste(tree,";",sep=""))
    tree
  }
)

#' Plot method for coalSim objects
#' @description
#' Plot method for coalSim objects.
#' @param object coalSim. The coalescent object.
#' @returns plot of the tree representing the coalescent.
#' @aliases plot
#' @rdname plot-methods
#' @importFrom ape plot.phylo
#' @importFrom ape read.tree
#' @export
 
plot.coalSim <- function(object) {
  ape::plot.phylo(ape::read.tree(text=coalescent_2_newick(object)))
}

setMethod(
  f="linearizedFstUndigraph",
  signature=c("TransitionBackward","numeric"),
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

#################DEPRECATED METHODS###################

##,transFor="transitionMatrixForward",transBack="getTransitionBackward",environment="landscape"

#Demographic<-setClass("Demographic",
#                      contains = "envDynLandscape",
#                      slots = c(sampleCells="integer"),
#                      validity = function(object){
#                        if(any(object@K<0))stop("K is negative")
#                        if(any(object@R<0))stop("R is negative")
#                        if(any(object@sampleCells>nCellA(object)))stop("Sample cell number outside the range")
#                      }
#)# Demographic contains all the information to run a coalescent and calculate probabilities and graph statistics

#setGeneric(
#  name = "getTransitionBackward",
#  def=function(K,R,mig){return(standardGeneric("getTransitionBackward"))}
#)

#setGeneric(
#  name = "sampleLandscape",
#  def=function(demographic, sampleSize,xy, option){return(standardGeneric("sampleLandscape"))}
#)

#setGeneric(
#  name = "transitionMatrixForward",
#  def=function(K,R,mig,meth){return(standardGeneric("transitionMatrixForward"))}
#)

#setMethod("buildRKLandscape",
#          signature=c("landscape","NicheModel"),
#          definition = function(object,model){                  #X=object, p=,shape=
#            Y=lapply(model@variables,function(x){
#              switch(model@reactNorms[[x]],
#                     scaling={setValues(object[[x]],rep(model@pNiche[[x]],ncell(object[[x]])))},
                     #proportional = {values(object[[x]])=object[[x]]*model@pNiche[[x]]},
#                     enveloppe = {object[[x]]=enveloppe(object[[x]],model@pNiche[[x]])},
#                     envelin={object[[x]]=envelinear(object[[x]],model@pNiche[[x]])},
#                     conQuadratic={object[[x]]=conQuadratic(object[[x]],model@pNiche[[x]])},
#                     conQuadraticSkw={object[[x]]=conQuadraticSkw(object[[x]],model@pNiche[[x]])},#conquadraticskewed=conquadraticskewed(object[,,(model@variables==x)],p),
#                     #conquadraticsq=conquadraticsq(object[,,(model@variables==x)],p),
                     #conquadraticskewedsq=conquadraticskewedsq(object[,,(model@variables==x)],p)
#                     stop("This variable does not exist for NicheModel !")
#              )
#            }
#            )
#            Y=prod(stack(Y))
#          }
#)


######### CREER TRANSITION MATRIX ###############################################################################
#setMethod(f="runsocioecoGeoDataModel",
#          signature=c("landscape","socioecoGeoDataModel"),
#          definition=function(object,model){
#            R<-buildRKLandscape(object,model["R"])
#            K<-buildRKLandscape(object,model["K"])
#            migrationMat<-migrationMatrix(object,model["migration"])
#            b<-getTransitionBackward(K=values(K),R=values(R),mig=migrationMat)      
#            f<-transitionMatrixForward(K=values(K),R=values(R),mig=migrationMat,meth = "non_overlap") # creates the forward transition matrix between cells
#            envDynLandscape(K=K,R=R,migration=migrationMat,transForMat = f,transBackMat = b)
#          }
#)

#setMethod(f="runsocioecoGeoDataModel",
#          signature=c("enDynLandscape"),
#          definition=function(object){
#            R<-buildRKLandscape(object["R"])
#            K<-runNich
#            m<-migrationMatrix(object,model["m"])
#            f <-getTransitionBackward(K=K,R=R,mig=m)      
#            b <-transitionMatrixForward(K=K,R=R,mig=m,meth = "non_overlap") # creates the forward transition matrix between cells
#            envDynLandscape(K=K,R=R,migration=m,transForMat = f,transBackMat = b)
#          }
#)


#setMethod(
#  f="migrationMatrix",
#  signature=c("landscape","migrationModel"),
#  definition=function(object,model)
#  {
#    Ndim = 1+all(ncell(object)!=dim(object)[1:2])
#    #if model["shapeMig"]=="contiguous" matrix()
#    migration = apply(object["distanceMatrix"], c(1,2),
#                      function(x)(switch(model["shapeMig"],
#                                         fat_tail1 = 1/(1+x^model["pMig"][2]/model["pMig"][1]),
#                                         gaussian = (dnorm(x, mean = 0, sd = model["pMig"][1], log = FALSE)),
#                                         exponential = (dexp(x, rate = 1/model["pMig"][1], log = FALSE)),
#                                         contiguous = (x==0)*(1-model["pMig"][1])+((x>0)-(x>1.4*res(object)[1]))*(model["pMig"][1]/(2*Ndim)),
#                                         contiguous8 = (x==0)*(1-object@migModel@pMig[[i]][1])+((x>0)-(x>2*res(object)[1]))*(model["pMig"][1]/(4*Ndim)),
#                                         island = (x==0)*(1-model["pMig"][1])+(x>0)*(model["pMig"][1]),
#                                         fat_tail2 = x^object@migModel@pMig[[i]][2]*exp(-2*x/(model["pMig"][1]^0.5)),
#                                         contiguous_long_dist_mixt = model["pMig"]["plongdist"]/nCellA(object)+(x==0)*(1-model["pMig"]["pcontiguous"]-model["pMig"]["plongdist"])+((x>0)-(x>1.4*res(object)[1]))*(model["pMig"]["pcontiguous"]/2),
#                                         gaussian_long_dist_mixt = object@migModel@pMig[[i]][2]/nCellA(object) + (dnorm(x, mean = 0, sd = model["pMig"][1], log = FALSE))
#                      )))
#    return(migration)
#  }
#)
#setMethod(f="getTransitionBackward",
#          signature=c("numeric","numeric","matrix"),
#          definition=function(R,K,mig){
#            if ((length(R)==1)&(length(K)==1)){transition = R * K * t(mig)}
#            if ((length(R)>1)&(length(K)==1)){transition = t(matrix(R,nrow=length(R),ncol=length(R))) * K * t(mig)}
#            if ((length(R)==1)&(length(K)>1)){transition = R * t(matrix(K,nrow=length(K),ncol=length(K))) * t(mig)}
#            if ((length(R)>1)&(length(K)==1)){transition = t(matrix(R,nrow=length(R),ncol=length(R))) * lpar$K * t(mig)}
#            if ((length(R)>1)&(length(K)>1)) {transition = t(matrix(R,nrow=length(R),ncol=length(R))) * t(matrix(K,nrow=length(K),ncol=length(K))) * t(mig)}
#            t<-transition/t(sapply(rowSums(transition),function(x)rep(x,ncol(transition))))
#            TransitionBackward(t)
#          }
#)


#setMethod(
#  f="transitionMatrixForward",
#  signature=c("numeric","numeric","matrix","character"),
#  definition=function(R,K,mig,meth)
#  {
#    rs = matrix(R,nrow=length(R),ncol=length(R))
#    Ku = t(matrix(K,nrow=length(K),ncol=length(K)))
#    leave = mig*(1+rs)*t(Ku); leave = leave - diag(leave)
#    tMF<-switch (meth,
#            non_overlap = mig * rs * Ku / colSums(rs * t(Ku) * mig),
#            overlap = mig * (1+rs) * Ku / (colSums((1+rs) * t(Ku) * mig - t(leave))),
#            stop("error in creation of transitionMatrixForward : the method does not exist !")
#    )
#    new(Class = "TransitionForward",tMF)
#  }
#)


#setMethod(f="sampleLandscape",
#          signature = c("Demographic","numeric","data.frame","character"),
          # if length(sampleCells)==1 a sample of length equals to sampleCells is taken at random 
          # from the landscape cells with probability proportional to the cells's K value
          # if length(sampleCells)>1, sampleCells contains the cell number of the sample to at to 
          # the demographic object to create
          # creates the environmental dynamic model with the landscape and parameters
          # creates the backward transition matrix between cells to run the coalescent
          # creates the environmental dynamic model with the landscape and parameters
#          definition = function(demographic, sampleSize,xy=NULL, option="randomfromK"){
#            if (!(option%in%c("randomefromK","fromCoords","K"))) stop("wrong option used in sampleLandscape funcion call")
#            if (option=="fromCoords") if (any(colnames(xy)!=c("x","y"))) stop("colnames of xy should include 'x' and 'y', for longitude and latitude)")
#            tmp <- switch (option,
#                           fromCoords = {
#                             sampleCells=as.vector(cellFromXY(landscape,yx[,c("x","y")]))
#                             names(sampleCells)=1:length(sampleCells)
#                             sampleCells
#                           },
#                           randomfromK = {
#                             sampleNo=as.vector(rmultinom(1,sampleSize,demographic$K/sum(landscapePopSize)))
#                             sampleCells=rep(1:length(sampleNo),sampleNo) # it creates a vector of the cells number where the sample occur
#                             names(sampleCells)=1:length(sampleCells)
#                             sampleCells
#                           },
#                           K ={
#                             sampleCells=rep(1:length(sampleNo),landscape$K) # it creates a vector of the cells number where the sample occur
#                             names(sampleCells)=1:length(sampleCells)
#                             sampleCells
#                           }
#            )
#          }
#)


#setMethod(f="demographic",
#          signature=c("envDynLandscape","integer"),
#          definition=function(envDynLand,sampleCells){
            #lpar<-runsocioecoGeoDataModel(object,model)             # creates the environmental dynamic model with the landscape and parameters
            #b<-getTransitionBackward(object,lpar)      
            #f<-transitionMatrixForward(lpar,"non_overlap") # creates the forward transition matrix between cells
            #sample according to socioecoGeoDataModel@K 
#            new(Class = "Demographic",object=envDynLand,sampleCells=sampleCells)
#          }
#)

#setMethod(
#  f = "nCellA",
#  signature = "Demographic",
#  definition = function(object){
#    nCellA(object[[1]])
#  }
#)



############################################
#setMethod(
#  f = "valuesA",
#  signature = "rasterstack",
#  definition = function(object){
#    x=na.omit(values(object))
#    colnames(x)=names(x)
#    rownames(x) <- Acells(object)
#    x
#  }
#)



 
## removed > Trash
#setValidity("envDynHistory",validityEnvDynHistory)

#setMethod("show",
#          "envDynHistory",
#          function(object){
#            cat("An object of class \"envDynHistory\":\nenvData ordered from most recent to most remote in the past\n\n", sep="")
#            cat("ending date \t:", object@endingDate,"\n")
#            cat("time unit\t:",object@timeUnit,"\n")
#            for (i in 1:length(object)) {
#              cat("\n",i,") Period -",i,":\n\n",sep="")           
#              cat("starting in time units at\t: ",object@pastStartingTimes[i],"\n\n",sep="")
#              show(object[[i]])
#            }
#          }
#)

#new("envDynHistory")
