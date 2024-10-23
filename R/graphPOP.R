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

new("geoEnvData",stack(x=c(temp=raster(matrix(c(5,3,2,3,2,3,2,3,5),nrow=3),xmn=0,xmx=3,ymn=0,ymx=3,crs=crs("+proj=longlat")),pops=raster(matrix(rep(1:3,3),nrow=3),xmn=0,xmx=3,ymn=0,ymx=3))),layerConnectionTypes=c("geographic","grouping"))

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
#' @inherit raster::RasterStack description
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

a=new("socioecoGroupData",stack(raster(matrix(1:4,nrow=1))),categories=c("susa","bogota","villapinzon","lacalera"))

socioecoGroupData<-function(categories=c("group1","group2"),Values=c(1:4), Nlayers= 2, layerNames=c("cuidado","alimentación"),Array=NULL,rasterStack=NULL){
  if (is.null(rasterStack)) {
    if (is.null(Array)) Array = array(Values,dim = c(1,length(categories),Nlayers),dimnames = list(1:1,categories,layerNames))
    rasterStack = stack(apply(Array,3,function(x){raster(x)}))
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

#' socioecoGeoData builder function
#' 
#' @description
#' Function to construct a socioecoGeoData object.
#' 
#' @param x Either a geoEnvData or a rasterStack object containing the geographical information.
#' @param socioecoList socioecoGroupsData object containing the groups and connection types.
#' @param stackConnectionType character. The name of the stack connection type. Default value `geographic`.
#' @returns socioecoGeoData object.
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
          {Stack=stack(sapply(1:dim(x)[3],function(i) raster(x[,,i])),layers=envLayerNames)
          names(Stack)=envLayerNames
          extent(Stack)=Extent
          crs(Stack) <- Crs
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

socioecoGeoData(x = geoEnvData(),socioecoList=socioecoGroupsData())

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
  
  new("samplePoints", geoCoordinates = sampleCoords,sampleCells = NULL, sampleTimes = sTimes)
}

#' Class that reunites the present and past socioecoData.
#' 
#' @description
#' Class to represent environmental dynamic data history. It inherits from socioecoData as the present socioecogeodata. Includes a past socioecogeodata list with parsing times.
#' The last past socioecogeodata in the list goes from the last parsing time to minus infinite.
#' @slot .Data socioecoGeoData object that contains the present data.
#' @slot pastSocioecoGeoData List of socioecoGeoData representing the past socio-ecological and geographical data.
#' @slot parsingTimes Numeric. List of the parsing times.
#' @slot timeUnit Character. Specifies the time unit.
#' @slot zeroTime POSIXlt. The present time.
#' @export

setClass("socioecoGeoDataHistoryAndSample",
         contains="socioecoGeoData",
         representation(pastSocioecoGeoData="list",parsingTimes="numeric",timeUnit="character",zeroTime="POSIXlt"),
         prototype(new("socioecoGeoData"),pastSocioecoGeoData=list(new("socioecoGeoData"),new("socioecoGeoData"),new("socioecoGeoData")),parsingTimes=c(0,-200,-5000,-20000),timeUnit="days",zeroTime=as.POSIXlt('2005-4-19 7:01:00'))
)

validitysocioecoGeoDataHistory = function(object){
  if (any(object@parsingTimes>0)) stop("the pastStartingTimes should be negative")
  if ((length(object@parsingTimes)-1)!=(length(object@pastSocioecoGeoData))) stop("slot pastStartingTimes should include all the starting dates between period, which is length(object)")
  if (length(object@parsingTimes)>1) for (i in 2:length(object@parsingTimes)) {if (object@parsingTimes[i]>=object@parsingTimes[i-1]) stop("the socioecoGeoDataList should order from recent to past")}
}

setValidity("socioecoGeoDataHistoryAndSample", validitysocioecoGeoDataHistory)

#' Creates an object of the class socioecoGeoDataHistoryAndSample.
#' 
#' @description
#' Class to represent environmental dynamic data history. It inherits from socioecoData as the present socioecogeodata. Includes a past socioecogeodata list with parsing times.
#' The last past socioecogeodata in the list goes from the last parsing time to minus infinite.
#' @param socioecoGeoData socioecoGeoData object that contains the present data.
#' @param PastSocioecoGeoData List of socioecoGeoData representing the past socio-ecological and geographical data.
#' @param ParsingTimes Numeric. List of the parsing times.
#' @param TimeUnit Character. Specifies the time unit. Defaults to "days".
#' @param ZeroTime POSIXlt. The present time.
#' @returns An object of the socioecoGeoDataHistoryAndSample class.
#' @export

socioecoGeoDataHistoryAndSample <- function(SocioecoGeoData=socioecoGeoData(),PastSocioecoGeoData=list(socioecoGeoData(),socioecoGeoData(),socioecoGeoData()),ParsingTimes=c(0,-200,-500,-2000),TimeUnit="days",ZeroTime=as.POSIXlt('2005-4-19 7:01:00')) {
  new("socioecoGeoDataHistoryAndSample",SocioecoGeoData,pastSocioecoGeoData=PastSocioecoGeoData,parsingTimes=ParsingTimes,timeUnit=TimeUnit,zeroTime=ZeroTime)
}

#socioecoGeoDataHistory <- function(socioecoGeoData)

#' show method for socioecoGeoDataHistoryAndSample objects.
#' 
#' @name show
#' @docType methods
#' @rdname show-methods
#' @aliases show,socioecoGeoDataHistoryAndSample

setMethod("show",
          "socioecoGeoDataHistoryAndSample",
          function(object){
            cat("An object of class 'socioecoGeoDataHistoryAndSample':\n\n")
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
  if(!is(object@sampledPoints,"samplePoints"))stop("Error in socioecoGeoDataModel samplePoints: this must be a samplePoints object !")
  TRUE
}

#' Class to contain the socioecoGeoDataModel built on the historical information
#' 
#' @description
#' This class builds the migration models using the historical and present socioecological and geographical information. 
#' 
#' @slot .Data socioecoGeoDataHistoryAndSample object containing all the historical and present socioecological and geographical information.
#' @slot SocioecoGeoData socioecoGeoData object.
#' @slot Kmodel nicheModel object. Carrying capacity niche model.
#' @slot Rmodel nicheModel object. Reproductive potential niche model.
#' @slot geoMigModel geoMigrationModel object. Contains the geographic migration model information.
#' @slot socioecoMigModel socioecoMigrationModel object. Contains the socioecological migration model information.
#' @slot sampledPoints samplePoints object. Contains the coordinates of the samples, the sampling times and the cells where these samples are located.
#' @export

setClass("socioecoGeoDataModel",
         contains = "socioecoGeoDataHistoryAndSample",
         representation(SocioecoGeoData = "socioecoGeoData",Kmodel="nicheModel",Rmodel="nicheModel",geoMigModel="geoMigrationModel",socioecoMigModel="socioecoMigrationModel", sampledPoints = "samplePoints"),
         validity=validitysocioecoGeoDataModel,
         prototype(new("socioecoGeoDataHistoryAndSample"),Kmodel=new("nicheModel"),Rmodel=new("nicheModel"),geoMigModel=new("geoMigrationModel"),socioecoMigModel=new("socioecoMigrationModel"), sampledPoints = new("samplePoints"))
      
)

socioecoGeoDataModel<-function(socioecoGeoDataHistoryAndSample=NULL,
                               SocioecoGeoData=socioecoGeoData(),PastSocioecoGeoData=list(socioecoGeoData(),socioecoGeoData(),socioecoGeoData()),
                               ParsingTimes=c(0,-200,-500,-2000),TimeUnit="days",ZeroTime=as.POSIXlt('2005-4-19 7:01:00'),samplePoints = new("samplePoints"),
                               nicheK=NULL,nicheR=NULL,migModel=NULL,
                               EnvStack=stack(x=c(temp=raster(matrix(c(5,4,2,4,2,4,2,4,5),nrow=3),xmn=0,xmx=3,ymn=0,ymx=3,crs="+proj=longlat"),pops=raster(matrix(c(1,2,2,1,1,2,1,1,1),nrow=3),xmn=0,xmx=3,ymn=0,ymx=3))),
                               stackConnectionType=c("geographic","grouping"),envLayerNames=NULL,Extent=NULL,
                               varNicheK="temp",reactNormsK=c(temp="scaling"),pNicheK=list(scalingK=100),
                               varNicheR=c("temp","temp"),reactNormsR=c(temp="envelin",temp="scaling"),pNicheR=list(envelin=c(1,4),scalingR=10),
                               modelConnectionType=c("geographic","grouping"),varMig=c("temp","pops"),shapeMig=c("gaussian","popSep"),pMig=list(1.10574E5/1.96,numeric(0)),pMixt=c(.5,.5),
                               sampledPoints = NULL)
  
{
  if (is.null(socioecoGeoDataHistoryAndSample)) socioecoGeoDataHistoryAndSample=socioecoGeoDataHistoryAndSample(SocioecoGeoData,PastSocioecoGeoData,ParsingTimes,TimeUnit,ZeroTime)
  if (is.null(nicheK)) nicheK=nicheModel(varNiche = varNicheK,reactNorms = reactNormsK, pNiche = pNicheK)
  if (is.null(nicheR)) nicheR=nicheModel(varNiche = varNicheR,reactNorms = reactNormsR, pNiche = pNicheR)
  if (is.null(migModel)) migModel= geoMigrationModel(modelConnectionType = modelConnectionType,varMig = varMig,shapeMig = shapeMig,pMig = pMig,pMixt = pMixt)
  if (is.null(sampledPoints)) sampledPoints = new("samplePoints")
  
  sampledPoints@sampleCell <- setNames(raster::cellFromXY(socioecoGeoDataHistoryAndSample,sampledPoints@geoCoordinates),1:length(sampledPoints@geoCoordinates))
  
  new("socioecoGeoDataModel",socioecoGeoDataHistoryAndSample,Kmodel=nicheK,Rmodel=nicheR,geoMigModel=migModel, sampledPoints = sampledPoints)
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
            cat("Data (Inherited)\t\t: socioecoGeoDataHistoryAndSample\n")
            show(object@.Data)
            cat("\nslot\t\t: socioecoGeoData\n")
            show(object@SocioecoGeoData)
            cat("\nslot\t\t: pastsocioecoGeoData \n")
            show(object@pastSocioecoGeoData)
            cat("\nslot\t\t: sampledPoints\n")
            show(object@sampledPoints)
            cat("Sampled cells:\n")
            cat(object@sampledPoints@sampleCell)
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
                             values(distanceFromPoints(object@geoEnvData,x))
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

a=socioecoGeoDataModel(EnvStack = stack(x=c(temp=raster(matrix(c(5,4,2,4,2,4,2,4,5),nrow=3),xmn=0,xmx=3,ymn=0,ymx=3,crs="+proj=longlat"),pops=raster(matrix(c(1,2,2,1,1,2,1,1,1),nrow=3),xmn=0,xmx=3,ymn=0,ymx=3))),pMig=list(1.10574E5/1.96,numeric(0)),pMixt=c(.5,.5))
b=buildMigrationMatrix(a)

setClass("TransitionBackward",
         contains = "matrix",
         validity = function(object){
           if (all(nrow(object)==0))stop("The matrix is empty.")
           if (nrow(object)!=ncol(object))stop("The matrix is not square")
           if (!all(rowSums(object)>0.999999999) && !all(rowSums(object)<1.000000001)) {stop("The sum of probabilities in each row is not 1")}
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

setGeneric(
  name = "buildTransitionBackward",
  def=function(object){return(standardGeneric("buildTransitionBackward"))}
)


setMethod(f="buildTransitionBackward",
          signature=c("socioecoGeoDataModel"),
          definition=function(object){
            RKland <- buildRKlandscape(object)
            R <- values(RKland)[,1]
            K <- values(RKland)[,2]
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

setGeneric(
  name = "buildTransitionForward", 
  def = function(object,meth){return(standardGeneric("buildTransitionForward"))}
)

setMethod(
  f="buildTransitionForward",
  signature=c("socioecoGeoDataModel","character"),
  definition=function(object,meth)
  {
    RKland <- buildRKlandscape(object)
    R <- values(RKland)[,1]
    K <- values(RKland)[,2]
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

envDynSet<-function(socioecoGeoDataModel=NULL,RKlandscape=NULL,geoDist=NULL,migrationMatrix=NULL,transitionForward=NULL,transitionBackward=NULL,
                          envData=NULL,
                          EnvStack=stack(x=c(temp=raster(matrix(c(5,4,2,4,2,4,2,4,5),nrow=3),xmn=0,xmx=3,ymn=0,ymx=3,crs="+proj=longlat"),pops=raster(matrix(c(1,2,2,1,1,2,1,1,1),nrow=3),xmn=0,xmx=3,ymn=0,ymx=3))),
                          stackConnectionType=c("geographic","grouping"),envLayerNames=NULL,Extent=NULL,
                          varNicheK="temp",reactNormsK=c(temp="scaling"),pNicheK=list(scalingK=100),
                          varNicheR=c("temp","temp"),reactNormsR=c(temp="envelin",temp="scaling"),pNicheR=list(envelin=c(1,4),scalingR=10),
                          modelConnectionType=c("geographic","grouping"),varMig=c("temp","pops"),shapeMig=c("gaussian","popSep"),pMig=list(1.10574E5/1.96,numeric(0)),pMixt=c(.5,.5)){
  if (is.null(socioecoGeoDataModel)) {
    if (is.null(envData)) envData=new("geoEnvData")
    socioecoGeoDataModel=socioecoGeoDataModel(envData = envData,nicheK = nicheModel(varNiche = varNicheK,reactNorms = reactNormsK, pNiche = pNicheK),nicheR = nicheModel(varNiche = varNicheR,reactNorms = reactNormsR, pNiche = pNicheR), migModel = migrationModel(modelConnectionType = modelConnectionType,varMig = varMig,shapeMig = shapeMig,pMig = pMig,pMixt = pMixt))}
  if (is.null(RKlandscape)) RKlandscape=buildRKLandscape(socioecoGeoDataModel)
  if (is.null(geoDist)) geoDist=buildGeodist(socioecoGeoDataModel)
  if (is.null(migrationMatrix)) RKlandscape=buildMigrationMatrix(socioecoGeoDataModel)
  if (is.null(transitionForward)) transitionForward=buildTransitionForward(socioecoGeoDataModel)
  if (is.null(transitionBackward)) transitionBackward=buildTransitionBackward(socioecoGeoDataModel)
  new("envDynSet",socioecoGeoDataModel,RKlandscape=RKlandscape,migrationMatrix=migrationMatrix,transitionForwar=transitionForward,transitionBackward=transitionBackward)
}

setMethod(
  f ="[",
  signature = c(x="envDynSet" ,i="character",j="missing"),
  definition = function (x ,i ,j , drop ){
    switch ( EXPR =i,
             "K" ={return(values(x@RKlandscape$K))} ,
             "R" ={return(values(x@RKlandscape$R))} ,
             "TransiBackw" ={return(x@transitionBackward)},
             "TransiForw" = {return(x@transitionForward)},
             "Present" = {return(x@samplePoints@sampleCell[which(x@samplePoints@sampleTime == 0)])},
             "Past" = {return(x@samplePoints@sampleCell[which(x@samplePoints@sampleTime != 0)])},
             "Times" = return(x@samplePoints@sampleTime),
             stop("This slot doesn't exist!")
    )
  }
)

a<-new("envDynSet")

###################Coalescence simulation methods#########################

setGeneric(
  name = "simulCoal",
  def=function(envDynSet,printCoal){return(standardGeneric("simulCoal"))}
)

setMethod(
  f="simulCoal", ##Simulates a coalescent
  signature=c("envDynSet","logical"),
  definition=function(envDynSet,printCoal)
  {
    #Initialization of the objects that will store the information about the coalescent
    prob_forward=NA
    N <- round(envDynSet["K"]);#N[N==0]<-1
    coalescent = list() #
    cell_number_of_nodes <- parent_cell_number_of_nodes <- envDynSet["Present"]
    nodes_remaining_by_cell = list()
    nodes_remaining <- as.numeric(names(envDynSet@samplePoints@sampleCell))
    time=0
    single_coalescence_events=0
    single_and_multiple_coalescence_events=0
    
    #Adds the remaining nodes per cell to the nodes_remaining_by_cell list for the coalescent simulation
    for (cell in 1:nCellA(envDynSet)[1])
    {
      nodes_remaining_by_cell[[cell]] <- which(cell_number_of_nodes==cell)
    }
    
    #Main cycle for the coalescent simulation. It will run until there is only one remaining node, including the nodes samples in the past
    while (length(nodes_remaining)>1) 
    {
      #Adding the nodes sampled in the past if there is any
      if(time > 0 && any(abs(envDynSet["Times"]) == time)) 
      {
        past_nodes_remaining <- envDynSet@samplePoints@sampleCell[which(abs(envDynSet["Times"]) == time)]
        
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
        parent_cell_number_of_nodes[node] = sample(x=nCellA(envDynSet)[1],size=1,prob=c(envDynSet["TransiBackw"][cell_number_of_nodes[node],]))
      }
      
      #After the spatial transition backwards, the nodes newly positioned in the cells are accounted for in this cycle
      for (cell in 1:nCellA(envDynSet)[1])
      {
        nodes_remaining_by_cell[[cell]] <- as.numeric(names(which(parent_cell_number_of_nodes==cell)))
      }
      
      #Calculation of the Forward transition probability of each iteration
      prob_forward[time] = sum(log(envDynSet["TransiForw"][parent_cell_number_of_nodes,cell_number_of_nodes]))
      time=time+1;  if(printCoal==TRUE){if (round(time/10)*10==time) {print(time)}}
      
      #The coalescent events are calculated in this cycle accounting for the new cell position of each node
      for (cell in 1:nCellA(envDynSet)[1])
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
        if (coalescing %in% tips) {coalescent[[i]]$br_length <- append(coalescent[[i]]$br_length,(coalescent[[i]]$time - abs(envDynSet@samplePoints@sampleTime[coalescing])))
        } else {
          coalescent[[i]]$br_length <- append(coalescent[[i]]$br_length,coalescent[[i]]$time-times[which(internals==coalescing)])
        }
      }
    }
    #Return a list with the coalescent and the total probability forward
    list(coalescent=coalescent,prob_forward=sum(prob_forward))
  }
)

setGeneric(
  name = "simulMultiCoal",
  def=function(envDynSet,printCoal,iteration){return(standardGeneric("simulMultiCoal"))}
)

setMethod(
  f="simulMultiCoal",
  signature=c("envDynSet","logical","numeric"),
  definition=function(envDynSet,printCoal,iteration){
    lapply(1:iteration,function(x)simulCoal(envDynSet,printCoal))
  }
)

##,transFor="transitionMatrixForward",transBack="getTransitionBackward",environment="landscape"


######################### |          | ############################### 
######################### | To TRASH | ############################### 
######################### V          V ############################### 



#Demographic<-setClass("Demographic",
#                      contains = "envDynLandscape",
#                      slots = c(sampleCells="integer"),
#                      validity = function(object){
#                        if(any(object@K<0))stop("K is negative")
#                        if(any(object@R<0))stop("R is negative")
#                        if(any(object@sampleCells>nCellA(object)))stop("Sample cell number outside the range")
#                      }
#)# Demographic contains all the information to run a coalescent and calculate probabilities and graph statistics


############## METHODS #####

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
