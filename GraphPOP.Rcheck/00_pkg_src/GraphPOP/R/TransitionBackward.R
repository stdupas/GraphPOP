source('~/Documents/STAGE_article/GraphPOP/GraphPOP/GraphPOP/R/Landscape.R', echo=TRUE)
source('~/Documents/STAGE_article/GraphPOP/GraphPOP/GraphPOP/R/genericTransitionBackward.R', echo=TRUE)
library(ape)
library(stringr)
library(markovchain)
library(matrixcalc)
library(MASS)
############## CLASS AND VALIDITY ####

setClass("TransitionForward",
         contains = "matrix",
         validity = function(object){
                        if (all(nrow(object)==0))stop("The matrix is empty.")
                        if (nrow(object)!=ncol(object))stop("The matrix is not square")
                      }
)

Demographic<-setClass("Demographic",
                      contains = "Landscape",
                      slots = c(K="numeric", R="numeric",TransiBackw="TransitionBackward",TransiForw="TransitionForward"),
                      validity = function(object){
                        if(any(object@K<0))stop("K is negative")
                        if(any(object@R<0))stop("R is negative")
                      }
)

############## METHODS #####

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

setMethod(f="transitionMatrixBackward",
          signature=c("Landscape","list"),
          definition=function(object,model){
            if ((length(model$R)==1)&(length(model$K)==1)){transition = model$R * model$K * t(model$migration)}
            if ((length(model$R)>1)&(length(model$K)==1)){transition = t(matrix(model$R,nrow=length(model$R),ncol=length(model$R))) * model$K * t(model$migration)}
            if ((length(model$R)==1)&(length(model$K)>1)){transition = model$R * t(matrix(model$K,nrow=length(model$K),ncol=length(model$K))) * t(model$migration)}
            if ((length(model$R)>1)&(length(model$K)==1)){transition = t(matrix(model$R,nrow=length(model$R),ncol=length(model$R))) * lpar$K * t(model$migration)}
            if ((length(model$R)>1)&(length(model$K)>1)) {transition = t(matrix(model$R,nrow=length(model$R),ncol=length(model$R))) * t(matrix(model$K,nrow=length(model$K),ncol=length(model$K))) * t(model$migration)}
            t<-transition/t(sapply(rowSums(transition),function(x)rep(x,ncol(transition))))
            TransitionBackward(t)

          }
)


setMethod(
  f="transitionMatrixForward",
  signature=c("list","character"),
  definition=function(param, meth)
  {
    rs = matrix(param$R,nrow=length(param$R),ncol=length(param$R))
    Ku = t(matrix(param$K,nrow=length(param$K),ncol=length(param$K)))
    leave = param$migration*(1+rs)*t(Ku); leave = leave - diag(leave)
    tMF<-switch (meth,
            non_overlap = param$migration * rs * Ku / colSums(rs * t(Ku) * param$migration),
            overlap = param$migration * (1+rs) * Ku / (colSums((1+rs) * t(Ku) * param$migration - t(leave))),
            stop("error in creation of transitionMatrixForward : the method does not exist !")
    )
    new(Class = "TransitionForward",tMF)
  }
)

setMethod(f="createDemographic",
          signature=c("Landscape","EnvDinModel"),
          definition=function(object,model){
            lpar<-runEnvDinModel(object,model)
            b<-transitionMatrixBackward(object,lpar)
            f<-transitionMatrixForward(lpar,"non_overlap")
            new(Class = "Demographic",object,K=lpar$K,R=lpar$R,TransiBackw=b,TransiForw=f)
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
  f="simul_coalescent",
  signature=c("Demographic","logical"),
  definition=function(demographic,printCoal)    # avec K,cell number ou nodes
  {
    prob_forward=NA
    N <- round(demographic["K"]);N[N==0]<-1
    coalescent = list()
    nodes = as.numeric(rownames(xyFromCellA(demographic)));names(nodes)=as.character(nodes)
    cell_number_of_nodes <- as.numeric(rownames(xyFromCellA(demographic)))             #point d'ou part la coalescent <- vecteurc numeric dont les valeur sont dans les cellules attribué
    names(cell_number_of_nodes) <- nodes
    parent_cell_number_of_nodes <- cell_number_of_nodes
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
  f="simul_coal_200",
  signature=c("Demographic","logical"),
  definition=function(demographic,printCoal){
    lapply(1:200,function(x)simul_coalescent(demographic,printCoal))
  }
)

setMethod(
  f="compare",
  signature=c("Demographic","Landscape","logical"),
  definition=function(demographic,popSize,printCoal){
    coalescent<-simul_coal_200(demo1,printCoal)
    lcoal<-lapply(1:200,function(n){
      coal_2<-coalescent_2_newick(coalescent[[n]][[1]])
      cat(coal_2, file = "ex.tre", sep = "\n")
      tree<-read.tree("ex.tre")
      cophenetic(tree)
    })
    a<-matrix(data = apply(sapply(lcoal,as.vector),1,mean),nrow = nrow(lcoal[[1]]),ncol = ncol(lcoal[[1]]))
    b<-linearizedFstDigraph(demographic["TransiBackw"],popSize)
    c<-linearizedFstUndigraph(demographic["TransiBackw"],popSize)
    d<-apply(popSize["distanceMatrix"],c(1,2),log)
    mat<-list(a,b,c,d)
    par(mfrow=c(2,2))
    for(i in 1:4){
      plot(bionj(mat[[i]]),main=title(switch(EXPR=as.character(i),
                                        "1"="Simul_coalescent_X200",
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
  signature = "Landscape",
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
