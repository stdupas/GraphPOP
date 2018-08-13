setGeneric(
  name = "transitionMatrixBackward",
  def=function(object,model){return(standardGeneric("transitionMatrixBackward"))}
)

setGeneric(
  name = "transitionMatrixForward",
  def=function(param, meth){return(standardGeneric("transitionMatrixForward"))}
)


setGeneric(
  name = "createDemographic",
  def=function(object,model){return(standardGeneric("createDemographic"))}
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
  name = "simul_coal_200",
  def=function(demographic,printCoal){return(standardGeneric("simul_coal_200"))}
)

setGeneric(
  name = "compare",
  def=function(demographic,popSize,printCoal){return(standardGeneric("compare"))}
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









