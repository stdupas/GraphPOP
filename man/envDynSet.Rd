% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graphPOP.R
\name{envDynSet}
\alias{envDynSet}
\title{Creates an envDynSet object.}
\usage{
envDynSet(
  socioecoGeoDataModel = NULL,
  RKlandscape = NULL,
  geoDist = NULL,
  migrationMatrix = NULL,
  transitionForward = NULL,
  transitionBackward = NULL,
  envData = NULL,
  EnvStack = raster::stack(x = c(temp = raster::raster(matrix(c(5, 4, 2, 4, 2, 4, 2, 4,
    5), nrow = 3), xmn = 0, xmx = 3, ymn = 0, ymx = 3, crs = "+proj=longlat"), pops =
    raster::raster(matrix(c(1, 2, 2, 1, 1, 2, 1, 1, 1), nrow = 3), xmn = 0, xmx = 3, ymn
    = 0, ymx = 3))),
  stackConnectionType = c("geographic", "grouping"),
  envLayerNames = NULL,
  Extent = NULL,
  varNicheK = "temp",
  reactNormsK = c(temp = "scaling"),
  pNicheK = list(scalingK = 100),
  varNicheR = c("temp", "temp"),
  reactNormsR = c(temp = "envelin", temp = "scaling"),
  pNicheR = list(envelin = c(1, 4), scalingR = 10),
  modelConnectionType = c("geographic", "grouping"),
  varMig = c("temp", "pops"),
  shapeMig = c("gaussian", "popSep"),
  pMig = list(110574/1.96, numeric(0)),
  pMixt = c(0.5, 0.5)
)
}
\arguments{
\item{socioecoGeoDataModel}{socioecoGeoDataModel object containing the socioecological and geographical information of the studied area.}

\item{RKlandscape}{RasterLayer object. This object will be calculated either from the data in the socioecoGeoDataModel or from the data provided directly to this function.}

\item{geoDist}{matrix. Geographical distance calculated from each cell to each other.}

\item{migrationMatrix}{matrix. Migration transition matrix calculated considering the socioecological and spatial variables.}

\item{transitionForward}{matrix. Calculated transition forward matrix that considers the socioecological and spatial variables.}

\item{transitionBackward}{matrix. Calculated transition backwards matrix that considers the socioecological and spatial variables.}

\item{envData}{envData object. If the socioecoGeoDataModel is not provided, the spatial data must be provided as an \link{envData} object.}

\item{EnvStack}{RasterStack object containing the environmental conditions of the studied area.}

\item{stackConnectionType}{Character. The type of connection in the different raster layers. Can be either geographic or grouping.}

\item{envLayerNames}{Character. Names of the different environmental layers of the raster.}

\item{varNicheK}{Character. Names of the variables considered for the K niche model.}

\item{reactNormsK}{Character. Reaction norms for the K niche model.}

\item{pNicheK}{Numeric list. Parameters for the K niche model calculation.}

\item{varNicheR}{Character. Names of the variables considered for the R niche model.}

\item{reactNormsR}{Character. Reaction norms for the R niche model.}

\item{pNicheR}{Numeric list. Parameters for the R niche model calculation}

\item{modelConnectionType}{Character. Type of connection of the migration model.}

\item{varMig}{Character. Variables involved in the calculation of the migration matrix. These are variables that should affect either the K or R values for a population.}

\item{pMig}{Numeric list. Parameters for the migration model.}

\item{pMixt}{Numeric. Parameters for the mixture model.}
}
\value{
envDynSet object.
}
\description{
This function creates an envDynSet object. It can either be created from a socioecoGeoDataModel object, or the information contained in that object separately.
}
