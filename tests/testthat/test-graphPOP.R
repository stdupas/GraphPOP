test_that("NAcells works", {
  test_obj<-geoEnvData(raster::stack(x=c(temp= raster::raster(matrix(c(5,3,NA,3,2,3,2,3,5),nrow=3),xmn=0,xmx=3,ymn=0,ymx=3,crs=crs("+proj=longlat")),
                                         pops= raster::raster(matrix(rep(1:3,3),nrow=3),xmn=0,xmx=3,ymn=0,ymx=3,crs=crs("+proj=longlat")))),
                       layerConnectionTypes=c("geographic","grouping"))
  expect_equal(NAcells(test_obj), 7)
})

test_that("Acells works", {
  test_obj<-geoEnvData(raster::stack(x=c(temp= raster::raster(matrix(c(5,3,NA,3,2,3,2,3,5),nrow=3),xmn=0,xmx=3,ymn=0,ymx=3,crs=crs("+proj=longlat")),
                                         pops= raster::raster(matrix(rep(1:3,3),nrow=3),xmn=0,xmx=3,ymn=0,ymx=3,crs=crs("+proj=longlat")))),
                       layerConnectionTypes=c("geographic","grouping"))
  expect_equal(Acells(test_obj), (1:9)[-7])
})