library(testthat)
library(geonode4R)

#test environment
geonodeUrl <- "https://stable.demo.geonode.org"
geonodeUsr <- Sys.getenv("GEONODE_USER")
geonodePwd <- Sys.getenv("GEONODE_PASSWORD")
geonodeLogger <- Sys.getenv("GEONODE_LOGGER")
geonode <- try(GeoNodeManager$new(geonodeUrl, geonodeUsr, geonodePwd, geonodeLogger))


if(is(geonode, "GeoNodeManager")){
  cat(sprintf("GeoNode test instance started at %s. Running integration tests...\n", gsUrl))
  test_check("geonode4R")
}else{
  cat("GeoNode test instance is not started. Skipping integration tests...\n")
}