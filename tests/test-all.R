library(testthat)
library(geonode4R)

#test environment
geonodeUrl <- Sys.getenv("GEONODE_URL")
if(geonodeUrl != "") geonodeUrl = "https://development.demo.geonode.org"
geonodeUsr <- Sys.getenv("GEONODE_USER")
geonodePwd <- Sys.getenv("GEONODE_PASSWORD")
geonodeLogger <- Sys.getenv("GEONODE_LOGGER")
GEONODE <- try(GeoNodeManager$new(geonodeUrl, geonodeUsr, geonodePwd, geonodeLogger))


if(is(GEONODE, "GeoNodeManager")){
  cat(sprintf("GeoNode test instance started at %s. Running integration tests...\n", geonodeUrl))
  test_check("geonode4R")
}else{
  cat("GeoNode test instance is not started. Skipping integration tests...\n")
}
