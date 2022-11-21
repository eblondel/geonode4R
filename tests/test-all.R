library(testthat)
library(geonode4R)

#test environment
geonodeUrl <- "https://master.demo.geonode.org"
geonodeUsr <- "user"
geonodePwd <- "password"
geonodeLogger <- "INFO"
geonode <- try(GeoNodeManager$new(geonodeUrl, geonodeUsr, geonodePwd, geonodeLogger))


if(is(gsman, "GeoNodeManager")){
  cat(sprintf("GeoNode test instance started at %s. Running integration tests...\n", gsUrl))
  test_check("geonode4R")
}else{
  cat("GeoNode test instance is not started. Skipping integration tests...\n")
}