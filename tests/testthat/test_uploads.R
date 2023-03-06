# test_uploads.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for GeoNodeManager.R / Uploads
#=======================
require(geonode4R, quietly = TRUE)
require(testthat)

context("GeoNodeManager-uploads")



test_that("GeoNodeManager uploads a resource and deletes it",{
  files = list.files(system.file("extdata/samples", package = "geonode4R"), pattern = "shapefile1", full.names = T)
  created = geonode$upload(files)
  expect_is(created, "list")
  expect_equal(created$status, "finished")
  expect_equal(created$crs$properties, "EPSG:4326")
  expect_true(created$success)
})