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
  created = GEONODE$upload(files)
  expect_is(created, "list")
  expect_equal(created$status, "finished")
  expect_equal(created$crs$properties, "EPSG:4326")
  expect_true(created$success)

  if(created$success){
    #getResource
    resource = GEONODE$getResource(created$dataset)
    expect_is(resource, "list")
    expect_true(length(names(resource))>1)
    #getDataset
    dataset = GEONODE$getDataset(created$dataset)
    expect_is(dataset, "list")
    #uploadMetadata
    #md_file = system.file("extdata/samples", "metadata1.xml", package = "geonode4R")
    #req = GEONODE$uploadMetadata(created$dataset, file = md_file)
    #deleteResource
    deleted = GEONODE$deleteResource(created$dataset)
    expect_true(deleted)
  }
})