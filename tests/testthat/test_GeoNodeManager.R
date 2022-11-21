# test_GeoNodeManager.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for GeoNodeManager.R
#=======================
require(geonode4R, quietly = TRUE)
require(testthat)

context("GeoNodeManager")

test_that("connect",{
  
  expect_is(geonode, "GeoNodeManager")
  
})