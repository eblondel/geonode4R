# test_GeoNodeManager.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for GeoNodeManager.R
#=======================
require(geonode4R, quietly = TRUE)
require(testthat)

context("GeoNodeManager")

test_that("connect",{
  expect_is(GEONODE, "GeoNodeManager")
  expect_is(GEONODE$version, "GeoNodeVersion")
  expect_equal(GEONODE$version$version, "4.2.0.dev0")
})