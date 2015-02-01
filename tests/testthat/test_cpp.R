context("Test C++")
test_that("IP normalisation recognises NULL XFFs", {
  expect_that(normalise_ips("192.168.0.1","-"), equals("192.168.0.1"))
})

test_that("IP normalisation recognises non-NULL XFFs", {
  expect_that(normalise_ips("192.168.0.1","10.0.0.1"), equals("10.0.0.1"))
})

test_that("IP normalisation recognises multiple-IP XFFs", {
  expect_that(normalise_ips("192.168.0.1","12.7.0.1, 10.0.0.1"), equals("12.7.0.1"))
})

test_that("IP normalisation recognises multiple-IP XFFs", {
  expect_that(normalise_ips("192.168.0.1","Unknown, 10.0.0.1"), equals("10.0.0.1"))
})