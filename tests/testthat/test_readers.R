test_file_location <- system.file("pageviews_dataset.tsv.gz", package = "pageviews")
sampled_log <- read_sampled_log(test_file_location)
context("Test file reading")

test_that("The example file has the right number of rows", {
  expect_that(nrow(sampled_log), equals(4001))
})

test_that("The example file has the right number of columns", {
  expect_that(ncol(sampled_log), equals(16))
})