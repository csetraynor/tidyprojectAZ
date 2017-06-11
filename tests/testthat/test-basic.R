context("basic operation")

test_that("libraries loaded",{

  expect_true("package:tidyproject" %in% search())

})

