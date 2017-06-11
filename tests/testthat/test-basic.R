context("basic operation")

test_that("libraries loaded",{

  expect_true("package:tidyproject" %in% search())

})

test_that("basic functionality",{

  if(!grepl("astrazeneca.net",Sys.info()["nodename"])) skip("not on AZ")

  test_proj_name <- "/projects/QCP_MODELING/ONC/azd6094/test"
  unlink(test_proj_name,recursive = TRUE,force = TRUE)
  expect_true(!file.exists(test_proj_name))
  expect_error(make_project(test_proj_name))
  unlink(test_proj_name,recursive = TRUE,force = TRUE)

  test_proj_name <- "~/tidyproject_package_test"
  unlink(test_proj_name,recursive = TRUE,force = TRUE)
  expect_true(!file.exists(test_proj_name))
  make_project(test_proj_name)
  expect_true(file.exists(test_proj_name))
  unlink(test_proj_name,recursive = TRUE,force = TRUE)

  test_proj_name <- "/projects/QCP_MODELING/ONC/azd6094"
  activities <- dir(test_proj_name,full.names = TRUE)
  for(activity in activities[grepl("tidyproject_package_test",activities)])
    unlink(activity,recursive = TRUE,force = TRUE)
  make_project(test_proj_name,type="poppk",name="tidyproject_package_test")
  activities <- dir(test_proj_name,full.names = TRUE)
  activity <- activities[grepl("poppk",activities) & grepl("tidyproject_package_test",activities)]
  expect_true(length(activity)>0)
  unlink(activity,recursive = TRUE,force = TRUE)

})
