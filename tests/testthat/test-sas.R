test_that("roe_valid_sas_data_set_name() works", {
  expect_silent(roe_valid_sas_data_set_name("one"))
  expect_error(roe_valid_sas_data_set_name("123"))
})
