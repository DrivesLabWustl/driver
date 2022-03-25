test_that("roe_valid_sas_data_set_name() works", {
  expect_silent(valid_sas_data_set_name("one"))
  expect_error(valid_sas_data_set_name("123"))
})
