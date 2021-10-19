test_that("roe_assert_valid_sas_data_set_name() works", {
  expect_silent(roe_assert_valid_sas_data_set_name("one"))
  expect_error(roe_assert_valid_sas_data_set_name("123"))
})
