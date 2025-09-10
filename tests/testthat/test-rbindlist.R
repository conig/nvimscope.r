test_that("nvimclip handles rbindlist class safety", {
  require(corx)
  cx <- corx(mtcars)
  expect_no_error(nvimclip(cx))
})
