test_that("nvimclip processes lists with simple types", {
  obj <- list(
    num_vec = 1:3,
    char_vec = c("apple", "banana", "apple"),
    factor_vec = factor(c("yes", "no", "yes")),
    logical_vec = c(TRUE, FALSE, TRUE, TRUE)
  )

  nvimscope.r::nvimclip(obj)
  output <- jsonlite::fromJSON("/tmp/nvim-rmdclip/menu.json")$contents

  numeric_entry <- output[output$name == "num_vec", "contents", drop = TRUE]
  character_entry <- output[output$name == "char_vec", "contents", drop = TRUE]
  factor_entry <- output[output$name == "factor_vec", "contents", drop = TRUE]
  logical_entry <- output[output$name == "logical_vec", "contents", drop = TRUE]

  expect_match(numeric_entry, "Mean:")
  expect_match(numeric_entry, "<integer>")

  expect_match(character_entry, "<character>")
  expect_match(character_entry, "Most common values:")

  expect_match(factor_entry, "<factor>")
  expect_match(factor_entry, "Most common values:")

  expect_match(logical_entry, "<logical>")
  expect_match(logical_entry, "Most common values:")
})
