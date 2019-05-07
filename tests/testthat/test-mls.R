context("mls")

test_that("basic string", {
  s <- mls("A long line of text that is more than 40 characters and",
           "a second long line of text, also more than 40 characters")
  cmp <- join_newline(c(
    "A long line of text that is more than",
    "40 characters and a second long line of",
    "text, also more than 40 characters"))
  expect_equal(
    format(s, 40),
    cmp)
})


test_that("preserve newlines", {
  s <- mls("A short line", "", "Another short line")
  cmp <- join_newline(c(
    "A short line", "", "Another short line"))
  expect_equal(
    format(s, 40),
    cmp)
})


test_that("preserve embedded newlines", {
  s <- mls("A short line\n\nAnother short line")
  cmp <- join_newline(c(
    "A short line", "", "Another short line"))
  expect_equal(
    format(s, 40),
    cmp)
})


test_that("sensible message", {
  s <- mls("A long line of text that will break over more than 40 chars")
  expect_message(
    withr::with_options(list(width = 40), message(s)),
    "A long line of text that will break\nover more than 40 chars")
})


test_that("sensible error", {
  s <- mls("A long line of text that will break over more than 40 chars")
  expect_error(
    withr::with_options(list(width = 40), stop(s)),
    "A long line of text that will break\nover more than 40 chars")
})


test_that("print", {
  s <- mls("A long line of text that will break over more than 40 chars")
  ## Slightly convoluted test to ensure we detect the trailing newline
  file <- tempfile()
  on.exit(unlink(file))
  res <- capture.output(
    withr::with_options(list(width = 40), print(s)),
    file = file)
  txt <- readChar(file, file.size(file))
  expect_equal(
    txt,
    "A long line of text that will break\nover more than 40 chars\n")

})
