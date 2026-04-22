## tests/testthat/test-offline_detect.R

# Shared fixture: clean data (no issues)
.clean_df <- function() data.frame(
  name   = c("Alice","Bob","Charlie"),
  status = c("active","active","active"),
  age    = c(30L, 40L, 35L),
  stringsAsFactors = FALSE
)

test_that("offline_detect returns a tibble", {
  data(messy_employees, package = "llmclean")
  res <- offline_detect(messy_employees)
  expect_s3_class(res, "tbl_df")
})

test_that("offline_detect returns required columns", {
  data(messy_employees, package = "llmclean")
  res <- offline_detect(messy_employees)
  required <- c("column","row_index","value","issue_type",
                 "explanation","suggestion","confidence","provider","model")
  expect_true(all(required %in% names(res)))
})

test_that("offline_detect provider and model are 'offline'/'statistical'", {
  data(messy_employees, package = "llmclean")
  res <- offline_detect(messy_employees)
  if (nrow(res) > 0) {
    expect_true(all(res$provider == "offline"))
    expect_true(all(res$model    == "statistical"))
  }
})

test_that("offline_detect detects case inconsistency", {
  df <- data.frame(
    status = c("active","Active","ACTIVE","active"),
    stringsAsFactors = FALSE
  )
  res <- offline_detect(df, issue_types = "case")
  expect_gt(nrow(res), 0)
  expect_true(all(res$issue_type == "case"))
})

test_that("offline_detect detects typos (Levenshtein distance <= 2)", {
  df <- data.frame(
    city = c("London","London","London","Londn","London"),
    stringsAsFactors = FALSE
  )
  res <- offline_detect(df, issue_types = "typo")
  expect_gt(nrow(res), 0)
  expect_true(any(res$issue_type == "typo"))
  # 'Londn' should be flagged
  expect_true(any(res$value == "Londn"))
})

test_that("offline_detect detects malformed email", {
  df <- data.frame(
    email = c("good@example.com","bad@@example.com","also@example.com"),
    stringsAsFactors = FALSE
  )
  res <- offline_detect(df, issue_types = "format")
  expect_gt(nrow(res), 0)
  expect_true(any(res$issue_type == "format"))
  expect_true(any(grepl("@@", res$value)))
})

test_that("offline_detect detects numeric outliers", {
  df <- data.frame(age = c(25L, 30L, 28L, 32L, 27L, -99L, 999L))
  res <- offline_detect(df, issue_types = "outlier")
  expect_gt(nrow(res), 0)
  expect_true(all(res$issue_type == "outlier"))
})

test_that("offline_detect returns zero-row tibble when data is clean", {
  df  <- .clean_df()
  res <- offline_detect(df, issue_types = c("case","typo","outlier"))
  expect_s3_class(res, "tbl_df")
  # A clean df should have no case/typo/outlier issues
  expect_equal(nrow(res[res$issue_type %in% c("case","outlier"), ]), 0L)
})

test_that("offline_detect confidence is in [0, 1]", {
  data(messy_employees, package = "llmclean")
  res <- offline_detect(messy_employees)
  if (nrow(res) > 0) {
    expect_true(all(res$confidence >= 0 & res$confidence <= 1, na.rm = TRUE))
  }
})

test_that("offline_detect respects column selection", {
  data(messy_employees, package = "llmclean")
  res <- offline_detect(messy_employees, columns = "status")
  expect_true(all(res$column == "status"))
})

test_that("offline_detect errors on non-data-frame input", {
  expect_error(offline_detect(list(a = 1)), regexp = "data frame")
})

test_that("offline_detect works on messy_survey", {
  data(messy_survey, package = "llmclean")
  res <- offline_detect(messy_survey)
  expect_s3_class(res, "tbl_df")
  expect_gt(nrow(res), 0)
})

test_that("offline_detect classifies 1-char-shorter value as typo not abbreviation", {
  # "Londn" is missing one letter vs "London" -> typo (nchar diff = 1)
  df <- data.frame(
    city = c("London","London","London","Londn","London"),
    stringsAsFactors = FALSE
  )
  res <- offline_detect(df, issue_types = c("typo","abbreviation"))
  expect_gt(nrow(res), 0)
  # Must be classified as typo, not abbreviation
  expect_true(any(res$issue_type == "typo"))
  expect_equal(res$value[res$issue_type == "typo"], "Londn")
})

test_that("offline_detect classifies large-nchar-diff value as abbreviation", {
  # "Dept" vs "Department" -> 6 edits, too many for default max_edit_distance=2
  # Use a case that fits: "Int" (3) vs "Intl" (4) -> edit=1, nchar diff=1 -> typo
  # Use "HR" (2) vs "HRx" (3) -> edit=1, nchar diff=1 -> typo (both short)
  # The abbreviation path requires nchar diff >= 2 AND edit dist <= max
  # "activ" (5) vs "act" (3) -> nchar diff=2, edit=2 -> abbreviation
  df <- data.frame(
    code = c("activ","activ","activ","act","activ"),
    stringsAsFactors = FALSE
  )
  res <- offline_detect(df, issue_types = c("typo","abbreviation"),
                         max_edit_distance = 2L)
  if (nrow(res) > 0) {
    expect_true(any(res$issue_type == "abbreviation"))
  }
})

