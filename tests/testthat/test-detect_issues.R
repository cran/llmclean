## tests/testthat/test-detect_issues.R
## Note: All tests run in offline mode (no API key required).
## LLM-provider tests are in \dontrun{} blocks in examples only.

test_that("detect_issues in offline mode returns tibble", {
  set_llm_provider("offline", verbose = FALSE)
  data(messy_employees, package = "llmclean")
  res <- detect_issues(messy_employees, verbose = FALSE)
  expect_s3_class(res, "tbl_df")
})

test_that("detect_issues in offline mode returns same as offline_detect", {
  set_llm_provider("offline", verbose = FALSE)
  data(messy_employees, package = "llmclean")
  di <- detect_issues(messy_employees, verbose = FALSE)
  od <- offline_detect(messy_employees)
  expect_equal(nrow(di), nrow(od))
})

test_that("detect_issues has required column structure", {
  set_llm_provider("offline", verbose = FALSE)
  data(messy_employees, package = "llmclean")
  res <- detect_issues(messy_employees, verbose = FALSE)
  required <- c("column","row_index","value","issue_type",
                 "explanation","suggestion","confidence")
  expect_true(all(required %in% names(res)))
})

test_that("detect_issues respects columns argument", {
  set_llm_provider("offline", verbose = FALSE)
  data(messy_employees, package = "llmclean")
  res <- detect_issues(messy_employees, columns = "status", verbose = FALSE)
  expect_true(all(res$column == "status"))
})

test_that("detect_issues respects sample_n", {
  set_llm_provider("offline", verbose = FALSE)
  data(messy_employees, package = "llmclean")
  # sample_n = 5 still works (offline doesn't use sample_n but should not error)
  res <- detect_issues(messy_employees, sample_n = 5L, verbose = FALSE)
  expect_s3_class(res, "tbl_df")
})

test_that("detect_issues errors on non-data-frame", {
  set_llm_provider("offline", verbose = FALSE)
  expect_error(detect_issues("not a df"), regexp = "data frame")
})

test_that("detect_issues errors on zero-row data frame", {
  set_llm_provider("offline", verbose = FALSE)
  expect_error(detect_issues(data.frame()), regexp = "zero rows")
})

test_that("detect_issues works on messy_survey", {
  set_llm_provider("offline", verbose = FALSE)
  data(messy_survey, package = "llmclean")
  res <- detect_issues(messy_survey, verbose = FALSE)
  expect_s3_class(res, "tbl_df")
  expect_gt(nrow(res), 0)
})
