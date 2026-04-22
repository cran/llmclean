## tests/testthat/test-apply_report.R

# Shared fixture
.make_issues <- function() {
  set_llm_provider("offline", verbose = FALSE)
  data(messy_employees, package = "llmclean")
  offline_detect(messy_employees)
}

# ── suggest_fixes ─────────────────────────────────────────────────────────────

test_that("suggest_fixes returns data frame with extra columns", {
  data(messy_employees, package = "llmclean")
  issues  <- .make_issues()
  enriched <- suggest_fixes(messy_employees, issues, verbose = FALSE)
  expect_s3_class(enriched, "data.frame")
  expect_true("alternatives" %in% names(enriched))
  expect_true("confidence_revised" %in% names(enriched))
})

test_that("suggest_fixes returns same nrow as input issues", {
  data(messy_employees, package = "llmclean")
  issues  <- .make_issues()
  enriched <- suggest_fixes(messy_employees, issues, verbose = FALSE)
  expect_equal(nrow(enriched), nrow(issues))
})

test_that("suggest_fixes with empty issues returns empty", {
  data(messy_employees, package = "llmclean")
  empty <- offline_detect(messy_employees[1, ], issue_types = "outlier")
  # May or may not have issues; test only that result is data frame
  enriched <- suggest_fixes(messy_employees, empty, verbose = FALSE)
  expect_s3_class(enriched, "data.frame")
})

# ── apply_fixes ───────────────────────────────────────────────────────────────

test_that("apply_fixes returns data frame with same dimensions", {
  data(messy_employees, package = "llmclean")
  issues <- .make_issues()
  clean  <- apply_fixes(messy_employees, issues,
                         confirm = FALSE, min_confidence = 0.85)
  expect_equal(nrow(clean), nrow(messy_employees))
  expect_equal(ncol(clean), ncol(messy_employees))
})

test_that("apply_fixes does not modify original df", {
  data(messy_employees, package = "llmclean")
  original <- messy_employees
  issues   <- .make_issues()
  apply_fixes(messy_employees, issues, confirm = FALSE, min_confidence = 0.85)
  expect_equal(messy_employees, original)
})

test_that("apply_fixes high min_confidence applies fewer fixes", {
  data(messy_employees, package = "llmclean")
  issues  <- .make_issues()
  clean99 <- apply_fixes(messy_employees, issues,
                          confirm = FALSE, min_confidence = 0.99)
  clean70 <- apply_fixes(messy_employees, issues,
                          confirm = FALSE, min_confidence = 0.70)
  n99 <- attr(clean99, "_applied")$n_applied %||% 0
  n70 <- attr(clean70, "_applied")$n_applied %||% 0
  expect_lte(n99, n70)
})

test_that("apply_fixes dry_run returns tibble without modifying data", {
  data(messy_employees, package = "llmclean")
  issues   <- .make_issues()
  plan     <- apply_fixes(messy_employees, issues, dry_run = TRUE)
  expect_s3_class(plan, "data.frame")
  # dry run result has current_value column
  expect_true("current_value" %in% names(plan))
})

test_that("apply_fixes never applies REVIEW suggestions", {
  df <- data.frame(x = c("a","b","c"), stringsAsFactors = FALSE)
  fake_issues <- dplyr::tibble(
    column      = "x",
    row_index   = 1L,
    value       = "a",
    issue_type  = "typo",
    explanation = "test",
    suggestion  = "REVIEW",
    confidence  = 0.95,
    provider    = "offline",
    model       = "statistical"
  )
  clean <- apply_fixes(df, fake_issues, confirm = FALSE, min_confidence = 0.5)
  expect_equal(clean$x[1], "a")  # unchanged
})

test_that("apply_fixes errors on non-data-frame inputs", {
  expect_error(apply_fixes(list(), data.frame()), regexp = "data frame")
  expect_error(apply_fixes(data.frame(), list()), regexp = "data frame")
})

# ── llmclean_report ───────────────────────────────────────────────────────────

test_that("llmclean_report returns named list with three elements", {
  data(messy_employees, package = "llmclean")
  issues <- .make_issues()
  clean  <- apply_fixes(messy_employees, issues,
                         confirm = FALSE, min_confidence = 0.85)
  rpt <- llmclean_report(messy_employees, clean, issues, print = FALSE)
  expect_type(rpt, "list")
  expect_named(rpt, c("summary","changes","metadata"))
})

test_that("llmclean_report summary has required columns", {
  data(messy_employees, package = "llmclean")
  issues <- .make_issues()
  clean  <- apply_fixes(messy_employees, issues,
                         confirm = FALSE, min_confidence = 0.85)
  rpt <- llmclean_report(messy_employees, clean, issues, print = FALSE)
  expect_true(all(c("column","issue_type","n_detected") %in% names(rpt$summary)))
})

test_that("llmclean_report metadata has expected fields", {
  data(messy_employees, package = "llmclean")
  issues <- .make_issues()
  clean  <- apply_fixes(messy_employees, issues,
                         confirm = FALSE, min_confidence = 0.85)
  rpt <- llmclean_report(messy_employees, clean, issues, print = FALSE)
  expect_true(all(c("provider","model","n_total","n_applied") %in%
                    names(rpt$metadata)))
})

test_that("llmclean_report changes has original and corrected columns", {
  data(messy_employees, package = "llmclean")
  issues <- .make_issues()
  clean  <- apply_fixes(messy_employees, issues,
                         confirm = FALSE, min_confidence = 0.85)
  rpt <- llmclean_report(messy_employees, clean, issues, print = FALSE)
  if (nrow(rpt$changes) > 0) {
    expect_true(all(c("column","row_index","original","corrected") %in%
                      names(rpt$changes)))
  }
})

# Helper: null coalescing for tests
`%||%` <- function(a, b) if (!is.null(a)) a else b
