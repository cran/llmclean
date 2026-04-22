## tests/testthat/test-provider.R

test_that("set_llm_provider returns list invisibly", {
  res <- set_llm_provider("offline", verbose = FALSE)
  expect_type(res, "list")
  expect_named(res, c("provider","model","api_key","base_url"))
})

test_that("set_llm_provider sets provider to offline", {
  set_llm_provider("offline", verbose = FALSE)
  cfg <- get_llm_provider()
  expect_equal(cfg$provider, "offline")
})

test_that("set_llm_provider sets model correctly", {
  set_llm_provider("offline", model = "none", verbose = FALSE)
  cfg <- get_llm_provider()
  expect_equal(cfg$model, "none")
})

test_that("set_llm_provider uses default model for each provider", {
  suppressWarnings(set_llm_provider("openai", verbose = FALSE))
  expect_equal(get_llm_provider()$model, "gpt-4o-mini")

  suppressWarnings(set_llm_provider("groq", verbose = FALSE))
  expect_equal(get_llm_provider()$model, "llama-3.1-8b-instant")

  set_llm_provider("ollama", verbose = FALSE)
  expect_equal(get_llm_provider()$model, "llama3")
})

test_that("set_llm_provider respects custom model", {
  suppressWarnings(set_llm_provider("openai", model = "gpt-4o", verbose = FALSE))
  expect_equal(get_llm_provider()$model, "gpt-4o")
})

test_that("set_llm_provider errors on invalid provider", {
  expect_error(set_llm_provider("nonexistent"), regexp = "provider")
})

test_that("get_llm_provider returns named list", {
  set_llm_provider("offline", verbose = FALSE)
  cfg <- get_llm_provider()
  expect_type(cfg, "list")
  expect_true(all(c("provider","model","base_url") %in% names(cfg)))
})

test_that("get_llm_provider does not expose api_key by default", {
  set_llm_provider("offline", verbose = FALSE)
  cfg <- get_llm_provider(show_key = FALSE)
  expect_false("api_key" %in% names(cfg))
})

test_that("get_llm_provider exposes api_key when show_key = TRUE", {
  set_llm_provider("offline", verbose = FALSE)
  cfg <- get_llm_provider(show_key = TRUE)
  expect_true("api_key" %in% names(cfg))
})

test_that("set_llm_provider persists across calls", {
  suppressWarnings(set_llm_provider("groq", model = "test-model", verbose = FALSE))
  cfg1 <- get_llm_provider()
  cfg2 <- get_llm_provider()
  expect_equal(cfg1$model, cfg2$model)
})
