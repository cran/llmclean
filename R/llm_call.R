# Internal: call the configured LLM and return the text response
# Supports: openai, anthropic, google, groq (openai-compat), ollama
.llm_call <- function(prompt,
                       system_prompt = NULL,
                       max_tokens    = 2000L,
                       temperature   = 0.1) {

  provider <- .llmclean_env$provider
  model    <- .llmclean_env$model
  api_key  <- .llmclean_env$api_key
  base_url <- .llmclean_env$base_url

  if (provider == "offline")
    rlang::abort("Provider is 'offline'. Call `set_llm_provider()` first.")

  if (!rlang::is_installed("httr2") || !rlang::is_installed("jsonlite"))
    rlang::abort(
      "Packages 'httr2' and 'jsonlite' are required for LLM calls.\n",
      "  Install with: install.packages(c('httr2', 'jsonlite'))")

  # Build messages
  messages <- list()
  if (!is.null(system_prompt) && provider != "anthropic") {
    messages <- c(messages, list(list(role = "system", content = system_prompt)))
  }
  messages <- c(messages, list(list(role = "user", content = prompt)))

  tryCatch({
    switch(provider,

      # -- OpenAI-compatible (openai, groq, ollama) -------------------------
      openai = ,
      groq   = ,
      ollama = {
        body <- list(
          model       = model,
          messages    = messages,
          max_tokens  = as.integer(max_tokens),
          temperature = temperature
        )
        headers <- list("Content-Type" = "application/json")
        if (!is.null(api_key) && nchar(api_key) > 0)
          headers[["Authorization"]] <- paste("Bearer", api_key)

        resp <- httr2::request(paste0(base_url, "/chat/completions")) |>
          httr2::req_headers(!!!headers) |>
          httr2::req_body_json(body) |>
          httr2::req_retry(max_tries = 3, backoff = ~ 2 * .x) |>
          httr2::req_timeout(60) |>
          httr2::req_perform()

        parsed <- jsonlite::fromJSON(
          httr2::resp_body_string(resp), simplifyVector = FALSE)
        parsed$choices[[1]]$message$content
      },

      # -- Anthropic ---------------------------------------------------------
      anthropic = {
        body <- list(
          model      = model,
          max_tokens = as.integer(max_tokens),
          messages   = messages
        )
        if (!is.null(system_prompt))
          body$system <- system_prompt

        resp <- httr2::request(paste0(base_url, "/messages")) |>
          httr2::req_headers(
            "Content-Type"      = "application/json",
            "x-api-key"         = api_key %||% "",
            "anthropic-version" = "2023-06-01"
          ) |>
          httr2::req_body_json(body) |>
          httr2::req_retry(max_tries = 3, backoff = ~ 2 * .x) |>
          httr2::req_timeout(60) |>
          httr2::req_perform()

        parsed <- jsonlite::fromJSON(
          httr2::resp_body_string(resp), simplifyVector = FALSE)
        parsed$content[[1]]$text
      },

      # -- Google Gemini -----------------------------------------------------
      google = {
        combined <- if (!is.null(system_prompt))
          paste0(system_prompt, "\n\n", prompt) else prompt

        body <- list(
          contents = list(list(
            parts = list(list(text = combined))
          )),
          generationConfig = list(
            maxOutputTokens = as.integer(max_tokens),
            temperature     = temperature
          )
        )

        url <- paste0(base_url, "/models/", model,
                       ":generateContent?key=", api_key %||% "")
        resp <- httr2::request(url) |>
          httr2::req_headers("Content-Type" = "application/json") |>
          httr2::req_body_json(body) |>
          httr2::req_retry(max_tries = 3, backoff = ~ 2 * .x) |>
          httr2::req_timeout(60) |>
          httr2::req_perform()

        parsed <- jsonlite::fromJSON(
          httr2::resp_body_string(resp), simplifyVector = FALSE)
        parsed$candidates[[1]]$content$parts[[1]]$text
      },

      rlang::abort(paste0("Unknown provider: '", provider, "'"))
    )
  }, error = function(e) {
    rlang::abort(paste0(
      "LLM API call failed (provider: ", provider, "):\n  ", e$message,
      "\nCheck your API key, network connection, and model name."))
  })
}

# Null coalescing operator (internal)
`%||%` <- function(a, b) if (!is.null(a) && nchar(as.character(a)) > 0) a else b
