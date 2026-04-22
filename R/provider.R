#' Configure the LLM Provider for Data Cleaning
#'
#' @description
#' Sets the LLM provider, API key, and model to use for all subsequent
#' \code{\link{detect_issues}()} and \code{\link{suggest_fixes}()} calls.
#' Configuration is stored in a package-level environment and persists for
#' the R session. Supports OpenAI, Anthropic (Claude), Google (Gemini),
#' Groq, and local Ollama models.
#'
#' @param provider Character. LLM provider name. One of:
#'   \code{"openai"}, \code{"anthropic"}, \code{"google"}, \code{"groq"},
#'   \code{"ollama"}, \code{"offline"} (default; no LLM call).
#' @param api_key Character or \code{NULL}. API key for the chosen provider.
#'   If \code{NULL} (default), the function looks for the appropriate
#'   environment variable: \code{OPENAI_API_KEY}, \code{ANTHROPIC_API_KEY},
#'   \code{GOOGLE_API_KEY}, \code{GROQ_API_KEY}. Not required for
#'   \code{"ollama"} or \code{"offline"}.
#' @param model Character or \code{NULL}. Model name. If \code{NULL}
#'   (default), a sensible default is chosen per provider:
#'   \describe{
#'     \item{openai}{\code{"gpt-4o-mini"} (fast, cheap, capable)}
#'     \item{anthropic}{\code{"claude-haiku-4-5-20251001"} (fast, cheap)}
#'     \item{google}{\code{"gemini-2.0-flash"} (free tier available)}
#'     \item{groq}{\code{"llama-3.1-8b-instant"} (free tier)}
#'     \item{ollama}{\code{"llama3"} (local)}
#'   }
#' @param base_url Character or \code{NULL}. Custom base URL for the API
#'   endpoint. Useful for self-hosted deployments or API proxies. If
#'   \code{NULL}, provider defaults are used.
#' @param verbose Logical. If \code{TRUE}, prints a confirmation message.
#'   Default \code{TRUE}.
#'
#' @details
#' \strong{Getting API keys (all free tiers available):}
#' \describe{
#'   \item{OpenAI}{\url{https://platform.openai.com/api-keys}}
#'   \item{Anthropic}{\url{https://console.anthropic.com/}}
#'   \item{Google Gemini}{\url{https://aistudio.google.com/app/apikey}}
#'   \item{Groq (free)}{\url{https://console.groq.com/keys}}
#'   \item{Ollama (local)}{\url{https://ollama.com/} --- run \code{ollama pull llama3}}
#' }
#'
#' \strong{Best practice:} Store API keys in environment variables rather
#' than in scripts. Add to your \code{.Renviron} file:
#' \preformatted{
#' OPENAI_API_KEY=sk-...
#' ANTHROPIC_API_KEY=sk-ant-...
#' GROQ_API_KEY=gsk_...
#' GOOGLE_API_KEY=AIza...
#' }
#'
#' @return Invisibly returns a list with the configured provider settings.
#'
#' @references
#' OpenAI (2024). GPT-4 Technical Report. \emph{arXiv preprint}
#' arXiv:2303.08774. \url{https://arxiv.org/abs/2303.08774}
#'
#' Anthropic (2024). Claude Model Overview.
#' \url{https://platform.claude.com/docs/en/docs/about-claude/models/overview}
#'
#' @seealso \code{\link{get_llm_provider}}, \code{\link{detect_issues}}
#'
#' @examples
#' # Use offline mode (no API key needed)
#' set_llm_provider("offline")
#'
#' # Configure OpenAI (reads key from env var OPENAI_API_KEY)
#' \dontrun{
#' set_llm_provider("openai", model = "gpt-4o-mini")
#'
#' # Configure Anthropic explicitly
#' set_llm_provider("anthropic",
#'                  api_key = Sys.getenv("ANTHROPIC_API_KEY"),
#'                  model   = "claude-haiku-4-5-20251001")
#'
#' # Configure free Groq
#' set_llm_provider("groq", model = "llama-3.1-8b-instant")
#'
#' # Local Ollama (no key needed)
#' set_llm_provider("ollama", model = "llama3")
#' }
#'
#' @export
set_llm_provider <- function(provider  = "offline",
                               api_key  = NULL,
                               model    = NULL,
                               base_url = NULL,
                               verbose  = TRUE) {

  valid_providers <- c("openai","anthropic","google","groq","ollama","offline")
  if (!provider %in% valid_providers)
    rlang::abort(paste0(
      "`provider` must be one of: ",
      paste(valid_providers, collapse = ", "), "."))

  # Default models per provider
  default_models <- c(
    openai    = "gpt-4o-mini",
    anthropic = "claude-haiku-4-5-20251001",
    google    = "gemini-2.0-flash",
    groq      = "llama-3.1-8b-instant",
    ollama    = "llama3",
    offline   = "none"
  )

  # Default env vars per provider
  env_vars <- c(
    openai    = "OPENAI_API_KEY",
    anthropic = "ANTHROPIC_API_KEY",
    google    = "GOOGLE_API_KEY",
    groq      = "GROQ_API_KEY",
    ollama    = "",
    offline   = ""
  )

  # Default base URLs
  base_urls <- c(
    openai    = "https://api.openai.com/v1",
    anthropic = "https://api.anthropic.com/v1",
    google    = "https://generativelanguage.googleapis.com/v1beta",
    groq      = "https://api.groq.com/openai/v1",
    ollama    = "http://localhost:11434/v1",
    offline   = ""
  )

  # Resolve API key
  if (is.null(api_key) && provider %in% c("openai","anthropic","google","groq")) {
    env_var <- env_vars[provider]
    api_key <- Sys.getenv(env_var)
    if (nchar(api_key) == 0) {
      rlang::warn(paste0(
        "No API key supplied and `", env_var, "` environment variable is empty.\n",
        "  Set it with: Sys.setenv(", env_var, " = 'your-key')"))
      api_key <- NULL
    }
  }

  resolved_model    <- if (is.null(model))    default_models[provider]  else model
  resolved_base_url <- if (is.null(base_url)) base_urls[provider]       else base_url

  .llmclean_env$provider <- provider
  .llmclean_env$api_key  <- api_key
  .llmclean_env$model    <- unname(resolved_model)
  .llmclean_env$base_url <- unname(resolved_base_url)

  if (verbose) {
    rlang::inform(paste0(
      "llmclean provider set:\n",
      "  Provider : ", provider, "\n",
      "  Model    : ", resolved_model, "\n",
      "  API key  : ", if (is.null(api_key)) "<none>" else
                        paste0(substr(api_key, 1, 6), "..."), "\n",
      "  Base URL : ", resolved_base_url
    ))
  }

  invisible(list(
    provider = provider,
    model    = unname(resolved_model),
    api_key  = api_key,
    base_url = unname(resolved_base_url)
  ))
}


#' Get Current LLM Provider Configuration
#'
#' @description
#' Returns the current LLM provider, model, and base URL configured by
#' \code{\link{set_llm_provider}()}.
#'
#' @param show_key Logical. If \code{TRUE}, include the API key (truncated)
#'   in the output. Default \code{FALSE}.
#'
#' @return A named list with elements \code{provider}, \code{model},
#'   \code{base_url}, and optionally \code{api_key}.
#'
#' @seealso \code{\link{set_llm_provider}}
#'
#' @examples
#' set_llm_provider("offline", verbose = FALSE)
#' get_llm_provider()
#'
#' @export
get_llm_provider <- function(show_key = FALSE) {
  out <- list(
    provider = .llmclean_env$provider,
    model    = .llmclean_env$model,
    base_url = .llmclean_env$base_url
  )
  if (show_key) out[["api_key"]] <- .llmclean_env$api_key %||% NA_character_
  out
}
