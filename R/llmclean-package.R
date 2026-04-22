#' llmclean: LLM-Assisted Data Cleaning with Multi-Provider Support
#'
#' @description
#' Detects and suggests fixes for semantic inconsistencies in data frames by
#' calling large language models through a unified, provider-agnostic interface.
#'
#' @section The problem:
#' Traditional data cleaning tools such as \code{janitor}, \code{validate},
#' and \code{pointblank} are rule-based: they catch type mismatches, range
#' violations, and schema errors. They cannot understand *meaning*.
#'
#' An LLM, by contrast, understands that \code{"NYC"}, \code{"New York"},
#' \code{"new york"}, and \code{"New Yrok"} all refer to the same city.
#' It recognises \code{"jane@@gmail.com"} as a malformed email, and
#' \code{"actve"} as a typo for \code{"active"}. These semantic issues are
#' the hardest to catch and the most damaging to downstream analyses.
#'
#' @section Supported providers:
#' \describe{
#'   \item{\strong{OpenAI}}{GPT-4o, GPT-4o-mini (API key required). The most
#'     widely used commercial LLM with mature tooling.}
#'   \item{\strong{Anthropic}}{Claude Sonnet, Claude Haiku (API key required).
#'     Large context window; strong instruction following.}
#'   \item{\strong{Google}}{Gemini 2.0 Flash, Gemini 1.5 Pro (API key
#'     required). Free tier available.}
#'   \item{\strong{Groq}}{LLaMA 3.1, Mixtral (free-tier API key). Fastest
#'     inference; suitable for large data frames.}
#'   \item{\strong{Ollama}}{Any local model (llama3, mistral, phi3). Fully
#'     offline; no API key; complete data privacy.}
#' }
#'
#' @section Workflow:
#' \enumerate{
#'   \item \strong{Configure provider}: \code{\link{set_llm_provider}()}
#'   \item \strong{Detect semantic issues}: \code{\link{detect_issues}()}
#'   \item \strong{Get suggested fixes}: \code{\link{suggest_fixes}()}
#'   \item \strong{Apply fixes interactively}: \code{\link{apply_fixes}()}
#'   \item \strong{Summary report}: \code{\link{llmclean_report}()}
#' }
#'
#' @section Offline fallback:
#' \code{\link{offline_detect}()} provides statistical and fuzzy-matching
#' detection without any LLM call, using Levenshtein string distances,
#' frequency analysis, and regex pattern matching. Use this when no API
#' key is available or for data privacy reasons.
#'
#' @section References:
#' de Jonge, E. and van der Loo, M. (2013). An introduction to data cleaning
#' with R. \emph{Statistics Netherlands Discussion Paper}, The Hague.
#' \url{https://cran.r-project.org/doc/contrib/de_Jonge+van_der_Loo-Introduction_to_data_cleaning_with_R.pdf}
#'
#' Chaudhuri, S., Ganjam, K., Ganti, V. and Motwani, R. (2003). Robust and
#' efficient fuzzy match for online data cleaning. \emph{Proceedings of the
#' 2003 ACM SIGMOD International Conference on Management of Data}, 313--324.
#' \doi{10.1145/872757.872796}
#'
#' van der Loo, M.P.J. and de Jonge, E. (2018). \emph{Statistical Data
#' Cleaning with Applications in R}. John Wiley & Sons, Chichester.
#' \doi{10.1002/9781118897126}
#'
#' OpenAI (2024). GPT-4 Technical Report. \emph{arXiv preprint}
#' arXiv:2303.08774. \url{https://arxiv.org/abs/2303.08774}
#'
#' @docType package
#' @name llmclean-package
#' @aliases llmclean

#' @importFrom dplyr tibble as_tibble bind_rows arrange select all_of .data
#' @importFrom rlang abort warn inform is_installed .data
#' @importFrom stats quantile
#' @importFrom utils adist head installed.packages
"_PACKAGE"

# Package-level environment for provider configuration
.llmclean_env <- new.env(parent = emptyenv())
.llmclean_env$provider <- "offline"
.llmclean_env$api_key  <- NULL
.llmclean_env$model    <- NULL
.llmclean_env$base_url <- NULL

# Suppress NOTE for global variables
utils::globalVariables(c(".data", "column", "row_index", "value",
                          "issue_type", "suggestion", "confidence",
                          "applied"))
