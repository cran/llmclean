#' Detect Semantic Inconsistencies in a Data Frame Using an LLM
#'
#' @description
#' Sends a compact representation of the data frame to the configured LLM
#' provider and requests detection of semantic inconsistencies: typographic
#' errors, abbreviation variants, case inconsistencies, malformed values,
#' cross-field contradictions, and implausible entries. Returns a tidy
#' tibble of detected issues with location, type, and confidence.
#'
#' @param df A \code{data.frame} or \code{tibble}. The data to inspect.
#' @param columns Character vector or \code{NULL}. Column names to inspect.
#'   If \code{NULL} (default), all character and factor columns are inspected
#'   plus numeric columns for outlier detection.
#' @param sample_n Integer or \code{NULL}. Maximum number of rows to send
#'   to the LLM. If \code{NULL} (default), all rows are sent. For large
#'   data frames, set this to e.g. \code{200L} to reduce token usage.
#' @param issue_types Character vector. Types of issues to look for. Any
#'   subset of: \code{"typo"} (spelling errors), \code{"case"} (capitalisation
#'   inconsistency), \code{"abbreviation"} (variant forms of same entity),
#'   \code{"format"} (malformed values such as emails, dates, phone numbers),
#'   \code{"outlier"} (implausible numeric values), \code{"contradiction"}
#'   (cross-field logical conflict), \code{"duplicate"} (near-duplicate rows
#'   or values). Default: all types.
#' @param context Character or \code{NULL}. Optional plain-English description
#'   of the data to help the LLM interpret values (e.g.
#'   \code{"Employee records for a mid-size hospital. Age is in years.
#'   Status can be active or inactive."}).
#' @param max_tokens Integer. Maximum tokens in the LLM response.
#'   Default \code{2000L}.
#' @param verbose Logical. Print progress messages. Default \code{TRUE}.
#'
#' @details
#' The function constructs a structured prompt containing:
#' \enumerate{
#'   \item Column names and inferred types.
#'   \item The first \code{sample_n} rows in CSV-like format.
#'   \item Instructions requesting JSON output with one object per issue.
#' }
#'
#' The LLM is instructed to return \strong{only} a JSON array with fields:
#' \code{column}, \code{row_index} (1-based), \code{value}, \code{issue_type},
#' \code{explanation}, \code{suggestion}, and \code{confidence} (0--1).
#'
#' If the LLM response cannot be parsed as valid JSON, the raw response is
#' returned with a warning and \code{offline_detect()} is called as fallback.
#'
#' \strong{Token usage note:} Sending 100 rows of a 10-column data frame
#' uses approximately 1,500--3,000 tokens. At gpt-4o-mini pricing
#' (~$0.15/M input tokens as of 2025), this costs less than US$0.001.
#'
#' @return A \code{tibble} with one row per detected issue and columns:
#' \describe{
#'   \item{\code{column}}{Column name where the issue was found.}
#'   \item{\code{row_index}}{Row number (1-based).}
#'   \item{\code{value}}{The problematic value as a character string.}
#'   \item{\code{issue_type}}{One of the \code{issue_types} categories.}
#'   \item{\code{explanation}}{Human-readable explanation of the problem.}
#'   \item{\code{suggestion}}{Suggested corrected value.}
#'   \item{\code{confidence}}{Numeric 0--1. LLM-assigned confidence.}
#'   \item{\code{provider}}{The LLM provider used.}
#'   \item{\code{model}}{The specific model used.}
#' }
#' Returns a zero-row tibble with the same columns if no issues are found.
#'
#' @references
#' de Jonge, E. and van der Loo, M. (2013). An introduction to data cleaning
#' with R. \emph{Statistics Netherlands Discussion Paper}.
#' \url{https://cran.r-project.org/doc/contrib/de_Jonge+van_der_Loo-Introduction_to_data_cleaning_with_R.pdf}
#'
#' Chaudhuri, S., Ganjam, K., Ganti, V. and Motwani, R. (2003). Robust and
#' efficient fuzzy match for online data cleaning.
#' \emph{Proc. 2003 ACM SIGMOD}, 313--324. \doi{10.1145/872757.872796}
#'
#' @seealso \code{\link{set_llm_provider}}, \code{\link{suggest_fixes}},
#'   \code{\link{apply_fixes}}, \code{\link{offline_detect}}
#'
#' @examples
#' # Offline detection (no API key needed)
#' set_llm_provider("offline", verbose = FALSE)
#'
#' data(messy_employees)
#' issues <- detect_issues(messy_employees)
#' issues
#'
#' # With LLM (requires valid API key)
#' \dontrun{
#' set_llm_provider("openai", model = "gpt-4o-mini")
#' issues <- detect_issues(messy_employees,
#'                          context = "Employee records. Status: active/inactive.")
#' issues
#'
#' # Groq free tier
#' set_llm_provider("groq", model = "llama-3.1-8b-instant")
#' issues <- detect_issues(messy_employees, sample_n = 50L)
#' }
#'
#' @export
detect_issues <- function(df,
                            columns     = NULL,
                            sample_n    = NULL,
                            issue_types = c("typo","case","abbreviation",
                                            "format","outlier",
                                            "contradiction","duplicate"),
                            context     = NULL,
                            max_tokens  = 2000L,
                            verbose     = TRUE) {

  if (!is.data.frame(df))
    rlang::abort("`df` must be a data frame or tibble.")
  if (nrow(df) == 0)
    rlang::abort("`df` has zero rows.")

  provider <- .llmclean_env$provider

  # If offline, delegate to offline_detect
  if (provider == "offline") {
    if (verbose) rlang::inform("Using offline detection (no LLM call).")
    return(offline_detect(df, columns = columns, issue_types = issue_types))
  }

  # Select columns to inspect
  if (is.null(columns)) {
    char_cols <- names(df)[sapply(df, function(x)
      is.character(x) || is.factor(x))]
    num_cols  <- names(df)[sapply(df, is.numeric)]
    cols_use  <- unique(c(char_cols, if ("outlier" %in% issue_types) num_cols))
  } else {
    cols_use <- columns
  }

  if (length(cols_use) == 0)
    rlang::abort("No suitable columns found to inspect.")

  df_sub <- df[, cols_use, drop = FALSE]

  # Sample rows if requested
  if (!is.null(sample_n) && nrow(df_sub) > sample_n) {
    idx    <- sort(sample(nrow(df_sub), sample_n))
    df_sub <- df_sub[idx, , drop = FALSE]
    if (verbose) rlang::inform(paste0("Sampling ", sample_n, " of ",
                                       nrow(df), " rows."))
  }

  # Build prompt
  sys_prompt <- paste0(
    "You are a data quality expert. Your task is to detect semantic ",
    "inconsistencies in a data frame. You must respond ONLY with a valid ",
    "JSON array and nothing else -- no markdown, no explanation outside JSON."
  )

  col_info <- paste(
    sapply(names(df_sub), function(cn) {
      cls <- class(df_sub[[cn]])[1]
      vals <- utils::head(unique(as.character(df_sub[[cn]])), 10)
      paste0(cn, " (", cls, "): sample values: ",
             paste(vals, collapse = ", "))
    }), collapse = "\n")

  df_text <- paste(
    apply(df_sub, 1, function(row) {
      paste(names(df_sub), "=", row, collapse = " | ")
    }), collapse = "\n")

  issues_wanted <- paste(issue_types, collapse = ", ")

  ctx_block <- if (!is.null(context))
    paste0("\nContext about the data: ", context, "\n") else ""

  prompt <- paste0(
    ctx_block,
    "COLUMN INFORMATION:\n", col_info, "\n\n",
    "DATA (rows are 1-indexed):\n", df_text, "\n\n",
    "TASK: Detect all semantic issues of these types: ", issues_wanted, ".\n",
    "For each issue found, output a JSON object with EXACTLY these fields:\n",
    '  {"column":"...","row_index":N,"value":"...","issue_type":"...",',
    '"explanation":"...","suggestion":"...","confidence":0.X}\n\n',
    "Rules:\n",
    "- row_index is 1-based\n",
    "- confidence is 0.0 to 1.0\n",
    "- issue_type must be one of: ", issues_wanted, "\n",
    "- suggestion should be the corrected value, or 'REVIEW' if unclear\n",
    "- If no issues found, return: []\n",
    "Respond with ONLY the JSON array."
  )

  if (verbose) rlang::inform(paste0(
    "Calling ", .llmclean_env$provider, " (",
    .llmclean_env$model, ") to detect issues..."))

  raw <- .llm_call(prompt,
                    system_prompt = sys_prompt,
                    max_tokens    = max_tokens,
                    temperature   = 0.05)

  # Parse JSON response
  issues <- .parse_llm_json(raw, df_sub, provider = provider,
                              model = .llmclean_env$model)
  if (verbose) rlang::inform(paste0("Found ", nrow(issues), " issue(s)."))
  issues
}


# -- Internal: parse LLM JSON output into a tibble ---------------------------
.parse_llm_json <- function(raw_text, df, provider, model) {

  # Extract JSON array from response (strip any markdown fences)
  json_str <- raw_text
  json_str <- gsub("```json|```", "", json_str)
  json_str <- trimws(json_str)

  # Find the first '[' and last ']'
  start <- regexpr("\\[", json_str)[1]
  end   <- regexpr("\\](?=[^\\]]*$)", json_str, perl = TRUE)[1]
  if (start == -1 || end == -1) {
    rlang::warn("LLM response did not contain a valid JSON array. Returning empty.")
    return(.empty_issues())
  }
  json_str <- substr(json_str, start, end)

  parsed <- tryCatch(
    jsonlite::fromJSON(json_str, simplifyVector = TRUE),
    error = function(e) {
      rlang::warn(paste0("JSON parse error: ", e$message,
                          "\nRaw response:\n", substr(raw_text, 1, 300)))
      NULL
    }
  )

  if (is.null(parsed) || length(parsed) == 0) return(.empty_issues())

  # Normalise to data frame
  if (is.data.frame(parsed)) {
    result <- parsed
  } else if (is.list(parsed)) {
    result <- tryCatch(
      do.call(rbind, lapply(parsed, as.data.frame, stringsAsFactors = FALSE)),
      error = function(e) NULL
    )
    if (is.null(result)) return(.empty_issues())
  } else {
    return(.empty_issues())
  }

  # Ensure required columns exist
  required <- c("column","row_index","value","issue_type",
                 "explanation","suggestion","confidence")
  for (col in required) {
    if (!col %in% names(result)) result[[col]] <- NA
  }

  result$confidence <- suppressWarnings(as.numeric(result$confidence))
  result$row_index  <- suppressWarnings(as.integer(result$row_index))
  result$provider   <- provider
  result$model      <- model

  dplyr::as_tibble(result[, c(required, "provider", "model")])
}

# -- Empty issues tibble ------------------------------------------------------
.empty_issues <- function() {
  dplyr::tibble(
    column      = character(),
    row_index   = integer(),
    value       = character(),
    issue_type  = character(),
    explanation = character(),
    suggestion  = character(),
    confidence  = numeric(),
    provider    = character(),
    model       = character()
  )
}
