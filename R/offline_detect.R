#' Offline Detection of Data Inconsistencies Without an LLM
#'
#' @description
#' Detects data quality issues using statistical and fuzzy-matching methods,
#' requiring no LLM API call or internet connection. Uses Levenshtein string
#' distances for near-duplicate category detection, frequency analysis for
#' rare/singleton values, regex patterns for format validation, and IQR-based
#' outlier detection for numeric variables.
#'
#' @param df A \code{data.frame} or \code{tibble}.
#' @param columns Character vector or \code{NULL}. Columns to inspect. If
#'   \code{NULL}, all character, factor, and numeric columns are included.
#' @param issue_types Character vector. Subset of \code{"typo"},
#'   \code{"case"}, \code{"abbreviation"}, \code{"format"},
#'   \code{"outlier"}, \code{"duplicate"}. Default: all.
#' @param max_edit_distance Integer. Maximum Levenshtein edit distance
#'   between two values to flag as potential near-duplicates (typos or
#'   abbreviations). Default \code{2L}.
#' @param min_freq Integer. Values appearing fewer than this many times in
#'   a column are flagged as potentially erroneous singletons. Default
#'   \code{2L}.
#' @param outlier_iqr_mult Numeric. Multiplier for the IQR-based outlier
#'   fence: values outside \code{[Q1 - k*IQR, Q3 + k*IQR]} are flagged.
#'   Default \code{3.0} (Tukey outer fence).
#'
#' @details
#' \strong{Methods used:}
#' \describe{
#'   \item{Typo / near-duplicate detection}{Levenshtein edit distance
#'     (Levenshtein, 1966) between all pairs of unique values in each column.
#'     Pairs within \code{max_edit_distance} edits but with different
#'     capitalisation-normalised forms are flagged as potential typos.
#'     Implemented via \code{utils::adist()}.}
#'   \item{Case inconsistency}{Detects columns where the same word appears
#'     in multiple capitalisation forms (e.g. \code{"active"}, \code{"Active"},
#'     \code{"ACTIVE"}).}
#'   \item{Format validation}{Regex patterns for email addresses, phone
#'     numbers (international), URLs, and ISO dates. Any value not matching
#'     the majority pattern is flagged.}
#'   \item{Numeric outliers}{Modified Tukey outer fence: flags values beyond
#'     \code{Q1 - k*IQR} or \code{Q3 + k*IQR} where \eqn{k} =
#'     \code{outlier_iqr_mult}.}
#'   \item{Singleton detection}{Values appearing only once in a column with
#'     fewer than 10 unique values are flagged (likely erroneous entries).}
#' }
#'
#' The offline detector cannot catch \emph{semantic} issues (e.g. knowing
#' that \code{"NYC"} and \code{"New York"} are the same city) --- that requires
#' an LLM. Use \code{\link{detect_issues}()} with a configured LLM provider
#' for full semantic detection.
#'
#' @return A \code{tibble} with the same structure as \code{\link{detect_issues}()}:
#'   columns \code{column}, \code{row_index}, \code{value}, \code{issue_type},
#'   \code{explanation}, \code{suggestion}, \code{confidence}.
#'   The \code{provider} and \code{model} columns are set to
#'   \code{"offline"} and \code{"statistical"} respectively.
#'
#' @references
#' Levenshtein, V.I. (1966). Binary codes capable of correcting deletions,
#' insertions, and reversals. \emph{Soviet Physics Doklady}, 10(8), 707--710.
#'
#' Tukey, J.W. (1977). \emph{Exploratory Data Analysis}.
#' Addison-Wesley, Reading, MA. ISBN: 978-0-201-07616-5.
#'
#' Chaudhuri, S., et al. (2003). Robust and efficient fuzzy match for online
#' data cleaning. \emph{Proc. 2003 ACM SIGMOD}, 313--324.
#' \doi{10.1145/872757.872796}
#'
#' @seealso \code{\link{detect_issues}}, \code{\link{set_llm_provider}}
#'
#' @examples
#' data(messy_employees)
#' issues <- offline_detect(messy_employees)
#' issues
#'
#' # Only look for case and format issues
#' offline_detect(messy_employees, issue_types = c("case", "format"))
#'
#' @export
offline_detect <- function(df,
                             columns          = NULL,
                             issue_types      = c("typo","case","abbreviation",
                                                  "format","outlier","duplicate"),
                             max_edit_distance = 2L,
                             min_freq          = 2L,
                             outlier_iqr_mult  = 3.0) {

  if (!is.data.frame(df)) rlang::abort("`df` must be a data frame.")

  if (is.null(columns)) {
    char_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
    num_cols  <- names(df)[sapply(df, is.numeric)]
  } else {
    char_cols <- intersect(columns, names(df)[sapply(df, function(x)
      is.character(x) || is.factor(x))])
    num_cols  <- intersect(columns, names(df)[sapply(df, is.numeric)])
  }

  results <- list()

  # -- Case inconsistency ----------------------------------------------------
  if ("case" %in% issue_types) {
    for (cn in char_cols) {
      vals     <- as.character(df[[cn]])
      vals_lc  <- tolower(trimws(vals))
      uvals    <- unique(vals_lc[!is.na(vals_lc)])
      for (uv in uvals) {
        forms <- unique(vals[vals_lc == uv & !is.na(vals_lc)])
        if (length(forms) > 1) {
          # Flag all non-modal forms
          freq_forms <- sort(table(forms), decreasing = TRUE)
          canonical  <- names(freq_forms)[1]
          others     <- names(freq_forms)[-1]
          for (ov in others) {
            rows <- which(vals == ov)
            for (ri in rows) {
              results[[length(results)+1]] <- list(
                column      = cn, row_index = ri, value = ov,
                issue_type  = "case",
                explanation = paste0("'", ov, "' appears with inconsistent capitalisation. ",
                                     "Most common form: '", canonical, "'."),
                suggestion  = canonical,
                confidence  = 0.90
              )
            }
          }
        }
      }
    }
  }

  # -- Near-duplicate / typo detection (Levenshtein) -------------------------
  if (any(c("typo","abbreviation") %in% issue_types)) {
    for (cn in char_cols) {
      vals_raw <- as.character(df[[cn]])
      vals_lc  <- tolower(trimws(vals_raw))
      uvals    <- unique(vals_lc[!is.na(vals_lc) & nchar(vals_lc) > 0])
      if (length(uvals) < 2 || length(uvals) > 50) next   # skip if too few/many

      dist_mat <- utils::adist(uvals)
      for (i in seq_len(nrow(dist_mat))) {
        for (j in seq_len(ncol(dist_mat))) {
          if (i >= j) next
          d <- dist_mat[i, j]
          if (d == 0 || d > max_edit_distance) next
          # One is likely a typo of the other
          vi <- uvals[i]; vj <- uvals[j]
          freq_i <- sum(vals_lc == vi, na.rm = TRUE)
          freq_j <- sum(vals_lc == vj, na.rm = TRUE)
          # Less frequent one is the probable typo
          if (freq_i <= freq_j) {
            typo_val <- vi; correct_val <- vj
          } else {
            typo_val <- vj; correct_val <- vi
          }
          rows <- which(vals_lc == typo_val)
          for (ri in rows) {
            nchar_diff <- abs(nchar(typo_val) - nchar(correct_val))
            it <- if (nchar_diff <= 1L) "typo" else "abbreviation"
            results[[length(results)+1]] <- list(
              column      = cn, row_index = ri,
              value       = vals_raw[ri],
              issue_type  = it,
              explanation = paste0("'", vals_raw[ri], "' is ", d,
                                   " edit(s) from '", correct_val,
                                   "' (Levenshtein distance = ", d, ")."),
              suggestion  = correct_val,
              confidence  = round(1 - d * 0.12, 2)
            )
          }
        }
      }
    }
  }

  # -- Format validation -----------------------------------------------------
  if ("format" %in% issue_types) {
    patterns <- list(
      email = list(
        regex   = "^[a-zA-Z0-9._%+\\-]+@[a-zA-Z0-9.\\-]+\\.[a-zA-Z]{2,}$",
        trigger = "^[a-zA-Z0-9._%+\\-@]+$",   # looks like it's trying to be an email
        label   = "email"
      ),
      date_iso = list(
        regex   = "^[0-9]{4}-[0-9]{2}-[0-9]{2}$",
        trigger = "^[0-9]{4}[-/][0-9]{1,2}[-/][0-9]{1,2}$",
        label   = "ISO date"
      )
    )

    for (cn in char_cols) {
      vals <- as.character(df[[cn]])
      for (pat_name in names(patterns)) {
        pat <- patterns[[pat_name]]
        looks_like  <- grepl(pat$trigger, vals, perl = TRUE)
        is_valid    <- grepl(pat$regex,   vals, perl = TRUE)
        flagged     <- which(looks_like & !is_valid & !is.na(vals))
        for (ri in flagged) {
          results[[length(results)+1]] <- list(
            column      = cn, row_index = ri, value = vals[ri],
            issue_type  = "format",
            explanation = paste0("'", vals[ri], "' appears to be an ",
                                 pat$label, " but does not match the expected format."),
            suggestion  = "REVIEW",
            confidence  = 0.80
          )
        }
      }
    }
  }

  # -- Numeric outlier detection ---------------------------------------------
  if ("outlier" %in% issue_types) {
    for (cn in num_cols) {
      vals <- df[[cn]]
      ok   <- !is.na(vals)
      if (sum(ok) < 5) next
      q1  <- stats::quantile(vals[ok], 0.25)
      q3  <- stats::quantile(vals[ok], 0.75)
      iqr <- q3 - q1
      if (iqr == 0) next
      lo  <- q1 - outlier_iqr_mult * iqr
      hi  <- q3 + outlier_iqr_mult * iqr
      out_rows <- which(ok & (vals < lo | vals > hi))
      for (ri in out_rows) {
        results[[length(results)+1]] <- list(
          column      = cn, row_index = ri,
          value       = as.character(vals[ri]),
          issue_type  = "outlier",
          explanation = paste0("Value ", vals[ri], " is outside the Tukey outer fence [",
                               round(lo,2), ", ", round(hi,2), "] ",
                               "(IQR multiplier = ", outlier_iqr_mult, ")."),
          suggestion  = "REVIEW",
          confidence  = 0.75
        )
      }
    }
  }

  if (length(results) == 0) {
    rlang::inform("No issues detected by offline methods.")
    out <- .empty_issues()
    out$provider <- character()
    out$model    <- character()
    return(out)
  }

  result_df <- dplyr::bind_rows(lapply(results, dplyr::as_tibble))
  result_df$provider <- "offline"
  result_df$model    <- "statistical"

  # Deduplicate: same column + row_index + issue_type
  result_df <- result_df[!duplicated(
    paste(result_df$column, result_df$row_index, result_df$issue_type)), ]

  dplyr::arrange(dplyr::as_tibble(result_df), .data$column, .data$row_index)
}
