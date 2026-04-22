#' Request Enriched Fix Suggestions for Detected Issues
#'
#' @description
#' Takes the issues tibble from \code{\link{detect_issues}()} or
#' \code{\link{offline_detect}()} and optionally sends it back to the LLM
#' with the full data context to obtain higher-quality, rank-ordered
#' suggestions for each issue. Useful when the initial detection pass
#' returned \code{suggestion = "REVIEW"} or low-confidence suggestions.
#'
#' @param df A \code{data.frame}. The original (uncleaned) data.
#' @param issues A \code{tibble} returned by \code{\link{detect_issues}()} or
#'   \code{\link{offline_detect}()}.
#' @param n_alternatives Integer. Number of alternative suggestions per
#'   issue (in addition to the primary). Default \code{2L}.
#' @param filter_confidence Numeric (0--1). Only re-query issues with
#'   confidence below this threshold. Default \code{0.80}. Set to \code{1.0}
#'   to re-query all issues.
#' @param verbose Logical. Print progress messages. Default \code{TRUE}.
#'
#' @details
#' If the current provider is \code{"offline"}, this function returns the
#' input \code{issues} tibble unchanged with a message, since no LLM is
#' available to enrich suggestions.
#'
#' The function sends each low-confidence issue to the LLM along with the
#' surrounding rows for context, and requests ranked alternatives. Results
#' are merged back into the issues tibble, replacing the original
#' \code{suggestion} with the top-ranked alternative and adding
#' \code{alternatives} as a comma-separated string column.
#'
#' @return The input \code{issues} tibble with two additional columns:
#'   \code{alternatives} (comma-separated list of alternative suggestions)
#'   and \code{confidence_revised} (updated confidence after re-querying).
#'
#' @references
#' de Jonge, E. and van der Loo, M. (2013). An introduction to data cleaning
#' with R. \emph{Statistics Netherlands Discussion Paper}.
#' \url{https://cran.r-project.org/doc/contrib/de_Jonge+van_der_Loo-Introduction_to_data_cleaning_with_R.pdf}
#'
#' @seealso \code{\link{detect_issues}}, \code{\link{apply_fixes}}
#'
#' @examples
#' set_llm_provider("offline", verbose = FALSE)
#' data(messy_employees)
#' issues <- offline_detect(messy_employees)
#'
#' # Offline: suggest_fixes returns issues unchanged
#' enriched <- suggest_fixes(messy_employees, issues)
#' enriched
#'
#' \dontrun{
#' set_llm_provider("openai", model = "gpt-4o-mini")
#' issues   <- detect_issues(messy_employees)
#' enriched <- suggest_fixes(messy_employees, issues, n_alternatives = 3L)
#' }
#'
#' @export
suggest_fixes <- function(df,
                            issues,
                            n_alternatives       = 2L,
                            filter_confidence    = 0.80,
                            verbose              = TRUE) {

  if (!is.data.frame(df))     rlang::abort("`df` must be a data frame.")
  if (!is.data.frame(issues)) rlang::abort("`issues` must be a data frame.")
  if (nrow(issues) == 0) {
    rlang::inform("No issues to enrich.")
    return(issues)
  }

  provider <- .llmclean_env$provider

  if (provider == "offline") {
    if (verbose) rlang::inform(
      "Provider is 'offline': returning issues with statistical suggestions unchanged.")
    issues$alternatives       <- issues$suggestion
    issues$confidence_revised <- issues$confidence
    return(issues)
  }

  # Filter to low-confidence issues
  to_enrich <- issues[!is.na(issues$confidence) &
                        issues$confidence < filter_confidence, ]

  if (nrow(to_enrich) == 0) {
    if (verbose) rlang::inform(
      "All issues already above confidence threshold. No LLM re-query needed.")
    issues$alternatives       <- issues$suggestion
    issues$confidence_revised <- issues$confidence
    return(issues)
  }

  if (verbose) rlang::inform(paste0(
    "Re-querying LLM for ", nrow(to_enrich), " low-confidence issue(s)..."))

  updated_suggestions <- vector("list", nrow(to_enrich))

  for (i in seq_len(nrow(to_enrich))) {
    issue <- to_enrich[i, ]
    cn    <- issue$column
    ri    <- issue$row_index

    # Context: surrounding rows
    ctx_rows <- max(1, ri-2):min(nrow(df), ri+2)
    ctx_df   <- df[ctx_rows, , drop = FALSE]
    ctx_text <- paste(
      apply(ctx_df, 1, function(r)
        paste(names(ctx_df), "=", r, collapse=" | ")),
      collapse = "\n")

    prompt <- paste0(
      "Column: ", cn, "\n",
      "Problematic value at row ", ri, ": '", issue$value, "'\n",
      "Issue type: ", issue$issue_type, "\n",
      "Surrounding data context:\n", ctx_text, "\n\n",
      "Provide ", n_alternatives + 1, " ranked suggestions (best first).\n",
      "Respond ONLY with a JSON array of strings, e.g.: [\"fix1\",\"fix2\",\"fix3\"]\n",
      "If truly unclear, include \"REVIEW\" as last option."
    )

    raw <- tryCatch(
      .llm_call(prompt, max_tokens = 200L, temperature = 0.05),
      error = function(e) NULL
    )

    if (!is.null(raw)) {
      json_str <- gsub("```json|```", "", raw)
      alts <- tryCatch(
        jsonlite::fromJSON(trimws(json_str), simplifyVector = TRUE),
        error = function(e) NULL
      )
      if (!is.null(alts) && is.character(alts) && length(alts) > 0) {
        updated_suggestions[[i]] <- list(
          suggestion        = alts[1],
          alternatives      = paste(alts, collapse = ", "),
          confidence_revised = min(0.95, issue$confidence + 0.15)
        )
      }
    }
    if (is.null(updated_suggestions[[i]])) {
      updated_suggestions[[i]] <- list(
        suggestion        = issue$suggestion,
        alternatives      = issue$suggestion,
        confidence_revised = issue$confidence
      )
    }
  }

  # Merge back
  issues$alternatives       <- issues$suggestion
  issues$confidence_revised <- issues$confidence

  to_enrich_idx <- which(!is.na(issues$confidence) &
                           issues$confidence < filter_confidence)
  for (i in seq_along(to_enrich_idx)) {
    idx <- to_enrich_idx[i]
    issues$suggestion[idx]         <- updated_suggestions[[i]]$suggestion
    issues$alternatives[idx]       <- updated_suggestions[[i]]$alternatives
    issues$confidence_revised[idx] <- updated_suggestions[[i]]$confidence_revised
  }

  issues
}


#' Apply Suggested Fixes to a Data Frame
#'
#' @description
#' Applies the suggestions from \code{\link{detect_issues}()} or
#' \code{\link{suggest_fixes}()} to the original data frame, either
#' automatically or with interactive human review of each fix.
#'
#' @param df A \code{data.frame}. The original (uncleaned) data.
#' @param issues A \code{tibble} of detected issues from
#'   \code{\link{detect_issues}()} or \code{\link{offline_detect}()}.
#' @param confirm Logical. If \code{TRUE} (default), show each proposed fix
#'   and ask the user to accept, reject, or enter a custom value.
#'   If \code{FALSE}, all fixes above \code{min_confidence} are applied
#'   automatically without prompting.
#' @param min_confidence Numeric (0--1). Only apply fixes with confidence
#'   at or above this threshold. Default \code{0.70}. Issues with
#'   \code{suggestion = "REVIEW"} are never applied automatically.
#' @param dry_run Logical. If \code{TRUE}, return a summary of what would
#'   be changed without actually modifying the data. Default \code{FALSE}.
#'
#' @details
#' The function creates a copy of \code{df}, then iterates over each row of
#' \code{issues} in order of descending confidence. For each issue:
#' \itemize{
#'   \item If \code{confirm = FALSE} and confidence >= \code{min_confidence}
#'     and suggestion != \code{"REVIEW"}: the value at
#'     \code{df[row_index, column]} is replaced with \code{suggestion}.
#'   \item If \code{confirm = TRUE}: the user is presented with the current
#'     value, the suggested fix, and the context, then prompted to
#'     accept (y), skip (n), or type a custom replacement.
#' }
#'
#' A \code{"_applied"} attribute is attached to the returned data frame
#' recording which fixes were applied, for use by
#' \code{\link{llmclean_report}()}.
#'
#' @return A \code{data.frame} with the same structure as \code{df} but
#'   with accepted fixes applied. The original \code{df} is not modified.
#'
#' @references
#' van der Loo, M.P.J. and de Jonge, E. (2018). \emph{Statistical Data
#' Cleaning with Applications in R}. John Wiley & Sons.
#' \doi{10.1002/9781118897126}
#'
#' @seealso \code{\link{detect_issues}}, \code{\link{suggest_fixes}},
#'   \code{\link{llmclean_report}}
#'
#' @examples
#' set_llm_provider("offline", verbose = FALSE)
#' data(messy_employees)
#' issues <- offline_detect(messy_employees)
#'
#' # Non-interactive: apply high-confidence fixes automatically
#' df_clean <- apply_fixes(messy_employees, issues,
#'                          confirm = FALSE, min_confidence = 0.85)
#' head(df_clean)
#'
#' # Dry run: see what would change
#' plan <- apply_fixes(messy_employees, issues, dry_run = TRUE)
#' plan
#'
#' @export
apply_fixes <- function(df,
                         issues,
                         confirm        = TRUE,
                         min_confidence = 0.70,
                         dry_run        = FALSE) {

  if (!is.data.frame(df))     rlang::abort("`df` must be a data frame.")
  if (!is.data.frame(issues)) rlang::abort("`issues` must be a data frame.")
  if (nrow(issues) == 0) {
    rlang::inform("No issues to apply.")
    return(df)
  }

  df_out   <- df           # copy
  applied  <- logical(nrow(issues))
  skipped  <- logical(nrow(issues))

  # Sort by confidence descending
  ord <- order(issues$confidence, decreasing = TRUE, na.last = TRUE)

  for (i in ord) {
    issue <- issues[i, ]
    cn    <- issue$column
    ri    <- issue$row_index
    sugg  <- issue$suggestion
    conf  <- issue$confidence

    # Skip impossible cases
    if (is.na(ri) || is.na(cn) || !cn %in% names(df_out)) {
      skipped[i] <- TRUE; next
    }
    if (!dry_run && (is.na(sugg) || sugg == "REVIEW")) {
      skipped[i] <- TRUE; next
    }
    if (!dry_run && !is.na(conf) && conf < min_confidence) {
      skipped[i] <- TRUE; next
    }

    current_val <- as.character(df_out[ri, cn])

    if (dry_run) {
      applied[i] <- TRUE
      next
    }

    if (confirm) {
      cat(sprintf(
        "\n[%d/%d] Column: %-15s  Row: %d\n  Current : %s\n  Suggest : %s\n  Type    : %-15s  Confidence: %.2f\n  %s\n",
        which(ord == i), length(ord),
        cn, ri, current_val, sugg,
        issue$issue_type, conf,
        issue$explanation
      ))
      cat("  Apply fix? [y = yes / n = skip / c = custom]: ")
      ans <- tryCatch(readLines(con = stdin(), n = 1), error = function(e) "n")
      ans <- trimws(tolower(ans))

      if (ans == "y") {
        df_out[ri, cn] <- sugg
        applied[i]     <- TRUE
        cat("  Applied.\n")
      } else if (ans == "c") {
        cat("  Enter custom value: ")
        custom <- tryCatch(readLines(con = stdin(), n = 1), error = function(e) "")
        if (nchar(trimws(custom)) > 0) {
          df_out[ri, cn] <- trimws(custom)
          applied[i]     <- TRUE
          cat("  Applied custom value:", trimws(custom), "\n")
        } else {
          skipped[i] <- TRUE
          cat("  Skipped (empty input).\n")
        }
      } else {
        skipped[i] <- TRUE
        cat("  Skipped.\n")
      }
    } else {
      # Auto-apply
      df_out[ri, cn] <- sugg
      applied[i]     <- TRUE
    }
  }

  n_applied  <- sum(applied)
  n_skipped  <- sum(skipped)
  n_total    <- nrow(issues)

  if (dry_run) {
    eligible <- issues[applied, , drop = FALSE]
    eligible$current_value <- mapply(
      function(cn, ri) as.character(df[ri, cn]),
      eligible$column, eligible$row_index
    )
    rlang::inform(paste0("Dry run: ", n_applied, " of ", n_total,
                          " fixes would be applied."))
    return(dplyr::select(eligible, dplyr::all_of(
      c("column","row_index","current_value","suggestion",
        "issue_type","confidence"))))
  }

  # Attach provenance
  attr(df_out, "_applied") <- list(
    n_applied = n_applied,
    n_skipped = n_skipped,
    n_total   = n_total,
    issues    = issues,
    applied   = applied
  )

  rlang::inform(paste0(
    "Applied ", n_applied, " / ", n_total, " fixes. ",
    n_skipped, " skipped."))

  df_out
}
