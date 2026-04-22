#' Generate a Summary Report of LLM-Assisted Data Cleaning
#'
#' @description
#' Produces a tidy summary of detected issues and applied fixes, grouped by
#' column and issue type. Suitable for inclusion in a data-quality audit log
#' or reproducible report. Optionally prints a formatted table to the console.
#'
#' @param df_original A \code{data.frame}. The original (uncleaned) data.
#' @param df_cleaned A \code{data.frame}. The cleaned data returned by
#'   \code{\link{apply_fixes}()}.
#' @param issues A \code{tibble} of detected issues from
#'   \code{\link{detect_issues}()} or \code{\link{offline_detect}()}.
#' @param print Logical. If \code{TRUE} (default), prints a formatted
#'   summary table to the console.
#'
#' @details
#' The report computes:
#' \itemize{
#'   \item Number of issues detected per column and issue type.
#'   \item Number of fixes applied vs skipped.
#'   \item Cell-level change summary (original value -> corrected value).
#'   \item Provider and model used.
#'   \item Data dimensions before and after cleaning.
#' }
#'
#' The applied-fix provenance is read from the \code{"_applied"} attribute
#' set by \code{\link{apply_fixes}()}. If the attribute is absent (e.g.
#' when \code{df_cleaned} was not produced by \code{apply_fixes()}), the
#' report uses the full \code{issues} tibble.
#'
#' @return Invisibly returns a named list with three elements:
#'   \code{summary} (tibble: column / issue_type / n_detected / n_applied),
#'   \code{changes} (tibble: column / row_index / original / corrected),
#'   \code{metadata} (list: provider, model, n_total, n_applied, n_skipped).
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
#' issues   <- offline_detect(messy_employees)
#' df_clean <- apply_fixes(messy_employees, issues,
#'                          confirm = FALSE, min_confidence = 0.85)
#' rpt <- llmclean_report(messy_employees, df_clean, issues)
#' rpt$summary
#'
#' @export
llmclean_report <- function(df_original,
                              df_cleaned,
                              issues,
                              print = TRUE) {

  if (!is.data.frame(df_original)) rlang::abort("`df_original` must be a data frame.")
  if (!is.data.frame(df_cleaned))  rlang::abort("`df_cleaned` must be a data frame.")
  if (!is.data.frame(issues))      rlang::abort("`issues` must be a data frame.")

  # Provenance from apply_fixes attribute
  prov <- attr(df_cleaned, "_applied")

  n_total   <- if (!is.null(prov)) prov$n_total   else nrow(issues)
  n_applied <- if (!is.null(prov)) prov$n_applied else NA_integer_
  n_skipped <- if (!is.null(prov)) prov$n_skipped else NA_integer_
  applied_v <- if (!is.null(prov)) prov$applied   else rep(TRUE, nrow(issues))

  # Summary by column + issue_type
  if (nrow(issues) > 0) {
    summary_tbl <- as.data.frame(table(
      column = issues$column, issue_type = issues$issue_type
    ))
    summary_tbl <- summary_tbl[summary_tbl$Freq > 0, ]
    names(summary_tbl)[3] <- "n_detected"

    applied_issues <- issues[applied_v, , drop = FALSE]
    if (nrow(applied_issues) > 0) {
      applied_cnt <- as.data.frame(table(
        column = applied_issues$column, issue_type = applied_issues$issue_type
      ))
      applied_cnt <- applied_cnt[applied_cnt$Freq > 0, ]
      names(applied_cnt)[3] <- "n_applied"
      summary_tbl <- merge(summary_tbl, applied_cnt,
                           by = c("column","issue_type"), all.x = TRUE)
      summary_tbl$n_applied[is.na(summary_tbl$n_applied)] <- 0L
    } else {
      summary_tbl$n_applied <- 0L
    }
  } else {
    summary_tbl <- data.frame(column = character(), issue_type = character(),
                               n_detected = integer(), n_applied = integer())
  }

  # Cell-level changes
  changes_list <- list()
  if (sum(applied_v) > 0) {
    app_issues <- issues[applied_v, , drop = FALSE]
    for (i in seq_len(nrow(app_issues))) {
      cn <- app_issues$column[i]
      ri <- app_issues$row_index[i]
      if (!cn %in% names(df_original) || is.na(ri)) next
      changes_list[[i]] <- dplyr::tibble(
        column    = cn,
        row_index = ri,
        original  = as.character(df_original[ri, cn]),
        corrected = as.character(df_cleaned[ri, cn])
      )
    }
  }
  changes_tbl <- if (length(changes_list) > 0)
    dplyr::bind_rows(changes_list) else
    dplyr::tibble(column=character(), row_index=integer(),
                  original=character(), corrected=character())

  provider <- if (nrow(issues) > 0 && "provider" %in% names(issues))
    issues$provider[1] else "offline"
  model    <- if (nrow(issues) > 0 && "model" %in% names(issues))
    issues$model[1] else "statistical"

  meta <- list(
    provider  = provider,
    model     = model,
    n_total   = n_total,
    n_applied = n_applied,
    n_skipped = n_skipped,
    nrow_orig = nrow(df_original),
    ncol_orig = ncol(df_original)
  )

  if (print) {
    cat("\n=== llmclean Data Quality Report ===\n")
    cat(sprintf("Provider    : %s (%s)\n", provider, model))
    cat(sprintf("Data        : %d rows x %d columns\n",
                nrow(df_original), ncol(df_original)))
    cat(sprintf("Issues found: %d\n", n_total))
    cat(sprintf("Fixes applied: %d / skipped: %s\n\n",
                n_applied %||% 0,
                if (is.na(n_skipped)) "unknown" else n_skipped))

    if (nrow(summary_tbl) > 0) {
      cat("Issues by column and type:\n")
      print(summary_tbl, row.names = FALSE)
    }

    if (nrow(changes_tbl) > 0) {
      cat("\nFixes applied:\n")
      print(changes_tbl, row.names = FALSE)
    }
    cat("=====================================\n\n")
  }

  invisible(list(
    summary  = dplyr::as_tibble(summary_tbl),
    changes  = changes_tbl,
    metadata = meta
  ))
}
