## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE, comment = "#>",
  fig.width = 7, out.width = "100%",
  warning = FALSE, message = FALSE
)

## ----load---------------------------------------------------------------------
library(llmclean)
library(dplyr)

## ----provider-----------------------------------------------------------------
set_llm_provider("offline")

## ----provider-llm, eval=FALSE-------------------------------------------------
# # Free Groq tier — fastest inference
# set_llm_provider("groq",
#                  api_key = Sys.getenv("GROQ_API_KEY"),
#                  model   = "llama-3.1-8b-instant")
# 
# # OpenAI
# set_llm_provider("openai",
#                  api_key = Sys.getenv("OPENAI_API_KEY"),
#                  model   = "gpt-4o-mini")
# 
# # Anthropic Claude
# set_llm_provider("anthropic",
#                  api_key = Sys.getenv("ANTHROPIC_API_KEY"),
#                  model   = "claude-haiku-4-5-20251001")
# 
# # Local Ollama (no key needed, model must be installed)
# set_llm_provider("ollama", model = "llama3")

## ----data---------------------------------------------------------------------
data(messy_employees)
data(messy_survey)

cat("messy_employees:", nrow(messy_employees), "rows x",
    ncol(messy_employees), "cols\n\n")

# Peek at known issues
cat("Status variants:\n"); print(table(messy_employees$status))
cat("\nDepartment variants:\n"); print(table(messy_employees$department))
cat("\nAge outliers:", messy_employees$age[messy_employees$age < 0 |
                                             messy_employees$age > 100], "\n")

## ----detect-------------------------------------------------------------------
issues <- detect_issues(messy_employees,
                         context = "HR employee records. Status values
                                    should be 'active' or 'inactive'.")
cat("Issues found:", nrow(issues), "\n\n")
print(issues[, c("column","row_index","value","issue_type",
                  "suggestion","confidence")])

## ----issue-types--------------------------------------------------------------
# Summary by type
as.data.frame(table(Type = issues$issue_type)) |>
  dplyr::arrange(dplyr::desc(Freq))

## ----case-issues--------------------------------------------------------------
# Show all case inconsistencies found
issues[issues$issue_type == "case",
       c("column","row_index","value","suggestion","confidence")]

## ----typo-issues--------------------------------------------------------------
issues[issues$issue_type == "typo",
       c("column","row_index","value","suggestion","explanation")]

## ----format-issues------------------------------------------------------------
issues[issues$issue_type == "format",
       c("column","row_index","value","suggestion")]

## ----outlier-issues-----------------------------------------------------------
issues[issues$issue_type == "outlier",
       c("column","row_index","value","explanation")]

## ----suggest------------------------------------------------------------------
enriched <- suggest_fixes(messy_employees, issues)
cat("Enriched columns:", paste(names(enriched), collapse = ", "), "\n")

# Show suggestions for status column
enriched[enriched$column == "status",
          c("row_index","value","suggestion","alternatives","confidence_revised")]

## ----apply-noninteractive-----------------------------------------------------
# Non-interactive: apply fixes with confidence >= 0.88
df_clean <- apply_fixes(
  messy_employees,
  enriched,
  confirm        = FALSE,
  min_confidence = 0.88
)

cat("Status before:", paste(sort(unique(messy_employees$status)), collapse=", "), "\n")
cat("Status after: ", paste(sort(unique(df_clean$status)), collapse=", "), "\n\n")

cat("Department before:",
    paste(sort(unique(messy_employees$department)), collapse=", "), "\n")
cat("Department after: ",
    paste(sort(unique(df_clean$department)), collapse=", "), "\n")

## ----dry-run------------------------------------------------------------------
plan <- apply_fixes(messy_employees, enriched, dry_run = TRUE)
cat("Planned changes:\n")
print(plan[, c("column","row_index","current_value","suggestion","issue_type")])

## ----offline------------------------------------------------------------------
# Works completely offline
offline_issues <- offline_detect(
  messy_survey,
  issue_types      = c("case","typo","format","outlier"),
  max_edit_distance = 2L
)

cat("Survey issues found:", nrow(offline_issues), "\n\n")
offline_issues[, c("column","value","issue_type","suggestion","confidence")]

## ----report-------------------------------------------------------------------
rpt <- llmclean_report(messy_employees, df_clean, issues)

## ----report-summary-----------------------------------------------------------
cat("Summary by column and type:\n")
print(rpt$summary)

cat("\nCell-level changes (first 8):\n")
print(head(rpt$changes, 8))

cat("\nMetadata:\n")
cat("  Provider  :", rpt$metadata$provider, "\n")
cat("  Model     :", rpt$metadata$model, "\n")
cat("  Detected  :", rpt$metadata$n_total, "\n")
cat("  Applied   :", rpt$metadata$n_applied, "\n")

## ----full-pipeline, eval=FALSE------------------------------------------------
# library(llmclean)
# 
# # 1. Configure provider (use Groq free tier)
# set_llm_provider("groq",
#                  api_key = Sys.getenv("GROQ_API_KEY"),
#                  model   = "llama-3.1-8b-instant")
# 
# # 2. Load data
# data(messy_employees)
# 
# # 3. Detect semantic issues
# issues <- detect_issues(
#   messy_employees,
#   context = "Employee records. Status: active/inactive. Age: 18-70."
# )
# 
# # 4. Enrich low-confidence suggestions
# enriched <- suggest_fixes(messy_employees, issues, n_alternatives = 2L)
# 
# # 5. Apply fixes non-interactively
# df_clean <- apply_fixes(messy_employees, enriched,
#                          confirm = FALSE, min_confidence = 0.80)
# 
# # 6. Generate audit report
# llmclean_report(messy_employees, df_clean, issues)

## ----session------------------------------------------------------------------
sessionInfo()

