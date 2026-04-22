#' Hypothetical Messy Employee Records Dataset
#'
#' @description
#' A hypothetical data frame of 20 employee records containing deliberate
#' data quality issues across all common inconsistency types: capitalisation
#' variants, typos, duplicate entries, malformed email addresses, out-of-range
#' numeric values, and cross-field redundancies. Designed to illustrate the
#' full range of issues detectable by \code{\link{detect_issues}()} and
#' \code{\link{offline_detect}()}.
#'
#' @format A \code{data.frame} with 20 rows and 8 variables:
#' \describe{
#'   \item{emp_id}{Integer. Unique employee identifier (1--20).}
#'   \item{name}{Character. Employee full name. Contains mixed-case
#'     inconsistencies (e.g. \code{"Alice Johnson"} vs \code{"alice johnson"}
#'     vs \code{"ALICE JOHNSON"}).}
#'   \item{department}{Character. Department name. Contains case variants
#'     (\code{"Finance"} vs \code{"finance"}), abbreviations (\code{"IT"}
#'     vs \code{"I.T."}), synonym variants (\code{"HR"} vs
#'     \code{"Human Resources"}), and a typo (\code{"Finanace"}).}
#'   \item{email}{Character. Email address. Contains malformed values:
#'     double \code{@@}, missing TLD, and leading dot.}
#'   \item{age}{Integer. Age in years. Contains two impossible values:
#'     \code{-5} and \code{150}.}
#'   \item{salary}{Numeric. Annual salary in USD. Contains a data entry
#'     error (\code{999999}) as an implausible outlier.}
#'   \item{status}{Character. Employment status (\code{active} /
#'     \code{inactive}). Contains case variants and two typos
#'     (\code{"actve"}).}
#'   \item{hire_date}{Character. Hire date. Contains a mixed-format date
#'     (\code{"2015/07/22"} vs \code{"2018-03-15"} ISO-8601 format).}
#' }
#'
#' @details
#' All data are entirely hypothetical and generated for illustrative purposes.
#' The inconsistency types are based on the taxonomy in de Jonge and van der
#' Loo (2013) and reflect common real-world data entry errors documented in
#' Muller and Freytag (2003).
#'
#' @source Hypothetical data generated for illustration. See
#'   \code{data-raw/generate_datasets.R}.
#'
#' @references
#' de Jonge, E. and van der Loo, M. (2013). An introduction to data cleaning
#' with R. \emph{Statistics Netherlands Discussion Paper}.
#' \url{https://cran.r-project.org/doc/contrib/de_Jonge+van_der_Loo-Introduction_to_data_cleaning_with_R.pdf}
#'
#' Muller, H. and Freytag, J.C. (2003). Problems, methods, and challenges in
#' comprehensive data cleansing. \emph{Technical Report, Humboldt University
#' Berlin}, HUB-IB-164.
#'
#' @seealso \code{\link{messy_survey}}, \code{\link{detect_issues}},
#'   \code{\link{offline_detect}}
#'
#' @examples
#' data(messy_employees)
#' str(messy_employees)
#'
#' # Quick overview of known issues
#' table(messy_employees$status)      # case inconsistency
#' table(messy_employees$department)  # variant forms
#' messy_employees$age[messy_employees$age < 0 | messy_employees$age > 100]
"messy_employees"


#' Hypothetical Messy Survey Response Dataset
#'
#' @description
#' A hypothetical survey data frame of 15 respondents from 5 countries
#' containing systematic data quality issues typical of free-text survey
#' responses: country name variants (\code{"USA"} vs \code{"United States"}),
#' satisfaction rating inconsistencies, spelling errors, and numeric outliers.
#'
#' @format A \code{data.frame} with 15 rows and 5 variables:
#' \describe{
#'   \item{respondent_id}{Integer. Unique respondent identifier (1--15).}
#'   \item{country}{Character. Respondent country. Contains abbreviations
#'     (\code{"USA"}, \code{"UK"}), synonyms (\code{"Deutschland"} for
#'     \code{"Germany"}), case variants, and a typo (\code{"Japn"}).}
#'   \item{satisfaction}{Character. Satisfaction rating (5-point Likert
#'     scale). Contains case inconsistencies and typos
#'     (\code{"Satisified"}, \code{"Nutral"}, \code{"Very Dissatified"}).}
#'   \item{age_group}{Character. Age group bracket. Clean column for
#'     comparison.}
#'   \item{income_usd}{Numeric. Reported annual income in USD. Contains a
#'     negative value (\code{-500}) and an implausible outlier
#'     (\code{999999}).}
#' }
#'
#' @details
#' All data are entirely hypothetical. Survey inconsistency patterns are
#' inspired by common free-text standardisation challenges described in
#' Chaudhuri et al. (2003).
#'
#' @source Hypothetical data generated for illustration.
#'   See \code{data-raw/generate_datasets.R}.
#'
#' @references
#' Chaudhuri, S., Ganjam, K., Ganti, V. and Motwani, R. (2003). Robust and
#' efficient fuzzy match for online data cleaning.
#' \emph{Proc. 2003 ACM SIGMOD}, 313--324. \doi{10.1145/872757.872796}
#'
#' @seealso \code{\link{messy_employees}}, \code{\link{detect_issues}}
#'
#' @examples
#' data(messy_survey)
#' str(messy_survey)
#' table(messy_survey$country)       # variant forms
#' table(messy_survey$satisfaction)  # case + typos
"messy_survey"
