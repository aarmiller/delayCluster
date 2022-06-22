#' Synthetic counts of visits for top 500 diagnosis codes before TB
#'
#' This dataset contains synthetic data of visits for each of the top 500 diagnosis
#' codes before a tuberculosis diagnosis. These counts are synthetic but were
#' generated to match patterns consistent with patterns in claims-based datasets.
#'
#' @format A data frame with 182,500 rows and 3 columns:
#' \describe{
#'   \item{dx}{ICD-9-CM code}
#'   \item{days_since_index}{Days Since the Index TB diagnosis}
#'   \item{n}{Number of patients with a visit for the given dx code}
#' }
"synth_dx_counts"

#' Synthetic counts of patients with visits before TB
#'
#' This dataset contains synthetic data of counts of patients with a visit each
#' day in the year prior to a theoretical diagnosis count. Data represents a theoretical
#' set of 30,000 patients with TB and is generated to match trends observed in
#' insurance claims data.
#'
#' @format A data frame with 365 rows and 2 columns:
#' \describe{
#'   \item{days_since_index}{Days Since the Index TB diagnosis}
#'   \item{n_patient_visits}{Number of patients with a visit on a given day}
#' }
"synth_visit_counts"

#' Fits of visit trends for synthetic visit counts using 7 day bins
#'
#' This dataset contains fitted values to the trends in the synthetic visit count
#' data (`synth_dx_counts`), using 7 days intervals to bin visits. The trends are
#' fitted to the normalized frequency counts (i.e., % of visits each day for a corresponding
#' diagnosis code)
#'
#' @format A data frame with 500 rows and 7 columns:
#' \describe{
#'   \item{index}{row number for the corresponding code}
#'   \item{code}{ICD-9-CM code for the given trend}
#'   \item{description}{Descriptions of the ICD-9-CM code}
#'   \item{cp}{change-point for the given code}
#'   \item{b0}{intercept term for the given code}
#'   \item{b1}{slope parameter for the first period (before opportunity window)}
#'   \item{b2}{slope parameter for the second period (during opportunity window)}
#' }
"synth_fits_7day"


