% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{synth_visit_counts}
\alias{synth_visit_counts}
\title{Synthetic counts of patients with visits before TB}
\format{
A data frame with 365 rows and 2 columns:
\describe{
  \item{days_since_index}{Days Since the Index TB diagnosis}
  \item{n_patient_visits}{Number of patients with a visit on a given day}
}
}
\usage{
synth_visit_counts
}
\description{
This dataset contains synthetic data of counts of patients with a visit each
day in the year prior to a theoretical diagnosis count. Data represents a theoretical
set of 30,000 patients with TB and is generated to match trends observed in
insurance claims data.
}
\keyword{datasets}
