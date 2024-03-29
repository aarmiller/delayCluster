% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{synth_fits_7day}
\alias{synth_fits_7day}
\title{Fits of visit trends for synthetic visit counts using 7 day bins}
\format{
A data frame with 500 rows and 7 columns:
\describe{
  \item{index}{row number for the corresponding code}
  \item{code}{ICD-9-CM code for the given trend}
  \item{description}{Descriptions of the ICD-9-CM code}
  \item{cp}{change-point for the given code}
  \item{b0}{intercept term for the given code}
  \item{b1}{slope parameter for the first period (before opportunity window)}
  \item{b2}{slope parameter for the second period (during opportunity window)}
}
}
\usage{
synth_fits_7day
}
\description{
This dataset contains fitted values to the trends in the synthetic visit count
data (`synth_dx_counts`), using 7 days intervals to bin visits. The trends are
fitted to the normalized frequency counts (i.e., % of visits each day for a corresponding
diagnosis code)
}
\keyword{datasets}
