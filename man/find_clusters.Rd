% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cluster_functions.R
\name{find_clusters}
\alias{find_clusters}
\title{Find clusters}
\usage{
find_clusters(fit_data, normalize = TRUE, k = 5, nstart = 50, iter.max = 50)
}
\arguments{
\item{fit_data}{a dataset of fitted trend values}

\item{normalize}{a logical indicator of whether to normalize the model parameter
values (default is TRUE)}

\item{k}{number of clusters to find (default is k = 5)}

\item{nstart}{if centers is a number, how many random sets should be chosen?}

\item{iter.max}{the maximum number of iterations allowed.}
}
\description{
Main function to find clusters from a dataset of fitted values using k-means
}
