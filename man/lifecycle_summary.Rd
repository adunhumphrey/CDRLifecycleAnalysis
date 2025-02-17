% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Lifecylcedata.R
\name{lifecycle_summary}
\alias{lifecycle_summary}
\title{Summarize Lifecycle Emissions Data}
\usage{
lifecycle_summary(data)
}
\arguments{
\item{data}{A data frame containing lifecycle emissions data.}
}
\value{
A summarized data frame grouped by technology.
}
\description{
This function provides a summary of lifecycle emissions, net removal efficiency,
and NCRR for each CDR technology in the dataset.
}
\examples{
# Example usage:
lifecycle_summary(CDR_Lifecycle)
}
