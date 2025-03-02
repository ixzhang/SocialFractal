% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gvf.R
\name{gvf}
\alias{gvf}
\title{Calculate the GVF metric (Goodness of Variance Fit)}
\usage{
gvf(data, num_breaks)
}
\arguments{
\item{data}{A numeric vector of observed values.}

\item{num_breaks}{An integer specifying the number of groups to
classify the data into.}
}
\value{
The GVF metric value, which ranges from 0 to 1.
A higher value indicates a better fit.
}
\description{
This function computes the GVF metric, which measures the goodness of
fit between observed data and a classification into predefined groups
using Jenks breaks. It calculates the proportion of variance explained
by the grouped data compared to the original data.
}
\examples{
# Example usage:
data <- rnorm(100, mean = 0, sd = 1) # Generate a numeric vector
gvf_result <- gvf(data, num_breaks = 5)
# Output the result
}
