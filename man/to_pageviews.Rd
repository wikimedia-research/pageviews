% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/filter.R
\name{to_pageviews}
\alias{to_pageviews}
\title{filter a sampled log file to pageviews}
\usage{
to_pageviews(data)
}
\arguments{
\item{data}{a data.frame returned from \code{\link{read_sampled_log}}.}
}
\value{
a data.frame with the same attributes as that of \code{\link{read_sampled_log}},
but only containing "pageviews".
}
\description{
\code{is_pageview} accepts a sampled log file, read
with \code{\link{read_sampled_log}}, and filters it to those rows
that meet the existing definition of "pageview", which is described
\href{https://meta.wikimedia.org/wiki/Research:Page_view}{here}.
}
\examples{
#Get the latest day's pageviews
files <- get_files()
data <- to_pageviews(read_sampled_log(files[length(files)]))
}
\seealso{
\code{\link{read_sampled_log}}, for retrieving the
sampled logs in the first place.
}

