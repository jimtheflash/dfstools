% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy_columns.R
\name{add_tidy_position}
\alias{add_tidy_position}
\title{Append column of tidy player positions}
\usage{
add_tidy_position(df, lookup = NULL, platform = NULL, sport = NULL)
}
\arguments{
\item{df}{data.frame with position column}

\item{platform}{character, which dfs platform to use}

\item{sport}{character, which sport}
}
\value{
data.frame with column "tidy_position" appended for easier joining
}
\description{
Append column of tidy player positions
}
