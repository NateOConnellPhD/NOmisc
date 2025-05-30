% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/remove_na_cols.R
\name{remove_na_cols}
\alias{remove_na_cols}
\title{Remove columns from dataset with all NA or blank spaces}
\usage{
remove_na_cols(df)
}
\arguments{
\item{df}{dataframe containing some missing columns}
}
\value{
A dataframe without the missing columns
}
\description{
This function's input is a dataframe. The function returns a cleaned dataframe
removing all columns with all NAs (no informative information)
}
