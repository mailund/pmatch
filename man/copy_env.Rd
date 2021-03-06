% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bind.R
\name{copy_env}
\alias{copy_env}
\title{Move the bound variables from one environment into another.}
\usage{
copy_env(from, to, names = ls(from, all.names = TRUE))
}
\arguments{
\item{from}{The environment we want to copy from.}

\item{to}{The environment where we want to bind the variables.}

\item{names}{Names of the variables to copy. By default, all of them.}
}
\description{
Move the bound variables from one environment into another.
}
