% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main_functions.R
\name{save_fibeR_batch}
\alias{save_fibeR_batch}
\title{Batch save a list of fibeR_data objects}
\usage{
save_fibeR_batch(
  fibeR_list,
  batch_output_path,
  showProgress = TRUE,
  verbose = FALSE
)
}
\arguments{
\item{fibeR_list}{a list of fibeR_data objects to save}

\item{batch_output_path}{the full path to the top level directory}

\item{showProgress}{logical. Whether to display progress bar}

\item{verbose}{logical. Whther to print  messages}
}
\value{
list of fibeR_data objects
}
\description{
Wrapper around save_fibeR to save a list of fibeR_data objects
}
