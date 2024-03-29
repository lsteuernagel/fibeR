% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main_functions.R
\name{process_fibeR_batch}
\alias{process_fibeR_batch}
\title{Batch save a list of fibeR_data objects}
\usage{
process_fibeR_batch(
  fibeR_list,
  showProgress = TRUE,
  start_note_all = 2,
  verbose = FALSE,
  ...
)
}
\arguments{
\item{fibeR_list}{a list of fibeR_data objects to save}

\item{showProgress}{logical. Whether to display progress bar}

\item{start_note_all}{one start note index for all (cannot handle differing startnotes at the moment !)}

\item{verbose}{logical. whether to print messages (mostly from process_fibeR)}

\item{...}{further parameters passed to \link[fibeR]{process_fibeR}}
}
\value{
list of fibeR_data objects
}
\description{
Wrapper around save_fibeR to save a list of fibeR_data objects
}
