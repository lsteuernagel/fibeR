

# Functions to export binary results to flat files that can be imported in R.

#' Import fiber photometry data into R via Matlab
#'
#' Runs Matlab functions to export either all or downsampled data from the binary TDT files.
#' This is a wrapper function that calls export_tdt and export_Notes to export TDT binary files to .txt flat files and then imports these results to R.
#' The intermediary exports are saved to a tempdir (cached for the session, so that when re-reading Matlab has not to be called twice). If you want to export them permanently, specify an existing directory in outputpath.
#' TODO: If you have previously processed data with fibeR and saved the results you can alternatively use the function load_fibeR to skip Matlab export/import.
#'
#' @param input_path The full path to the binary files from TDT.
#' @param sample_id The id that will be used. Defaults to NULL which will use the name of the .Tbk file in the specified 'path'
#' @param id_infererence Only used if sample_id is NULL. Then 'pathname' will use the lowest level of path as id (default). 'tbkfile' will use the name of the tbk file in path as id. Anything else will assign a random number as id.
#' @inheritParams export_tdt
#' @return the imported data of the sample
#'
#' @export
#'
#' @importFrom utils read.table
#'
#'

import_fibeR = function(input_path, sample_id = NULL,id_infererence="pathname", outputpath = tempdir(),matlab_path = "/beegfs/bin/matlab_2014b", export_full = FALSE,downsample_freq = 1000,channel_names = c("x465A","x405A"),verbose =TRUE){

  # check that input_path exists (will also be checked by export_tdt, just to be sure that we can define sample id etc. )
  if(!base::dir.exists(input_path)){stop("Cannot find path (input data)!")}

  # check that tbk exists (will also be checked by export_tdt, just to be sure that we can define sample id etc. )
  tbk_file = base::list.files(path = input_path,pattern = ".Tbk")
  if(length(tbk_file)<1){stop("Cannot find .Tbk file in path!")}

  # check sample_id
  if(is.null(sample_id)){
    if(id_infererence=="pathname"){
      sample_id = base::strsplit(input_path,split="/")[[1]][length(strsplit(input_path,split="/")[[1]])]
      if(verbose){message("import_fibeR: Using last subfolder as id: ",sample_id)}
    }else if(id_infererence=="tbkfile"){
      # get from tbk file name
      sample_id = gsub(".Tbk","",tbk_file[1])
      if(verbose){message("import_fibeR: Using tbk file name as id: ",sample_id)}
    }else{
      sample_id = paste0("id_",sample(1:100000000,1))
      if(verbose){message("import_fibeR: Using random number as id: ",sample_id)}
    }

  }

  # execute export_tdt
  export_tdt_result_file = export_tdt(path = input_path,
                                      id = sample_id,
                                      return_cached = TRUE,
                                      outputpath = outputpath,
                                      matlab_path = matlab_path,
                                      export_full = export_full,
                                      downsample_freq = downsample_freq,
                                      channel_names = channel_names,
                                      verbose =verbose)

  # execute note export:
  export_tdt_note_file = export_Notes(path = input_path,
                                      id = sample_id,
                                      outputpath = outputpath,
                                      verbose=verbose)
  # import results into R
  raw.data = utils::read.table(export_tdt_result_file,header = FALSE)
  colnames(raw.data) = c("time",channel_names)
  notes = utils::read.table(export_tdt_note_file,header = TRUE)

  # make result list
  fibeR_sample = list(
   id = sample_id,
   raw.data = raw.data,
   processed.data = NULL,
   notes = notes
  )

  # return
  return(fibeR_sample)
}
