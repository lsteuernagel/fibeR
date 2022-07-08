

# Functions to export binary results to flat files that can be imported in R.

##########
### import_fibeR
##########

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
  class(fibeR_sample) <- "fibeR_data"

  # return
  return(fibeR_sample)
}

##########
### load_fibeR
##########

# load_fibeR = function(input_path, sample_id = NULL){
#
#
# }


##########
### process_fibeR
##########

#' Process a fibeR sample
#'
#' Make sure start_note it set correct!
#' Pre-processes the data and then estimates dFF using a baseline calculate on the data until intervention. If you want to calculate on all data provide start_note = "stop" .
#'
#' Details on dFF:
#' dFF = (signal - signalBase) / signalBase
#' Currently there are three approaches:
#'
#' median: Take the median of the baseline and use as signalBase
#'
#' fit: Apply linear fit with lm according to Lerner et al. This approach ignores the intervention time point.
#'
#' decay: Fit a negative exponential decay to estimate baseline and then use the extrapolated data as signalBase. If model fit fails it falls back to median.
#'
#'
#' @param fibeR_input matrix or dataframe OR a list. If list has to have the standard fibeR format
#' @param name_signal character string with the name of the signal column in fibeR_input. Defaults to the MPI standard: 'x465A'
#' @param name_control character string with the name of the control column in fibeR_input. Defaults to the MPI standard: 'x405A'
#' @param downsample_data boolean: Whether to downsample data. Defaults to FALSE, because in most cases the raw.data should already be downsampled in Matlab. If FALSE but data longer than 1M rows, will bet set to TRUE.
#' @param downsample_k how many points to downsample. 1000 roughly gives a data point per second (for standard MPI data)
#' @param verbose boolean.: return messages
#' @param cutoff_start how many seconds to cut off at start
#' @param cutoff_end how many seconds to cut off at end
#' @param start_note which note gives the timepoint of intervention. Either a integer index or a character giving the exact note text. This is important to determine when the intervention happened ! Defaults to 2 (1 being the automatic start node from tdt).
#' @param intervention_second_fallback a fallback if not start_node can be found: Use this many seconds as baseline. Defaults to 10 minutes (600 seconds)
#' @param reduce_for_comparability boolean (Default: TRUE): the relative timeline in the processed data is rounded to full seconds. If two downsampled data points fall into the same second, this removes one of them, which allows for esier alignment with other data.
#' @param remove_intervals boolean (Default: FALSE): Tries to remove longer intervals where the laser was off and adjust the relative timeline by also cutting out this time
#' @param dff_method vector of character strings: which method to use for dFF calculation. Supports three strings at the moment: "median","fit", "decay". Can provide any or all of them.
#' @param correct_with_control boolean (Default: TRUE): for median and decay: Subtract control to correct for motion artifcats
#' @inheritParams fit_decay_power
#' @return Downsampled matrix with the first column being the time index and other columns being downsampled signals
#'
#' @export
#'
#' @import magrittr dplyr
#' @importFrom stats median lm predict coef
#' @importFrom methods is
#'

process_fibeR = function(fibeR_input,name_signal = "x465A",name_control = "x405A",downsample_data = FALSE,downsample_k=1000,verbose=TRUE,cutoff_start = 10,cutoff_end = 5,
                         start_note = 2,intervention_second_fallback = 600,reduce_for_comparability =TRUE,remove_intervals=FALSE,
                         dff_method = c("decay","median","fit"),correct_with_control=TRUE, pct_fallback = 0.3, b_val = 10){

  # check if input is fibeR_data
  if(!is(fibeR_input,"fibeR_data")){
    if(verbose){message("Warning: fibeR_input is not an object of class fibeR_data!")}
    if(! (is(fibeR_input,"matrix") | is(fibeR_input,"data.frame"))){
      stop("Please prvode wither a an object of class fibeR_data, a matrix or a data.frame with the raw data as input to fibeR_input.")
    }else{
      # make result list
      fibeR_input = list(
        id = "sample",
        raw.data = fibeR_input,
        processed.data = NULL,
        notes = data.frame()
      )
      class(fibeR_input) <- "fibeR_data"
    }
  }
  if(verbose){message("Processing fibeR_input...")}

  # check $raw.data
  if(is.null(fibeR_input$raw.data)){stop("raw.data is NULL.")}
  if(colnames(fibeR_input$raw.data)[1] != "time"){stop("First column of raw.data has to be named time.")}
  if(nrow(fibeR_input$raw.data) < 2){stop("raw.data does not contain suffcient rows.")}

  # extract raw.data
  raw.data = fibeR_input$raw.data
  if(!downsample_data & nrow(raw.data) > 1000000){
    if(verbose){message("Warning: fibeR_input data has more than 1,000,000 data points but downsample = FALSE. Setting downsample to TRUE.")}
    downsample = TRUE
  }
  # downsample
  if(downsample_data){
    raw.data = downsample(raw.data,k=downsample_k)
    raw.data = as.data.frame(raw.data)
  }
  # delete cutoff_end from the end
  process.data = raw.data[raw.data$time < max(raw.data)-cutoff_end & raw.data$time > cutoff_start,]

  # get notes:
  notes = fibeR_input$notes
  rownames(notes) = notes$text
  # check if start node can be found
  if(nrow(notes)<=2){
    if(verbose){message("Notes are empty or only contain start and end time. Using intervention_second_fallback.")}
    start_time_seconds = intervention_second_fallback
  }
  if(is.numeric(start_note)){
    if(start_note >= nrow(notes)){
      if(verbose){message("Warning: Cannot find start_note in notes. Using intervention_second_fallback.")}
      start_time_seconds = intervention_second_fallback
    }else{
      start_time_seconds = notes[start_note,"note_time"]
    }
  }
  if(is.character(start_note)){
    if(!start_note %in% rownames(notes)){
      if(verbose){message("Warning: Cannot find start_note in notes. Using intervention_second_fallback.")}
      start_time_seconds = intervention_second_fallback
    }else{
      start_time_seconds = notes[start_note,"note_time"][1]
    }
  }

  # add to object
  fibeR_input$intervention_seconds = start_time_seconds

  # Add a relative timeline
  time_from_intervention = floor(process.data$time - start_time_seconds)

  # reorder
  temp = data.frame(time = process.data$time, time_from_intervention = time_from_intervention) # , time_from_intervention_integer = time_from_intervention_integer
  process.data = cbind(temp,process.data[,2:ncol(process.data)]) %>% as.data.frame()

  # distinct rounded second
  if(reduce_for_comparability){
    n_occ = table(process.data$time_from_intervention)
    #  This allows for easy comparison other samples.
    if(verbose){message("Removing ",length(n_occ[n_occ >= 2])," data points to reduce the data to one point per (rounded) second. Set reduce_for_comparability to FALSE to disable this behavior.")}
    process.data = process.data %>% dplyr::distinct(time_from_intervention,.keep_all = TRUE) %>% as.data.frame()
  }

  # if start not is smaller than cutoff seconds: skip
  if(start_time_seconds <= cutoff_start){
    stop("Cannot process sample: start_time_seconds <= cutoff_start!")
  }

  #Remove intervals (if laser was turned off)
  if(remove_intervals){
    # remove intervals by checking where signal or control are close to 0 mV (or much lower than expected)
    length_before = nrow(process.data)
    additional_cut_length = 5
    process.data = reduce_fiberdata(process.data,
                                    name_c1 = name_signal,
                                    name_c2 = name_control,
                                    max_value_c1 = max(process.data[,name_signal])*0.05,
                                    max_value_c2 = max(process.data[,name_control])*0.05,
                                    additional_cut_length=additional_cut_length,
                                    index_column = "time_from_intervention")
    if(verbose){message(paste0("Removed: ",length_before-nrow(process.data)," values where laser was off."))}
  }

  # dFF
  # dff_method = c("decay","median","fit")
  dff_method = dff_method[ dff_method %in% c("decay","median","fit") ]
  if(length(dff_method) == 0){if(verbose){message(paste0("Warning: Cannot find valid dff_method for dFF calculation. Using 'median'."))};dff_method = "median"}
  # go over all approaches:

  ## get the median baseline for signal dFF:
  median_signal_baseline = stats::median(process.data[process.data$time_from_intervention > 0, name_signal])
  median_control_baseline = stats::median(process.data[process.data$time_from_intervention > 0, name_control])

  # make df with base line info
  baseline.data = process.data[,1:2]
  baseline.data$median_signal = median_signal_baseline
  baseline.data$median_control = median_control_baseline

  # use  median
  if("median" %in% dff_method ){
    if(verbose){message("dFF: Estimate median")}
    # calculate relative dFF with median
    signal_relative_median = (process.data[,name_signal]-median_signal_baseline)/median_signal_baseline
    control_relative_median = (process.data[,name_control]-median_control_baseline)/median_control_baseline
    # subtract to correct artifacts
    if(correct_with_control){
      signal_relative_median = signal_relative_median - control_relative_median
    }
    process.data$dFF_median = signal_relative_median
  }

  # lerner et al fit
  if( "fit" %in% dff_method){
    if(verbose){message("dFF: Fitting control to signal")}
    # fit data
    lm_fitted=stats::lm(process.data[,name_signal] ~ process.data[,name_control])
    control_fit_all = lm_fitted$coefficients[2]*process.data[,name_control] + lm_fitted$coefficients[1]
    baseline.data$fit_control = control_fit_all
    # calculate dF/F, 0 for control in this case
    signal_relative_fit = (process.data[,name_signal] - control_fit_all)/control_fit_all
    process.data$dFF_fit = signal_relative_fit
  }

  # fit a power like model to estimate bleaching with an 'exponential decay' and use that as baseline for dFF
  if("decay" %in% dff_method){
    if(verbose){message("dFF: Estimate exponetial decay")}
    ## get the decay baseline for signal dFF:
    decay_power_model_signal=fit_decay_power(process.data[process.data$time_from_intervention < 0, name_signal],
                                             smooth_with_butter=TRUE,
                                             b_val = 10,
                                             verbose = 0,
                                             pct_fallback = 0.3)$fit_model
    if(length(decay_power_model_signal)>0){
      predicted_signal_baseline_power = as.numeric(stats::predict(decay_power_model_signal,
                                                           newdata=data.frame(x=process.data$time_from_intervention-min(process.data$time_from_intervention))))
    }else{
      # if model failed: use median
      predicted_signal_baseline_power = median_signal_baseline
    }
    # calculate relative
    signal_relative_decay_power = (process.data[, name_signal]-predicted_signal_baseline_power)/predicted_signal_baseline_power

    ## get the decay_power baseline for control dFF:
    decay_power_model_control =fit_decay_power(process.data[process.data$time_from_intervention < 0, name_control],
                                               smooth_with_butter=TRUE,
                                               b_val = 10,
                                               verbose = 0,
                                               pct_fallback = 0.3)$fit_model
    if(length(decay_power_model_control)>0){
      if(length(predicted_signal_baseline_power)==1){
        predicted_control_baseline_power = median_control_baseline # if signal failed, also use median here
      }else{
        predicted_control_baseline_power = as.numeric(stats::predict(decay_power_model_control,
                                                              newdata=data.frame(x=process.data$time_from_intervention-min(process.data$time_from_intervention))))
      }
    }else{
      # if model failed: use median
      predicted_control_baseline_power = median_control_baseline
      predicted_signal_baseline_power = median_signal_baseline   # use median for signal as well
    }
    # add to baseline.data
    baseline.data$decay_signal = predicted_signal_baseline_power
    baseline.data$decay_control = predicted_control_baseline_power

    # calculate relative
    control_relative_decay_power = (process.data[, name_control]-predicted_control_baseline_power)/predicted_control_baseline_power

    # subtract to correct artifacts
    if(correct_with_control){
      signal_relative_decay_power = (process.data[, name_signal]-predicted_signal_baseline_power)/predicted_signal_baseline_power # re run because maybe it changed to median!
      signal_relative_decay_power = signal_relative_decay_power - control_relative_decay_power # subtraction step
    }
    # add to  process.data
    process.data$dFF_decay = signal_relative_decay_power
  }

  # update process.data
  fibeR_input$process.data = process.data
  fibeR_input$baseline.data = baseline.data

  if(verbose){message("Processing complete.")}
  # return
  return(fibeR_input)

}
