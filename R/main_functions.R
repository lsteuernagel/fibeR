
# Functions to import / save / load and prcoess data.


# functions to ADD.
# make meta_data

##########
### import_fibeR
##########

#' Import fiber photometry data into R via Matlab
#'
#' Runs Matlab functions to export either all or downsampled data from the binary TDT files.
#' This is a wrapper function that calls export_tdt and export_Notes to export TDT binary files to .txt flat files and then imports these results to R.
#' The intermediary exports are saved to a tempdir (cached for the session, so that when re-reading Matlab has not to be called twice). If you want to export them permanently, specify an existing directory in outputpath.
#' If you have previously processed data with fibeR and saved the results you can alternatively use the function \link[fibeR]{load_fibeR} to skip Matlab export/import.
#'
#' @param input_path The full path to the binary files from TDT.
#' @param sample_id The id that will be used. Defaults to NULL which will infer the id (see id_infererence).
#' @param id_infererence Only used if sample_id is NULL. Then 'pathname' will use the lowest level of path as id (default). 'tbkfile' will use the name of the tbk file in path as id. Anything else will assign a random number as id.
#' @inheritParams export_tdt
#' @return the imported data of the sample
#'
#' @export
#'
#' @importFrom data.table fread
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
      if(sample_id != ""){
        if(verbose){message("import_fibeR: Using last subfolder as id: ",sample_id)}
      }else{
        sample_id = gsub(".Tbk","",tbk_file[1])
        if(verbose){message("import_fibeR: Cannot infer from pathname. trying tbk file instead: ",sample_id)}
      }
    }else if(id_infererence=="tbkfile"){
      # get from tbk file name
      sample_id = gsub(".Tbk","",tbk_file[1])
      if(sample_id != ""){
        if(verbose){message("import_fibeR: Using tbk file name as id: ",sample_id)}
      }else{
        sample_id = base::strsplit(input_path,split="/")[[1]][length(strsplit(input_path,split="/")[[1]])]
        if(verbose){message("import_fibeR: Cannot infer from tbk file. trying subfolder name instead: ",sample_id)}
      }
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
  raw.data = data.table::fread(export_tdt_result_file,header = FALSE,data.table = FALSE)
  colnames(raw.data) = c("time",channel_names)
  notes = data.table::fread(export_tdt_note_file,header = TRUE,data.table = FALSE)

  # make result list
  fibeR_sample = list(
    id = sample_id,
    raw.data = raw.data,
    process.data = NULL,
    notes = notes
  )
  class(fibeR_sample) <- "fibeR_data"

  # return
  return(fibeR_sample)
}

##########
### import_fibeR_batch
##########

#' Batch import fiber photometry data into R via Matlab
#'
#' Wrapper around import_fibeR to handle directories with multiple samples.
#'
#' @param batch_path the full path to the top level directory
#' @param batch_output_path where to store the data on disk. Defaults to a temp directory but it is recommended to set to a permanent location to speed up re-loading .
#' @param showProgress logical. Whether to display progress bar
#' @inheritParams import_fibeR
#' @param ... further parameters passed to \link[fibeR]{import_fibeR}
#' @return list of fibeR_data objects
#'
#' @export
#'
#' @importFrom utils txtProgressBar setTxtProgressBar
#'
#'

import_fibeR_batch = function(batch_path, batch_output_path = paste0(tempdir(),"/fibeR_batch/"), showProgress = TRUE, verbose =FALSE, id_infererence = "pathname",...){

  # mkdir batch_output_path
  system(paste0("mkdir -p ",batch_output_path))

  # list all tbk full paths in batch_path
  all_tbk_files = list.files(batch_path,pattern = ".Tbk$",full.names = TRUE,recursive = TRUE)

  # check that enough samples exist
  if(length(all_tbk_files)<1){stop("Cannot find any subdirectories with .Tbk files in ",batch_path)}

  # extract sample ids
  all_tbk_files = sapply(all_tbk_files,function(x){gsub("//","/",x)})
  if(id_infererence=="pathname"){nval = 1}else if(id_infererence=="tbkfile"){nval = 0}else{  nval = 0}
  sample_ids = sapply(all_tbk_files,function(x,n=0){base::strsplit(x,split="/")[[1]][length(strsplit(x,split="/")[[1]])-n]},n = nval)

  # format input folders:
  input_folders = sapply(all_tbk_files,function(x,n=1){paste0(base::strsplit(x,split="/")[[1]][1:(length(strsplit(x,split="/")[[1]])-n)],collapse = "/")})
  input_folders = sapply(input_folders,function(x){paste0(x,"/")})
  names(input_folders) = sample_ids

  # format output structure
  output_folders = sapply(all_tbk_files,function(x){gsub(batch_path,"",x)})
  output_folders = sapply(output_folders,function(x,n=2){paste0(base::strsplit(x,split="/")[[1]][1:(length(strsplit(x,split="/")[[1]])-n)],collapse = "/")})
  output_folders_full = sapply(output_folders,function(x,prefix){paste0(prefix,x,"/")},prefix=batch_output_path)
  names(output_folders_full) = sample_ids

  # init progress
  if(showProgress){ progress_bar = txtProgressBar(min = 0, max = length(input_folders), initial = 0,style = 3)}
  if(verbose){ message("Importing ",length(input_folders)," samples from ",batch_path)}
  # iterate over all samples:
  fiber_sample_list = list()
  # save them in batch_output_path using the same folder structure as found in batch_path
  for(i in 1:length(input_folders)){
    # clean up path:
    output_folders_full[i] = gsub(" ","_",output_folders_full[i])
    # and create
    system(paste0("mkdir -p ",output_folders_full[i]))
    # read:
    tmp_value = tryCatch({
      import_res = import_fibeR(input_path = input_folders[i],
                                sample_id = names(input_folders)[i],
                                outputpath = output_folders_full[i],
                                verbose = verbose,
                                ... )
      import_res
    },
    error=function(cond) {
      message("Error while importing sample : ",names(input_folders)[i],". Skipping . Error message: ",cond)
      return("ERROR")
    })#
    if(tmp_value != "ERROR"){
      fiber_sample_list[[i]] = tmp_value
      fiber_sample_list[[i]]$folder_name = output_folders[i]
      names(fiber_sample_list)[i] = fiber_sample_list[[i]]$id
    }
    # update progress
    if(showProgress){ setTxtProgressBar(progress_bar,i) }
  }


  return(fiber_sample_list)

}

##########
### load_fibeR
##########

#' Save an object of class fibeR_data
#'
#' Saves data.frame from a list-like fibeR_data to individual .txt tables on output_path.
#'
#' @param id character string with id
#' @param input_path path
#'
#' @export
#'
#' @importFrom data.table fread
#'


load_fibeR = function(id,input_path){

  all_files = list.files(path = input_path,pattern = id,recursive = TRUE,full.names = TRUE)
  all_files = all_files[grepl("\\.notes|raw|baseline|process\\.",all_files)]
  if(length(all_files) == 0){
    stop("Cannot find files with correct id in input_path.")
  }
  #print(all_files)
  # init
  fibeR_input = list(
    id = id
  )
  #read
  for(i in 1:length(all_files)){
    slot_name = sub(".","",gsub("/","",gsub(".txt","",gsub(id,"",gsub(input_path,"",all_files[i])))))
    #print(gsub("//","/",all_files[i] ))
    table1 = data.table::fread(file = gsub("//","/",all_files[i] ),header = TRUE)
    fibeR_input[[slot_name]] = table1
  }
  class(fibeR_input) <- "fibeR_data"
  return(fibeR_input)
}


##########
### load_fibeR_batch
##########

#' Batch load previously saved fiber photometry data into R
#'
#' Wrapper around load_fibeR to handle directories with multiple samples.
#'
#' @param batch_path the full path to the top level directory
#' @param verbose logical. Whther to print  messages
#' @return list of fibeR_data objects
#'
#' @export
#'
#' @importFrom data.table fread
#' @importFrom utils txtProgressBar setTxtProgressBar
#'
#'

load_fibeR_batch = function(batch_path, verbose =FALSE){

  # list all tbk full paths in batch_path
  all_raw_files = list.files(batch_path,pattern = ".raw.data.txt",full.names = TRUE,recursive = TRUE)

  # check that enough samples exist
  if(length(all_raw_files)<1){stop("Cannot find any subdirectories with .raw.txt files in ",batch_path)}

  # clean
  all_raw_files = sapply(all_raw_files,function(x){gsub("//","/",x)})

  sample_ids = sapply(all_raw_files,function(x,n=0){base::strsplit(x,split="/")[[1]][length(strsplit(x,split="/")[[1]])-n]})
  sample_ids = gsub(".raw.data.txt","",sample_ids)

  # format input folders:
  input_folders = sapply(all_raw_files,function(x,n=1){paste0(base::strsplit(x,split="/")[[1]][1:(length(strsplit(x,split="/")[[1]])-n)],collapse = "/")})
  input_folders = sapply(input_folders,function(x){paste0(x,"/")})
  names(input_folders) = sample_ids

  # init progress
  #if(showProgress){ progress_bar = txtProgressBar(min = 0, max = length(input_folders), initial = 0,style = 3)}

  # iterate over all samples:
  fiber_sample_list = list()
  # save them in batch_output_path using the same folder structure as found in batch_path
  for(i in 1:length(input_folders)){

    # read:
    fiber_sample_list[[i]] = load_fibeR(id = names(input_folders)[i], input_path = input_folders[i])
    fiber_sample_list[[i]]$folder_name = gsub(batch_path,"",input_folders[i])
    names(fiber_sample_list)[i] = fiber_sample_list[[i]]$id
    # update progress
    #if(showProgress){ setTxtProgressBar(progress_bar,i) }
  }


  return(fiber_sample_list)

}

##########
### save_fibeR
##########

#' Save an object of class fibeR_data
#'
#' Saves data.frame from a list-like fibeR_data to individual .txt tables on output_path.
#'
#' @param fibeR_data object of class fibeR_data
#' @param output_path path
#'
#' @export
#'
#' @importFrom data.table fwrite
#'
#'

save_fibeR = function(fibeR_data, output_path){
  if(!is(fibeR_data,"fibeR_data")){
    stop("Please provide a valid fibeR_data to save with this function.")
  }
  output_path = gsub("//","/",paste0(output_path,"/"))
  file_name_prefix = paste0(output_path,fibeR_data$id)
  parts_to_write = names(which(sapply(fibeR_data,is.data.frame)))
  for(i in 1:length(parts_to_write)){
    df_to_write = parts_to_write[i]
    file_name = paste0(file_name_prefix,".",df_to_write,".txt")
    # ensure that file name is not completely broken
    file_name = gsub(" |\\+","_",file_name)
    file_name = gsub("\\\n","",file_name)
    data.table::fwrite(fibeR_data[[df_to_write]],file = file_name,sep = "\t")
  }
}


##########
### save_fibeR_batch
##########

#' Batch save a list of fibeR_data objects
#'
#' Wrapper around save_fibeR to save a list of fibeR_data objects
#'
#' @param fibeR_list a list of fibeR_data objects to save
#' @param batch_output_path the full path to the top level directory
#' @param showProgress logical. Whether to display progress bar
#' @param verbose logical. Whther to print  messages
#' @return list of fibeR_data objects
#'
#' @export
#'
#' @importFrom data.table fread
#'
#'

save_fibeR_batch = function(fibeR_list,batch_output_path, showProgress = TRUE, verbose =FALSE){

  # make path
  system(paste0("mkdir -p ",batch_output_path))
  # init progress
  if(showProgress){ progress_bar = txtProgressBar(min = 0, max = length(fibeR_list), initial = 0,style = 3)}
  # iterate over all samples:
  fiber_sample_list = list()
  # save them in batch_output_path using the same folder structure as found in batch_path
  for(i in 1:length(fibeR_list)){

    fibeR_sample = fibeR_list[[i]]
    if(!is.null(fibeR_sample$folder_name)){
      full_path = paste0(batch_output_path,"/",fibeR_sample$folder_name[1],"/")
      system(paste0("mkdir -p ",full_path))
    }else{
      full_path = batch_output_path
    }
    full_path = gsub("//","/",full_path)
    # save:
    tryCatch({
      save_fibeR(fibeR_sample,full_path)
    },
    error=function(cond) {
      message("Cannot save_fibeR. Skipping . Error:",cond)
      return(NA)
    })
    # update progress
    if(showProgress){ setTxtProgressBar(progress_bar,i) }
  }
}

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
                         start_note = 2,intervention_second_fallback = 600,reduce_for_comparability =FALSE,remove_intervals=FALSE,
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
        process.data = NULL,
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
  median_signal_baseline = stats::median(process.data[process.data$time_from_intervention < 0, name_signal])
  median_control_baseline = stats::median(process.data[process.data$time_from_intervention < 0, name_control])

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
    decay_power_model_signal = fit_decay_power(process.data[process.data$time_from_intervention < 0, name_signal],
                                               smooth_with_butter=TRUE,
                                               b_val = 10,
                                               verbose = 0,
                                               pct_fallback = 0.3)$fit_model %>% suppressMessages()
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
                                               pct_fallback = 0.3)$fit_model %>% suppressMessages()
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


##########
### process_fibeR_batch
##########

#' Batch save a list of fibeR_data objects
#'
#' Wrapper around save_fibeR to save a list of fibeR_data objects
#'
#' @param fibeR_list a list of fibeR_data objects to save
#' @param showProgress logical. Whether to display progress bar
#' @param start_note_all one start note index for all (cannot handle differing startnotes at the moment !)
#' @param verbose logical. whether to print messages (mostly from process_fibeR)
#' @param ... further parameters passed to \link[fibeR]{process_fibeR}
#' @return list of fibeR_data objects
#'
#' @export
#'
#' @importFrom data.table fread
#' @importFrom utils txtProgressBar setTxtProgressBar
#'

process_fibeR_batch = function(fibeR_list,showProgress =TRUE,start_note_all = 2,verbose=FALSE, ...){

  if(showProgress){ progress_bar = txtProgressBar(min = 0, max = length(fibeR_list), initial = 0,style = 3)}
  # iterate over all samples:
  for(i in 1:length(fibeR_list)){

    # get sample
    fiber_sample = fibeR_list[[i]]
    # process:
    tmp_value = tryCatch({
      process_res = process_fibeR(fiber_sample,
                                  start_note = start_note_all,
                                  verbose=verbose,
                                  ...)
      process_res
    },
    error=function(cond) {
      message("Error while processing sample : ",fiber_sample$id,". Skipping . Error message: ",cond)
      return("ERROR")
    })#
    if(tmp_value != "ERROR"){
      fibeR_list[[i]] = tmp_value
    }

    # update progress
    if(showProgress){ setTxtProgressBar(progress_bar,i) }

  }
  return(fibeR_list)

}

##########
### align_fibeR
##########

#' Align all fibeR samples in one list to the same timeline.
#'
#' This functions makes use of the zoo package to align samples in one matrix (for each data type). \link[fibeR]{process_fibeR} already adds an integer time value centered on the intervention time point, which is used by this function!.
#'
#' @param fibeR_list a list of fibeR_data objects to save
#' @param columns_to_align  one or more of 'signal', 'control', 'decay', 'median', 'fit' . Nothing else will work ! (These are the standard columns in processed data. If the signal ist still named x465A (or whatever you specify in name_signal) in process.data it will be automatically renamed.).
#' @param input_data_name  dataframe with data for alignment. Only change if you know what you are doing !
#' @param name_signal character string with the name of the signal column in fibeR_input. Defaults to the MPI standard: 'x465A'
#' @param name_control character string with the name of the control column in fibeR_input. Defaults to the MPI standard: 'x405A'
#' @param verbose logical. whether to print messages (mostly from process_fibeR)
#' @return aligned matrix or list of multiple aligned matrices
#'
#' @export
#'
#' @import magrittr zoo
#' @importFrom dplyr rename
#' @importFrom rlang sym
#'
#'

align_fibeR = function(fibeR_list, columns_to_align = c("decay"), input_data_name = "process.data",name_signal = "x465A",name_control = "x405A",verbose=FALSE){

  # init main list (of lists):
  aligned_colum_list = list()


  ## for each sample:
  for(i in 1:length(fibeR_list)){

    fiber_sample = fibeR_list[[i]]
    current_id = fibeR_list[[i]]$id
    columns_to_align_sample = columns_to_align

    # todo check on
    # check object and
    if(!is(fiber_sample,"fibeR_data")){next}
    if(is.null(fiber_sample[[input_data_name]])){next}

    # get data
    if(name_signal %in% colnames(fiber_sample[[input_data_name]]) & name_control %in% colnames(fiber_sample[[input_data_name]]) ){
      data_as_input = fiber_sample[[input_data_name]] %>% dplyr::rename(signal = !!rlang::sym(name_signal),control = !!rlang::sym(name_control)  ) %>% as.data.frame()
    }else{
      data_as_input = fiber_sample[[input_data_name]] %>% as.data.frame()
    }
    # if data frame is empty: skip
    if(nrow(data_as_input) < 5){next}

    # reduce to valid columns
    if( input_data_name == "process.data"){
      columns_to_align_sample[! columns_to_align_sample %in% c("signal","control")] = paste0("dFF_",columns_to_align_sample[!columns_to_align_sample%in% c("signal","control")])
    }
    columns_to_align_sample = columns_to_align_sample[columns_to_align_sample %in% colnames(data_as_input)]
    # subset to valid columns
    data_as_input = data_as_input[,c("time_from_intervention",columns_to_align_sample),drop=FALSE]

    # ensure that time_from_intervention is distinct
    data_as_input = data_as_input %>% dplyr::distinct(time_from_intervention,.keep_all = TRUE)

    #convert target columns to zoo amd add to list
    for(col in columns_to_align_sample){
      zoo_temp = zoo::zoo(data_as_input[,col],data_as_input$time_from_intervention)
      aligned_colum_list[[paste0("results_",col,"_list")]][[current_id]] = zoo_temp
    }
  }

  ## make matrices:
  matrix_list = list()
  for(col in columns_to_align_sample){
    current_list = aligned_colum_list[[paste0("results_",col,"_list")]]
    # check for corrupted time series
    corr_index=sapply(current_list,function(x){zoo::index(x)})
    keep_idx = which(!is.na(corr_index))
    if(length(keep_idx) < length(corr_index)){message("Warning: Dropping ",length(corr_index) - length(keep_idx)," samples with corrupted time index")}
    # remove
    current_list = current_list[keep_idx]
    # make df:
    matrix_list[[col]] = as.data.frame(do.call(merge,current_list))
    # current_result$time_from_intervention = rownames(current_result)
    # current_result = current_result[,c(ncol(current_result),1:(ncol(current_result)-1))]
  }

  # return
  if(length(matrix_list)==1){
    return(matrix_list[[1]])
  }else{
    return(matrix_list)
  }

}


