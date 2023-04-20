
# Functions to export binary results to flat files that can be imported in R.

##########
### export_tdt
##########

#' Extract fiber photometry from binary files using the TDT Matlab SDK
#'
#' Runs Matlab functions to export either all or downsampled data from the binary TDT files.
#' Requires a valid Matlab path !
#'
#' @param path The full paths to the binary files
#' @param id The id that will be used to name the export file. Typically should be also in teh input file name but does not have to !
#' @param return_cached Defaults to TRUE. If TRUE will not run Matlab export if a '_export.txt' file with the provided id already exists in outputpath
#' @param outputpath Where to store the data. Defaults to temp dir of session
#' @param matlab_path The Matlab installaition on your system ! Defaults to the MPI beegfs one.
#' @param export_full Defaults to FALSE: Will downsample in matlab by taking the mean over downsample_freq data points. If TRUE: export every data point. This is much slower, because a much bigger files has to be written to disk from Matlab.
#' @param downsample_freq downsample in matlab by taking the mean over downsample_freq data points
#' @param channel_names the channel names in the TDT binary files (required by Matlab function). Default to "x465A","x405A"
#' @param tdt_lib_path only for debugging and custom usage. Defaults to NULL (use package system path).
#' @param matlab_fun_path only for debugging and custom usage. Defaults to NULL (use package system path).
#' @param verbose whether to print messages
#' @param print_full_matlab_output whether to print the full matlab command line output
#'
#' @return the file name of the exported data as a character string
#'
#' @export
#'
#'

export_tdt = function(path, id,return_cached = TRUE, outputpath = tempdir(),matlab_path = "/beegfs/bin/matlab_2014b" ,export_full = FALSE,downsample_freq = 1000,channel_names = c("x465A","x405A"),
                      tdt_lib_path = NULL,matlab_fun_path =NULL,verbose =TRUE, print_full_matlab_output = FALSE){

  # Check input:
  if(export_full & outputpath == tempdir()){stop("Please specifiy a permanent outputpath when exporting the full data from Matlab!")}
  if(!base::dir.exists(outputpath)){stop("Cannot find outputpath!")}
  if(!base::dir.exists(path)){stop("Cannot find path (input data)!")}
  if(length(base::list.files(path = path,pattern = ".Tbk"))<1){stop("Cannot find .Tbk file in path!")}

  # tdt_lib_path
  if(is.null(tdt_lib_path)){
    #tdt_lib_path = "/beegfs/scratch/bruening_scratch/lsteuernagel/projects/fibeR/inst/SDK"
    tdt_lib_path = system.file("SDK", package = "fibeR")#"inst/SDK/"
  }else{
    if(!base::dir.exists(tdt_lib_path)){stop("Cannot find TDT SDK path! Provide NULL to use the version included in the package.")}
  }
  # matlab_fun_path
  if(is.null(matlab_fun_path)){
    #matlab_fun_path = "/beegfs/scratch/bruening_scratch/lsteuernagel/projects/fibeR/inst/Matlab/"
    matlab_fun_path = system.file("Matlab", package = "fibeR")#"inst/SDK/"
  }else{
    if(!base::dir.exists(matlab_fun_path)){stop("Cannot find Matlab functions.")}
  }

  # define outputpath for Matlab with id
  outputpath = gsub("//","/",paste0(outputpath,"/"))
  # paste channel names in correct format
  channels_matlab = paste0("{'",paste0(channel_names,collapse = "','"),"'}")
  # define file name of output
  if(export_full){outputfile = paste0(outputpath,id,"_export.txt")}else{outputfile = paste0(outputpath,id,"_export_small.txt")}


  if(return_cached & file.exists(outputfile)){
    if(verbose){message("export_tdt: Found existing export file for this id in outputpath. Not running Matlab. Set return_cached to FALSE to overwrite this behavior.")}
  }else{
    if(!file.exists(matlab_path)){stop("Cannot find file in specified matlab path!")}
    if(!base::dir.exists(tdt_lib_path)){stop("Cannot find SDK path (should be included in package)!")}

    if(verbose){message("export_tdt: Running Matlab tdt export ... ")}
    # run Matlab
    if(export_full){matlab_fun = "tdt_export"}else{matlab_fun ="tdt_export_small"}
    #define command:
    command = as.character(paste0(matlab_path,
                                  " -nodisplay -nodesktop -nojvm -r \"cd '",matlab_fun_path,"/'; try ",matlab_fun,"('",
                                  paste0(path),"','",
                                  outputpath,"','",
                                  tdt_lib_path,"','",
                                  id,"',",
                                  channels_matlab,",",
                                  downsample_freq,"); catch; end; quit\""))
    output = system(command,intern = TRUE)
    if(grepl(pattern = "read from",x = output[12])){
      if(verbose){message("Completed export: ", output[12])}
    }else{
      if(verbose){message("Warning: Matlab export potentially failed!")}
    }
    if(print_full_matlab_output){
      message("Full Matlab output: ")
      message(output)
    }
  }
  return(outputfile)

}

##########
### parseNotes
##########

#' Parses lines from Notes.txt
#'
#' Parse notes file
#'
#' @param lines_from_notes Character Vector of readLines output
#' @param expected_id String. The expected id of the animal
#' @param verbose whether to print messages
#' @return data.frame with formatted note information
#'
#' @export
#'
#' @import stringr
#'

parseNotes = function(lines_from_notes,expected_id="",verbose=TRUE){
  if(expected_id != gsub("Subject: ","",lines_from_notes[grepl("Subject",lines_from_notes)])){
    if(verbose){message("Warning: Id in notes differs from expected id")}
  }
  # extract start and stop time and convert to date
  start_time = gsub("Start: ","",stringr::str_extract(lines_from_notes[grepl("Start:",lines_from_notes)], "Start: [0-9]*:[0-9]*:[0-9]*[a-z]+ [0-9]*/[0-9]*/[0-9]*"))
  start_time = as.POSIXlt(start_time,format="%I:%M:%S%p %m/%d/%Y")
  stop_time = gsub("Stop: ","",stringr::str_extract(lines_from_notes[grepl("Stop:",lines_from_notes)], "Stop: [0-9]*:[0-9]*:[0-9]*[a-z]+ [0-9]*/[0-9]*/[0-9]*"))
  stop_time = as.POSIXlt(stop_time,format="%I:%M:%S%p %m/%d/%Y")
  date = format(start_time, "%m-%d-%Y")

  # extract note times and convert to date, then set to date (day and month) of start
  note_times = gsub("Note-[0-9]*: ","",stringr::str_extract(lines_from_notes[grepl("Note-[0-9]*:",lines_from_notes)], "Note-[0-9]*: [0-9]*:[0-9]*:[0-9]*[a-z]+"))
  note_times = as.POSIXlt(note_times,format="%I:%M:%S%p")
  note_times = as.POSIXct(base::sub("\\S+", as.character(date), as.character(note_times)),format="%m-%d-%Y %H:%M:%S")

  # extract not text
  note_texts = gsub("Note-[0-9]*: [0-9]*:[0-9]*:[0-9]*[a-z]+ ","",stringr::str_extract(lines_from_notes[grepl("Note-[0-9]*: [0-9]*:[0-9]*:[0-9]*",lines_from_notes)], "Note-[0-9]*: [0-9]*:[0-9]*:[0-9]*.*"))
  note_texts = gsub("\"","",note_texts)

  note_df = data.frame(note_id = 1:(length(note_texts)+2), note_date = c(start_time,note_times,stop_time), text = c("start",note_texts,"stop"))
  note_df$note_time = as.numeric(note_df$note_date - note_df$note_date[1])

  return(note_df)
}


##########
### export_Notes
##########

#' Export Notes file to table formatted .txt file.
#'
#' Wrapper around parseNotes
#'
#' @param path The full paths to the binary files
#' @param id The id that will be used to name the export file. Typically should be also in the input file name but does not have to !
#' @param outputpath Where to store the data. Defaults to temp dir of session
#' @param verbose whether to print messages
#' @param return_cached don't overwrite existing files
#' @return filename of notes file as character string
#'
#' @export
#'
#' @importFrom data.table fwrite
#'
#'
#'

export_Notes = function(path,id, outputpath = tempdir(),verbose=TRUE, return_cached = TRUE){

  if(!base::dir.exists(outputpath)){stop("Cannot find outputpath!")}
  if(!base::dir.exists(path)){stop("Cannot find path (input data)!")}
  path = gsub("//","/",paste0(path,"/"))
  # notes
  if(file.exists(paste0(c(path,"Notes.txt"),collapse = ""))){
    notes_out_file =  paste0(c(outputpath,id,"_notes_parsed.txt"),collapse = "")
    if(return_cached & file.exists(notes_out_file)){
      if(verbose){message("export_tdt: Found existing note file for this id in outputpath. Not overwriting. Set return_cached to FALSE to overwrite this behavior.")}
    }else{
      if(verbose){message(paste0("export_Notes: Exporting notes "))}
      lines_from_notes=readLines(paste0(c(path,"Notes.txt"),collapse = ""))
      notes_parsed = parseNotes(lines_from_notes,id,verbose=verbose)
      data.table::fwrite(notes_parsed,file = notes_out_file,sep="\t")
    }
  }else{
    stop("Cannot find notes file in path.")
  }
  return(notes_out_file)
}

##########
###
##########

# find_matlab = function(dir = "/beegfs/bin/"){
#
#   # this is the simple check on our own system ...
#   if(file.exists("/beegfs/bin/matlab_2014b")){
#     matlab_path = "/beegfs/bin/matlab_2014b"
#   }else{
#     # this tries to actually find a suitable matlab installation
#     # all_matlab_files = base::list.files(dir,pattern = "matlab_[0-9]+", recursive=TRUE, full.names=TRUE) # too slow
#   }
#   return(matlab_path)
# }

# import_tdt_R = function(filename){
#
#
# }


