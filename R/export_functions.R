





# function that exctracts from Matlab

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
#' @param verbose whether to print messages
#'
#' @return the file name of the exported data as a character string
#'
#' @export
#'
#'

export_tdt = function(path, id,return_cached = TRUE, outputpath = tempdir(),matlab_path = "/beegfs/bin/matlab_2014b", export_full = FALSE,downsample_freq = 1000,channel_names = c("x465A","x405A"),verbose =TRUE){

  # Check input:
  if(export_full & outputpath == tempdir()){stop("Error in export_tdt: Please specifiy a permanent outputpath when exporting the full data from Matlab!")}
  if(!base::dir.exists(outputpath)){stop("Error in export_tdt: Cannot find outputpath!")}
  if(!base::dir.exists(path)){stop("Error in export_tdt: Cannot find path (input data)!")}
  if(length(base::list.files(path = path,pattern = ".Tbk"))<1){stop("Error in export_tdt: Cannot find .Tbk file in path!")}
  if(!file.exists(matlab_path)){stop("Error in export_tdt: Cannot find file in specified matlab path!")}

  # tdt_lib_path
  tdt_lib_path = system.file("SDK", package = "fibeR")#"inst/SDK/"
  # matlab_fun_path
  matlab_fun_path = system.file("Matlab", package = "fibeR")#"inst/SDK/"
  # define outputpath for Matlab with id
  outputpath = gsub("//","/",paste0(outputpath,"/"))
  # paste channel names in correct format
  channels_matlab = paste0("{'",paste0(channel_names,collapse = "','"),"'}")
  # define file name of output
  if(export_full){outputfile = paste0(outputpath,id,"_export.txt")}else{outputfile = paste0(outputpath,id,"_export_small.txt")}


  if(return_cached & file.exists(outputfile)){
    if(verbose){message("export_tdt: Found existing export file for this id in outputpath. Not running Matlab. Set return_cached to FALSE to overwrite this behavior.")}
  }else{
    if(verbose){message("export_tdt: Running Matlab tdt export ... ")}
    # run Matlab
    if(export_full){matlab_fun = "tdt_export"}else{matlab_fun ="tdt_export_small"}
    command = as.character(paste0(matlab_path,
                                  " -nodisplay -nojvm -r \"cd '",matlab_fun_path,"/'; try ",matlab_fun,"('",
                                  paste0(path),"','",
                                  outputpath,"','",
                                  tdt_lib_path,"','",
                                  id,"',",
                                  channels_matlab,",",
                                  downsample_freq,"); catch; end; quit\""))
    output = system(command,intern = TRUE)
    if(verbose){message("complete")}
  }
  return(outputfile)

}





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




