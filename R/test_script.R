#
# source("R/main_functions.R")
# source("R/export_functions.R")
# source("R/processing_functions.R")
#
# tdt_lib_path = "/beegfs/scratch/bruening_scratch/lsteuernagel/projects/fibeR/inst/SDK"
# #tempdir()
# temp_dir = "/tmp/RtmppeGw50/"
#
# #fiber_sample = import_fibeR(input_path = "/beegfs/v0/labnet-data/calcium/cbauder/BAU0000439310520-200531-135900/",outputpath=temp_dir,tdt_lib_path="/beegfs/scratch/bruening_scratch/lsteuernagel/projects/fibeR/inst/SDK")
# input_data_path = "/beegfs/v0/labnet-data/calcium/cbauder/BAU0000444100620-200610-173408/"
# input_id = "BAU0000444100620-200610-173408"
# # input_data_path = "/beegfs/v0/labnet-data/calcium/pprada/NOPrightSetup/NophMRO/NophM3032RO-220608-152007/"
# # input_id = "NophM3032RO-220608-152007"
#
#
# exported_file = export_tdt(input_data_path,
#                            input_id,
#                            return_cached = FALSE,
#                            outputpath = temp_dir,
#                            matlab_path = "/beegfs/bin/matlab_2014b",
#                            matlab_fun_path = "/beegfs/scratch/bruening_scratch/lsteuernagel/projects/fibeR/inst/Matlab/",
#                            tdt_lib_path="/beegfs/scratch/bruening_scratch/lsteuernagel/projects/fibeR/inst/SDK/")
#
# sample_table = utils::read.table(file = exported_file,header = TRUE)
#
# export_Notes(input_data_path,input_id,outputpath = temp_dir,verbose = TRUE)
#
# "/tmp/Rtmp7AlRha/BAU0000439310520-200531-135900_export_small.txt"
# # the import_fibeR function is impotring data from the
# notes =data.table::fread("/tmp/Rtmp7AlRhaBAU0000439310520-200531-135900_notes_parsed.txt",data.table = F)
#
# read.table("/tmp/Rtmp7AlRhaBAU0000439310520-200531-135900_notes_parsed.txt",header = TRUE)
#
