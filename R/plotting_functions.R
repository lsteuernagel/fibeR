
# Functions to visualize results

##########
### plot_fibeR
##########

#' Plot an object of class fibeR_data
#'
#' Plot the raw or processed data from a list-like fibeR_data using ggplot. The most important option is datatype ('raw' for raw data or 'decay','fit','median' for processed (dFF) data).
#'
#' split_plots controls whether to split signal/control or the different dFFs into multiple facets.
#'
#' When plotting processed data the function process_fibeR has to be run first!
#'
#' @param fibeR_data object of class fibeR_data
#' @param datatype character vector. 'raw' for raw data or one or multiple of 'decay','fit','median' for processed (dFF) data
#' @param split_plots logical. Whether to split ggplot by source (signal/control, dFFs). Defaults to TRUE.
#' @param datatype_baseline character vector. 'none' to ignore baseline in raw plot. One or multiple of 'decay','fit','median' for baseline that is used in processed data. If no valid baseline.data can be found, will automatically fall back to 'none'. Defaults to 'decay'.
#' @param save_plot logical. Whether to save ggplot object to output_path. Defaults to FALSE
#' @param output_path existing directory to save plot
#' @param name_signal character string with the name of the signal column in fibeR_input. Defaults to the MPI standard: 'x465A'
#' @param name_control character string with the name of the control column in fibeR_input. Defaults to the MPI standard: 'x405A'
#' @param linesize the ggplot line size. defaults to 0.3
#' @param min_yaxis_dFF for processed data only: Controls the minimum y axis value. If dFF is larger it will be automatically increased. Defaults to 0.2 (-20% to 20%).
#' @param add_hline for processed data only: logical. If TRUE a grey line is added at y=0
#' @param add_vline logical. If TRUE a grey line is added at y=intervention_time
#' @param text_size_param the ggplot text size. defaults to 15
#' @param color_vector a vector which maps custom colors to datatypes
#' @return ggplot object
#'
#' @export
#'
#' @import ggplot2 magrittr
#' @importFrom tidyr gather
#' @importFrom dplyr rename
#' @importFrom rlang sym
#'

plot_fibeR = function(fibeR_data,datatype = "raw",split_plots = TRUE, datatype_baseline = "decay",save_plot = FALSE,output_path = character(0),name_signal = "x465A",name_control = "x405A", linesize = 0.3, min_yaxis_dFF = 0.2,add_hline = TRUE,add_vline = TRUE, text_size_param = 15,color_vector = c(signal = "#009E73", control = "#D55E00", decay = "#0072B2", fit = "#CC79A7", median = "#E69F00")){

  # check object
  if(!is(fibeR_data,"fibeR_data")){
    stop("Please provide a valid fibeR_data to save with this function.")
  }
  # find all valid data columns
  if("raw" %in% datatype){check_raw =TRUE}else{check_raw =FALSE}
  datatype = paste0("dFF_",datatype)
  valid_columns = datatype[datatype %in% colnames(fibeR_data$process.data)]
  # check if raw and procssed was provided
  if(check_raw & length(valid_columns) > 0 ){ #& !split_plots
    # stop("Cannot plot raw and processed data together without enabling splitting (faceting) of plots. Please set to TRUE or change 'datatype' inout to either raw or one or more processed types")
    stop("Cannot plot raw and processed data together. Please set to TRUE or change 'datatype' to either 'raw' or one or more processed types ('median','fit','decay').")
  }
  # check if valid processed data was provided if no raw
  if(!check_raw & length(valid_columns) == 0){
    stop("Cannot find data for provided 'datatype' argument. Please provide one or more of 'median','fit','decay' and make sure you procseed the data. OR provide 'raw' to plot the raw signal.")
  }
  # warn if not all columns could be found
  if(length(datatype[datatype!="dFF_raw"]) > length(valid_columns)){
    message("Warning: cannot find all of provided datatypes. Please provide one or more of 'median','fit','decay' or 'raw'. And make sure that the data has been processed before.")
  }

  # add baseline handling if check_raw == TRUE
  if(!is.null(fibeR_data$baseline.data) & check_raw){
    # validate
    valid_baseline_columns = colnames(fibeR_data$baseline.data)[gsub("_control|_signal","",colnames(fibeR_data$baseline.data)) %in% datatype_baseline]
    if(length(valid_baseline_columns) > 0){
      add_baseline = fibeR_data$baseline.data[,valid_baseline_columns]
      datatype_baseline = datatype_baseline[datatype_baseline!="none"] # ensure there is no 'none'
    }else{
      if(datatype_baseline != "none"){
        datatype_baseline = "none"
        message("Warning: cannot find any of provided baseline types. Setting to 'none'")
      }
    }
  }else{
    if(datatype_baseline != "none" & check_raw){
      message("Warning: cannot find baseline.data in fibeR_data object. Have you processed the fibeR_data object?")
    }
  }

  # update color_vector
  names(color_vector)[! names(color_vector) %in% c("signal","control")] = paste0("dFF_",names(color_vector)[!names(color_vector)%in% c("signal","control")])

  # make plot data frame and base plot
  if(check_raw){
    # for raw data:
    plotdf = fibeR_data$process.data[,c( "time","time_from_intervention",name_signal,name_control)] %>% dplyr::rename(signal = !!rlang::sym(name_signal),control = !!rlang::sym(name_control)  )
    # add baseline
    if( datatype_baseline != "none"){
      if(nrow(add_baseline) == nrow(plotdf)){
        plotdf = cbind(plotdf,add_baseline) %>% as.data.frame()
      }else{
        stop("Baseline and dFF raw data are of different length. Corretly process the data or provide datatype_baseline = 'none' to skip baseline plotting.")
      }
    }
    # make long version
    plotdf_long = plotdf %>% tidyr::gather(key = "type",value = "value",-time,-time_from_intervention)
    plotdf_long$source = stringr::str_match(plotdf_long$type,"signal|control")
    plotdf_long$source = factor(plotdf_long$source ,levels = c("signal","control"))
    plotdf_long$baseline  = stringr::str_match(plotdf_long$type,"decay|median|fit")[,1]
    plotdf_long$baseline[is.na(plotdf_long$baseline)] ="data"
    yaxis_name = "mV"
    # plot raw
    p1 = ggplot(plotdf_long,aes(x=time,y=value,group=type,color=source,linetype=baseline))+
      geom_line(size=linesize)
  }else{
    # for processed data:
    plotdf = fibeR_data$process.data[,c( "time","time_from_intervention",valid_columns)]
    # make long version
    plotdf_long = plotdf %>% tidyr::gather(key = "type",value = "value",-time,-time_from_intervention)
    plotdf_long$type = factor(plotdf_long$type ,levels = c(valid_columns))
    plotdf_long$source =  plotdf_long$type
    yaxis_name = "dFF"
    # plot process
    p1 = ggplot(plotdf_long,aes(x=time,y=value,group=type,color=source))+
      geom_line(size=linesize)+
      ylim(c(min(-1*min_yaxis_dFF,min(plotdf_long$value,na.rm = TRUE)*1.1),max(min_yaxis_dFF,max(plotdf_long$value,na.rm = TRUE)*1.1)))
  }

  # subset color_vector
  color_vector = color_vector[names(color_vector) %in% unique(plotdf_long$source)]

  # finalize plot:
  p1 = p1 +
    scale_color_manual(values=color_vector)+
    theme_bw()+
    theme(text=element_text(size=text_size_param))+
    xlab("Time (seconds)")+
    ylab(yaxis_name)+
    ggtitle(fibeR_data$id)# %>% suppressWarnings()

  # hline
  if(add_hline & !check_raw){ p1 = p1 + geom_hline(yintercept = 0,color="grey40")}
  if(add_vline){ p1 = p1 + geom_vline(xintercept = plotdf_long$time[plotdf_long$time_from_intervention == 0][1],color="grey40")}

  # optionally split
  if(split_plots & (length(valid_columns) > 1 | check_raw)){
    p1 = p1 + facet_grid(source ~ . , scales='free')
  }
  if(save_plot){
    # TODO : check output_path

    # filename
    if(check_raw){
      filename = paste0(c(output_path,fibeR_data$id,"_","raw",".png"))
    }else{
      filename = paste0(c(output_path,fibeR_data$id,"_","dFF_",paste0(gsub("dFF_","",valid_columns),collapse = "_"),".png"),collapse = "")
    }
    # save
    ggsave(filename = filename,
           plot = p1, "png",dpi=400,width=350,height = 200,units="mm")
  }

  return(p1)
}


##########
### plot_aligned_fibeR
##########

#' Plot a data.frame that was created with \link[fibeR]{align_fibeR}
#'
#' todo: description
#'
#' @param aligned_fibeR dataframe with aligned fiber photometry results and time integers as rownames
#' @param summary_stat logical. If TRUE: plot mean and sd instead of individual lines
#' @param summary_color color for mean and sd
#' @param yaxis_label label for yaxis
#' @inheritParams plot_fibeR
#' @return ggplot object
#'
#' @export
#'
#' @import ggplot2 magrittr
#' @importFrom tidyr gather
#' @importFrom dplyr group_by mutate
#' @importFrom rlang sym
#' @importFrom grDevices colorRamp
#' @importFrom stats sd
#'


plot_aligned_fibeR = function(aligned_fibeR,summary_stat = FALSE,summary_color = "#009E73",yaxis_label = "dFF",linesize = 0.3, min_yaxis_dFF = 0.2,add_hline = TRUE,add_vline = TRUE, text_size_param = 15){

  okabe_to_palette = c("#000000","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7","#999999")

  # check ncols

  # make long
  aligned_fibeR_long = aligned_fibeR %>% dplyr::mutate(time = as.numeric(rownames(aligned_fibeR))) %>% tidyr::gather(key="sample",value="value",-time)

  # plot
  if(!summary_stat){
    p1 = ggplot(aligned_fibeR_long,aes(x=time,y=value,group=sample,color=sample))+
      geom_line(size=linesize)+
      ylim(c(min(-1*min_yaxis_dFF,min(aligned_fibeR_long$value,na.rm = TRUE)*1.1),max(min_yaxis_dFF,max(aligned_fibeR_long$value,na.rm = TRUE)*1.1)))
  }else{
    aligned_fibeR_long = aligned_fibeR_long %>% dplyr::group_by(time) %>% dplyr::mutate(mean = mean(value), sd = stats::sd(value))
    p1 = ggplot(data = aligned_fibeR_long, aes(x = time, group = sample)) +
      geom_line(aes(y = mean),color= summary_color)+
      geom_ribbon(aes(y = mean, ymin = mean - sd, ymax = mean + sd),fill = summary_color,alpha = 0.2)
  }

  # finalize plot:
  p1 = p1 +
    #scale_color_manual(values=color_vector)+
    theme_bw()+
    theme(text=element_text(size=text_size_param))+
    xlab("Time (seconds)")+
    ylab(yaxis_label)
  # hline
  if(add_hline){ p1 = p1 + geom_hline(yintercept = 0,color="grey40")}
  if(add_vline){ p1 = p1 + geom_vline(xintercept = 0,color="grey40")}

  # color:
  if(!summary_stat){
    getOkabeItoPalette = grDevices::colorRampPalette(okabe_to_palette)
    p1 = p1+scale_color_manual(values=getOkabeItoPalette(max(ncol(aligned_fibeR),9)))
  }

  return(p1)
}

##########
### plot_heat_aligned_fibeR
##########

#' Plot a heatmap of a data.frame that was created with \link[fibeR]{align_fibeR}
#'
#' Makes a heatmp plot
#'
#' @param aligned_fibeR dataframe with aligned fiber photometry results and time integers as rownames
#' @param max_value_heat which value to cap heatmap at. Defaults to 0.2 (20% dFF). Dependinng on the data to plots this should be adjusted.
#' @param min_time time to start heatmap (before intervention). useful when some samples are much longer than others
#' @param max_time time to end heatmap
#' @param text_size_param numeric to scale ggplot text size
#' @param heatmap_cols a cector with three colors that is passed to ggplot2::scale_fill_gradientn
#' @return ggplot object
#'
#' @export
#'
#' @import ggplot2 magrittr
#' @importFrom tidyr gather
#' @importFrom dplyr group_by mutate
#' @importFrom rlang sym
#' @importFrom scales squish
#'


plot_heat_aligned_fibeR = function(aligned_fibeR,max_value_heat = 0.2,min_time = -1000,max_time=4000,text_size_param=15,heatmap_cols = c("#0072B2","#ffffff","#D55E00")){

  # check ncols
  #TODO

  # make long
  aligned_fibeR_long = aligned_fibeR %>% dplyr::mutate(time = as.numeric(rownames(aligned_fibeR))) %>%tidyr::gather(key="sample",value="value",-time)

  # filter
  aligned_fibeR_long= aligned_fibeR_long[aligned_fibeR_long$time > min_time & aligned_fibeR_long$time < max_time,]

  # make heatmap
 # all_values_max = max(c(abs(aligned_fibeR_long$dff),1),na.rm = TRUE)
  min_value= -1 * max_value_heat
  max_value= max_value_heat
  if(max_value_heat < max(abs(aligned_fibeR_long$value),na.rm = TRUE)){message("Warning: Heatmap scale is capped at ",max_value_heat, " but data exceeds this value (",round(max(abs(aligned_fibeR_long$value),na.rm = TRUE) - max_value_heat,4),")! Consider changing max_value_heat." )}

  fibeR_heatmap = ggplot(aligned_fibeR_long, aes(x = time, y = sample, fill = value)) +
    geom_tile() +
    theme_bw()+
    theme(text=element_text(size=text_size_param))+
    ggplot2::scale_fill_gradientn(colours=heatmap_cols,
                                  na.value = "grey",
                                  breaks=c(min_value,0,max_value),
                                  labels=c(round(min_value,3),"0",round(max_value,3)),
                                  limits=c(min_value,max_value), oob=scales::squish) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

  return(fibeR_heatmap)
}
