
# helper functions for processing

##########
### Downsample
##########

#' Downsamples data by taking the median around every k-th point.
#'
#' Description
#'
#' @param input_data matrix or dataframe to downsample
#' @param k Group every k points. Defaults to 1000.
#' @return Downsampled matrix with the first column being the time index and other columns being downsampled signals
#'
#' @export
#'
#' @import magrittr
#' @importFrom dplyr summarise group_by
#' @importFrom stats median
#'

downsample = function(input_data,k=1000){
  input_data= as.matrix(input_data)
  # make grouping
  n = ceiling(nrow(input_data)/k)
  grp_elements = unlist(lapply(1:n,function(x,k){rep(x,k)},k=k))
  grp_elements = grp_elements[1:nrow(input_data)]
  # initiate result matrix
  mat_downsampled = matrix(nrow = n,ncol = ncol(input_data))
  # aggregate
  for(i in 1:ncol(input_data)){
    df = tibble(data = input_data[,i],grp_elements = grp_elements,stringsAsFactors = F)
    df_down = df %>% dplyr::group_by(grp_elements) %>% dplyr::summarise(summ = stats::median(data))
    mat_downsampled[,i] = df_down$summ
  }
  return(mat_downsampled)
}

##########
### butter_filter: Filter signal data
##########

#' Passes signal through a zero-lag Butterworth filter from signal package.
#'
#' @param input Numeric vector of the signal to be filtered
#' @param extend_butter Single Value. Extend vector at both ends by this length to get good filtering results. Should be large compared to vector length. Check by plotting the filtered result.
#' @param w_butter Single Value. Critical frequencies of the filter from signal::butter.
#' @param order_butter Single Value. Filter order of the filter from signal::butter. Defaults to 2.
#' @param filtertype Character. Type of the filter from signal::butter. Defaults to "low".
#' @return Numeric vector of the filtered signal.
#'
#' @export
#'
#' @importFrom signal filtfilt butter
#'

butter_filter = function(input, extend_butter=5000,w_butter = (1/50), order_butter = 2,filtertype="low"){
  # construct butterworth filter
  butter_pass <- signal::butter(order_butter, w_butter, type=filtertype)
  # add two flat lines at both ends of the selected baseline data
  filter_data_ext= c(rep(input[1],extend_butter),input,rep(input[length(input)],extend_butter))
  # run zero-lag filter on baseline data
  filter_data_filtered <- signal::filtfilt(butter_pass, filter_data_ext)
  # remove extensions
  filter_data_filtered=filter_data_filtered[(extend_butter+1):(length(filter_data_filtered)-extend_butter)]
  return(filter_data_filtered)
}

##########
### reduce_fiberdata
##########


#' Remove time intervals when laser was off
#'
#' @param input_data data with time and raw signal/control channels
#' @param name_c1 column name for channel 1 (signal) in input_data
#' @param name_c2 column name for channel 2 (control) in input_data
#' @param max_value_c1 Single Value. Extend vector at both ends by this length to get good filtering results. Should be large compared to vector length. Check by plotting the filtered result.
#' @param max_value_c2 Single Value. Critical frequencies of the filter from signal::butter.
#' @param additional_cut_length Single Value. Filter order of the filter from signal::butter. Defaults to 2.
#' @param index_column Character. column in input_data. Defaults to 'time_from_intervention_int'
#' @return Numeric vector of the filtered signal.
#'
#' @export
#'
#' @importFrom dplyr lag
#'

reduce_fiberdata = function(input_data, name_c1 , name_c2 , max_value_c1=10,max_value_c2=5, additional_cut_length=5,index_column = "time_from_intervention_int"){
  # cut section where values are below max_value
  input_data_reduced = input_data[input_data[,name_c1] > max_value_c1 & input_data[,name_c2] > max_value_c2,]
  if(nrow(input_data_reduced) < nrow(input_data)){
    if(nrow(input_data_reduced)>1){
      # find section where in is discontinued
      lagged_subtract = input_data_reduced[,index_column] - dplyr::lag(input_data_reduced[,index_column],k=1)
      # additional indices to cut:
      if(length(which(lagged_subtract > 10))>0){
        also_remove = unlist(lapply(which(lagged_subtract > 10),function(x,additional_cut_length){(x-additional_cut_length):(x+additional_cut_length)},additional_cut_length=additional_cut_length))
        input_data_reduced = input_data_reduced[-also_remove,]
      }
      # update int
      input_data_reduced[,index_column][input_data_reduced[,index_column] <0] = (-1*nrow(input_data_reduced[input_data_reduced[,index_column]<0,])):(-1)
      input_data_reduced[,index_column][input_data_reduced[,index_column] >=0] = 0:(nrow(input_data_reduced[input_data_reduced[,index_column]>=0,])-1)
    }
  }else{
    input_data_reduced = input_data
  }
  # return
  return(input_data_reduced)
}

##########
### power like model
##########

#' Fit an exponential decay to estimate fiber photometry baseline with bleaching.
#'
#' Fit an exponential decay using a power like model adapted from:
#' https://www.sciencedirect.com/science/article/pii/S0006349503745032?via%3Dihub
#'
#' Additionally smoothes the data with a low-pass butter filter before trying to fit the model with stats::nls
#' If model fitting fails, it tries to change the input data to max (at the start) and min (at the end) and optionally artificially elongates the the 'tail' with the minimum value.
#'
#' TODO: describe params
#'
#'
#' @param data Numeric vector of the signal to be fitted (y)
#' @param smooth_with_butter apply low pass butter filter t smooth the data ?
#' @param pct_fallback when the model fitting fails the data is restricted to the maximum within in the first pct_fallback % (e.20%) and the minimum in the last 20%
#' @param b_val manually defined starting value for formula: y ~ a*(1-(1-b)*(x/c))^(1/(1-b))
#' @param ntry  parameter to stop recursion when functions calls itself (defaults to 0 when called by user). Function will not run if bigger than > 10
#' @param verbose 0=nothing, 1= only errors, 2: messages 3 plot
#' @return List. reference_data: Fitted values with model or median. fit_model: Model from stats::nls, if possible.
#'
#' @export
#'
#' @importFrom stats nls formula fitted coef
#' @importFrom graphics plot lines points
#'

fit_decay_power = function(data,smooth_with_butter = TRUE,pct_fallback = 0.2,b_val = 0.5,ntry=0,verbose=0){

  # handle boolean verbose
  if(is.logical(verbose)){
   if(verbose){
     verbose = 2
   }else{
     verbose = 0
   }
  }
  # get data a
  data_forfit = data.frame(y = data, x=1:length(data))
  if(verbose >=3){print(graphics::plot(1:length(data_forfit$y),data_forfit$y,type="l"))}
  if(smooth_with_butter){
    data_forfit$y=butter_filter(data_forfit$y, extend_butter=length(data_forfit$y)*3,w_butter = (1/50), order_butter = 2,filtertype="low")
    if(verbose >=3){print(graphics::lines(1:length(data_forfit$y),data_forfit$y,type="l",lt="dotted"))}
  }

  ## set model
  formula_model = stats::formula(y ~ a*(1-(1-b)*(x/c))^(1/(1-b)))
  starting_values_model = c(a = max(data_forfit$y), b = b_val, c = (1-(1/exp(1)))*data_forfit$x[length(data_forfit$x)])
  if(verbose >=2){message("Starting values: ")}
  if(verbose >=2){message(paste0(starting_values_model,collapse = " | "))  }

  # try to fit model
  fitted_model=list()
  fitted_model <- tryCatch({
    fit = stats::nls(formula = formula_model,data = data_forfit,start = starting_values_model)
    fit
  },error=function(cond){
    fitted_model <- tryCatch({
      if(verbose >=1){message("Error while fitting: ",cond)}
      if(verbose >=2){message("Retrying model fit with new start and end points")}
      # set start point to max value
      max_first = max(data_forfit$y[1:max(1,floor(length(data_forfit$x)*pct_fallback))])
      if(verbose >=2){message("using max_first: ",which(data_forfit$y==max_first)[1]," ",max_first)}
      # set end point to min value
      min_last =  min((data_forfit$y[max(1,floor(length(data_forfit$x)*(1-pct_fallback))):length(data_forfit$x)]))
      if(verbose >=2){message("using minlst: ",which(data_forfit$y==min_last)[1]," ",min_last)}
      # add points to plot
      if(verbose >=3){print(graphics::points(x=c(which(data_forfit$y==max_first)[1],which(data_forfit$y==min_last)[1]),y=c(max_first,min_last),col="red",pch=21))}
      # cut down data
      data_forfit = data_forfit[1:which(data_forfit$y==min_last)[1],]
      data_forfit = data_forfit[which(data_forfit$y==max_first)[1]:nrow(data_forfit),]
      # try again
      fit = stats::nls(formula = formula_model,data = data_forfit,start = starting_values_model)
      fit
    },error=function(cond){
      message("Cannot fit exponetial decay: ",cond)
    })
    return(fitted_model)
  })

  ## if model exists
  if(length(fitted_model)!=0){
    reference_data = as.numeric(stats::fitted(fitted_model))
    if(stats::coef(fitted_model)['b'] < 0 & ntry < 10){
      if(verbose >=2){message("Retrying model with elongated data using the minimum of baseline")}
      fitted_model = fit_decay_power(c(data_forfit$y,rep(min(data_forfit$y),pct_fallback*length(data_forfit$y))),smooth_with_butter = F,pct_fallback = pct_fallback+0.1,b_val = b_val,verbose=verbose,ntry=ntry+1)$fit_model
    }
    if(verbose >=2){message("Estimated coeffiecients: ")}
    if(verbose >=2){message(paste0(stats::coef(fitted_model),collapse = " | "))}
  }else{
    reference_data =rep(median(data_forfit$y),length(data_forfit$y))
  }
  if(verbose >=3){print(graphics::lines(1:length(reference_data),reference_data,type="l",col="blue"))}
  #return both the model and the fitted values
  return(list(reference_data = reference_data, fit_model = fitted_model))

}
