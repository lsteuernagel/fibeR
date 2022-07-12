
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fibeR

<!-- badges: start -->
<!-- badges: end -->

Fiber photometry processing and data analysis

## Installation

Install fibeR using:

``` r
devtools::install_github("lsteuernagel/fibeR")
```

Check whether fibeR can be loaded:

``` r
library(fibeR)
```

## Quick start (one sample)

``` r
sample_path =   "/beegfs/v0/labnet-data/calcium/cbauder/BAU0000444100620-200610-173408/" # add your own path
fiber_sample = import_fibeR(input_path = sample_path,verbose =FALSE)
fiber_sample = process_fibeR(fiber_sample,start_note = 2,correct_with_control=TRUE,verbose =FALSE) # specify the intervention note, typically 2 (1 being the start of recording)
plot_fibeR(fiber_sample,datatype = "decay") # use 'raw' to get raw data plot
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

## Quick start (multiple samples)

The ’\_batch’ functions allow running the import and processing on a
folder with many samples (that can be organized in various subfolders).
Depending on the number of samples this can take a while (\~10 sec per
sample).

It is recommended to specify an output folder
(‘batch_sample_output_path’), then re-importing or loading data is much
faster in the next session (because Matlab and downsampling are
skipped).

``` r
batch_sample_path =   "/beegfs/v0/labnet-data/calcium/cbauder/2020_07_09/" # add your own path
batch_sample_output_path = "/beegfs/scratch/bruening_scratch/lsteuernagel/data/fiberPhotometry/testexport/readme/" # change to your own path on scratch
fiber_sample_list = import_fibeR_batch(batch_path = batch_sample_path,batch_output_path = batch_sample_output_path)
#>   |                                                                              |                                                                      |   0%  |                                                                              |==================                                                    |  25%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================================                  |  75%  |                                                                              |======================================================================| 100%
fiber_sample_list = process_fibeR_batch(fiber_sample_list,start_note_all = 2) # specify the intervention note, typically 2 (1 being the start of recording)
#>   |                                                                              |                                                                      |   0%  |                                                                              |==================                                                    |  25%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================================                  |  75%  |                                                                              |======================================================================| 100%
save_fibeR_batch(fiber_sample_list,batch_output_path = batch_sample_output_path) # save processed result to same path
#>   |                                                                              |                                                                      |   0%  |                                                                              |==================                                                    |  25%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================================                  |  75%  |                                                                              |======================================================================| 100%
#> list()
names(fiber_sample_list) ## access list elements like this : fiber_sample_list[[1]] or fiber_sample_list[["090720BRA0012161-200709-141509"]]
#> [1] "090720BRA0000517-200709-132227" "090720BRA0010685-200709-161148"
#> [3] "090720BRA0012142-200709-152204" "090720BRA0012161-200709-141509"
```

## Example with one sample

In the readme we are only highlighting the basic usage and the most
important parameters. You can always type ?function (e.g. ?process_fibeR
to read about more parameters etc.)

### Import a TDT sample

Import an example fiber sample by specifying the path to the folder
containing the .tbk files from TDT:

Notes: - In this example I am loading a file from the beegfs because the
TDT file are too large to include examples in the package). You can
change sample path to your own samples. - To compile the example I am
loading from an existing (cached) file. When running this yourself for
the first time (for each sample), it will run Matlab.

``` r
sample_path =  "/beegfs/v0/labnet-data/calcium/cbauder/BAU0000444100620-200610-173408/"
fiber_sample = import_fibeR(input_path = sample_path)
#> import_fibeR: Using last subfolder as id: BAU0000444100620-200610-173408
#> export_tdt: Found existing export file for this id in outputpath. Not running Matlab. Set return_cached to FALSE to overwrite this behavior.
#> export_Notes: Exporting notes
#> Warning: Id in notes differs from expected id
```

The result is an fibeR_data object (essentially an R list) with the data
and id:

``` r
str(fiber_sample)
#> List of 4
#>  $ id          : chr "BAU0000444100620-200610-173408"
#>  $ raw.data    :'data.frame':    2749 obs. of  3 variables:
#>   ..$ time : num [1:2749] 0.000983 0.984023 1.967063 2.950103 3.933143 ...
#>   ..$ x465A: num [1:2749] 64.8 75 74.2 73.7 72.6 ...
#>   ..$ x405A: num [1:2749] 53.9 62 61.6 61.5 61.1 ...
#>  $ process.data: NULL
#>  $ notes       :'data.frame':    4 obs. of  4 variables:
#>   ..$ note_id  : int [1:4] 1 2 3 4
#>   ..$ note_date: POSIXct[1:4], format: "2020-06-10 15:34:11" "2020-06-10 15:49:12" ...
#>   ..$ text     : chr [1:4] "start" "add caged food" "end food" "stop"
#>   ..$ note_time: int [1:4] 0 901 981 2704
#>  - attr(*, "class")= chr "fibeR_data"
```

### Import a TDT sample

We can then process the sample. This mostly includes calculation of dFF.
For most dFF calculations we require an intervention time point to
calculate the baseline on the pre-intervention data

The most important parameters:

-   cutoff_start: how many seconds to cut of at start

-   cutoff_end: how many seconds to cut of at end

-   **start_note**: which note tells process_fibeR when the intervention
    happened. Iif you have not taken notes in the system, you can
    specify the second with the ‘intervention_second_fallback’
    parameter. If you want to include all data into the baseline
    calculation set it to “stop” which should be the last note that
    specifies the end of the recording (automatically included by TDT).

-   **correct_with_control**: whether to subtract the control when
    calculating the final dFF

``` r
fiber_sample = process_fibeR(fiber_sample,
                             cutoff_start = 10,
                             cutoff_end = 5,
                             start_note = 2,
                             correct_with_control=TRUE,
                             verbose =TRUE)
#> Processing fibeR_input...
#> Removing 46 data points to reduce the data to one point per (rounded) second. Set reduce_for_comparability to FALSE to disable this behavior.
#> dFF: Estimate median
#> dFF: Fitting control to signal
#> dFF: Estimate exponetial decay
#> Processing complete.
```

How did the sample change:

``` r
str(fiber_sample)
#> List of 6
#>  $ id                  : chr "BAU0000444100620-200610-173408"
#>  $ raw.data            :'data.frame':    2749 obs. of  3 variables:
#>   ..$ time : num [1:2749] 0.000983 0.984023 1.967063 2.950103 3.933143 ...
#>   ..$ x465A: num [1:2749] 64.8 75 74.2 73.7 72.6 ...
#>   ..$ x405A: num [1:2749] 53.9 62 61.6 61.5 61.1 ...
#>  $ process.data        :'data.frame':    2686 obs. of  7 variables:
#>   ..$ time                  : num [1:2686] 10.8 11.8 12.8 13.8 14.7 ...
#>   ..$ time_from_intervention: num [1:2686] -891 -890 -889 -888 -887 -886 -885 -884 -883 -882 ...
#>   ..$ x465A                 : num [1:2686] 68.8 68.2 68.8 68.2 68.3 ...
#>   ..$ x405A                 : num [1:2686] 59.5 59.2 59.2 58.8 59 ...
#>   ..$ dFF_median            : num [1:2686] -0.0227 -0.0255 -0.0157 -0.019 -0.0202 ...
#>   ..$ dFF_fit               : num [1:2686] 0.0899 0.0831 0.0919 0.0842 0.0851 ...
#>   ..$ dFF_decay             : num [1:2686] 0.0171 0.015 0.0241 0.0214 0.021 ...
#>  $ notes               :'data.frame':    4 obs. of  4 variables:
#>   ..$ note_id  : int [1:4] 1 2 3 4
#>   ..$ note_date: POSIXct[1:4], format: "2020-06-10 15:34:11" "2020-06-10 15:49:12" ...
#>   ..$ text     : chr [1:4] "start" "add caged food" "end food" "stop"
#>   ..$ note_time: int [1:4] 0 901 981 2704
#>  $ intervention_seconds: int 901
#>  $ baseline.data       :'data.frame':    2686 obs. of  7 variables:
#>   ..$ time                  : num [1:2686] 10.8 11.8 12.8 13.8 14.7 ...
#>   ..$ time_from_intervention: num [1:2686] -891 -890 -889 -888 -887 -886 -885 -884 -883 -882 ...
#>   ..$ median_signal         : num [1:2686] 59.4 59.4 59.4 59.4 59.4 ...
#>   ..$ median_control        : num [1:2686] 50.4 50.4 50.4 50.4 50.4 ...
#>   ..$ fit_control           : num [1:2686] 63.1 63 63 62.9 62.9 ...
#>   ..$ decay_signal          : num [1:2686] 68.4 68.3 68.1 68 67.8 ...
#>   ..$ decay_control         : num [1:2686] 60.2 60.1 60 59.9 59.8 ...
#>  - attr(*, "class")= chr "fibeR_data"
```

The processing function added the processed data (and also baseline
information) to the fibeR_data object.

### Plot the results

We can use the plot_fibeR function to plot the content of the fibeR_data
object

#### Raw data

First we plot the raw input ( we could also do this before the
processing, but then we cannot include the baseline into this plot)

The two most important parameters are: - **datatype**: We set this to
‘raw’ to get the signal and control - **split_plots**: If TRUE signal
and control are plotted in two facets.

``` r
plot_fibeR(fiber_sample,datatype = "raw",split_plots = TRUE)
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" />

#### Processed data

Then we can plot the processed (and normalized) dFF signal.

For this we set **datatype** to “decay” which means we will plot the
data normalized by subtracting and dividing by a baseline based on a
negative exponential decay that was fitted to the pre-intervention data.
Alternatives are ‘median’ (median of baseline) or ‘fit’ (Lerner method).

``` r
plot_fibeR(fiber_sample,datatype = "decay")
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="100%" />

### Save the results

We can export the fibeR_data object into easily readable flat table
files using the save function.

**Change example path (just a temporary path for this readme) to a path
in your own directory !**

``` r
example_path = paste0(tempdir(),"/")
save_fibeR(fiber_sample, output_path = example_path)
```

#### Re-load data

load_fibeR allows to re-assemble a fibeR_data object from the exported
files. This saves us re-exporting (from matlab) and re-processing. This
function requires the id of the sample (which be default is the folder
name).

``` r
id_to_load = "BAU0000444100620-200610-173408"
newly_loaded = load_fibeR(id=id_to_load,input_path = example_path)
names(newly_loaded)
#> [1] "id"            "baseline.data" "notes"         "process.data" 
#> [5] "raw.data"
```

## Multiple Samples (batch)

To be extended!

The ’\_batch’ functions allow running the import and processing on a
folder with many samples (that can be organized in various subfolders).
Depending on the number of samples this can take a while (\~10 sec per
sample).

It is recommended to specify an output folder
(‘batch_sample_output_path’), then re-importing or loading data is much
faster in the next session (because Matlab and downsampling are
skipped).

### Import data

Similar to quick start:

``` r
batch_sample_path =   "/beegfs/v0/labnet-data/calcium/cbauder/2020_07_09/" # add your own path
batch_sample_output_path = "/beegfs/scratch/bruening_scratch/lsteuernagel/data/fiberPhotometry/testexport/readme/" # change to your own path on scratch
fiber_sample_list = import_fibeR_batch(batch_path = batch_sample_path,batch_output_path = batch_sample_output_path)
#>   |                                                                              |                                                                      |   0%  |                                                                              |==================                                                    |  25%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================================                  |  75%  |                                                                              |======================================================================| 100%
```

### Process data

``` r
fiber_sample_list = process_fibeR_batch(fiber_sample_list,start_note_all = 2) # specify the intervention note, typically 2 (1 being the start of recording)
#>   |                                                                              |                                                                      |   0%  |                                                                              |==================                                                    |  25%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================================                  |  75%  |                                                                              |======================================================================| 100%
names(fiber_sample_list) ## access list elements like this : fiber_sample_list[[1]] or fiber_sample_list[["090720BRA0012161-200709-141509"]]
#> [1] "090720BRA0000517-200709-132227" "090720BRA0010685-200709-161148"
#> [3] "090720BRA0012142-200709-152204" "090720BRA0012161-200709-141509"
```

### Access individual samples

We can subset the list to individual samples by index or sample_id and
then work with those using the functions shown in the above section
‘Example with one sample’

``` r
single_sample = fiber_sample_list[["090720BRA0012161-200709-141509"]]
str(single_sample)
#> List of 7
#>  $ id                  : chr "090720BRA0012161-200709-141509"
#>  $ raw.data            :'data.frame':    2750 obs. of  3 variables:
#>   ..$ time : num [1:2750] 0.000983 0.984023 1.967063 2.950103 3.933143 ...
#>   ..$ x465A: num [1:2750] 55.2 64.5 63.8 62.5 61.3 ...
#>   ..$ x405A: num [1:2750] 59 69.4 69.4 69.1 68.8 ...
#>  $ process.data        :'data.frame':    2687 obs. of  7 variables:
#>   ..$ time                  : num [1:2687] 10.8 11.8 12.8 13.8 14.7 ...
#>   ..$ time_from_intervention: num [1:2687] -890 -889 -888 -887 -886 -885 -884 -883 -882 -881 ...
#>   ..$ x465A                 : num [1:2687] 58.3 58.8 58.4 58.1 57.9 ...
#>   ..$ x405A                 : num [1:2687] 68.4 68.3 68.3 68.1 68.1 ...
#>   ..$ dFF_median            : num [1:2687] -0.0249 -0.016 -0.0219 -0.0261 -0.0285 ...
#>   ..$ dFF_fit               : num [1:2687] 0.0985 0.1047 0.098 0.0881 0.0833 ...
#>   ..$ dFF_decay             : num [1:2687] -0.0249 -0.016 -0.0219 -0.0261 -0.0285 ...
#>  $ notes               :'data.frame':    4 obs. of  4 variables:
#>   ..$ note_id  : int [1:4] 1 2 3 4
#>   ..$ note_date: POSIXct[1:4], format: "2020-07-09 12:15:12" "2020-07-09 12:30:12" ...
#>   ..$ text     : chr [1:4] "start" "start caged food" "end food" "stop"
#>   ..$ note_time: int [1:4] 0 900 929 2704
#>  $ folder_name         : Named chr "090720BRA0012161-200709-141509"
#>   ..- attr(*, "names")= chr "/beegfs/v0/labnet-data/calcium/cbauder/2020_07_09//090720BRA0012161-200709-141509/Exp_corinna-200528-115116_090"| __truncated__
#>  $ intervention_seconds: int 900
#>  $ baseline.data       :'data.frame':    2687 obs. of  7 variables:
#>   ..$ time                  : num [1:2687] 10.8 11.8 12.8 13.8 14.7 ...
#>   ..$ time_from_intervention: num [1:2687] -890 -889 -888 -887 -886 -885 -884 -883 -882 -881 ...
#>   ..$ median_signal         : num [1:2687] 56.8 56.8 56.8 56.8 56.8 ...
#>   ..$ median_control        : num [1:2687] 65 65 65 65 65 ...
#>   ..$ fit_control           : num [1:2687] 53.1 53.2 53.2 53.4 53.4 ...
#>   ..$ decay_signal          : num [1:2687] 56.8 56.8 56.8 56.8 56.8 ...
#>   ..$ decay_control         : num [1:2687] 65 65 65 65 65 ...
#>  - attr(*, "class")= chr "fibeR_data"
```

We can also plot this sample:

``` r
plot_fibeR(fiber_sample_list[["090720BRA0012161-200709-141509"]],datatype = "raw",split_plots = TRUE)
```

<img src="man/figures/README-unnamed-chunk-16-1.png" width="100%" />

### Plot all samples

### 

### Save & load data

Similar to thesection ‘Example with one sample’ we can save and load the
results. With many samples, this saves time processing the data.

``` r
save_fibeR_batch(fiber_sample_list,batch_output_path = batch_sample_output_path) # save processed result to same path
#>   |                                                                              |                                                                      |   0%  |                                                                              |==================                                                    |  25%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================================                  |  75%  |                                                                              |======================================================================| 100%
#> list()
```

## Details on Matlab and caching

Will be extended in the future.

Most importantly: You can change the outputpath of import_fibeR to a
permanent location in your beegfs/scratch or home directoy to save the
Matlab exports permanently which decreases load times when working
repeatedly with samples!

## Details on Visualization

Will be extended in the future.

For example: You can visualize multiple processing approaches in
individual facets:

``` r
plot_fibeR(fiber_sample,datatype = c("decay","median","fit"),split_plots = TRUE)
```

<img src="man/figures/README-unnamed-chunk-18-1.png" width="100%" />
