# PSMC_Away 

- [PSMC\_Away](#psmc_away)
  - [A short introduction](#a-short-introduction)
  - [Prerequisites](#prerequisites)
    - [PSMC](#psmc)
    - [R Libraries](#r-libraries)
      - [Prerequisites.R](#prerequisitesr)
  - [Functions](#functions)
    - [Functions.R](#functionsr)
  - [How to run the functions](#how-to-run-the-functions)
    - [Analysis](#analysis)
      - [Run.R](#runr)
    - [Additional visualization](#additional-visualization)
      - [Add\_plots.R](#add_plotsr)
      - [Overlay\_plots.R](#overlay_plotsr)
  - [Under the hood of BinCorr\_psmc (function)](#under-the-hood-of-bincorr_psmc-function)



## A short introduction
PSMC_Away is a set of R functions for processing Pairwise Sequentially Markovian Coalescent (PSMC) (Li & Durbin, 2011) outputs and exploring coupled demographic histories following the framework proposed by O'Connell and colleagues (preprint #REF2) across a range of mutation rates and generation times. Effective population size trajectories are rescaled under different parameterizations and compared by restricting analyses to overlapping temporal ranges. Because PSMC estimates are produced in variable-width coalescent time intervals that differ across parameterizations, direct comparison of raw outputs between species is not meaningful. To standardize trajectories, this method restricts analyses to the overlapping temporal range shared between two given species (datasets). Trajectories are then aligned on a common time grid and interpolated with the resolution determined by the dataset with fewer bins. Synchrony is assessed using Spearman correlation, Pearson or Kendall correlations (using BINCOR). This provides a framework for cross-species demographic comparisons that accounts for differences in temporal resolution and parameterization. One species will be tested with a set generation time(years) and mutation rate(num), whereas the other species can have varied generation times and mutation rates. A beta version for varying the parameters of both species has been made; however, the output of such comparisons is difficult to interpret - please contact me if you would like to use such a framework. 

This method was developed for: ###INSERT THE PAPER HERE NAT !! #### 

    REF 1: Li, H., Durbin, R. Inference of human population history from individual whole-genome sequences. Nature 475, 493–496 (2011). https://doi.org/10.1038/nature10231

    REF 2: O'Connell D, Kaiser-Bunbury C, Aizpurua O, Bechsgaard J, Borregaard M, Alberdi A, Galetti M, Gilbert MT, Gonçalves F, Heinen J, Jønsson K, Ramos-Madrigal J, Moyle L, Pedersen M, Schroeder H, Simmons B, Temeles EJ, Thomsen P, de Vere N, Morales H, Vollstädt M, Bilde T and Dalsgaard B (preprint). Islands as laboratories of mutualistic interactions: integrating molecular approaches to advance to an eco-evolutionary understanding of mutualism. Authorea.

## Prerequisites

### PSMC 
Each PSMC output (Li & Durbin, 2011) should be placed in it's own separate folder, whether the analysis is run locally or on a cluster. e.g 
The example_data is

### R Libraries
The needed R Libraries are:   
  lmtest   
  imputeTS   
  dplyr   
  tseries   
  BINCOR   
  ggplot2   
  dplyr   
  flextable   
  webshot2   

These can be acquired through running: 
#### [Prerequisites.R](./Prerequisites/)
And are loaded by each function when needed. 


## Functions

#### [Functions.R](./Functions/) 

**Functions.R** contain three functions that can be loaded by running the full script. 

**psmc.result**  (Modified from a script by Shenglin Liu, Mar 25, 2019.)   #please see the [How to run the functions]  section for further information on implementation.   
`psmc.result <- function(file, i.iteration = 25,  mu=mu, s=100, g=g)` 

By default, it is set to the ith iteration of 25 (i.iteration=25) and bin size of 100 (s=100).
**If a PSMC is run differently, this should of course be changed !!**   
The function parses PSMC outputs by identifying recombination (RS) and theta (TR) entries, computing a scaling constant θ.
θ0 (theta0) is the first scaled mutation parameter. 


```R
theta0<-as.numeric(strsplit(TR,"\t")[[1]][2])
N0<-theta0/4/mu/s
        
a<-t(as.data.frame(strsplit(RS,"\t")))
Time<-2*N0*as.numeric(a[,3])*g
Ne<-N0*as.numeric(a[,4])
```
The output of psmc.result is a data frame with two columns: Time (years before present) and Ne, in the expected stepwise bin structure of a PSMC.
This can be plotted as the full PSMC. 


**BinCorr_psmc**   
`BinCorr_psmc <- function(PSMC_1, PSMC_2, mu1, g1, list_mu2, list_g2)`  #please see the [How to run the functions]  section for further information on implementation. 

This function automates parameter exploration and computes all Spearman correlations (rank-based monotonic), Pearson (linear) and Kendall (rank-based concordance) between two species’ Ne trajectories over the input parameter space.   
The function utilizes **psmc.result** and standardizes trajectories by discarding the first 12 time points, as PSMC is less reliable in recent times and large fluctuations are often attributed to technical issues. Further, the analysis is restricted to the overlapping temporal range shared between the two species, given the parameter space.

A weighted interpolation method that respects the original PSMC binning structure is applied. For each species, the Ne value assigned to a common time point is determined by the PSMC bin in which it fell, weighted by the bin width. Where bins intersect, Ne values are averaged or linearly interpolated between flanking bins. This ensures that differences in binning resolution do not bias comparisons, while retaining the biological meaning of PSMC estimates as averages across coalescent intervals. The number of interpolations is set to the smaller of the two datasets, ensuring that the species with fewer points dictates the resolution. 

```R
weighted_interp <- function(psmc_df, common_time) {
  sapply(common_time, function(t) {
    bin_idx <- which(psmc_df$Time - psmc_df$twidth/2 <= t & t <= psmc_df$Time + psmc_df$twidth/2)
    if(length(bin_idx) == 1) {
    return(psmc_df$Ne[bin_idx])
      } else if(length(bin_idx) > 1) {
        return(mean(psmc_df$Ne[bin_idx]))
      } else {
      return(approx(psmc_df$Time, psmc_df$Ne, xout = t)$y)
      }
    })
}
```

The correlation is then tested on this interpolated time series data. Please see the "Under the hood of BinCorr_psmc" section for further details, *a step-by-step manual* (coming soon :D), and a (mostly) oneliner R script detailing and plotting the data transformations. 

The output of this function is a data.frame: 
```R
> head(Result)
     mu g   Method Correlation      P_value CI_lower CI_upper N_common
rho     1e-09       3 spearman   0.4873026 3.466721e-03       NA       NA       34
rho1    1e-09       9 spearman  -0.4427063 3.438973e-02       NA       NA       23
rho2    1e-09      15 spearman  -0.9137129 1.175458e-07       NA       NA       18
rho3    5e-09       3 spearman   0.8945961 9.121002e-19       NA       NA       51
rho4    5e-09       9 spearman   0.6789663 1.464487e-06       NA       NA       40
rho5    5e-09      15 spearman   0.4873026 3.466721e-03       NA       NA       34
```
Where: 

mu = the tested mutation rate  
g = the tested generation time   
Method = correlation method used   
Correlation = the correlation coefficient   
P_value = the P value   
CI_Lower & CI_upper = confidence intervals (num if Pearson, if Spearman or Kendall = NA)  
N_common = the number of interpolations (the number of "overlapping" bins dictated by the dataset with fewer bins).

**By default, all correlations are calculated!**   
All three ("spearman", "pearson", "kendall") are run simultaneously, and there will be three rows pr comparison in the output file.  

**However:** If the parameter space you wish to explore is very wide (e.g, above 50mu*50g), processing may be slowed. If this is the case, you can tweak the BinCorr_psmc function to only estimate one of the correlations.

- This can easily be changed in the function.R by exchanging this line:   

```R
# Loop over correlation methods
for(method in c("spearman", "pearson", "kendall")){
```
**To:**
```R
# Loop over correlation methods
for(method in c("spearman")){
```
**OR** "pearson" or "kendall" (if that is believed to be a better fit). 

And re-running the Functions.R script. 

**plot_heatmap**   
`plot_heatmap <- function(df, method_choice, species_name)`  #please see the [How to run the functions]  section for further information on implementation.

The correlation results can be visualized as heatmaps with this function (using ggplot2).   
P-values < 1 × 10⁻⁵ with positive correlation coefficients are displayed as a gradient (white to blue). Negative correlations are colored in grey, while non-significant positive correlations are shaded solid white (visually close to P-values < 1 × 10⁻⁵).

- This can be changed directly in the function. 
```R
      color_group = case_when(
        Correlation < 0 ~ "neg_corr",
        Correlation >= 0 & P_value >= 1e-5 ~ "gray_tile",
        Correlation >= 0 & P_value < 1e-5 ~ "heat"
```

- Which can be changed to any preferred color scheme directly in the function. 
```R
    scale_fill_gradient(
      low = "darkblue", high = "white", na.value = "transparent",
      limits = c(1e-27, 0.05),   # fixed range
      trans = "log10",
      name = "P-value"
    ) +
    geom_tile(
      data = subset(df_sub, color_group == "neg_corr"),
      aes(x = factor(Plant_mu), y = factor(Plant_g)),
      fill = "lightgray", inherit.aes = FALSE
    ) +
    geom_tile(
      data = subset(df_sub, color_group == "gray_tile"),
      aes(x = factor(Plant_mu), y = factor(Plant_g)),
      fill = "white", inherit.aes = FALSE
    ) 
```

## How to run the functions 


The functions are run by: 


**psmc.result**  (Modified from a script by Shenglin Liu, Mar 25, 2019.)   
```R
psmc.result <- (file, i.iteration = 25,  mu=mu, s=100, g=g)
```
Where: 

**file** = The PSMC output file of a species. "chr"
**mu** = The mutation rate. "num"
**g** = The generation time(years). "num"

**BinCorr_psmc** 

```R
Result <- BinCorr_psmc(PSMC_1, PSMC_2, mu1, g1, list_mu2, list_g2)
```
Where: 
**PSMC_1** = The PSMC output file of species 1, where the parameters are set. "chr"
**mu1** = The set mutation rate for species 1. "num"
**g1** = The set generation time(years) for species 1. "num"
**PSMC_2** = The P output file of species 2, where the parameters may vary. "chr"
**list_mu2** = A list mutation rates for species 2. "c(,)" / "c(num)"
**list_g2** =  A list mutation rates for specieThe s 2. "c(,)" / "c(num)"

If no output name is assigned through "chr" <-  BinCorr_psmc(PSMC_1, PSMC_2, mu1, g1, list_mu2, list_g2)
-> the output will be called: All_Results


**plot_heatmap**

```R
plot1 <- plot_heatmap(df, "method_choice", "species_name")
```
#Where:
**df** = The resulting dataframe from the BinCorr_psmc function   
**method_choice**  = The correlation method you would like plotted, "spearman", "pearson", or "kendall"   
**species_name** = A name that will be input in the title of the heatmap   

### Analysis  

An example run of the three functions are found in : 

#### [Run.R](./Analysis/)

Alongside the PSMC outputs from the paper:  IN PUT PAPER ! :D

### Additional visualization 

#### [Add_plots.R](./Additional_visualization/)

The code found in **Add_plots.R** can bes used to: 

To visualize the Raw PSMC trajectories, use the section: **General figure creation. ****

To visualize the PSMC trajectories over the full parameter space as a matrix-styled plot (rows=list_mu2, coll=list_g2), this can be done through the section: **MATRIX styled plots**


#### [Overlay_plots.R](./Additional_visualization/)

You can also visualize the *^matrix styled plot* with the heatmap p-values as the background color of each plot. This can be done through the script: **Overlay_plots.R**

[See images] ??!!

## Under the hood of BinCorr_psmc (function)
This is a (mostly) oneliner R script that picks apart the BinCorr_psmc and plots the intermediate data to visualize the transformation from raw PSMC output to a testable common time grid binned dataset. 

**Under_The_Hood.R**  
