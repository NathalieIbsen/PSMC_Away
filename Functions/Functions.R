#PSMC with bin_corr 

#####
#Writing Jesper B' script to a function at that takes a set of parameters  
# And returns a vector for that parameter space. The naming based on the speices and the parameters 

# Secondly 
# Write a function to compare the trend between PSMC time intervals of one species to another with bin_Corr. 

#THE RAW SCRIPT SEND by Jesper B

####FIRST 
#Load the function by  Modified from old script by Shenglin Liu, Mar 25, 2019.

# i.iteration: the ith iteration
# mu: mutation rate
# s: bin size
# g: years per generation

psmc.result<-function(file,i.iteration=25,mu=mu,s=100,g=g)
{
  X<-scan(file=file,what="",sep="\n",quiet=TRUE)
  START<-grep("^RD",X)
  END<-grep("^//",X)
  X<-X[START[i.iteration+1]:END[i.iteration+1]]
  
  TR<-grep("^TR",X,value=TRUE)
  RS<-grep("^RS",X,value=TRUE)
  
  theta0<-as.numeric(strsplit(TR,"\t")[[1]][2])
  N0<-theta0/4/mu/s
  
  a<-t(as.data.frame(strsplit(RS,"\t")))
  Time<-2*N0*as.numeric(a[,3])*g
  Ne<-N0*as.numeric(a[,4])
  
  n<-length(Ne)
  Time<-c(as.numeric(rbind(Time[-n],Time[-1])),Time[n])
  Ne<-c(as.numeric(rbind(Ne[-n],Ne[-n])),Ne[n])
  
  data.frame(Time,Ne)
}




########### Nath. Ibs Functions ###########


#####Running PSMC, 
####Cutting to covered time period ? maybe not needed ?? 
#> OBS Jesper


#Running a Bin corr test.

##Below a function: 
#To automate species comparisons and facilitate parameter exploration. :D

#correlations calculated in the below funCtion

#Pearson.Type:	Linear	Relationship detected: Linear	Sensitive to outliers? Yes	Yes (ideally)
#Spearman.Type:	Rank-based	Relationship detected:Monotonic	Sensitive to outliers? No	Assumes normality?No
#Kendall.Type:	Rank-based	Relationship detected:Monotonic	Sensitive to outliers?Less sensitive	Assumes normality?No


##### Bin corr function 

BinCorr_psmc <- function(PSMC_1, PSMC_2, mu1, g1, list_mu2, list_g2) {
  library(lmtest)
  library(imputeTS)
  library(dplyr)
  library(tseries)
  library(BINCOR)
  
  # Initialize an empty data frame to accumulate results
  All_Results <- data.frame()
  
  # Run PSMC for dataset 1
  PSMC1 <- psmc.result(PSMC_1, i.iteration = 25, mu = mu1, s = 100, g = g1)
  subset_PSMC1 <- PSMC1[-(1:12), ]
  
  for(mu2 in list_mu2) {
    for(g2 in list_g2) {
      
      # Run PSMC for dataset 2
      PSMC2 <- psmc.result(PSMC_2, i.iteration = 25, mu = mu2, s = 100, g = g2)
      subset_PSMC2 <- PSMC2[-(1:12), ]
      
      # Overlapping time range
      overlap_min <- max(min(subset_PSMC1$Time), min(subset_PSMC2$Time))
      overlap_max <- min(max(subset_PSMC1$Time), max(subset_PSMC2$Time))
      
      PSMC1_overlap <- subset_PSMC1[subset_PSMC1$Time >= overlap_min & subset_PSMC1$Time <= overlap_max, ]
      PSMC2_overlap <- subset_PSMC2[subset_PSMC2$Time >= overlap_min & subset_PSMC2$Time <= overlap_max, ]
      
      # Average duplicates
      PSMC1_overlap <- aggregate(Ne ~ Time, data = PSMC1_overlap, FUN = mean)
      PSMC2_overlap <- aggregate(Ne ~ Time, data = PSMC2_overlap, FUN = mean)
      
      # Bin widths
      PSMC1_overlap$twidth <- c(diff(PSMC1_overlap$Time), tail(diff(PSMC1_overlap$Time), 1))
      PSMC2_overlap$twidth <- c(diff(PSMC2_overlap$Time), tail(diff(PSMC2_overlap$Time), 1))
      
      # Common time grid
      n1 <- nrow(PSMC1_overlap)
      n2 <- nrow(PSMC2_overlap)
      n_common <- min(n1, n2)
      
      common_time <- seq(
        max(min(PSMC1_overlap$Time), min(PSMC2_overlap$Time)),
        min(max(PSMC1_overlap$Time), max(PSMC2_overlap$Time)),
        length.out = n_common
      )
      
      # Weighted interpolation
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
      
      interp1 <- weighted_interp(PSMC1_overlap, common_time)
      interp2 <- weighted_interp(PSMC2_overlap, common_time)
      
      bints1 <- data.frame(Time = common_time, Ne = interp1)
      bints2 <- data.frame(Time = common_time, Ne = interp2)
      
      # Loop over correlation methods
      for(method in c("spearman")) { ## ONE can add "pearson" or "kendall" (if that is believed to be a better fit)  or all three ("spearman", "pearson", "kendall"), they can all be run simultaneously
        # Compute binned correlation with BINCOR
        cor_ts(
          bints1, bints2,
          varnamets1 = "PSMC1",
          varnamets2 = "PSMC2",
          KoCM = method,
          rmltrd = "N",
          device = "screen",
          Hfig = 6, Wfig = 8,
          Hpdf = 8, Wpdf = 10,
          resfig = 300,
          ofilename = paste0("cor_psmc1_psmc2_", method, "_mu", mu2, "_g", g2)
        )
        
        # Safe correlation test
        cor_test <- tryCatch(
          cor.test(bints1$Ne, bints2$Ne, method = method),
          error = function(e) return(NULL)
        )
        
        if(!is.null(cor_test)) {
          CI <- if(!is.null(cor_test$conf.int)) cor_test$conf.int else c(NA, NA)
          All_Results <- rbind(All_Results, data.frame(
            Mu2 = mu2,
            G2 = g2,
            Method = method,
            Correlation = cor_test$estimate,
            P_value = cor_test$p.value,
            CI_lower = CI[1],
            CI_upper = CI[2],
            N_common = n_common,
            stringsAsFactors = FALSE
          ))
        } else {
          All_Results <- rbind(All_Results, data.frame(
            Mu2 = mu2,
            G2 = g2,
            Method = method,
            Correlation = NA,
            P_value = NA,
            CI_lower = NA,
            CI_upper = NA,
            N_common = n_common,
            stringsAsFactors = FALSE
          ))
        }
      } # end method loop
    } # end g loop
  } # end mu loop
  
  return(All_Results)
}



plot_heatmap <- function(df, method_choice, species_name) {
  
  library(ggplot2)
  library(dplyr)
  
  df_sub <- df %>%
    filter(Method == method_choice) %>%
    mutate(
      color_group = case_when(
        Correlation < 0 ~ "neg_corr",
        Correlation >= 0 & P_value >= 1e-5 ~ "gray_tile",
        Correlation >= 0 & P_value < 1e-5 ~ "heat"
      ),
      heat_val = ifelse(color_group == "heat", P_value, NA)
    )
  
  ggplot(df_sub, aes(x = factor(Mu2), y = factor(G2), fill = heat_val)) +
    geom_tile(color = "white") +
    scale_fill_gradient(
      low = "darkblue", high = "white", na.value = "transparent",
      limits = c(1e-27, 0.05),   # fixed range
      trans = "log10",
      name = "P-value"
    ) +
    geom_tile(
      data = subset(df_sub, color_group == "neg_corr"),
      aes(x = factor(Mu2), y = factor(G2)),
      fill = "lightgray", inherit.aes = FALSE
    ) +
    geom_tile(
      data = subset(df_sub, color_group == "gray_tile"),
      aes(x = factor(Mu2), y = factor(G2)),
      fill = "white", inherit.aes = FALSE
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    ) +
    labs(
      title = bquote("Heatmap of P-values for " * italic(.(species_name)) ~ "-" ~ .(method_choice) ~ " correlation"),
      x = "Mutation rate (mu)", 
      y = "Generation time (years)"
    )
}

