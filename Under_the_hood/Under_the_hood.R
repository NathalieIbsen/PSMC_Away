#Under the hood of the BinCorr function
## RUNNING THE SCRIPT Line-By-Line
# And plotting some context figures for the data transformation steps! 


####FIRST 
## First a I use: 
### A Modified from old script by Shenglin Liu, Mar 25, 2019.

#Load the function by 


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

#Data


EulJugA <- dir("C://","psmc$", full.names=T) # main PSMC results
HelBih <- dir("C://","psmc$", full.names=T) # main PSMC results
HelCar <- dir("C://","psmc$", full.names=T) # main PSMC results
EulHol <- dir("C://","psmc$", full.names=T) # main PSMC results


# Run the Function for a selected set of parameters: 
psmcbird <- psmc.result(EulJugA, i.iteration=25, mu=2.8e-9, s=100, g=2.3)
psmc_Car <- psmc.result(HelCar, i.iteration=25, mu=7.620690e-09, s=100, g=6)
psmc_Bih <- psmc.result(HelBih, i.iteration=25, mu=8.724138e-09, s=100, g=5)
psmc_Z_bird <- psmc.result(EulJugZ, i.iteration=25, mu=2.8e-9, s=100, g=2.3)




#Just check it out now!! (...funk soul brother!...) 
head(psmc_Bih)
head(psmc_Car)
head(psmcbird)
head(psmc_Z_bird)


#Plot the "raw" psmc. 


ggplot() +
  geom_line(aes(x = psmc_Bih$Time, y = psmc_Bih$Ne), 
            size = 1, color = "red2", na.rm = TRUE) +
  geom_line(aes(x = psmc_Car$Time, y = psmc_Car$Ne), 
            size = 1, color = "yellow3", na.rm = TRUE) +
  geom_line(aes(x = psmcbird$Time, y = psmcbird$Ne), 
            size = 1, color = "purple", na.rm = TRUE) +
  # No legend if not needed
  xlim(0, 6000000) +
  ylim(0, 1500000) +
  theme_minimal() 





#To run the analysis please install the below required packages: 
#Install: 
#and read in: 


library(lmtest)
library(imputeTS)
library(dplyr)
library(tseries)
library(tsibble)
library(ggplot2)
library(purrr)


#data.frame for results
All_Results <- data.frame()




# cut away the first 12 time points. 

subset_psmcbird <- psmcbird[-(1:12), ]
subset_psmc_Car <- psmc_Car[-(1:12), ]
subset_psmc_Bih <- psmc_Bih[-(1:12), ]

#Plot the cut psmc. 

ggplot() +
  geom_line(aes(x = subset_psmc_Bih$Time, y = subset_psmc_Bih$Ne), 
            size = 1, color = "red2", na.rm = TRUE) +
  #geom_line(aes(x = subset_psmc_Car$Time, y = subset_psmc_Car$Ne), 
  #size = 1, color = "yellow3", na.rm = TRUE) +
  geom_line(aes(x = subset_psmcbird$Time, y = subset_psmcbird$Ne), 
            size = 1, color = "purple", na.rm = TRUE) +
  # No legend if not needed
  xlim(0, 6000000) +
  ylim(0, 1500000) +
  theme_minimal() 



min_bird <- min(subset_psmcbird$Time)
max_bird <- max(subset_psmcbird$Time)


#Testing Bih 

# Overlapping time range
overlap_min <- max(min(subset_psmcbird$Time), min(subset_psmc_Bih$Time))
overlap_max <- min(max(subset_psmcbird$Time), max(subset_psmc_Bih$Time))

bird_overlap <- subset_psmcbird[subset_psmcbird$Time >= overlap_min & subset_psmcbird$Time <= overlap_max, ]
plant_overlap <- subset_psmc_Bih[subset_psmc_Bih$Time >= overlap_min & subset_psmc_Bih$Time <= overlap_max, ]

# Average duplicates
bird_overlap <- aggregate(Ne ~ Time, data = bird_overlap, FUN = mean)
plant_overlap <- aggregate(Ne ~ Time, data = plant_overlap, FUN = mean)

# Bin widths recording
bird_overlap$twidth <- c(diff(bird_overlap$Time), tail(diff(bird_overlap$Time), 1))
plant_overlap$twidth <- c(diff(plant_overlap$Time), tail(diff(plant_overlap$Time), 1))

# Common time grid 
n_bird  <- nrow(bird_overlap)
n_plant <- nrow(plant_overlap)
n_common <- min(n_bird, n_plant)

#Transform the a common time

common_time <- seq(
  max(min(bird_overlap$Time), min(plant_overlap$Time)),
  min(max(bird_overlap$Time), max(plant_overlap$Time)),
  length.out = n_common
)

# Weighted interpolation based on the bin original width of the PSMC 
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

bird_interp <- weighted_interp(bird_overlap, common_time)
plant_interp <- weighted_interp(plant_overlap, common_time)

bints1 <- data.frame(Time = common_time, Ne = bird_interp)
bints2 <- data.frame(Time = common_time, Ne = plant_interp)

plot(x = bints2$Time, y = bints2$Ne)
plot(x = bints1$Time, y = bints1$Ne)
plot(x = subset_psmcbird$Time, y = subset_psmcbird$Ne)
plot(x = subset_psmc_Bih$Time, y = subset_psmc_Bih$Ne)


#Plot the Weighted interpolation

ggplot() +
  geom_point(aes(x = bints2$Time, y = bints2$Ne), 
             size = 1, color = "red2", na.rm = TRUE) +
  #geom_line(aes(x = subset_psmc_Car$Time, y = subset_psmc_Car$Ne), 
  #size = 1, color = "yellow3", na.rm = TRUE) +
  geom_point(aes(x = bints1$Time, y = bints1$Ne), 
             size = 1, color = "purple", na.rm = TRUE) +
  # No legend if not needed
  xlim(0, 6000000) +
  ylim(0, 500000) +
  theme_minimal() 


#Plot the original data from the cut PSMC

ggplot() +
  geom_point(aes(x = subset_psmc_Bih$Time, y = subset_psmc_Bih$Ne), 
             size = 1, color = "red2", na.rm = TRUE) +
  #geom_line(aes(x = subset_psmc_Car$Time, y = subset_psmc_Car$Ne), 
  #size = 1, color = "yellow3", na.rm = TRUE) +
  geom_point(aes(x = subset_psmcbird$Time, y = subset_psmcbird$Ne), 
             size = 1, color = "purple", na.rm = TRUE) +
  # No legend if not needed
  xlim(0, 6000000) +
  ylim(0, 500000) +
  theme_minimal() 

#Plot the Weighted interpolation as a PSMC: 

ggplot() +
  geom_line(aes(x = bints2$Time, y = bints2$Ne), 
            size = 1, color = "red2", na.rm = TRUE) +
  #geom_line(aes(x = subset_psmc_Car$Time, y = subset_psmc_Car$Ne), 
  #size = 1, color = "yellow3", na.rm = TRUE) +
  geom_line(aes(x = bints1$Time, y = bints1$Ne), 
            size = 1, color = "purple", na.rm = TRUE) +
  # No legend if not needed
  xlim(0, 6000000) +
  ylim(0, 1500000) +
  theme_minimal() 


# the original plot of the cut PSMC


ggplot() +
  geom_line(aes(x = subset_psmc_Bih$Time, y = subset_psmc_Bih$Ne), 
            size = 1, color = "red2", na.rm = TRUE) +
  #geom_line(aes(x = subset_psmc_Car$Time, y = subset_psmc_Car$Ne), 
  #size = 1, color = "yellow3", na.rm = TRUE) +
  geom_line(aes(x = subset_psmcbird$Time, y = subset_psmcbird$Ne), 
            size = 1, color = "purple", na.rm = TRUE) +
  # No legend if not needed
  xlim(0, 6000000) +
  ylim(0, 1500000) +
  theme_minimal() 


# Loop over correlation methods
for(method in c("pearson", "spearman", "kendall")) {
  # Compute binned correlation with BINCOR
  cor_ts(
    bints1, bints2,
    varnamets1 = "Bird",
    varnamets2 = "Plant",
    KoCM = method,
    rmltrd = "N",
    device = "screen",
    Hfig = 6, Wfig = 8,
    Hpdf = 8, Wpdf = 10,
    resfig = 300,
    ofilename = paste0("cor_bird_plant_", method, "_mu", mu, "_g", g)
  )
  
  # Safe correlation test
  cor_test <- tryCatch(
    cor.test(bints1$Ne, bints2$Ne, method = method),
    error = function(e) return(NULL)
  )
  
  if(!is.null(cor_test)) {
    CI <- if(!is.null(cor_test$conf.int)) cor_test$conf.int else c(NA, NA)
    All_Results <- rbind(All_Results, data.frame(
      Plant_mu = mu,
      Plant_g = g,
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
      Plant_mu = mu,
      Plant_g = g,
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



