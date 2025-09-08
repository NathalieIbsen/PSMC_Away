#Run.R 

#An example of how to run an analysis

#set dir 
setwd("C:....")


#______________INPUTS______________

#Data
datafolder <- "C:/Users/au601132/OneDrive - Aarhus universitet/Skrivebord/PhD/Kolibri_heliconia/PSMC/"

# Main PSMC results
EulJugA <- dir(paste0(datafolder, "Auto/"), pattern = "psmc$", full.names = TRUE)
HelBih  <- dir(paste0(datafolder, "HBih/"), pattern = "psmc$", full.names = TRUE)
HelCar  <- dir(paste0(datafolder, "HCar/"), pattern = "psmc$", full.names = TRUE)
EulHol  <- dir(paste0(datafolder, "Ehol/"), pattern = "psmc$", full.names = TRUE)


##Define parameter space of PCMS1


#MUTATION RATE 
mu1 <- 2.8e-9

#GENERATION TIME
g1 <- 2.5


##Define parameter space of PCMS2

#MUTATION RATE 
#a linear sequence within this range
list_mu2 <- seq(1e-8, 9e-8, length.out = 11)  

#alternatively within another range: 
#list_mu2_9 <- seq(1e-9, 9e-9, length.out = 30)  


#GENERATION TIME 
list_g2 = c(3,6,9,12,15,18,21,24,27,30,33) # (as many as wanted)

#Reversing the list for downstream plotting 
list_g2 <- rev(list_g2)   # (optional - But more intuitive)


#______That is it for inputs_________

# Lets get to it ! 

###Running the Bincorr function

#E. jujularis vs. H. bihai 
Result_EulJugA_HelBih_8 <- BinCorr_psmc(EulJugA, HelBih, mu1, g1, list_mu2, list_g2)

#E. jujularis vs. H. caribea
Result_EulJugA_HelCar_8 <- BinCorr_psmc(EulJugA, HelCar, mu1, g1, list_mu2, list_g2)


#### less of a mutualist  
#E. holosericeus vs H. bihai 
Result_EulHol_HelBih_8 <- BinCorr_psmc(EulHol, HelBih, mu1, g1, list_mu2, list_g2)

#E. holosericeus vs H. caribea 
Result_EulHol_HelCar_8 <- BinCorr_psmc(EulHol, HelCar, mu1, g1, list_mu2, list_g2)


#plot the results! 

plot_heatmap(Result_EulJugA_HelBih_8, "spearman", "H. bihai")

plot_heatmap(Result_EulJugA_HelCar_8, "spearman", "H. caribea")

plot_heatmap(Result_EulHol_HelBih_8, "spearman", "H. bihai")

plot_heatmap(Result_EulHol_HelCar_8, "spearman", "H. caribea")



#ONE EXPAMLE of how to extract the best fits

#Extract the 10 most significant per correlation method. 

library(dplyr)
library(flextable)
library(webshot2)

# Extract top 10 per method
top10_per_method <- Result_EulJugA_HelBih_8 %>%
  group_by(Method) %>%
  arrange(P_value, .by_group = TRUE) %>%
  slice_head(n = 10)

# Create flextable
ft <- flextable(top10_per_method)
ft <- set_caption(ft, "Top 10 Most Significant Correlations per Method EulJugA_HelBih")
ft <- autofit(ft)

# Save as PNG in WD 
save_as_image(ft, path = "Top10_e-8_CorrelationsEulJugA_HelBih.png")






