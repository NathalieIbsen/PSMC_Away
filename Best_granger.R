
#Varying and contrasting different plant mutationrates (mu / m) and generationtimes (g / gen)
library(gridExtra)
library(ggplot2)
library(grid)


###
#First line of questioning: 
# can a reasonable parameter space be found in which the Ne trend of H. Caribae and E.Jugularis ?
# and H. Bihai and E.Jugularis ? 
# YES ! 
# do they differ ?
# YES! 
# Are these plant parameter spaces robust to changes in the parameter space of E.jugularis ?
# YES ! 


###INPUT VARIABLES

#a linear sequence within this range
plant_mu <- seq(1e-8, 9e-8, length.out = 30)  # Adjust length.out as needed
print(plant_mu)


plant_gen = c(3,
              4,
              5,
              6,
              7,
              8,
              9,
              10,
              11,
              12,
              13,
              14,
              15, # "15-18 H. Bihai and H. caribea" 
              16,
              17,
              18,
              19,
              20,
              21,
              22,
              23,
              24,
              25,
              26,
              27,
              28,
              29,
              30,
              31,
              32) # tilføj alle 3-25




setwd("C:/Users/au601132/OneDrive - Aarhus universitet/Skrivebord/PhD/Kolibri_heliconia/-log10/")


####Give the paths to The dir where PSMC is stored 

###INPUT PATHS 

EulJugA <- dir("C:/Users/au601132/OneDrive - Aarhus universitet/Skrivebord/PhD/Kolibri_heliconia/PSMC/Auto/","psmc$", full.names=T) # main PSMC results
HelBih <- dir("C:/Users/au601132/OneDrive - Aarhus universitet/Skrivebord/PhD/Kolibri_heliconia/PSMC/HBih/","psmc$", full.names=T) # main PSMC results
HelCar <- dir("C:/Users/au601132/OneDrive - Aarhus universitet/Skrivebord/PhD/Kolibri_heliconia/PSMC/HCar/","psmc$", full.names=T) # main PSMC results
EulHol <- dir("C:/Users/au601132/OneDrive - Aarhus universitet/Skrivebord/PhD/Kolibri_heliconia/PSMC/EHol/","psmc$", full.names=T) # main PSMC results
AVer <- dir("C:/Users/au601132/OneDrive - Aarhus universitet/Skrivebord/PhD/Kolibri_heliconia/PSMC/AVer/","psmc$", full.names=T) # main PSMC results

OrCr <- dir("C:/Users/au601132/OneDrive - Aarhus universitet/Skrivebord/PhD/Kolibri_heliconia/PSMC/OrCr/","psmc$", full.names=T) # main PSMC results
CyBi <- dir("C:/Users/au601132/OneDrive - Aarhus universitet/Skrivebord/PhD/Kolibri_heliconia/PSMC/CyBi/","psmc$", full.names=T) # main PSMC results

# 
# EU_HELCAR <- NEW_Granger_psmc(EulJugA, HelCar, bird_mu=6.8e-8, plant_mu, plant_gen, bird_g=2.3)
# EU_HELBIH <- NEW_Granger_psmc(EulJugA, HelBih, bird_mu=6.8e-8, plant_mu, plant_gen, bird_g=2.3)


bird_gen = c(2, 2.3, 2.6, 2.9)
bird_mu = c(5.6e-9,2.5e-9,1.8e-9)
# #If i want the above comparisons no of ?

result <- c()  # Initialize an empty vector

for (mu in bird_mu) {
  for (g in bird_gen) {
    result <- c(result, paste0(mu, "_", g))  # Append new combinations
  }
}

length(result)

# #### alternatively: 
results_HELCAR <- list() 
results_HELBIH <- list()

# 
# for (mu in bird_mu) {
#   for (g in bird_gen) {
#     key <- paste0(mu, "_", g)  # Create a unique key for indexing
# 
#     results_HELCAR[[key]] <- NEW_Granger_psmc(EulJugA, HelCar, bird_mu=mu, plant_mu, plant_gen, bird_g=g)
#     results_HELBIH[[key]] <- NEW_Granger_psmc(EulJugA, HelBih, bird_mu=mu, plant_mu, plant_gen, bird_g=g)
#   }
# }

# Access results like th
str(results_HELCAR)  #lookup 

print(results_HELCAR[["6.5e-08_2"]])
names(results_HELCAR)  # Print all keys






V2_EU_HELBIH <- create_matrix(EU_HELBIH, "V2", "H.B effect on E.jugularis, Order 1")
V3_EU_HELBIH <- create_matrix(EU_HELBIH, "V3", "H.B effect on E.jugularis, Order 2")
V4_EU_HELBIH <- create_matrix(EU_HELBIH, "V4", "H.B effect on E.jugularis, Order 3")
V5_EU_HELBIH <- create_matrix(EU_HELBIH, "V5", "H.B effect on E.jugularis, Order 4")
V6_EU_HELBIH <- create_matrix(EU_HELBIH, "V6", "H.B effect on E.jugularis, Order 5")
V7_EU_HELBIH <- create_matrix(EU_HELBIH, "V7", "E.jugularis effect on H.B, Order 1")
V8_EU_HELBIH <- create_matrix(EU_HELBIH, "V8", "E.jugularis effect on H.B, Order 2")
V9_EU_HELBIH <- create_matrix(EU_HELBIH, "V9", "E.jugularis effect on H.B, Order 3")
V10_EU_HELBIH <- create_matrix(EU_HELBIH, "V10", "E.jugularis effect on H.B, Order 4")
V11_EU_HELBIH <- create_matrix(EU_HELBIH, "V11", "E.jugularis effect on H.B, Order 5")



V2_EU_HELCAR <- create_matrix(EU_HELCAR, "V2", "H.C effect on E.Jugularis, Order 1")
V3_EU_HELCAR <- create_matrix(EU_HELCAR, "V3", "H.C effect on E.Jugularis, Order 2")
V4_EU_HELCAR <- create_matrix(EU_HELCAR, "V4", "H.C effect on E.Jugularis, Order 3")
V5_EU_HELCAR <- create_matrix(EU_HELCAR, "V5", "H.C effect on E.Jugularis, Order 4")
V6_EU_HELCAR <- create_matrix(EU_HELCAR, "V6", "H.C effect on E.Jugularis, Order 5")
V7_EU_HELCAR <- create_matrix(EU_HELCAR, "V7", "E.jugularis effect on H.C, Order 1")
V8_EU_HELCAR <- create_matrix(EU_HELCAR, "V8", "E.jugularis effect on H.C, Order 2")
V9_EU_HELCAR <- create_matrix(EU_HELCAR, "V9", "E.jugularis effect on H.C, Order 3")
V10_EU_HELCAR <- create_matrix(EU_HELCAR, "V10", "E.jugularis effect on H.C, Order 4")
V11_EU_HELCAR <- create_matrix(EU_HELCAR, "V11", "E.jugularis effect on H.C, Order 5")



# Convert pheatmaps to grobs
plot1 <- grid.grabExpr(print(V2_EU_HELBIH$plot))
plot2 <- grid.grabExpr(print(V3_EU_HELBIH$plot))
plot3 <- grid.grabExpr(print(V4_EU_HELBIH$plot))
plot4 <- grid.grabExpr(print(V5_EU_HELBIH$plot))
plot5 <- grid.grabExpr(print(V6_EU_HELBIH$plot))

plot6 <- grid.grabExpr(print(V7_EU_HELBIH$plot))
plot7 <- grid.grabExpr(print(V8_EU_HELBIH$plot))
plot8 <- grid.grabExpr(print(V9_EU_HELBIH$plot))
plot9 <- grid.grabExpr(print(V10_EU_HELBIH$plot))
plot10 <- grid.grabExpr(print(V11_EU_HELBIH$plot))

plot11 <- grid.grabExpr(print(V2_EU_HELCAR$plot))
plot12 <- grid.grabExpr(print(V3_EU_HELCAR$plot))
plot13 <- grid.grabExpr(print(V4_EU_HELCAR$plot))
plot14 <- grid.grabExpr(print(V5_EU_HELCAR$plot))
plot15 <- grid.grabExpr(print(V6_EU_HELCAR$plot))

plot16 <- grid.grabExpr(print(V7_EU_HELCAR$plot))
plot17 <- grid.grabExpr(print(V8_EU_HELCAR$plot))
plot18 <- grid.grabExpr(print(V9_EU_HELCAR$plot))
plot19 <- grid.grabExpr(print(V10_EU_HELCAR$plot))
plot20 <- grid.grabExpr(print(V11_EU_HELCAR$plot))




# Open a PDF file to save the plots
pdf("Plant_mu_e08_vs_bird_mu_6.8e-8_g2.3.pdf", width = 14, height = 10)



# Arrange the plots in a 2-row layout
grid.arrange(
  plot1, plot2, plot3, plot4, plot5,
  plot6, plot7, plot8, plot9, plot10,
  plot11, plot12, plot13, plot14, plot15,
  plot16, plot17, plot18, plot19, plot20,
  nrow = 4
)

# Close the PDF device
dev.off()





### DO bothway effect plots ! 
###  -> <- 



EU_HB_sum_1 <- V2_EU_HELBIH$log_matrix + V7_EU_HELBIH$log_matrix 
EU_HB_sum_2 <- V3_EU_HELBIH$log_matrix + V8_EU_HELBIH$log_matrix 
EU_HB_sum_3 <- V4_EU_HELBIH$log_matrix + V9_EU_HELBIH$log_matrix 
EU_HB_sum_4 <- V5_EU_HELBIH$log_matrix + V10_EU_HELBIH$log_matrix 
EU_HB_sum_5 <- V6_EU_HELBIH$log_matrix + V11_EU_HELBIH$log_matrix 

EU_HC_sum_1 <- V2_EU_HELCAR$log_matrix + V7_EU_HELCAR$log_matrix 
EU_HC_sum_2 <- V3_EU_HELCAR$log_matrix + V8_EU_HELCAR$log_matrix 
EU_HC_sum_3 <- V4_EU_HELCAR$log_matrix + V9_EU_HELCAR$log_matrix 
EU_HC_sum_4 <- V5_EU_HELCAR$log_matrix + V10_EU_HELCAR$log_matrix 
EU_HC_sum_5 <- V6_EU_HELCAR$log_matrix + V11_EU_HELCAR$log_matrix 


EU_HB_PLOT1 <- grid.grabExpr(quickplot_heat(EU_HB_sum_1, "H.B VS. E.jugularis, order = 1"))
EU_HB_PLOT2 <- grid.grabExpr(quickplot_heat(EU_HB_sum_2, "H.B VS. E.jugularis, order = 2"))
EU_HB_PLOT3 <- grid.grabExpr(quickplot_heat(EU_HB_sum_3, "H.B VS. E.jugularis, order = 3"))
EU_HB_PLOT4 <- grid.grabExpr(quickplot_heat(EU_HB_sum_4, "H.B VS. E.jugularis, order = 4"))
EU_HB_PLOT5 <- grid.grabExpr(quickplot_heat(EU_HB_sum_5, "H.B VS. E.jugularis, order = 5"))


EU_HC_PLOT1 <- grid.grabExpr(quickplot_heat(EU_HC_sum_1, "H.C VS. E.jugularis, order = 1"))
EU_HC_PLOT2 <- grid.grabExpr(quickplot_heat(EU_HC_sum_2, "H.C VS. E.jugularis, order = 2"))
EU_HC_PLOT3 <- grid.grabExpr(quickplot_heat(EU_HC_sum_3, "H.C VS. E.jugularis, order = 3"))
EU_HC_PLOT4 <- grid.grabExpr(quickplot_heat(EU_HC_sum_4, "H.C VS. E.jugularis, order = 4"))
EU_HC_PLOT5 <- grid.grabExpr(quickplot_heat(EU_HC_sum_5, "H.C VS. E.jugularis, order = 5"))


# Open a PDF file to save the plots
pdf("Plant+bird_mu_e08_6.8e-8_g2.3.pdf", width = 14, height = 8)



# Arrange the plots in a 2-row layout
grid.arrange(
  EU_HB_PLOT1,EU_HB_PLOT2,EU_HB_PLOT3,EU_HB_PLOT4,EU_HB_PLOT5,
  EU_HC_PLOT1,EU_HC_PLOT2,EU_HC_PLOT3,EU_HC_PLOT4,EU_HC_PLOT5,
  nrow = 2
)


##EXploring the bird parameter space


######################STEP 1


# #If i want the above comparisons^^^ 
# Just check how many
# result <- c()  # Initialize an empty vector
# 
# for (mu in bird_mu) {
#   for (g in bird_gen) {

#### Run The Granger function 
# results_HELCAR <- list()
# results_HELBIH <- list()
# 
# for (mu in bird_mu) {
#   for (g in bird_gen) {
#     key <- paste0(mu, "_", g)  # Create a unique key for indexing
#     
#     results_HELCAR[[key]] <- NEW_Granger_psmc(EulJugA, HelCar, bird_mu=mu, plant_mu, plant_gen, bird_g=g)
#     results_HELBIH[[key]] <- NEW_Granger_psmc(EulJugA, HelBih, bird_mu=mu, plant_mu, plant_gen, bird_g=g)
#   }
# }

#Check out 
str(results_HELBIH)  #lookup 

List_HelBIH <- names(results_HELBIH)

str(results_HELBIH[["1.8e-08_2.3"]])


######################STEP 2
#For H. Bihai

joined_EU_BIH <- list()  # Initialize an empty list
combined_effects <- list()
for (name in List_HelBIH)  {
  joined_EU_BIH[[name]]$V2 <- create_matrix(results_HELBIH[[name]], "V2", "H.B on E.jugularis, Order 1")  # Copy the original data frame
  joined_EU_BIH[[name]]$V7 <- create_matrix(results_HELBIH[[name]], "V7", "E.jugularis on H.B, Order 1")  # Copy the original data frame
  combined_effects[[name]]$summed_effect_OD1 <- joined_EU_BIH[[name]]$V2$log_matrix + joined_EU_BIH[[name]]$V7$log_matrix
}

str(joined_EU_BIH[["1.8e-09_2.6"]]$matrix)

combined_effects[["1.8e-09_2.6"]]$summed_effect_OD1


EU_HB_PLOT12 <- grid.grabExpr(quickplot_heat(combined_effects[["5.6e-09_2"]]$summed_effect_OD1, "H.B VS. E.jugularis, 1.8e-08_2"))
EU_HB_PLOT11 <- grid.grabExpr(quickplot_heat(combined_effects[["5.6e-09_2.3"]]$summed_effect_OD1, "H.B VS. E.jugularis, 1.8e-08_2.3"))
EU_HB_PLOT10 <- grid.grabExpr(quickplot_heat(combined_effects[["5.6e-09_2.6"]]$summed_effect_OD1, "H.B VS. E.jugularis, 1.8e-08_2.6"))
EU_HB_PLOT9 <- grid.grabExpr(quickplot_heat(combined_effects[["5.6e-09_2.9"]]$summed_effect_OD1, "H.B VS. E.jugularis, 1.8e-08_2.9"))
EU_HB_PLOT8 <- grid.grabExpr(quickplot_heat(combined_effects[["2.5e-09_2"]]$summed_effect_OD1, "H.B VS. E.jugularis, 2.8e-08_2"))
EU_HB_PLOT7 <- grid.grabExpr(quickplot_heat(combined_effects[["2.5e-09_2.3"]]$summed_effect_OD1, "H.B VS. E.jugularis, 2.8e-08_2.3"))
EU_HB_PLOT6 <- grid.grabExpr(quickplot_heat(combined_effects[["2.5e-09_2.6"]]$summed_effect_OD1, "H.B VS. E.jugularis, 2.8e-08_2.6"))
EU_HB_PLOT5 <- grid.grabExpr(quickplot_heat(combined_effects[["2.5e-09_2.9"]]$summed_effect_OD1, "H.B VS. E.jugularis, 2.8e-08_2.9"))
EU_HB_PLOT1 <- grid.grabExpr(quickplot_heat(combined_effects[["1.8e-09_2"]]$summed_effect_OD1, "H.B VS. E.jugularis, 3.8e-08_2"))
EU_HB_PLOT2 <- grid.grabExpr(quickplot_heat(combined_effects[["1.8e-09_2.3"]]$summed_effect_OD1, "H.B VS. E.jugularis, 3.8e-08_2.3"))
EU_HB_PLOT3 <- grid.grabExpr(quickplot_heat(combined_effects[["1.8e-09_2.6"]]$summed_effect_OD1, "H.B VS. E.jugularis, 3.8e-08_2.6"))
EU_HB_PLOT4 <- grid.grabExpr(quickplot_heat(combined_effects[["1.8e-09_2.9"]]$summed_effect_OD1, "H.B VS. E.jugularis, 3.8e-08_2.9"))



EU_HB_PLOT13 <- grid.grabExpr(quickplot_heat(combined_effects[["4.8e-08_2"]]$summed_effect_OD1, "H.B VS. E.jugularis, 4.8e-08_2"))
EU_HB_PLOT14 <- grid.grabExpr(quickplot_heat(combined_effects[["4.8e-08_2.3"]]$summed_effect_OD1, "H.B VS. E.jugularis, 4.8e-08_2.3"))
EU_HB_PLOT15 <- grid.grabExpr(quickplot_heat(combined_effects[["4.8e-08_2.6"]]$summed_effect_OD1, "H.B VS. E.jugularis, 4.8e-08_2.6"))
EU_HB_PLOT16 <- grid.grabExpr(quickplot_heat(combined_effects[["4.8e-08_2.9"]]$summed_effect_OD1, "H.B VS. E.jugularis, 4.8e-08_2.9"))
EU_HB_PLOT17 <- grid.grabExpr(quickplot_heat(combined_effects[["5.8e-08_2"]]$summed_effect_OD1, "H.B VS. E.jugularis, 5.8e-08_2"))
EU_HB_PLOT18 <- grid.grabExpr(quickplot_heat(combined_effects[["5.8e-08_2.3"]]$summed_effect_OD1, "H.B VS. E.jugularis, order = 1"))
EU_HB_PLOT19 <- grid.grabExpr(quickplot_heat(combined_effects[["5.8e-08_2.6"]]$summed_effect_OD1, "H.B VS. E.jugularis, order = 1"))
EU_HB_PLOT20 <- grid.grabExpr(quickplot_heat(combined_effects[["5.8e-08_2.9"]]$summed_effect_OD1, "H.B VS. E.jugularis, order = 1"))
EU_HB_PLOT21 <- grid.grabExpr(quickplot_heat(combined_effects[["6.8e-08_2"]]$summed_effect_OD1, "H.B VS. E.jugularis, order = 1"))
EU_HB_PLOT22 <- grid.grabExpr(quickplot_heat(combined_effects[["6.8e-08_2.3"]]$summed_effect_OD1, "H.B VS. E.jugularis, order = 1"))
EU_HB_PLOT23 <- grid.grabExpr(quickplot_heat(combined_effects[["6.8e-08_2.6"]]$summed_effect_OD1, "H.B VS. E.jugularis, order = 1"))
EU_HB_PLOT24 <- grid.grabExpr(quickplot_heat(combined_effects[["6.8e-08_2.9"]]$summed_effect_OD1, "H.B VS. E.jugularis, order = 1"))
EU_HB_PLOT25 <- grid.grabExpr(quickplot_heat(combined_effects[["7.8e-08_2"]]$summed_effect_OD1, "H.B VS. E.jugularis, order = 1"))
EU_HB_PLOT26 <- grid.grabExpr(quickplot_heat(combined_effects[["7.8e-08_2.3"]]$summed_effect_OD1, "H.B VS. E.jugularis, order = 1"))
EU_HB_PLOT27 <- grid.grabExpr(quickplot_heat(combined_effects[["7.8e-08_2.6"]]$summed_effect_OD1, "H.B VS. E.jugularis, order = 1"))
EU_HB_PLOT28 <- grid.grabExpr(quickplot_heat(combined_effects[["7.8e-08_2.9"]]$summed_effect_OD1, "H.B VS. E.jugularis, order = 1"))
EU_HB_PLOT29 <- grid.grabExpr(quickplot_heat(combined_effects[["8.8e-08_2"]]$summed_effect_OD1, "H.B VS. E.jugularis, order = 1"))
EU_HB_PLOT30 <- grid.grabExpr(quickplot_heat(combined_effects[["8.8e-08_2.3"]]$summed_effect_OD1, "H.B VS. E.jugularis, order = 1"))
EU_HB_PLOT31 <- grid.grabExpr(quickplot_heat(combined_effects[["8.8e-08_2.6"]]$summed_effect_OD1, "H.B VS. E.jugularis, order = 1"))
EU_HB_PLOT32 <- grid.grabExpr(quickplot_heat(combined_effects[["8.8e-08_2.9"]]$summed_effect_OD1, "H.B VS. E.jugularis, order = 1"))


setwd("C:/Users/au601132/OneDrive - Aarhus universitet/Skrivebord/PhD/Kolibri_heliconia/-log10")

# Open a PDF file to save the plots
pdf("V2_Varying_CYBi_mu9_to_HB_mu_8.pdf", width = 28, height = 14)
#png("Varying_EU_to_HB_mu_e8.png")

# Arrange the plots in a 4-row layout
grid.arrange(EU_HB_PLOT1, EU_HB_PLOT5, EU_HB_PLOT9,# EU_HB_PLOT13, EU_HB_PLOT17, EU_HB_PLOT21, EU_HB_PLOT24, EU_HB_PLOT24, EU_HB_PLOT29,
             EU_HB_PLOT2, EU_HB_PLOT6, EU_HB_PLOT10,# EU_HB_PLOT14, EU_HB_PLOT18, EU_HB_PLOT22, EU_HB_PLOT22, EU_HB_PLOT26, EU_HB_PLOT30,
             EU_HB_PLOT3, EU_HB_PLOT7, EU_HB_PLOT11, #EU_HB_PLOT15, EU_HB_PLOT19, EU_HB_PLOT23, EU_HB_PLOT23, EU_HB_PLOT27, EU_HB_PLOT31,
             EU_HB_PLOT4, EU_HB_PLOT8, EU_HB_PLOT12, #EU_HB_PLOT16, EU_HB_PLOT20, EU_HB_PLOT24, EU_HB_PLOT24, EU_HB_PLOT28, EU_HB_PLOT32,
             
             nrow = 4
)

dev.off()


# Arrange the plots in a 4-row layout
grid.arrange(EU_HB_PLOT1,EU_HB_PLOT2,EU_HB_PLOT3,EU_HB_PLOT4,
             EU_HB_PLOT5,EU_HB_PLOT6,EU_HB_PLOT7,EU_HB_PLOT8,
             EU_HB_PLOT9,EU_HB_PLOT10,EU_HB_PLOT11,EU_HB_PLOT12,
             EU_HB_PLOT13,EU_HB_PLOT14,EU_HB_PLOT15,EU_HB_PLOT16,
             EU_HB_PLOT17,EU_HB_PLOT18,EU_HB_PLOT19,EU_HB_PLOT20,
             EU_HB_PLOT21,EU_HB_PLOT22,EU_HB_PLOT23,EU_HB_PLOT24,
             EU_HB_PLOT24,EU_HB_PLOT26,EU_HB_PLOT27,EU_HB_PLOT28,
             EU_HB_PLOT29,EU_HB_PLOT30,EU_HB_PLOT31,EU_HB_PLOT32,
             nrow = 8
)

#dev.new()




###GET max
##
max(combined_effects[["2.5e-09_2.3"]]$summed_effect_OD1)
colnames(combined_effects[["2.5e-09_2.3"]]$summed_effect_OD1)


# Flatten the matrix/data frame
flat <- as.vector(combined_effects[["2.5e-09_2.3"]]$summed_effect_OD1)

# Get the indices of the 10 largest values
top10_idx <- order(flat, decreasing = TRUE)[1:10]

# Get the actual values
top10_values <- flat[top10_idx]

# Map back to row/column using arrayInd
top10_pos <- arrayInd(top10_idx, 
                      .dim = dim(combined_effects[["2.5e-09_2.3"]]$summed_effect_OD1))

# Combine positions and values
top10_df <- data.frame(
  row = top10_pos[, 1],
  col = top10_pos[, 2],
  value = top10_values,
  rowname = rownames(combined_effects[["2.5e-09_2.3"]]$summed_effect_OD1)[top10_pos[, 1]],
  colname = colnames(combined_effects[["2.5e-09_2.3"]]$summed_effect_OD1)[top10_pos[, 2]]
)

print(top10_df)
#Column	Meaning
#row =	Index in the matrix (1-based)
#col =	Column index in the matrix
#value =	The actual value of summed_effect_OD1 at that position
#rowname =	Actual label of the row (e.g., OD1 level or time point?)
#colname =	The label of the column (Mu)

# Flatten the matrix/data frame
flat <- as.vector(combined_effects[["2.5e-09_2.3"]]$summed_effect_OD1)

# Get the indices of the 10 smallest values
bottom10_idx <- order(flat, decreasing = FALSE)[1:10]

# Get the actual values
bottom10_values <- flat[bottom10_idx]

# Map back to row/column using arrayInd
bottom10_pos <- arrayInd(bottom10_idx, 
                             .dim = dim(combined_effects[["2.5e-09_2.3"]]$summed_effect_OD1))

# Combine positions and values
bottom10_df <- data.frame(
  row = bottom10_pos[, 1],
  col = bottom10_pos[, 2],
  value = bottom10_values,
  rowname = rownames(combined_effects[["2.5e-09_2.3"]]$summed_effect_OD1)[bottom10_pos[, 1]],
  colname = colnames(combined_effects[["2.5e-09_2.3"]]$summed_effect_OD1)[bottom10_pos[, 2]]
)

print(bottom10_df)





######################STEP 2

#For H. BiCAR


List_HelCAR <- names(results_HELCAR)

str(results_HELCAR)


joined_EU_CAR <- list()  # Initialize an empty list
combined_effects_CAR <- list()
for (name in List_HelCAR)  {
  joined_EU_CAR[[name]]$V2 <- create_matrix(results_HELCAR[[name]], "V2", "H.C on E.jugularis, Order 1")  # Copy the original data frame
  joined_EU_CAR[[name]]$V7 <- create_matrix(results_HELCAR[[name]], "V7", "E.jugularis on H.C, Order 1")  # Copy the original data frame
  combined_effects_CAR[[name]]$summed_effect_OD1 <- joined_EU_CAR[[name]]$V2$log_matrix + joined_EU_CAR[[name]]$V7$log_matrix
}

str(joined_EU_BIH[["1.8e-08_2.3"]]$V2)

combined_effects[["1.8e-08_2.3"]]$summed_effect_OD1

EU_HC_PLOT9 <- grid.grabExpr(quickplot_heat(combined_effects_CAR[["5.6e-09_2.9"]]$summed_effect_OD1, "H.C VS. E.jugularis, order = 1"))
EU_HC_PLOT10 <- grid.grabExpr(quickplot_heat(combined_effects_CAR[["5.6e-09_2.6"]]$summed_effect_OD1, "H.C VS. E.jugularis, order = 1"))
EU_HC_PLOT11 <- grid.grabExpr(quickplot_heat(combined_effects_CAR[["5.6e-09_2.3"]]$summed_effect_OD1, "H.C VS. E.jugularis, order = 1"))
EU_HC_PLOT12 <- grid.grabExpr(quickplot_heat(combined_effects_CAR[["5.6e-09_2"]]$summed_effect_OD1, "H.C VS. E.jugularis, order = 1"))
EU_HC_PLOT5 <- grid.grabExpr(quickplot_heat(combined_effects_CAR[["2.5e-09_2.9"]]$summed_effect_OD1, "H.C VS. E.jugularis, order = 1"))
EU_HC_PLOT6 <- grid.grabExpr(quickplot_heat(combined_effects_CAR[["2.5e-09_2.6"]]$summed_effect_OD1, "H.C VS. E.jugularis, order = 1"))
EU_HC_PLOT7 <- grid.grabExpr(quickplot_heat(combined_effects_CAR[["2.5e-09_2.3"]]$summed_effect_OD1, "H.C VS. E.jugularis, order = 1"))
EU_HC_PLOT8 <- grid.grabExpr(quickplot_heat(combined_effects_CAR[["2.5e-09_2"]]$summed_effect_OD1, "H.C VS. E.jugularis, order = 1"))
EU_HC_PLOT1 <- grid.grabExpr(quickplot_heat(combined_effects_CAR[["1.8e-09_2.9"]]$summed_effect_OD1, "H.B VS. E.jugularis, order = 1"))
EU_HC_PLOT2 <- grid.grabExpr(quickplot_heat(combined_effects_CAR[["1.8e-09_2.6"]]$summed_effect_OD1, "H.C VS. E.jugularis, order = 1"))
EU_HC_PLOT3 <- grid.grabExpr(quickplot_heat(combined_effects_CAR[["1.8e-09_2.3"]]$summed_effect_OD1, "H.C VS. E.jugularis, order = 1"))
EU_HC_PLOT4 <- grid.grabExpr(quickplot_heat(combined_effects_CAR[["1.8e-09_2"]]$summed_effect_OD1, "H.C VS. E.jugularis, order = 1"))





EU_HC_PLOT13 <- grid.grabExpr(quickplot_heat(combined_effects_CAR[["4.8e-08_2"]]$summed_effect_OD1, "H.B VS. E.jugularis, order = 1"))
EU_HC_PLOT14 <- grid.grabExpr(quickplot_heat(combined_effects_CAR[["4.8e-08_2.3"]]$summed_effect_OD1, "H.C VS. E.jugularis, order = 1"))
EU_HC_PLOT15 <- grid.grabExpr(quickplot_heat(combined_effects_CAR[["4.8e-08_2.6"]]$summed_effect_OD1, "H.C VS. E.jugularis, order = 1"))
EU_HC_PLOT16 <- grid.grabExpr(quickplot_heat(combined_effects_CAR[["4.8e-08_2.9"]]$summed_effect_OD1, "H.C VS. E.jugularis, order = 1"))
EU_HC_PLOT17 <- grid.grabExpr(quickplot_heat(combined_effects_CAR[["5.8e-08_2"]]$summed_effect_OD1, "H.B VS. E.jugularis, order = 1"))
EU_HC_PLOT18 <- grid.grabExpr(quickplot_heat(combined_effects_CAR[["5.8e-08_2.3"]]$summed_effect_OD1, "H.C VS. E.jugularis, order = 1"))
EU_HC_PLOT19 <- grid.grabExpr(quickplot_heat(combined_effects_CAR[["5.8e-08_2.6"]]$summed_effect_OD1, "H.C VS. E.jugularis, order = 1"))
EU_HC_PLOT20 <- grid.grabExpr(quickplot_heat(combined_effects_CAR[["5.8e-08_2.9"]]$summed_effect_OD1, "H.C VS. E.jugularis, order = 1"))
EU_HC_PLOT21 <- grid.grabExpr(quickplot_heat(combined_effects_CAR[["6.8e-08_2"]]$summed_effect_OD1, "H.C VS. E.jugularis, order = 1"))
EU_HC_PLOT22 <- grid.grabExpr(quickplot_heat(combined_effects_CAR[["6.8e-08_2.3"]]$summed_effect_OD1, "H.C VS. E.jugularis, order = 1"))
EU_HC_PLOT23 <- grid.grabExpr(quickplot_heat(combined_effects_CAR[["6.8e-08_2.6"]]$summed_effect_OD1, "H.C VS. E.jugularis, order = 1"))
EU_HC_PLOT24 <- grid.grabExpr(quickplot_heat(combined_effects_CAR[["6.8e-08_2.9"]]$summed_effect_OD1, "H.C VS. E.jugularis, order = 1"))
EU_HC_PLOT25 <- grid.grabExpr(quickplot_heat(combined_effects_CAR[["7.8e-08_2"]]$summed_effect_OD1, "H.C VS. E.jugularis, order = 1"))
EU_HC_PLOT26 <- grid.grabExpr(quickplot_heat(combined_effects_CAR[["7.8e-08_2.3"]]$summed_effect_OD1, "H.C VS. E.jugularis, order = 1"))
EU_HC_PLOT27 <- grid.grabExpr(quickplot_heat(combined_effects_CAR[["7.8e-08_2.6"]]$summed_effect_OD1, "H.C VS. E.jugularis, order = 1"))
EU_HC_PLOT28 <- grid.grabExpr(quickplot_heat(combined_effects_CAR[["7.8e-08_2.9"]]$summed_effect_OD1, "H.C VS. E.jugularis, order = 1"))
EU_HC_PLOT29 <- grid.grabExpr(quickplot_heat(combined_effects_CAR[["8.8e-08_2"]]$summed_effect_OD1, "H.C VS. E.jugularis, order = 1"))
EU_HC_PLOT30 <- grid.grabExpr(quickplot_heat(combined_effects_CAR[["8.8e-08_2.3"]]$summed_effect_OD1, "H.C VS. E.jugularis, order = 1"))
EU_HC_PLOT31 <- grid.grabExpr(quickplot_heat(combined_effects_CAR[["8.8e-08_2.6"]]$summed_effect_OD1, "H.C VS. E.jugularis, order = 1"))
EU_HC_PLOT32 <- grid.grabExpr(quickplot_heat(combined_effects_CAR[["8.8e-08_2.9"]]$summed_effect_OD1, "H.C VS. E.jugularis, order = 1"))




setwd("C:/Users/au601132/OneDrive - Aarhus universitet/Skrivebord/PhD/Kolibri_heliconia/-log10")

# Open a PDF file to save the plots
pdf("V2_Varying_CyBi_mu9_to_HC_mu_e8.pdf", width = 28, height = 14)


# Arrange the plots in a 4-row layout
grid.arrange(EU_HC_PLOT1, EU_HC_PLOT5, EU_HC_PLOT9, #EU_HC_PLOT13, EU_HC_PLOT17, EU_HC_PLOT21, EU_HC_PLOT24, EU_HC_PLOT24, EU_HC_PLOT29,
             EU_HC_PLOT2, EU_HC_PLOT6, EU_HC_PLOT10, #EU_HC_PLOT14, EU_HC_PLOT18, EU_HC_PLOT22, EU_HC_PLOT22, EU_HC_PLOT26, EU_HC_PLOT30,
             EU_HC_PLOT3, EU_HC_PLOT7, EU_HC_PLOT11, #EU_HC_PLOT15, EU_HC_PLOT19, EU_HC_PLOT23, EU_HC_PLOT23, EU_HC_PLOT27, EU_HC_PLOT31,
             EU_HC_PLOT4, EU_HC_PLOT8, EU_HC_PLOT12, #EU_HC_PLOT16, EU_HC_PLOT20, EU_HC_PLOT24, EU_HC_PLOT24, EU_HC_PLOT28, EU_HC_PLOT32,
             nrow = 4
)


dev.off()

dev.new()






# Arrange the plots in a 4-row layout
grid.arrange(EU_HC_PLOT1, EU_HC_PLOT2, EU_HC_PLOT3, EU_HC_PLOT4,
             EU_HC_PLOT5,EU_HC_PLOT6,EU_HC_PLOT7,EU_HC_PLOT8,
             EU_HC_PLOT9,EU_HC_PLOT10,EU_HC_PLOT11,EU_HC_PLOT12,
             EU_HC_PLOT13,EU_HC_PLOT14,EU_HC_PLOT15,EU_HC_PLOT16,
             EU_HC_PLOT17,EU_HC_PLOT18, EU_HC_PLOT19, EU_HC_PLOT20,
             EU_HC_PLOT21,EU_HC_PLOT22,EU_HC_PLOT23,EU_HC_PLOT24,
             EU_HC_PLOT24,EU_HC_PLOT26, EU_HC_PLOT27, EU_HC_PLOT28,
             EU_HC_PLOT29,EU_HC_PLOT30, EU_HC_PLOT31, EU_HC_PLOT32,
             nrow = 8
)

dev.new()



###GET max
##


max(combined_effects_CAR[["2.5e-09_2.3"]]$summed_effect_OD1)
colnames(combined_effects_CAR[["2.5e-09_2.3"]]$summed_effect_OD1)


which(combined_effects_CAR[["2.5e-09_2.3"]]$summed_effect_OD1 == 
        +   max(combined_effects_CAR[["2.5e-09_2.3"]]$summed_effect_OD1), arr.ind = TRUE)


# Flatten the matrix/data frame "VECTORIZE" 
flat_CAR <- as.vector(combined_effects_CAR[["2.5e-09_2.3"]]$summed_effect_OD1)

# Get the indices of the 10 largest values
top10_idx_CAR <- order(flat_CAR, decreasing = TRUE)[1:10]

# Get the actual values
top10_values_CAR <- flat_CAR[top10_idx_CAR]

# Map back to row/column using arrayInd
top10_pos_CAR <- arrayInd(top10_idx_CAR, 
                          .dim = dim(combined_effects_CAR[["2.5e-09_2.3"]]$summed_effect_OD1))

# Combine positions and values
top10_df_CAR <- data.frame(
  row = top10_pos_CAR[, 1],
  col = top10_pos_CAR[, 2],
  value = top10_values_CAR,
  rowname = rownames(combined_effects_CAR[["2.5e-09_2.3"]]$summed_effect_OD1)[top10_pos_CAR[, 1]],
  colname = colnames(combined_effects_CAR[["2.5e-09_2.3"]]$summed_effect_OD1)[top10_pos_CAR[, 2]]
)

print(top10_df_CAR)


### GET MIN !! 
# Flatten the matrix/data frame
flat_CAR <- as.vector(combined_effects_CAR[["2.5e-09_2.3"]]$summed_effect_OD1)

# Get the indices of the 10 smallest values
bottom10_idx_CAR <- order(flat_CAR, decreasing = FALSE)[1:10]

# Get the actual values
bottom10_values_CAR <- flat_CAR[bottom10_idx_CAR]

# Map back to row/column using arrayInd
bottom10_pos_CAR <- arrayInd(bottom10_idx_CAR, 
                             .dim = dim(combined_effects_CAR[["2.5e-09_2.3"]]$summed_effect_OD1))

# Combine positions and values
bottom10_df_CAR <- data.frame(
  row = bottom10_pos_CAR[, 1],
  col = bottom10_pos_CAR[, 2],
  value = bottom10_values_CAR,
  rowname = rownames(combined_effects_CAR[["2.5e-09_2.3"]]$summed_effect_OD1)[bottom10_pos_CAR[, 1]],
  colname = colnames(combined_effects_CAR[["2.5e-09_2.3"]]$summed_effect_OD1)[bottom10_pos_CAR[, 2]]
)

print(bottom10_df_CAR)


#Column	Meaning
#row =	Index in the matrix (1-based)
#col =	Column index in the matrix
#value =	The actual value of summed_effect_OD1 at that position
#rowname =	Actual label of the row (e.g., OD1 level or time point?)
#colname =	The label of the column (likely concentration, dose, or a condition)

## Another humming bird Spp

AVer <- dir("C:/Users/au601132/OneDrive - Aarhus universitet/Skrivebord/PhD/Kolibri_heliconia/PSMC/AVer/","psmc$", full.names=T) # main PSMC results

############STEP 1
#### Run The Granger function 

results_AVer_HELCAR <- list()
results_AVer_HELBIH <- list()

for (mu in bird_mu) {
  for (g in bird_gen) {
    key <- paste0(mu, "_", g)  # Create a unique key for indexing
    
    results_AVer_HELCAR[[key]] <- NEW_Granger_psmc(AVer, HelCar, bird_mu=mu, plant_mu, plant_gen, bird_g=g)
    results_AVer_HELBIH[[key]] <- NEW_Granger_psmc(AVer, HelBih, bird_mu=mu, plant_mu, plant_gen, bird_g=g)
  }
}



#Check it out 
str(results_AVer_HELBIH)  #lookup 

List_AVer_HelBIH <- names(results_AVer_HELBIH)

str(results_AVer_HELBIH[["1.8e-08_2.3"]])


######################STEP 2
#For H. Bihai

joined_AVer_BIH <- list()  # Initialize an empty list
combined_effects_AVer <- list()
for (name in List_AVer_HelBIH)  {
  joined_AVer_BIH[[name]]$V2 <- create_matrix(results_AVer_HELBIH[[name]], "V2", "H.B on AVer, Order 1")  # Copy the original data frame
  joined_AVer_BIH[[name]]$V7 <- create_matrix(results_AVer_HELBIH[[name]], "V7", "E.jugularis on AVer, Order 1")  # Copy the original data frame
  combined_effects_AVer[[name]]$summed_effect_OD1 <- joined_AVer_BIH[[name]]$V2$log_matrix + joined_AVer_BIH[[name]]$V7$log_matrix
}

#Check it out
str(joined_AVer_BIH[["1.8e-08_2.3"]]$V2)

combined_effects_AVer[["1.8e-08_2.3"]]$summed_effect_OD1


EU_HB_PLOT1 <- grid.grabExpr(quickplot_heat(combined_effects_AVer[["1.8e-08_2"]]$summed_effect_OD1, "H.B VS. E.jugularis, 1.8e-08_2"))
EU_HB_PLOT2 <- grid.grabExpr(quickplot_heat(combined_effects_AVer[["1.8e-08_2.3"]]$summed_effect_OD1, "H.B VS. E.jugularis, 1.8e-08_2.3"))
EU_HB_PLOT3 <- grid.grabExpr(quickplot_heat(combined_effects_AVer[["1.8e-08_2.6"]]$summed_effect_OD1, "H.B VS. E.jugularis, 1.8e-08_2.6"))
EU_HB_PLOT4 <- grid.grabExpr(quickplot_heat(combined_effects_AVer[["1.8e-08_2.9"]]$summed_effect_OD1, "H.B VS. E.jugularis, 1.8e-08_2.9"))
EU_HB_PLOT5 <- grid.grabExpr(quickplot_heat(combined_effects_AVer[["2.8e-08_2"]]$summed_effect_OD1, "H.B VS. E.jugularis, 2.8e-08_2"))
EU_HB_PLOT6 <- grid.grabExpr(quickplot_heat(combined_effects_AVer[["2.8e-08_2.3"]]$summed_effect_OD1, "H.B VS. E.jugularis, 2.8e-08_2.3"))
EU_HB_PLOT7 <- grid.grabExpr(quickplot_heat(combined_effects_AVer[["2.8e-08_2.6"]]$summed_effect_OD1, "H.B VS. E.jugularis, 2.8e-08_2.6"))
EU_HB_PLOT8 <- grid.grabExpr(quickplot_heat(combined_effects_AVer[["2.8e-08_2.9"]]$summed_effect_OD1, "H.B VS. E.jugularis, 2.8e-08_2.9"))
EU_HB_PLOT9 <- grid.grabExpr(quickplot_heat(combined_effects_AVer[["3.8e-08_2"]]$summed_effect_OD1, "H.B VS. E.jugularis, 3.8e-08_2"))
EU_HB_PLOT10 <- grid.grabExpr(quickplot_heat(combined_effects_AVer[["3.8e-08_2.3"]]$summed_effect_OD1, "H.B VS. E.jugularis, 3.8e-08_2.3"))
EU_HB_PLOT11 <- grid.grabExpr(quickplot_heat(combined_effects_AVer[["3.8e-08_2.6"]]$summed_effect_OD1, "H.B VS. E.jugularis, 3.8e-08_2.6"))
EU_HB_PLOT12 <- grid.grabExpr(quickplot_heat(combined_effects_AVer[["3.8e-08_2.9"]]$summed_effect_OD1, "H.B VS. E.jugularis, 3.8e-08_2.9"))
EU_HB_PLOT13 <- grid.grabExpr(quickplot_heat(combined_effects_AVer[["4.8e-08_2"]]$summed_effect_OD1, "H.B VS. E.jugularis, 4.8e-08_2"))
EU_HB_PLOT14 <- grid.grabExpr(quickplot_heat(combined_effects_AVer[["4.8e-08_2.3"]]$summed_effect_OD1, "H.B VS. E.jugularis, 4.8e-08_2.3"))
EU_HB_PLOT15 <- grid.grabExpr(quickplot_heat(combined_effects_AVer[["4.8e-08_2.6"]]$summed_effect_OD1, "H.B VS. E.jugularis, 4.8e-08_2.6"))
EU_HB_PLOT16 <- grid.grabExpr(quickplot_heat(combined_effects_AVer[["4.8e-08_2.9"]]$summed_effect_OD1, "H.B VS. E.jugularis, 4.8e-08_2.9"))
EU_HB_PLOT17 <- grid.grabExpr(quickplot_heat(combined_effects_AVer[["5.8e-08_2"]]$summed_effect_OD1, "H.B VS. E.jugularis, 5.8e-08_2"))
EU_HB_PLOT18 <- grid.grabExpr(quickplot_heat(combined_effects_AVer[["5.8e-08_2.3"]]$summed_effect_OD1, "H.B VS. E.jugularis, order = 1"))
EU_HB_PLOT19 <- grid.grabExpr(quickplot_heat(combined_effects_AVer[["5.8e-08_2.6"]]$summed_effect_OD1, "H.B VS. E.jugularis, order = 1"))
EU_HB_PLOT20 <- grid.grabExpr(quickplot_heat(combined_effects_AVer[["5.8e-08_2.9"]]$summed_effect_OD1, "H.B VS. E.jugularis, order = 1"))
EU_HB_PLOT21 <- grid.grabExpr(quickplot_heat(combined_effects_AVer[["6.8e-08_2"]]$summed_effect_OD1, "H.B VS. E.jugularis, order = 1"))
EU_HB_PLOT22 <- grid.grabExpr(quickplot_heat(combined_effects_AVer[["6.8e-08_2.3"]]$summed_effect_OD1, "H.B VS. E.jugularis, order = 1"))
EU_HB_PLOT23 <- grid.grabExpr(quickplot_heat(combined_effects_AVer[["6.8e-08_2.6"]]$summed_effect_OD1, "H.B VS. E.jugularis, order = 1"))
EU_HB_PLOT24 <- grid.grabExpr(quickplot_heat(combined_effects_AVer[["6.8e-08_2.9"]]$summed_effect_OD1, "H.B VS. E.jugularis, order = 1"))
EU_HB_PLOT25 <- grid.grabExpr(quickplot_heat(combined_effects_AVer[["7.8e-08_2"]]$summed_effect_OD1, "H.B VS. E.jugularis, order = 1"))
EU_HB_PLOT26 <- grid.grabExpr(quickplot_heat(combined_effects_AVer[["7.8e-08_2.3"]]$summed_effect_OD1, "H.B VS. E.jugularis, order = 1"))
EU_HB_PLOT27 <- grid.grabExpr(quickplot_heat(combined_effects_AVer[["7.8e-08_2.6"]]$summed_effect_OD1, "H.B VS. E.jugularis, order = 1"))
EU_HB_PLOT28 <- grid.grabExpr(quickplot_heat(combined_effects_AVer[["7.8e-08_2.9"]]$summed_effect_OD1, "H.B VS. E.jugularis, order = 1"))
EU_HB_PLOT29 <- grid.grabExpr(quickplot_heat(combined_effects_AVer[["8.8e-08_2"]]$summed_effect_OD1, "H.B VS. E.jugularis, order = 1"))
EU_HB_PLOT30 <- grid.grabExpr(quickplot_heat(combined_effects_AVer[["8.8e-08_2.3"]]$summed_effect_OD1, "H.B VS. E.jugularis, order = 1"))
EU_HB_PLOT31 <- grid.grabExpr(quickplot_heat(combined_effects_AVer[["8.8e-08_2.6"]]$summed_effect_OD1, "H.B VS. E.jugularis, order = 1"))
EU_HB_PLOT32 <- grid.grabExpr(quickplot_heat(combined_effects_AVer[["8.8e-08_2.9"]]$summed_effect_OD1, "H.B VS. E.jugularis, order = 1"))


setwd("C:/Users/au601132/OneDrive - Aarhus universitet/Skrivebord/PhD/Kolibri_heliconia/-log10")

# Open a PDF file to save the plots
pdf("Varying_AVER_to_HB_mu_e8.pdf", width = 28, height = 14)
#png("Varying_EU_to_HB_mu_e8.png")

# Arrange the plots in a 4-row layout
grid.arrange(EU_HB_PLOT1, EU_HB_PLOT5, EU_HB_PLOT9, EU_HB_PLOT13, EU_HB_PLOT17, EU_HB_PLOT21, EU_HB_PLOT24, EU_HB_PLOT24, EU_HB_PLOT29,
             EU_HB_PLOT2, EU_HB_PLOT6, EU_HB_PLOT10, EU_HB_PLOT14, EU_HB_PLOT18, EU_HB_PLOT22, EU_HB_PLOT22, EU_HB_PLOT26, EU_HB_PLOT30,
             EU_HB_PLOT3, EU_HB_PLOT7, EU_HB_PLOT11, EU_HB_PLOT15, EU_HB_PLOT19, EU_HB_PLOT23, EU_HB_PLOT23, EU_HB_PLOT27, EU_HB_PLOT31,
             EU_HB_PLOT4, EU_HB_PLOT8, EU_HB_PLOT12, EU_HB_PLOT16, EU_HB_PLOT20, EU_HB_PLOT24, EU_HB_PLOT24, EU_HB_PLOT28, EU_HB_PLOT32,
             
             nrow = 4
)

dev.off()




######################STEP 2
#For H. caribea

str(results_AVer_HELCAR)  #lookup 

List_AVer_HelCAR <- names(results_AVer_HELCAR)

str(results_AVer_HELCAR[["1.8e-08_2.3"]])


joined_AVer_CAR <- list()  # Initialize an empty list
combined_effects_AVer_HCAR <- list()
for (name in List_AVer_HelCAR)  {
  joined_AVer_CAR[[name]]$V2 <- create_matrix(results_AVer_HELCAR[[name]], "V2", "H.B on AVer, Order 1")  # Copy the original data frame
  joined_AVer_CAR[[name]]$V7 <- create_matrix(results_AVer_HELCAR[[name]], "V7", "E.jugularis on AVer, Order 1")  # Copy the original data frame
  combined_effects_AVer_HCAR[[name]]$summed_effect_OD1 <- joined_AVer_CAR[[name]]$V2$log_matrix + joined_AVer_CAR[[name]]$V7$log_matrix
}

#Check it out
str(joined_AVer_CAR[["1.8e-08_2.3"]]$V2)
str(joined_AVer_CAR[["1.8e-08_2.3"]]$V7)


combined_effects_AVer_HCAR[["1.8e-08_2.3"]]$summed_effect_OD




EU_HC_PLOT1 <- grid.grabExpr(quickplot_heat(combined_effects_AVer_HCAR[["1.8e-08_2"]]$summed_effect_OD1, "H.B VS. E.jugularis, 1.8e-08_2"))
EU_HC_PLOT2 <- grid.grabExpr(quickplot_heat(combined_effects_AVer_HCAR[["1.8e-08_2.3"]]$summed_effect_OD1, "H.B VS. E.jugularis, 1.8e-08_2.3"))
EU_HC_PLOT3 <- grid.grabExpr(quickplot_heat(combined_effects_AVer_HCAR[["1.8e-08_2.6"]]$summed_effect_OD1, "H.B VS. E.jugularis, 1.8e-08_2.6"))
EU_HC_PLOT4 <- grid.grabExpr(quickplot_heat(combined_effects_AVer_HCAR[["1.8e-08_2.9"]]$summed_effect_OD1, "H.B VS. E.jugularis, 1.8e-08_2.9"))
EU_HC_PLOT5 <- grid.grabExpr(quickplot_heat(combined_effects_AVer_HCAR[["2.8e-08_2"]]$summed_effect_OD1, "H.B VS. E.jugularis, 2.8e-08_2"))
EU_HC_PLOT6 <- grid.grabExpr(quickplot_heat(combined_effects_AVer_HCAR[["2.8e-08_2.3"]]$summed_effect_OD1, "H.B VS. E.jugularis, 2.8e-08_2.3"))
EU_HC_PLOT7 <- grid.grabExpr(quickplot_heat(combined_effects_AVer_HCAR[["2.8e-08_2.6"]]$summed_effect_OD1, "H.B VS. E.jugularis, 2.8e-08_2.6"))
EU_HC_PLOT8 <- grid.grabExpr(quickplot_heat(combined_effects_AVer_HCAR[["2.8e-08_2.9"]]$summed_effect_OD1, "H.B VS. E.jugularis, 2.8e-08_2.9"))
EU_HC_PLOT9 <- grid.grabExpr(quickplot_heat(combined_effects_AVer_HCAR[["3.8e-08_2"]]$summed_effect_OD1, "H.B VS. E.jugularis, 3.8e-08_2"))
EU_HC_PLOT10 <- grid.grabExpr(quickplot_heat(combined_effects_AVer_HCAR[["3.8e-08_2.3"]]$summed_effect_OD1, "H.B VS. E.jugularis, 3.8e-08_2.3"))
EU_HC_PLOT11 <- grid.grabExpr(quickplot_heat(combined_effects_AVer_HCAR[["3.8e-08_2.6"]]$summed_effect_OD1, "H.B VS. E.jugularis, 3.8e-08_2.6"))
EU_HC_PLOT12 <- grid.grabExpr(quickplot_heat(combined_effects_AVer_HCAR[["3.8e-08_2.9"]]$summed_effect_OD1, "H.B VS. E.jugularis, 3.8e-08_2.9"))
EU_HC_PLOT13 <- grid.grabExpr(quickplot_heat(combined_effects_AVer_HCAR[["4.8e-08_2"]]$summed_effect_OD1, "H.B VS. E.jugularis, 4.8e-08_2"))
EU_HC_PLOT14 <- grid.grabExpr(quickplot_heat(combined_effects_AVer_HCAR[["4.8e-08_2.3"]]$summed_effect_OD1, "H.B VS. E.jugularis, 4.8e-08_2.3"))
EU_HC_PLOT15 <- grid.grabExpr(quickplot_heat(combined_effects_AVer_HCAR[["4.8e-08_2.6"]]$summed_effect_OD1, "H.B VS. E.jugularis, 4.8e-08_2.6"))
EU_HC_PLOT16 <- grid.grabExpr(quickplot_heat(combined_effects_AVer_HCAR[["4.8e-08_2.9"]]$summed_effect_OD1, "H.B VS. E.jugularis, 4.8e-08_2.9"))
EU_HC_PLOT17 <- grid.grabExpr(quickplot_heat(combined_effects_AVer_HCAR[["5.8e-08_2"]]$summed_effect_OD1, "H.B VS. E.jugularis, 5.8e-08_2"))
EU_HC_PLOT18 <- grid.grabExpr(quickplot_heat(combined_effects_AVer_HCAR[["5.8e-08_2.3"]]$summed_effect_OD1, "H.B VS. E.jugularis, order = 1"))
EU_HC_PLOT19 <- grid.grabExpr(quickplot_heat(combined_effects_AVer_HCAR[["5.8e-08_2.6"]]$summed_effect_OD1, "H.B VS. E.jugularis, order = 1"))
EU_HC_PLOT20 <- grid.grabExpr(quickplot_heat(combined_effects_AVer_HCAR[["5.8e-08_2.9"]]$summed_effect_OD1, "H.B VS. E.jugularis, order = 1"))
EU_HC_PLOT21 <- grid.grabExpr(quickplot_heat(combined_effects_AVer_HCAR[["6.8e-08_2"]]$summed_effect_OD1, "H.B VS. E.jugularis, order = 1"))
EU_HC_PLOT22 <- grid.grabExpr(quickplot_heat(combined_effects_AVer_HCAR[["6.8e-08_2.3"]]$summed_effect_OD1, "H.B VS. E.jugularis, order = 1"))
EU_HC_PLOT23 <- grid.grabExpr(quickplot_heat(combined_effects_AVer_HCAR[["6.8e-08_2.6"]]$summed_effect_OD1, "H.B VS. E.jugularis, order = 1"))
EU_HC_PLOT24 <- grid.grabExpr(quickplot_heat(combined_effects_AVer_HCAR[["6.8e-08_2.9"]]$summed_effect_OD1, "H.B VS. E.jugularis, order = 1"))
EU_HC_PLOT25 <- grid.grabExpr(quickplot_heat(combined_effects_AVer_HCAR[["7.8e-08_2"]]$summed_effect_OD1, "H.B VS. E.jugularis, order = 1"))
EU_HC_PLOT26 <- grid.grabExpr(quickplot_heat(combined_effects_AVer_HCAR[["7.8e-08_2.3"]]$summed_effect_OD1, "H.B VS. E.jugularis, order = 1"))
EU_HC_PLOT27 <- grid.grabExpr(quickplot_heat(combined_effects_AVer_HCAR[["7.8e-08_2.6"]]$summed_effect_OD1, "H.B VS. E.jugularis, order = 1"))
EU_HC_PLOT28 <- grid.grabExpr(quickplot_heat(combined_effects_AVer_HCAR[["7.8e-08_2.9"]]$summed_effect_OD1, "H.B VS. E.jugularis, order = 1"))
EU_HC_PLOT29 <- grid.grabExpr(quickplot_heat(combined_effects_AVer_HCAR[["8.8e-08_2"]]$summed_effect_OD1, "H.B VS. E.jugularis, order = 1"))
EU_HC_PLOT30 <- grid.grabExpr(quickplot_heat(combined_effects_AVer_HCAR[["8.8e-08_2.3"]]$summed_effect_OD1, "H.B VS. E.jugularis, order = 1"))
EU_HC_PLOT31 <- grid.grabExpr(quickplot_heat(combined_effects_AVer_HCAR[["8.8e-08_2.6"]]$summed_effect_OD1, "H.B VS. E.jugularis, order = 1"))
EU_HC_PLOT32 <- grid.grabExpr(quickplot_heat(combined_effects_AVer_HCAR[["8.8e-08_2.9"]]$summed_effect_OD1, "H.B VS. E.jugularis, order = 1"))


setwd("C:/Users/au601132/OneDrive - Aarhus universitet/Skrivebord/PhD/Kolibri_heliconia/-log10")

# Open a PDF file to save the plots
pdf("Varying_AVER_to_HCC_mu_e8.pdf", width = 28, height = 14)


# Arrange the plots in a 4-row layout
grid.arrange(EU_HC_PLOT1, EU_HC_PLOT5, EU_HC_PLOT9, EU_HC_PLOT13, EU_HC_PLOT17, EU_HC_PLOT21, EU_HC_PLOT24, EU_HC_PLOT24, EU_HC_PLOT29,
             EU_HC_PLOT2, EU_HC_PLOT6, EU_HC_PLOT10, EU_HC_PLOT14, EU_HC_PLOT18, EU_HC_PLOT22, EU_HC_PLOT22, EU_HC_PLOT26, EU_HC_PLOT30,
             EU_HC_PLOT3, EU_HC_PLOT7, EU_HC_PLOT11, EU_HC_PLOT15, EU_HC_PLOT19, EU_HC_PLOT23, EU_HC_PLOT23, EU_HC_PLOT27, EU_HC_PLOT31,
             EU_HC_PLOT4, EU_HC_PLOT8, EU_HC_PLOT12, EU_HC_PLOT16, EU_HC_PLOT20, EU_HC_PLOT24, EU_HC_PLOT24, EU_HC_PLOT28, EU_HC_PLOT32,
             
             nrow = 4
)

dev.off()






































