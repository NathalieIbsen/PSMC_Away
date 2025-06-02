##BEST_granger_functions

#Collection of the optimized and currently used functions. 
#
#  + the function by ""Modified from old script by Shenglin Liu, Mar 25, 2019.""
#modified by who ?? (I dont know)

###
## ALL functions
#


#####
#Writing Jesper B' script to a function at that takes a set of parameters  
# And returns a vector for that parameter space. The naming based on the speices and the parameters 

####FIRST 
#Load the function by  Modified from old script by Shenglin Liu, Mar 25, 2019.
#modified by who ?? (I dont know)


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



create_matrix <- function(data, col, title) {
  library(dplyr)
  library(stringr)
  
  data <- data %>%
    mutate(mu = str_extract(V1, "(?<=mu)[0-9eE.-]+") %>% as.numeric(),
           g = str_extract(V1, "(?<=g)[0-9]+") %>% as.numeric())
  
  # Use [[ to extract the column by name
  matrix_data <- cbind(data$mu, data$g, as.numeric(ifelse(data[[col]] == 0, 1e-20, data[[col]])))
  
  
  # Convert the data frame into a matrix
  matrix_resized <- matrix(matrix_data[,3], nrow = length(unique(data$g)), byrow = TRUE)
  
  # Set the row names to 'g' and column names to 'mu'
  rownames(matrix_resized) <- unique(data$g)
  colnames(matrix_resized) <- unique(data$mu)
  
  # View the matrix
  print(matrix_resized)
  
  # Log transformation
  matrix_log_transformed <- -log10(matrix_resized)
  
  library(circlize)
  
  # Heatmap visualization
  library(RColorBrewer)
  library(pheatmap)
  # 
  # # Define color palettes
  # red_palette <- colorRampPalette(c("red3", "orange3"))(30)     # Red to Yellow Gradient
  # orange_palette <- colorRampPalette(c("yellow", "orange"))(20) # Yellow to Orange Gradient
  # blue_palette <- colorRampPalette(c("lightgreen", "blue"))(50)  # Light Blue to Blue Gradient
  # 
  # # Combine the palettes
  # my_palette <- c(red_palette, orange_palette, blue_palette)
  # 
  # # Define breaks: finer granularity for values < 0.05 and gradient for 0.05 - 1
  # breaks <- c(seq(0, 0.0049999, length.out = 31), 
  #             seq(0.005, 0.0499999, length.out = 21), 
  #             seq(0.05, 1, length.out = 50))
  # # Create the heatmap
  # p <- pheatmap(matrix_resized,
  #          scale = "none",    # You can change to "row" or "column" for normalization
  #          color = my_palette,  # Custom colors for heatmap
  #          breaks = breaks,  # Custom breaks for color mapping
  #          main = title,  # Title of heatmap
  #          cluster_rows = FALSE,   # Whether to cluster rows
  #          cluster_cols = FALSE,   # Whether to cluster columns
  #          show_rownames = TRUE,  # Display row names
  #          show_colnames = TRUE   # Display column names
  # )
  # 
  
  # Load required libraries
  # Load required libraries
  library(pheatmap)
  library(viridis)
  # Create a viridis palette with 50 colors
  new_palette <- viridis(50, option = "C")
  
  # Define the color for 'NA' (gray) and 'NaN' (dark gray)
  color_na <- "lightgray"  # gray for NA
  color_nan <- "#4d4d4d"  # dark gray for NaN
  
  # Define breaks: We will use the placeholder values for NA and NaN
  breaks <- c(-1, -0.5, seq(0, 10, length.out = 49))
  
  # Modify the matrix to handle NaN and NA
  matrix_log_transformed[is.nan(matrix_log_transformed)] <- -1  # Replace NaN with -1
  matrix_log_transformed[is.na(matrix_log_transformed)] <- -0.5  # Replace NA with -0.5
  colnames(matrix_log_transformed) <- formatC(as.numeric(colnames(matrix_log_transformed)), format = "e", digits = 2)
  
  # Create the heatmap with the updated breaks and color palette
  p <- pheatmap(matrix_log_transformed,
                scale = "none",
                color = c(color_na, color_nan, new_palette),  # Custom color for NaN and NA
                breaks = breaks,  # Adjust breaks to include NaN and NA
                main = title,
                cluster_rows = FALSE,
                cluster_cols = FALSE,
                show_rownames = TRUE,
                show_colnames = TRUE,
                legend_breaks = seq(0, 10, 1),  
                legend_labels = seq(0, 10, 1),  
                fontsize_row = 6,  
                fontsize_col = 6,  
                fontsize = 7)
  
  # Print the heatmap
  print(p)
  
  
  return(list(matrix = matrix_resized, log_matrix=matrix_log_transformed, plot = p))
}


quickplot_heat <- function(matrix_log_transformed, title) {
  # Load required libraries
  # Load required libraries
  library(pheatmap)
  library(viridis)
  
  # Create a viridis palette with 50 colors
  new_palette <- viridis(50, option = "C")
  
  # Define the color for 'NA' (gray) and 'NaN' (dark gray)
  color_na <- "lightgray"  # gray for NA
  color_nan <- "#4d4d4d"  # dark gray for NaN
  
  # Define breaks: We will use the placeholder values for NA and NaN
  breaks <- c(-1, -0.5, seq(0, 28, length.out = 49))
  
  # Modify the matrix to handle NaN and NA
  matrix_log_transformed[is.nan(matrix_log_transformed)] <- -1  # Replace NaN with -1
  matrix_log_transformed[is.na(matrix_log_transformed)] <- -0.5  # Replace NA with -0.5
  colnames(matrix_log_transformed) <- formatC(as.numeric(colnames(matrix_log_transformed)), format = "e", digits = 2)
  
  # Create the heatmap with the updated breaks and color palette
  p <- pheatmap(matrix_log_transformed,
                scale = "none",
                color = c(color_na, color_nan, new_palette),  # Custom color for NaN and NA
                breaks = breaks,  # Adjust breaks to include NaN and NA
                main = title,
                cluster_rows = FALSE,
                cluster_cols = FALSE,
                show_rownames = TRUE,
                show_colnames = TRUE,
                legend_breaks = seq(0, 28, 1),  
                legend_labels = seq(0, 28, 1),  
                fontsize_row = 2,  
                fontsize_col = 2,  
                fontsize = 5)
  # Print the heatmap
  print(p)
  
  return(p)
}






quickplot_heat <- function(matrix_log_transformed, title) {
  # Load required libraries
  library(pheatmap)
  library(viridis)
  
  # Create a viridis palette with 50 colors
  new_palette <- viridis(50, option = "C")
  
  # Define the color for 'NA' (gray) and 'NaN' (dark gray)
  color_na <- "lightgray"  # gray for NA
  color_nan <- "#4d4d4d"   # dark gray for NaN
  
  # Define breaks: We will use the placeholder values for NA and NaN
  breaks <- c(-1, -0.5, seq(0, 28, length.out = 49))
  
  # Modify the matrix to handle NaN and NA
  matrix_log_transformed[is.nan(matrix_log_transformed)] <- -1  # Replace NaN with -1
  matrix_log_transformed[is.na(matrix_log_transformed)] <- -0.5  # Replace NA with -0.5
  colnames(matrix_log_transformed) <- formatC(as.numeric(colnames(matrix_log_transformed)), format = "e", digits = 2)
  
  # Create the heatmap without title, legend, and row names
  p <- pheatmap(matrix_log_transformed,
                scale = "none",
                color = c(color_na, color_nan, new_palette),  # Custom color for NaN and NA
                breaks = breaks,  # Adjust breaks to include NaN and NA
                main = NA,  # Use NA instead of NULL to avoid the error
                cluster_rows = FALSE,
                cluster_cols = FALSE,
                show_rownames = FALSE,  # Remove row names
                show_colnames = FALSE,   # Keep column names
                legend = FALSE,         # Remove legend
                fontsize_row = 3,       # Adjust font size for rows (though not shown)
                fontsize_col = 3,       # Adjust font size for columns
                fontsize = 3,# Adjust general font size
                border_color = "gray91",  # Set grid line color to light gray
                border = TRUE,            # Turn on borders
                border_gp = gpar(lwd = 0.1))
  
  # Print the heatmap
  print(p)
  
  return(p)
}





#AAAAlternative granger 
#AAAAlternative granger 

NEW_Granger_psmc <- function(bird, plant, plant_mu, bird_mu, bird_g, plant_gen) {
  library(lmtest)
  library(imputeTS)
  library(dplyr)
  library(tseries)
  
  #to accumulate results as they are processed
  # Initialize an empty data frame with character storage
  All_Results <- data.frame(stringsAsFactors = FALSE)
  
  # Run PSMC for bird
  psmcbird <- psmc.result(bird, i.iteration=25, mu=bird_mu, s=100, g=bird_g)
  subset_psmcbird <- psmcbird[-(1:12), ]
  subset_psmcbird <- subset_psmcbird[seq(2, nrow(subset_psmcbird), by = 2), ]
  
  for (mu in plant_mu) {
    for (g in plant_gen) {
      #print(g)
      #print(mu)
      
      # Run PSMC for plant with dynamic mu and g
      psmcplant <- psmc.result(plant, i.iteration=25, mu=mu, s=100, g=g)
      #cut the turtle neck and remove the step!
      subset_psmcplant <- psmcplant[-(1:12), ]
      subset_psmcplant <- subset_psmcplant[seq(2, nrow(subset_psmcplant), by = 2), ]
      
      #fit a line to the estimated steps (down-stream use)
      smooth_plant <- smooth.spline(subset_psmcplant)
      smooth_bird <- smooth.spline(subset_psmcbird)
      
      # Get filtering stats
      min_plant <- min(subset_psmcplant$Time)
      max_bird <- max(subset_psmcbird$Time)
      
      # Merge the two datasets
      merged_df <- merge(subset_psmcbird, subset_psmcplant, by = "Time",
                         suffixes = c("_bird", "_plant"), all = TRUE)
      
      cut_merged_df <- merged_df %>%
        filter(Time > (min_plant-1) & Time < max_bird)
      
      NA_bird <- sum(is.na(cut_merged_df$Ne_bird))  # Count NA values in Ne_bird
      NA_plant <- sum(is.na(cut_merged_df$Ne_plant)) # Count NA values in Ne_plant
      
      # print("missing data in 1) plant & 2) bird:")
      # print(NA_plant)
      # print(NA_bird)
      
      # Skip processing if there's no sufficient overlap
      if (nrow(cut_merged_df) < 25) {
        warning("Insufficient overlap (n < 25), skipping comparison for mu=", mu, " g=", g)
        
        # Assign NaN values to the p-value vectors
        p_values_plant <- rep(NaN, 5)
        p_values_bird <- rep(NaN, 5)
        
      } else {
        #### Predicting
        if (NA_plant > NA_bird) {
          time <- subset_psmcplant %>%
            filter(Time > (min_plant - 1) & Time < max_bird) %>%
            select(Time)
          # Predict bird
          Newvals <- data.frame(Time = time)
          Newvals$pred_bird <- predict(smooth_bird, Newvals$Time)$y
          # Predict plant
          Newvals$pred_plant <- predict(smooth_plant, Newvals$Time)$y
        } else {
          time <- subset_psmcbird %>%
            filter(Time > (min_plant - 1) & Time < max_bird) %>%
            select(Time)
          # Predict bird
          Newvals <- data.frame(Time = time)
          Newvals$pred_bird <- predict(smooth_bird, Newvals$Time)$y
          # Predict plant
          Newvals$pred_plant <- predict(smooth_plant, Newvals$Time)$y
        }
        
        # Convert to time series object
        bird_ts <- ts(Newvals$pred_bird, frequency = 1)
        bird_ts <- ts(rev(bird_ts), start = start(bird_ts), frequency = frequency(bird_ts))
        
        plant_ts <- ts(Newvals$pred_plant, frequency = 1)
        plant_ts <- ts(rev(plant_ts), start = start(plant_ts), frequency = frequency(plant_ts))
        
        # Vectors to store p-values
        p_values_plant <- rep(NA,5)  
        p_values_bird <- rep(NA,5) # pad the vectors
        
        for (i in 1:5) {
          tryCatch({ 
            BIRD_Test_result <- lmtest::grangertest(plant_ts, bird_ts, order = i, test = "F")
            PLANT_Test_result <- lmtest::grangertest(bird_ts, plant_ts, order = i, test = "F")
            
            p_values_plant[i] <- PLANT_Test_result$`Pr(>F)`[2]
            p_values_bird[i] <- BIRD_Test_result$`Pr(>F)`[2]
            
          }, error = function(e) {
            message("Error in Granger test for order ", i, ": ", e$message)
            
            if (grepl("aliased coefficients", e$message, ignore.case = TRUE)) {
              p_values_plant[i] <- 99
              p_values_bird[i] <- 99
              #print(paste("Assigned '404' at lag:", i))
            } else if (grepl("subscript out of bounds", e$message, ignore.case = TRUE)) {
              p_values_plant[i] <- 404 
              p_values_bird[i] <- 404
              #print(paste("Assigned NA for 'subscript out of bounds' at lag:", i))
            } else {
              p_values_plant[i] <- 99
              p_values_bird[i] <- 99
              #print(paste("Assigned '99' for an unexpected error at lag:", i))
            }
          })
        }
      }
      # Dynamic name for the column
      plant_name <- sub(".*/PSMC/([^/]+)/.*", "\\1", plant)
      param_name <- paste0(plant_name, "_mu", mu, "_g", g)
      
      # Combine all results into one row
      Result <- as.data.frame(t(c(param_name, as.character(p_values_bird), as.character(p_values_plant))),
                              stringsAsFactors = FALSE)
      
      # Accumulate results
      All_Results <- rbind(All_Results, Result)
    }
  }
  return(All_Results)
}



Ratio_Granger_psmc <- function(bird, CHRZ, plant1, plant2, plant_mu, bird_mu, bird_g, plant_gen) {
  library(lmtest)
  library(imputeTS)
  library(dplyr)
  library(tseries)
  
  #to accumulate results as they are processed
  # Initialize an empty data frame with character storage
  All_Results <- data.frame(stringsAsFactors = FALSE)
  
  # Run PSMC for bird
  psmcbird <- psmc.result(bird, i.iteration=25, mu=bird_mu, s=100, g=bird_g)
  subset_psmcbird <- psmcbird[-(1:12), ]
  subset_psmcbird <- subset_psmcbird[seq(2, nrow(subset_psmcbird), by = 2), ]
  psmcCHRZ <- psmc.result(psmcCHRZ, i.iteration=25, mu=bird_mu, s=100, g=bird_g)
  subset_psmcCHRZ <- psmcCHRZ [-(1:12), ]
  subset_psmcCHRZ <- subset_psmcCHRZ[seq(2, nrow(subset_psmcCHRZ), by = 2), ]
  
  for (mu in plant_mu) {
    for (g in plant_gen) {
      #print(g)
      #print(mu)
      
      # Run PSMC for plant with dynamic mu and g
      psmcplant1 <- psmc.result(plant1, i.iteration=25, mu=mu, s=100, g=g)
      #cut the turtle neck and remove the step!
      subset_psmcplant1 <- psmcplant1[-(1:12), ]
      subset_psmcplant1 <- subset_psmcplant1[seq(2, nrow(subset_psmcplant), by = 2), ]
      
      # Run PSMC for plant with dynamic mu and g
      psmcplant2 <- psmc.result(plant2, i.iteration=25, mu=mu, s=100, g=g)
      #cut the turtle neck and remove the step!
      subset_psmcplant2 <- psmcplant2[-(1:12), ]
      subset_psmcplant2 <- subset_psmcplant2[seq(2, nrow(subset_psmcplant), by = 2), ]
      
      #fit a line to the estimated steps (down-stream use)
      smooth_plant1 <- smooth.spline(subset_psmcplant1)
      smooth_plant2 <- smooth.spline(subset_psmcplant2)
      
      smooth_bird <- smooth.spline(subset_psmcbird)
      smooth_CHRZ <- smooth.spline(subset_psmcCHRZ)
      
      
      # Get filtering stats
      min_plant <- min(subset_psmcplant$Time)
      max_bird <- max(subset_psmcbird$Time)
      
      # Merge the two datasets
      merged_df <- merge(subset_psmcbird, subset_psmcplant, by = "Time",
                         suffixes = c("_bird", "_plant"), all = TRUE)
      
      cut_merged_df <- merged_df %>%
        filter(Time > (min_plant-1) & Time < max_bird)
      
      NA_bird <- sum(is.na(cut_merged_df$Ne_bird))  # Count NA values in Ne_bird
      NA_plant <- sum(is.na(cut_merged_df$Ne_plant)) # Count NA values in Ne_plant
      
      # print("missing data in 1) plant & 2) bird:")
      # print(NA_plant)
      # print(NA_bird)
      
      # Skip processing if there's no sufficient overlap
      if (nrow(cut_merged_df) < 25) {
        warning("Insufficient overlap (n < 25), skipping comparison for mu=", mu, " g=", g)
        
        # Assign NaN values to the p-value vectors
        p_values_plant <- rep(NaN, 5)
        p_values_bird <- rep(NaN, 5)
        
      } else {
        #### Predicting
        if (NA_plant > NA_bird) {
          time <- subset_psmcplant %>%
            filter(Time > (min_plant - 1) & Time < max_bird) %>%
            select(Time)
          # Predict bird
          Newvals <- data.frame(Time = time)
          Newvals$pred_bird <- predict(smooth_bird, Newvals$Time)$y
          # Predict plant
          Newvals$pred_plant <- predict(smooth_plant, Newvals$Time)$y
        } else {
          time <- subset_psmcbird %>%
            filter(Time > (min_plant - 1) & Time < max_bird) %>%
            select(Time)
          # Predict bird
          Newvals <- data.frame(Time = time)
          Newvals$pred_bird <- predict(smooth_bird, Newvals$Time)$y
          # Predict plant
          Newvals$pred_plant <- predict(smooth_plant, Newvals$Time)$y
        }
        
        # Convert to time series object
        bird_ts <- ts(Newvals$pred_bird, frequency = 1)
        bird_ts <- ts(rev(bird_ts), start = start(bird_ts), frequency = frequency(bird_ts))
        
        plant_ts <- ts(Newvals$pred_plant, frequency = 1)
        plant_ts <- ts(rev(plant_ts), start = start(plant_ts), frequency = frequency(plant_ts))
        
        # Vectors to store p-values
        p_values_plant <- rep(NA,5)  
        p_values_bird <- rep(NA,5) # pad the vectors
        
        for (i in 1:5) {
          tryCatch({ 
            BIRD_Test_result <- lmtest::grangertest(plant_ts, bird_ts, order = i, test = "F")
            PLANT_Test_result <- lmtest::grangertest(bird_ts, plant_ts, order = i, test = "F")
            
            p_values_plant[i] <- PLANT_Test_result$`Pr(>F)`[2]
            p_values_bird[i] <- BIRD_Test_result$`Pr(>F)`[2]
            
          }, error = function(e) {
            message("Error in Granger test for order ", i, ": ", e$message)
            
            if (grepl("aliased coefficients", e$message, ignore.case = TRUE)) {
              p_values_plant[i] <- 99
              p_values_bird[i] <- 99
              #print(paste("Assigned '404' at lag:", i))
            } else if (grepl("subscript out of bounds", e$message, ignore.case = TRUE)) {
              p_values_plant[i] <- 404 
              p_values_bird[i] <- 404
              #print(paste("Assigned NA for 'subscript out of bounds' at lag:", i))
            } else {
              p_values_plant[i] <- 99
              p_values_bird[i] <- 99
              #print(paste("Assigned '99' for an unexpected error at lag:", i))
            }
          })
        }
      }
      # Dynamic name for the column
      plant_name <- sub(".*/PSMC/([^/]+)/.*", "\\1", plant)
      param_name <- paste0(plant_name, "_mu", mu, "_g", g)
      
      # Combine all results into one row
      Result <- as.data.frame(t(c(param_name, as.character(p_values_bird), as.character(p_values_plant))),
                              stringsAsFactors = FALSE)
      
      # Accumulate results
      All_Results <- rbind(All_Results, Result)
    }
  }
  return(All_Results)
}


