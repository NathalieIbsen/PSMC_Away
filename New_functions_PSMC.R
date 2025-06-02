#####
#Writing Jesper B' script to a function at that takes a set of parameters  
# And returns a vector for that parameter space. The naming based on the speices and the parameters 


# Secondly 
# Write a function to compare the trend between time intervals of one species to another. 


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

library(tsibble)
library(imputeTS)



##A function that takes the variables, file (path) mu(int) and g(int) and return the vector comparision between temporal points 
## out put a vector (1,0,-1)
##(increasing(1), stable(0), decreasing(-1)) named by the used variables

# temporal_change_psmc_OLD <- function(file, mu, g, newvals) {
#   # Run psmc.result and smooth the spline
#   x <- psmc.result(file, i.iteration=25, mu=mu, s=100, g=g)
#   subset_x <- x[-(1:12), ]
#   newvals <- data.frame(Time=seq(5000, max(subset_x[,1], na.rm=T), 3000))
#   a1 <- smooth.spline(subset_x)
#   
#   # Predict values
#   prediction <- cbind(newvals, value=predict(a1, newvals)$y)
#   
#   # Compute temporal differences
#   temporal_diff <- diff(prediction[,2])
#   
#   # Compare differences to 100 and return vector
#   comparison <- ifelse(temporal_diff > 100, 1, ifelse(temporal_diff < -100, -1, 0))
#   
# 
#   return(comparison)
# }



##A function that takes the variables, file (path) mu(int) and g(int) and return the vector comparision between temporal points 
## out put a vector (1,0,-1)
##(increasing(1), stable(0), decreasing(-1)) named by the used variables

temporal_change_psmc <- function(file, mu, g, newvals) {

  ## 1 Run psmc.result # function(x,y,z) {} #
  x <- psmc.result(file, i.iteration=25, mu=mu, s=100, g=g)
  subset_x <- x[-(1:12), ]
  
  # Define time breakpoints
  breakpoints <- seq(5000, max(subset_x$Time), 3000)
  min_time <- min(subset_x$Time)
  
  if (min_time < 5000) {
    newvals <- data.frame(Time=seq(5000, max(subset_x[,1], na.rm=T), 3000))
  } else if (min_time > 5000) {
    # Find the correct starting time based on min_time
    start_time <- breakpoints[findInterval(min_time, breakpoints) + 1]
    # Create newvals dataframe
    newvals <- data.frame(Time = seq(start_time, max(subset_x[,1], na.rm=T), 3000))
  }
  

  # Run smooth the spline
  a1 <- smooth.spline(subset_x)
  
  predicted_values <- predict(a1, newvals$Time)$y
  
  # Predict values
  prediction <- cbind(newvals, predicted_values)
  
  if (min_time > 5000) {
    NA_vals <- data.frame(Time=seq(5000, max(min_time, na.rm=T), 3000),predicted_values=NA)
    prediction <- rbind(NA_vals, prediction)
    prediction[,2] <- as.numeric(as.character(prediction[,2]))
  } 
  
  
  temporal_diff <- diff(prediction[,2])
  
  # Compare differences to 100 and return vector
  comparison <- ifelse(temporal_diff > 100, 1, ifelse(temporal_diff < -100, -1, 0))
  

  return(list(comparison=comparison,prediction=prediction))
}






#extract max man min Time from psmc.result
#Input in a data.frame. 

temporal_points_psmc <- function(file, mu, gen) { 
  
  # make an empty data frame to "return" later :D
  new_df <- data.frame(mu = numeric(), g = numeric(), max = numeric(), min = numeric(), stringsAsFactors = FALSE)
  
  for (m in mu) {
    for (g in gen) {
      x <- psmc.result(file, i.iteration=25, mu=m, s=100, g=g)
      
      max_val <- max(x$Time)
      min_val <- min(x$Time)
      
      # Append new row to the data frame (for a nicly joined output)
      new_df <- rbind(new_df, data.frame(mu = m, g = g, max = max_val, min = min_val))
    }  
  }
  
  return(new_df)
}




 

comparative_psmc <- function(bird, plant, bird_mu, plant_mu, bird_g, plant_gen)  {
  ## 1 Run psmc.result on the bird 
  psmcbird <- psmc.result(bird, i.iteration=25, mu=bird_mu, s=100, g=bird_g)
  subset_psmcbird <- psmcbird[-(1:12), ]
  
  # extract time points. Define time breakpoints
  min_time <- min(subset_psmcbird$Time)
  max_time <- max(subset_psmcbird$Time)
  step_size <- max_time*0.01
  breakpoints <- seq(5000, max_time, step_size)
  
  if (min_time < 5000) {
    newvals <- seq(5000, max_time, step_size)
  } else if (min_time > 5000) {
    # Find the correct starting time based on min_time
    start_time <- breakpoints[findInterval(min_time, breakpoints) + 1]
    # Create newvals dataframe
    newvals <-seq(start_time, max_time, step_size)
  }

  
  # Run smooth the spline
  a1 <- smooth.spline(subset_psmcbird)
  
  predicted_values <- predict(a1, newvals)$y
  
  # Predict values
  prediction <- cbind(newvals, predicted_values)
  
  if (min_time > 5000) {
    NA_vals <- data.frame(Time=seq(5000, max(min_time, na.rm=T), step_size),predicted_values=NA)
    prediction <- rbind(NA_vals, prediction)
    prediction[,2] <- as.numeric(as.character(prediction[,2]))
  } 
  
  
  temporal_diff <- diff(prediction[,2])
  
  # Compare differences to 100 and return vector
  Bird_trend_comparison <- ifelse(temporal_diff > 100, 1, ifelse(temporal_diff < -100, -1, 0))
  
  Result <- cbind(Bird_trend_comparison)
  
  for (mu in plant_mu) {
    for (g in plant_gen) {
      
      psmc <- psmc.result(plant, i.iteration=25, mu=mu, s=100, g=g)
      subset_psmc <- psmc[-(1:12), ]
      # extract time points. but keep breakpoints
      min_plant <- min(subset_psmc$Time)
      
      if (min_plant < 5000) {
        newvals <- seq(5000, max_time, step_size)
      } else if (min_time > 5000) {
        # Find the correct starting time based on min_time
        start_time <- breakpoints[findInterval(min_time, breakpoints) + 1]
        # Create newvals dataframe
        newvals <-seq(start_time, max_time, step_size)
      }
      
      # Run smooth the spline
      a1 <- smooth.spline(subset_psmc)
      
      predicted_values <- predict(a1, newvals)$y
      
      # Predict values
      prediction <- cbind(newvals, predicted_values)
      
      if (min_time > 5000) {
        NA_vals <- data.frame(Time=seq(5000, max(min_time, na.rm=T), step_size),predicted_values=NA)
        prediction <- rbind(NA_vals, prediction)
        prediction[,2] <- as.numeric(as.character(prediction[,2]))
      } 
      
      temporal_diff <- diff(prediction[,2])
      
      plant_name <- sub(".*/PSMC/([^/]+)/.*", "\\1", plant)
      
      # Create the dynamic name for the column
      col_name <- paste0(plant_name, "_mu", mu, "_g", g)
      
      # Calculate the vector for the column
      trend_vector <- ifelse(temporal_diff > 100, 1, ifelse(temporal_diff < -100, -1, 0))
      
      # Combine using cbind and set the name dynamically
      Result <- cbind(Result, setNames(data.frame(trend_vector), col_name))
    }
  }
  
  return(Result)
}





comparative_psmc <- function(bird, plant, bird_mu, plant_mu, bird_g, plant_gen)  {
  ## 1 Run psmc.result on the bird 
  psmcbird <- psmc.result(bird, i.iteration=25, mu=bird_mu, s=100, g=bird_g)
  subset_psmcbird <- psmcbird[-(1:12), ]
  
  for (mu in plant_mu) {
    for (g in plant_gen) {
      
      psmc <- psmc.result(plant, i.iteration=25, mu=mu, s=100, g=g)
      subset_psmc <- psmc[-(1:12), ]

      
      
      plant_name <- sub(".*/PSMC/([^/]+)/.*", "\\1", plant)
      
      # Create the dynamic name for the column
      col_name <- paste0(plant_name, "_mu", mu, "_g", g)
      
      # Calculate the vector for the column
      trend_vector <- ifelse(temporal_diff > 100, 1, ifelse(temporal_diff < -100, -1, 0))
      
      # Combine using cbind and set the name dynamically
      
      Result <- cbind(Result, setNames(data.frame(trend_vector), col_name))
    }
  }
  
  return(Result)
}





### A function to to estimate the predictive power of one trend with the other, 
#Over the parameter-space. 





#Grenger test. 

#   
#   #RUn psmc 
# Granger_psmc <- function(bird, plant, bird_mu, plant_mu, bird_g, plant_gen)  
#   psmcbird <- psmc.result(bird, i.iteration=25, mu=6.8e-8, s=100, g=2.3)
#   subset_psmcbird <- psmcbird[-(1:12), ]
#   for (mu in plant_mu) {
#     for (g in plant_gen) {
#       
#     psmcplant <- psmc.result(plant, i.iteration=25, mu=8e-8, s=100, g=15)
#     subset_psmcplant <- psmcplant[-(1:12), ]
#     
#     #Get filtering stats.
#     min_plant <- min(subset_psmcplant$Time)
#     
#     merged_df <- merge(subset_psmcbird, subset_psmcplant, by = "Time", 
#                        suffixes = c("_bird", "_plant"), all = TRUE)
#     #Filter 
#     cut_merged_df <- merged_df %>%
#       filter(Time > (min_plant-1) & Time < 259000)
#     
#     ##interpolate missing Data. 
#     
#     cut_merged_df$Bird_Ne <- na_interpolation(cut_merged_df$Ne_bird)
#     cut_merged_df$Plant_Ne <- na_interpolation(cut_merged_df$Ne_plant)
#     
#     
#     # Convert to time series object
#     bird_ts <- ts(cut_merged_df$Bird_Ne, frequency = 1)  # Adjust frequency as needed
#     plant_ts <- ts(cut_merged_df$Plant_Ne, frequency = 1)
#     
#     
#     # Augmented Dickey-Fuller Test  ---> testing weather 
#     adf.test(bird_ts)
#     adf.test(plant_ts)
#     
#     
#     ##OBS issue !!! the data is not stadinary?? 
#     #The mean, variance, or autocorrelation of the series are not constant over time. !!! 
#     
#     ###This should be a handled !! one way !
#     
#     # # If non-stationary, apply first differencing
#     bird_ts_diff <- diff(bird_ts, differences = 1)
#     plant_ts_diff <- diff(plant_ts, differences = 1)
#     # 
#     # 
#     
#     #trying anyway with a simple test ?
#     
#     # Initialize vectors to store p-values for each order
#     p_values_plant <- numeric(5)
#     p_values_bird <- numeric(5)
#     
#     # Loop through orders 1 to 5
#     for (i in 1:5) {
#       # Perform Granger causality test for plant -> bird
#       BIRD_Test_result <- lmtest::grangertest(plant_ts, bird_ts, order = i, test = "F")
#       
#       # Perform Granger causality test for bird -> plant
#       PLANT_Test_result <- lmtest::grangertest(bird_ts, plant_ts, order = i, test = "F")
#       
#       # Extract the p-values and store them in vectors
#       p_values_plant[i] <- PLANT_Test_result$`Pr(>F)`[2]  # Second element is the p-value
#       p_values_bird[i] <- BIRD_Test_result$`Pr(>F)`[2]
#     }
#     
#     # Display the p-values for each order
#     p_values_plant
#     p_values_bird
#     
#     plant_name <- sub(".*/PSMC/([^/]+)/.*", "\\1", plant)
#     
#     # Create the dynamic name for the column
#     param_name <- paste0(plant_name, "_mu", 1, "_g", 2)
#     
#     Result <- as.data.frame(t(c(param_name, p_values_bird, p_values_plant)))
#   
#     }
#   }
#   return(Result)
# }






#AAA Alternative granger - if data is will not run normalized, or  Not enough data points to interpolate

Granger_psmc_entry_FUCKED <- function(bird, plant, bird_mu, plant_mu, bird_g, plant_gen, max_bird) {
  library(lmtest)
  library(imputeTS)
  library(dplyr)

  # Initialize an empty data frame to accumulate results
  All_Results <- data.frame()

  # Run PSMC for bird
  psmcbird <- psmc.result(bird, i.iteration=25, mu=6.8e-8, s=100, g=2.3)
  subset_psmcbird <- psmcbird[-(1:12), ]

  for (mu in plant_mu) {
    for (g in plant_gen) {

      # Run PSMC for plant with dynamic mu and g
      psmcplant <- psmc.result(plant, i.iteration=25, mu=mu, s=100, g=g)
      subset_psmcplant <- psmcplant[-(1:12), ]

      # Get filtering stats
      min_plant <- min(subset_psmcplant$Time)

      # Merge the two datasets
      merged_df <- merge(subset_psmcbird, subset_psmcplant, by = "Time",
                         suffixes = c("_bird", "_plant"), all = TRUE)

      # Filter merged dataframe
      cut_merged_df <- merged_df %>%
        filter(Time > (min_plant-1) & Time < max_bird)

      # Check if there is enough data before interpolation
      if (sum(!is.na(cut_merged_df$Ne_bird)) > 1 && sum(!is.na(cut_merged_df$Ne_plant)) > 1) {
        # Interpolate missing Data
        cut_merged_df$Bird_Ne <- na_interpolation(cut_merged_df$Ne_bird)
        cut_merged_df$Plant_Ne <- na_interpolation(cut_merged_df$Ne_plant)

        # Convert to time series object
        bird_ts <- ts(cut_merged_df$Bird_Ne, frequency = 1)
        plant_ts <- ts(cut_merged_df$Plant_Ne, frequency = 1)

        # Apply first differencing for stationarity
        bird_ts_diff <- diff(bird_ts, differences = 1)
        plant_ts_diff <- diff(plant_ts, differences = 1)

        # Initialize vectors to store p-values
        p_values_plant <- numeric(5)
        p_values_bird <- numeric(5)

        # Run the Granger causality test for different lags
        for (i in 1:5) {
          tryCatch({ # Allow #simple - acceptable Errors to pass through :D with the error massage and NA as p-value 
            BIRD_Test_result <- lmtest::grangertest(plant_ts, bird_ts, order = i, test = "F")
            PLANT_Test_result <- lmtest::grangertest(bird_ts, plant_ts, order = i, test = "F")

            p_values_plant[i] <- PLANT_Test_result$`Pr(>F)`[2]
            p_values_bird[i] <- BIRD_Test_result$`Pr(>F)`[2]
          }, error = function(e) {
            message("Error in Granger test for order ", i, ": ", e$message)
            p_values_plant[i] <- NA
            p_values_bird[i] <- NA
          })
        }

        # Dynamic name for the column
        plant_name <- sub(".*/PSMC/([^/]+)/.*", "\\1", plant)
        param_name <- paste0(plant_name, "_mu", mu, "_g", g)

        # Combine all results into one row
        Result <- as.data.frame(t(c(param_name, p_values_bird, p_values_plant)))

        # Accumulate results
        All_Results <- rbind(All_Results, Result)
      } else {
        # If not enough data for interpolation, assign NA for p-values
        message("Not enough data points to interpolate for plant: ", plant, ", mu: ", mu, ", g: ", g)

        # Assign NA for p-values
        p_values_plant <- rep(NA, 5)
        p_values_bird <- rep(NA, 5)

        # Combine all results into one row with NAs
        plant_name <- sub(".*/PSMC/([^/]+)/.*", "\\1", plant)
        param_name <- paste0(plant_name, "_mu", mu, "_g", g)

        Result <- as.data.frame(t(c(param_name, p_values_bird, p_values_plant)))

        # Accumulate results
        All_Results <- rbind(All_Results, Result)
      }
    }
  }

  return(All_Results)
}


#AAA Alternative granger - if data is will not run normalized, or  Not enough data points to interpolate

Granger_psmc_entry_FUCKED_diff <- function(bird, plant, bird_mu, plant_mu, bird_g, plant_gen, max_bird) {
  library(lmtest)
  library(imputeTS)
  library(dplyr)
  
  # Initialize an empty data frame to accumulate results
  All_Results <- data.frame()
  
  # Run PSMC for bird
  psmcbird <- psmc.result(bird, i.iteration=25, mu=6.8e-8, s=100, g=2.3)
  subset_psmcbird <- psmcbird[-(1:12), ]
  
  for (mu in plant_mu) {
    for (g in plant_gen) {
      
      # Run PSMC for plant with dynamic mu and g
      psmcplant <- psmc.result(plant, i.iteration=25, mu=mu, s=100, g=g)
      subset_psmcplant <- psmcplant[-(1:12), ]
      
      # Get filtering stats
      min_plant <- min(subset_psmcplant$Time)
      
      # Merge the two datasets
      merged_df <- merge(subset_psmcbird, subset_psmcplant, by = "Time",
                         suffixes = c("_bird", "_plant"), all = TRUE)
      
      # Filter merged dataframe
      cut_merged_df <- merged_df %>%
        filter(Time > (min_plant-1) & Time < max_bird)
      
      # Check if there is enough data before interpolation
      if (sum(!is.na(cut_merged_df$Ne_bird)) > 1 && sum(!is.na(cut_merged_df$Ne_plant)) > 1) {
        # Interpolate missing Data
        cut_merged_df$Bird_Ne <- na_interpolation(cut_merged_df$Ne_bird)
        cut_merged_df$Plant_Ne <- na_interpolation(cut_merged_df$Ne_plant)
        
        # Convert to time series object
        bird_ts <- ts(cut_merged_df$Bird_Ne, frequency = 1)
        plant_ts <- ts(cut_merged_df$Plant_Ne, frequency = 1)
        
        # Apply first differencing for stationarity
        bird_ts_diff <- diff(bird_ts, differences = 1)
        plant_ts_diff <- diff(plant_ts, differences = 1)
        
        # Initialize vectors to store p-values
        p_values_plant <- numeric(5)
        p_values_bird <- numeric(5)
        
        # Run the Granger causality test for different lags
        for (i in 1:5) {
          tryCatch({ # Allow #simple - acceptable Errors to pass through :D with the error massage and NA as p-value 
            BIRD_Test_result <- lmtest::grangertest(plant_ts_diff, bird_ts_diff, order = i, test = "F")
            PLANT_Test_result <- lmtest::grangertest(bird_ts_diff, plant_ts_diff, order = i, test = "F")
            
            p_values_plant[i] <- PLANT_Test_result$`Pr(>F)`[2]
            p_values_bird[i] <- BIRD_Test_result$`Pr(>F)`[2]
          }, error = function(e) {
            message("Error in Granger test for order ", i, ": ", e$message)
            p_values_plant[i] <- NA
            p_values_bird[i] <- NA
          })
        }
        
        # Dynamic name for the column
        plant_name <- sub(".*/PSMC/([^/]+)/.*", "\\1", plant)
        param_name <- paste0(plant_name, "_mu", mu, "_g", g)
        
        # Combine all results into one row
        Result <- as.data.frame(t(c(param_name, p_values_bird, p_values_plant)))
        
        # Accumulate results
        All_Results <- rbind(All_Results, Result)
      } else {
        # If not enough data for interpolation, assign NA for p-values
        message("Not enough data points to interpolate for plant: ", plant, ", mu: ", mu, ", g: ", g)
        
        # Assign NA for p-values
        p_values_plant <- rep(NA, 5)
        p_values_bird <- rep(NA, 5)
        
        # Combine all results into one row with NAs
        plant_name <- sub(".*/PSMC/([^/]+)/.*", "\\1", plant)
        param_name <- paste0(plant_name, "_mu", mu, "_g", g)
        
        Result <- as.data.frame(t(c(param_name, p_values_bird, p_values_plant)))
        
        # Accumulate results
        All_Results <- rbind(All_Results, Result)
      }
    }
  }
  
  return(All_Results)
}



Granger_psmc <- function(bird, plant, bird_mu, plant_mu, bird_g, plant_gen, max_bird) {
  library(lmtest)
  library(imputeTS)
  library(dplyr)
  
  # Initialize an empty data frame to accumulate results
  All_Results <- data.frame()
  
  # Run PSMC for bird
  psmcbird <- psmc.result(bird, i.iteration=25, mu=6.8e-8, s=100, g=2.3)
  subset_psmcbird <- psmcbird[-(1:12), ]
  subset_psmcbird <- subset_psmcbird %>%
    group_by(Time) %>%
    summarise(Ne = mean(Ne))
  
  for (mu in plant_mu) {
    for (g in plant_gen) {
      
      # Run PSMC for plant with dynamic mu and g
      psmcplant <- psmc.result(plant, i.iteration=25, mu=mu, s=100, g=g)
      subset_psmcplant <- psmcplant[-(1:12), ]
      subset_psmcplant <- subset_psmcplant %>%
        group_by(Time) %>%
        summarise(Ne = mean(Ne))
      
      # Get filtering stats
      min_plant <- min(subset_psmcplant$Time)
      
      # Merge the two datasets
      merged_df <- merge(subset_psmcbird, subset_psmcplant, by = "Time", 
                         suffixes = c("_bird", "_plant"), all = TRUE)
      
      # Filter merged dataframe
      cut_merged_df <- merged_df %>%
        filter(Time > (min_plant-1) & Time < max_bird)
      
      # Interpolate missing Data
      cut_merged_df$Bird_Ne <- na_interpolation(cut_merged_df$Ne_bird)
      cut_merged_df$Plant_Ne <- na_interpolation(cut_merged_df$Ne_plant)
      
      # Convert to time series object
      bird_ts <- ts(cut_merged_df$Bird_Ne, frequency = 1)
      plant_ts <- ts(cut_merged_df$Plant_Ne, frequency = 1)
      
      # Apply first differencing for stationarity
      bird_ts_diff <- diff(bird_ts, differences = 1)
      plant_ts_diff <- diff(plant_ts, differences = 1)
      
      # Initialize vectors to store p-values
      p_values_plant <- numeric(5)
      p_values_bird <- numeric(5)
      
      for (i in 1:5) {
        tryCatch({  # Allow #simple - acceptable Errors to pass through :D with the error massage and NA as p-value 
          BIRD_Test_result <- lmtest::grangertest(plant_ts, bird_ts, order = i, test = "F")
          PLANT_Test_result <- lmtest::grangertest(bird_ts, plant_ts, order = i, test = "F")
          
          p_values_plant[i] <- PLANT_Test_result$`Pr(>F)`[2]  
          p_values_bird[i] <- BIRD_Test_result$`Pr(>F)`[2]
        }, error = function(e) {
          message("Error in Granger test for order ", i, ": ", e$message)
          p_values_plant[i] <- NA
          p_values_bird[i] <- NA
        })
      }
      
      # Dynamic name for the column
      plant_name <- sub(".*/PSMC/([^/]+)/.*", "\\1", plant)
      param_name <- paste0(plant_name, "_mu", mu, "_g", g)
      
      # Combine all results into one row
      Result <- as.data.frame(t(c(param_name, p_values_bird, p_values_plant)))
      
      # Accumulate results
      All_Results <- rbind(All_Results, Result)
    }
  }
  return(All_Results)
}



#using the difference in the time series 


Granger_psmc_diff <- function(bird, plant, bird_mu, plant_mu, bird_g, plant_gen, max_bird) {
  library(lmtest)
  library(imputeTS)
  library(dplyr)
  
  # Initialize an empty data frame to accumulate results
  All_Results <- data.frame()
  
  # Run PSMC for bird
  psmcbird <- psmc.result(bird, i.iteration=25, mu=6.8e-8, s=100, g=2.3)
  subset_psmcbird <- psmcbird[-(1:12), ]
  subset_psmcbird <- subset_psmcbird %>%
    group_by(Time) %>%
    summarise(Ne = mean(Ne))
  
  
  for (mu in plant_mu) {
    for (g in plant_gen) {
      
      # Run PSMC for plant with dynamic mu and g
      psmcplant <- psmc.result(plant, i.iteration=25, mu=mu, s=100, g=g)
      subset_psmcplant <- psmcplant[-(1:12), ]
      subset_psmcplant <- subset_psmcplant %>%
        group_by(Time) %>%
        summarise(Ne = mean(Ne))
      
      # Get filtering stats
      min_plant <- min(subset_psmcplant$Time)
      
      # Merge the two datasets
      merged_df <- merge(subset_psmcbird, subset_psmcplant, by = "Time", 
                         suffixes = c("_bird", "_plant"), all = TRUE)
      
      # Filter merged dataframe
      cut_merged_df <- merged_df %>%
        filter(Time > (min_plant-1) & Time < max_bird)
      
      # Interpolate missing Data
      cut_merged_df$Bird_Ne <- na_interpolation(cut_merged_df$Ne_bird)
      cut_merged_df$Plant_Ne <- na_interpolation(cut_merged_df$Ne_plant)
      
      # Convert to time series object
      bird_ts <- ts(cut_merged_df$Bird_Ne, frequency = 1)
      plant_ts <- ts(cut_merged_df$Plant_Ne, frequency = 1)
      
      # Apply first differencing for stationarity
      bird_ts_diff <- diff(bird_ts, differences = 1) 
      plant_ts_diff <- diff(plant_ts, differences = 1)
      
      # Initialize vectors to store p-values
      p_values_plant <- numeric(5)
      p_values_bird <- numeric(5)
      
      for (i in 1:5) {
        tryCatch({  # Allow #simple - acceptable Errors to pass through :D with the error massage and NA as p-value 
          BIRD_Test_result <- lmtest::grangertest(plant_ts_diff, bird_ts_diff, order = i, test = "F")
          PLANT_Test_result <- lmtest::grangertest(bird_ts_diff, plant_ts_diff, order = i, test = "F")
          
          p_values_plant[i] <- PLANT_Test_result$`Pr(>F)`[2]  
          p_values_bird[i] <- BIRD_Test_result$`Pr(>F)`[2]
        }, error = function(e) {
          message("Error in Granger test for order ", i, ": ", e$message)
          p_values_plant[i] <- NA
          p_values_bird[i] <- NA
        })
      }
      
      # Dynamic name for the column
      plant_name <- sub(".*/PSMC/([^/]+)/.*", "\\1", plant)
      param_name <- paste0(plant_name, "_mu", mu, "_g", g)
      
      # Combine all results into one row
      Result <- as.data.frame(t(c(param_name, p_values_bird, p_values_plant)))
      
      # Accumulate results
      All_Results <- rbind(All_Results, Result)
    }
  }
  return(All_Results)
}





###INPUT PATHS 

EulJugA <- dir("C:/Users/au601132/OneDrive - Aarhus universitet/Skrivebord/PhD/Kolibri_heliconia/PSMC/Auto/","psmc$", full.names=T) # main PSMC results
HelBih <- dir("C:/Users/au601132/OneDrive - Aarhus universitet/Skrivebord/PhD/Kolibri_heliconia/PSMC/HBih/","psmc$", full.names=T) # main PSMC results
HelCar <- dir("C:/Users/au601132/OneDrive - Aarhus universitet/Skrivebord/PhD/Kolibri_heliconia/PSMC/HCar/","psmc$", full.names=T) # main PSMC results
EulHol <- dir("C:/Users/au601132/OneDrive - Aarhus universitet/Skrivebord/PhD/Kolibri_heliconia/PSMC/Ehol/","psmc$", full.names=T) # main PSMC results




###INPUT VARIABLES

plant_mu = c(4.5e-9, 
             5e-9,
             5.5e-9,
             6e-9,
             6.5e-9,
             7e-9,
             7.5e-9,
             8e-9,
             8.5e-9,
             9e-9,
             9.5e-9,
             1e-8,
             1.5e-8,
             2e-8,
             2.5e-8,
             3e-8,
             3.5e-8,
             4e-8, # "esimated"  
             4.5e-8,
             5e-8,
             5.5e-8,
             6e-8,
             6.5e-8,
             7e-8,
             7.5e-8,
             8e-8,
             8.5e-8,
             9e-8,
             9.5e-8,
             1e-7)

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
              21,
              22,
              23,
              24,
              25) # tilføj alle 3-25




##RUNRNS of funktion Granger_psmc, Granger_psmc_diff, & Granger_psmc_entry_FUCKED


EU_HELCAR <- Granger_psmc(EulJugA, HelCar, bird_mu=6.8e-8, plant_mu, plant_gen, bird_g=2.3, 259000)
#EU_HELBIH <- Granger_psmc(EulJugA, HelBih, bird_mu=6.8e-8, plant_mu, plant_gen, bird_g=2.3)

EU_HELCAR_diff <- Granger_psmc_diff(EulJugA, HelCar, bird_mu=6.8e-8, plant_mu, plant_gen, bird_g=2.3, 259000)
#EU_HELBIH_diff <- Granger_psmc_diff(EulJugA, HelBih, bird_mu=6.8e-8, plant_mu, plant_gen, bird_g=2.3)

#I wanst able to run reagular Granger --> so i ran one that allows for NA.
EU_HELBIH <- Granger_psmc_entry_FUCKED(EulJugA, HelBih, bird_mu=6.8e-8, plant_mu, plant_gen, bird_g=2.3, 259000)
EU_HELBIH_diff <- Granger_psmc_entry_FUCKED_diff(EulJugA, HelBih, bird_mu=6.8e-8, plant_mu, plant_gen, bird_g=2.3, 259000)





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
  library(pheatmap)
  library(viridis)
  # Create a viridis palette with 50 colors
  new_palette <- viridis(25, option = "C")
  
  # Define breaks: 51 breaks for 50 colors (breaks must be length(colors) + 1)
  breaks <- seq(-0.1, 98, length.out = 26)  
  
  # Create the heatmap
  p <- pheatmap(matrix_log_transformed,
                scale = "none",
                color = new_palette,
                breaks = breaks,  # Ensure the breaks align with colors
                main = title,
                cluster_rows = FALSE,
                cluster_cols = FALSE,
                show_rownames = TRUE,
                show_colnames = TRUE,
                legend_breaks = seq(0, 98, 5),  
                legend_labels = seq(0, 98, 5),  
                fontsize_row = 6,  
                fontsize_col = 6,  
                fontsize = 7)     
  
  print(p)

  return(list(matrix = matrix_resized, log_matrix=matrix_log_transformed, plot = p))
}

          


V2_EU_HELBIH <- create_matrix(EU_HELBIH, "V2", "H.B effect on E.Jugularis, Order 1")
V3_EU_HELBIH <- create_matrix(EU_HELBIH, "V3", "H.B effect on E.Jugularis, Order 2")
V4_EU_HELBIH <- create_matrix(EU_HELBIH, "V4", "H.B effect on E.Jugularis, Order 3")
V5_EU_HELBIH <- create_matrix(EU_HELBIH, "V5", "H.B effect on E.Jugularis, Order 4")
V6_EU_HELBIH <- create_matrix(EU_HELBIH, "V6", "H.B effect on E.Jugularis, Order 5")
V7_EU_HELBIH <- create_matrix(EU_HELBIH, "V7", "E.jugularis effect on H.B, Order 1")
V8_EU_HELBIH <- create_matrix(EU_HELBIH, "V8", "E.jugularis effect on H.B, Order 2")
V9_EU_HELBIH <- create_matrix(EU_HELBIH, "V9", "E.jugularis effect on H.B, Order 3")
V10_EU_HELBIH <- create_matrix(EU_HELBIH, "V10", "E.Jugularis effect on H.B, Order 4")
V11_EU_HELBIH <- create_matrix(EU_HELBIH, "V11", "E.Jugularis effect on H.B, Order 5")


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


###
##Diff
#


V2_EU_HELBIH <- create_matrix(EU_HELBIH_diff, "V2", "H.B effect on E.Jugularis, Order 1")
V3_EU_HELBIH <- create_matrix(EU_HELBIH_diff, "V3", "H.B effect on E.Jugularis, Order 2")
V4_EU_HELBIH <- create_matrix(EU_HELBIH_diff, "V4", "H.B effect on E.Jugularis, Order 3")
V5_EU_HELBIH <- create_matrix(EU_HELBIH_diff, "V5", "H.B effect on E.Jugularis, Order 4")
V6_EU_HELBIH <- create_matrix(EU_HELBIH_diff, "V6", "H.B effect on E.Jugularis, Order 5")
V7_EU_HELBIH <- create_matrix(EU_HELBIH_diff, "V7", "E.jugularis effect on H.B, Order 1")
V8_EU_HELBIH <- create_matrix(EU_HELBIH_diff, "V8", "E.jugularis effect on H.B, Order 2")
V9_EU_HELBIH <- create_matrix(EU_HELBIH_diff, "V9", "E.jugularis effect on H.B, Order 3")
V10_EU_HELBIH <- create_matrix(EU_HELBIH_diff, "V10", "E.Jugularis effect on H.B, Order 4")
V11_EU_HELBIH <- create_matrix(EU_HELBIH_diff, "V11", "E.Jugularis effect on H.B, Order 5")


V2_EU_HELCAR <- create_matrix(EU_HELCAR_diff, "V2", "H.C effect on E.Jugularis, Order 1")
V3_EU_HELCAR <- create_matrix(EU_HELCAR_diff, "V3", "H.C effect on E.Jugularis, Order 2")
V4_EU_HELCAR <- create_matrix(EU_HELCAR_diff, "V4", "H.C effect on E.Jugularis, Order 3")
V5_EU_HELCAR <- create_matrix(EU_HELCAR_diff, "V5", "H.C effect on E.Jugularis, Order 4")
V6_EU_HELCAR <- create_matrix(EU_HELCAR_diff, "V6", "H.C effect on E.Jugularis, Order 5")
V7_EU_HELCAR <- create_matrix(EU_HELCAR_diff, "V7", "E.jugularis effect on H.C, Order 1")
V8_EU_HELCAR <- create_matrix(EU_HELCAR_diff, "V8", "E.jugularis effect on H.C, Order 2")
V9_EU_HELCAR <- create_matrix(EU_HELCAR_diff, "V9", "E.jugularis effect on H.C, Order 3")
V10_EU_HELCAR <- create_matrix(EU_HELCAR_diff, "V10", "E.jugularis effect on H.C, Order 4")
V11_EU_HELCAR <- create_matrix(EU_HELCAR_diff, "V11", "E.jugularis effect on H.C, Order 5")



#Save a PDF with 
#these plots in the top Row
setwd("C:/Users/au601132/OneDrive - Aarhus universitet/Skrivebord/PhD/Kolibri_heliconia/-log10/")

#setwd("C:/Users/au601132/OneDrive - Aarhus universitet/Skrivebord/")


# Convert pheatmaps to grobs
plot1 <- grid.grabExpr(print(V2_EU_HELBIH$plot))
plot2 <- grid.grabExpr(print(V3_EU_HELBIH$plot))
plot3 <- grid.grabExpr(print(V4_EU_HELBIH$plot))
plot4 <- grid.grabExpr(print(V5_EU_HELBIH$plot))
plot5 <- grid.grabExpr(print(V6_EU_HELBIH$plot))

plot6 <- grid.grabExpr(print(V2_EU_HELCAR$plot))
plot7 <- grid.grabExpr(print(V3_EU_HELCAR$plot))
plot8 <- grid.grabExpr(print(V4_EU_HELCAR$plot))
plot9 <- grid.grabExpr(print(V5_EU_HELCAR$plot))
plot10 <- grid.grabExpr(print(V6_EU_HELCAR$plot))


plot11 <- grid.grabExpr(print(V7_EU_HELBIH$plot))
plot12 <- grid.grabExpr(print(V8_EU_HELBIH$plot))
plot13 <- grid.grabExpr(print(V9_EU_HELBIH$plot))
plot14 <- grid.grabExpr(print(V10_EU_HELBIH$plot))
plot15 <- grid.grabExpr(print(V11_EU_HELBIH$plot))

plot16 <- grid.grabExpr(print(V7_EU_HELCAR$plot))
plot17 <- grid.grabExpr(print(V8_EU_HELCAR$plot))
plot18 <- grid.grabExpr(print(V9_EU_HELCAR$plot))
plot19 <- grid.grabExpr(print(V10_EU_HELCAR$plot))
plot20 <- grid.grabExpr(print(V11_EU_HELCAR$plot))


# Open a PDF file to save the plots
pdf("Plant_vs_Bird_7&.pdf", width = 14, height = 10)

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





#Other species FULL trend!!! 
#Explore HOLO ##holosericeus

EU_HOLO_HELBIH_diff <- Granger_psmc_entry_FUCKED_diff(EulHol, HelBih, bird_mu=6.8e-8, plant_mu, plant_gen, bird_g=2.3, 259000)
EU_HOLO_HELCAR_diff <- Granger_psmc_entry_FUCKED_diff(EulHol, HelCar, bird_mu=6.8e-8, plant_mu, plant_gen, bird_g=2.3, 259000)


EU_HOLO_HELBIH <- Granger_psmc_entry_FUCKED(EulHol, HelBih, bird_mu=6.8e-8, plant_mu, plant_gen, bird_g=2.3, 259000)
EU_HOLO_HELCAR <- Granger_psmc_entry_FUCKED(EulHol, HelCar, bird_mu=6.8e-8, plant_mu, plant_gen, bird_g=2.3, 259000)

V2_EU_HOLO_HELBIH <- create_matrix(EU_HOLO_HELBIH_diff, "V2", "H.B effect on E.holosericeus, Order 1")
V3_EU_HOLO_HELBIH <- create_matrix(EU_HOLO_HELBIH_diff, "V3", "H.B effect on E.holosericeus, Order 2")
V4_EU_HOLO_HELBIH <- create_matrix(EU_HOLO_HELBIH_diff, "V4", "Heatmap of H.B effect on E.holosericeus, Order 3")
V5_EU_HOLO_HELBIH <- create_matrix(EU_HOLO_HELBIH_diff, "V5", "Heatmap of H.B effect on E.holosericeus, Order 4")
V6_EU_HOLO_HELBIH <- create_matrix(EU_HOLO_HELBIH_diff, "V6", "Heatmap of H.B effect on E.holosericeus, Order 5")
V7_EU_HOLO_HELBIH <- create_matrix(EU_HOLO_HELBIH_diff, "V7", "Heatmap of E.holosericeus effect on H.B, Order 1")
V8_EU_HOLO_HELBIH <- create_matrix(EU_HOLO_HELBIH_diff, "V8", "Heatmap of E.holosericeus effect on H.B, Order 2")
V9_EU_HOLO_HELBIH <- create_matrix(EU_HOLO_HELBIH_diff, "V9", "Heatmap of E.holosericeus effect on H.B, Order 3")
V10_EU_HOLO_HELBIH <- create_matrix(EU_HOLO_HELBIH_diff, "V10", "Heatmap of E.holosericeus effect on H.B, Order 4")
V11_EU_HOLO_HELBIH <- create_matrix(EU_HOLO_HELBIH_diff, "V11", "Heatmap of E.holosericeus effect on H.B, Order 5")


V2_EU_HOLO_HELCAR <- create_matrix(EU_HOLO_HELCAR_diff, "V2", "Heatmap of H.C effect on E.holosericeus, Order 1")
V3_EU_HOLO_HELCAR <- create_matrix(EU_HOLO_HELCAR_diff, "V3", "Heatmap of H.C effect on E.holosericeus, Order 2")
V4_EU_HOLO_HELCAR <- create_matrix(EU_HOLO_HELCAR_diff, "V4", "Heatmap of H.C effect on E.holosericeus, Order 3")
V5_EU_HOLO_HELCAR <- create_matrix(EU_HOLO_HELCAR_diff, "V5", "Heatmap of H.C effect on E.holosericeus, Order 4")
V6_EU_HOLO_HELCAR <- create_matrix(EU_HOLO_HELCAR_diff, "V6", "Heatmap of H.C effect on E.holosericeus, Order 5")
V7_EU_HOLO_HELCAR <- create_matrix(EU_HOLO_HELCAR_diff, "V7", "Heatmap of E.holosericeus effect on H.C, Order 1")
V8_EU_HOLO_HELCAR <- create_matrix(EU_HOLO_HELCAR_diff, "V8", "Heatmap of E.holosericeus effect on H.C, Order 2")
V9_EU_HOLO_HELCAR <- create_matrix(EU_HOLO_HELCAR_diff, "V9", "Heatmap of E.holosericeus effect on H.C, Order 3")
V10_EU_HOLO_HELCAR <- create_matrix(EU_HOLO_HELCAR_diff, "V10", "Heatmap of E.holosericeus effect on H.C, Order 4")
V11_EU_HOLE_HELCAR <- create_matrix(EU_HOLO_HELCAR_diff, "V11", "Heatmap of E.holosericeus effect on H.C, Order 5")




V2_EU_HOLO_HELBIH <- create_matrix(EU_HOLO_HELBIH, "V2", "Heatmap of H.B effect on E.holosericeus, Order 1")
V3_EU_HOLO_HELBIH <- create_matrix(EU_HOLO_HELBIH, "V3", "Heatmap of H.B effect on E.holosericeus, Order 2")
V4_EU_HOLO_HELBIH <- create_matrix(EU_HOLO_HELBIH, "V4", "Heatmap of H.B effect on E.holosericeus, Order 3")
V5_EU_HOLO_HELBIH <- create_matrix(EU_HOLO_HELBIH, "V5", "Heatmap of H.B effect on E.holosericeus, Order 4")
V6_EU_HOLO_HELBIH <- create_matrix(EU_HOLO_HELBIH, "V6", "Heatmap of H.B effect on E.holosericeus, Order 5")
V7_EU_HOLO_HELBIH <- create_matrix(EU_HOLO_HELBIH, "V7", "Heatmap of E.holosericeus effect on H.B, Order 1")
V8_EU_HOLO_HELBIH <- create_matrix(EU_HOLO_HELBIH, "V8", "Heatmap of E.holosericeus effect on H.B, Order 2")
V9_EU_HOLO_HELBIH <- create_matrix(EU_HOLO_HELBIH, "V9", "Heatmap of E.holosericeus effect on H.B, Order 3")
V10_EU_HOLO_HELBIH <- create_matrix(EU_HOLO_HELBIH, "V10", "Heatmap of E.holosericeus effect on H.B, Order 4")
V11_EU_HOLO_HELBIH <- create_matrix(EU_HOLO_HELBIH, "V11", "Heatmap of E.holosericeus effect on H.B, Order 5")


V2_EU_HOLO_HELCAR <- create_matrix(EU_HOLO_HELCAR, "V2", "Heatmap of H.C effect on E.holosericeus, Order 1")
V3_EU_HOLO_HELCAR <- create_matrix(EU_HOLO_HELCAR, "V3", "Heatmap of H.C effect on E.holosericeus, Order 2")
V4_EU_HOLO_HELCAR <- create_matrix(EU_HOLO_HELCAR, "V4", "Heatmap of H.C effect on E.holosericeus, Order 3")
V5_EU_HOLO_HELCAR <- create_matrix(EU_HOLO_HELCAR, "V5", "Heatmap of H.C effect on E.holosericeus, Order 4")
V6_EU_HOLO_HELCAR <- create_matrix(EU_HOLO_HELCAR, "V6", "Heatmap of H.C effect on E.holosericeus, Order 5")
V7_EU_HOLO_HELCAR <- create_matrix(EU_HOLO_HELCAR, "V7", "Heatmap of E.holosericeus effect on H.C, Order 1")
V8_EU_HOLO_HELCAR <- create_matrix(EU_HOLO_HELCAR, "V8", "Heatmap of E.holosericeus effect on H.C, Order 2")
V9_EU_HOLO_HELCAR <- create_matrix(EU_HOLO_HELCAR, "V9", "Heatmap of E.holosericeus effect on H.C, Order 3")
V10_EU_HOLO_HELCAR <- create_matrix(EU_HOLO_HELCAR, "V10", "Heatmap of E.holosericeus effect on H.C, Order 4")
V11_EU_HOLE_HELCAR <- create_matrix(EU_HOLO_HELCAR, "V11", "Heatmap of E.holosericeus effect on H.C, Order 5")





# Convert pheatmaps to grobs
plot1 <- grid.grabExpr(print(V2_EU_HELBIH$plot))
plot2 <- grid.grabExpr(print(V3_EU_HELBIH$plot))
plot3 <- grid.grabExpr(print(V4_EU_HELBIH$plot))
plot4 <- grid.grabExpr(print(V5_EU_HELBIH$plot))
plot5 <- grid.grabExpr(print(V6_EU_HELBIH$plot))

plot6 <- grid.grabExpr(print(V2_EU_HELCAR$plot))
plot7 <- grid.grabExpr(print(V3_EU_HELCAR$plot))
plot8 <- grid.grabExpr(print(V4_EU_HELCAR$plot))
plot9 <- grid.grabExpr(print(V5_EU_HELCAR$plot))
plot10 <- grid.grabExpr(print(V6_EU_HELCAR$plot))

plot11 <- grid.grabExpr(print(V2_EU_HOLO_HELBIH$plot))
plot12 <- grid.grabExpr(print(V3_EU_HOLO_HELBIH$plot))
plot13 <- grid.grabExpr(print(V4_EU_HOLO_HELBIH$plot))
plot14 <- grid.grabExpr(print(V5_EU_HOLO_HELBIH$plot))
plot15 <- grid.grabExpr(print(V6_EU_HOLO_HELBIH$plot))

plot16 <- grid.grabExpr(print(V2_EU_HOLO_HELCAR$plot))
plot17 <- grid.grabExpr(print(V3_EU_HOLO_HELCAR$plot))
plot18 <- grid.grabExpr(print(V4_EU_HOLO_HELCAR$plot))
plot19 <- grid.grabExpr(print(V5_EU_HOLO_HELCAR$plot))
plot20 <- grid.grabExpr(print(V6_EU_HOLO_HELCAR$plot))



# Open a PDF file to save the plots
pdf("Plant_on_HOLO_vsJUR_7.pdf", width = 14, height = 10)

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











    # The last 100.000 years! 
#Other species 
#Explore HOLO ##holosericeus

recent_EU_HELCAR <- Granger_psmc_entry_FUCKED(EulJugA, HelCar, bird_mu=6.8e-8, plant_mu, plant_gen, bird_g=2.3, 100000)
#EU_HELBIH <- Granger_psmc(EulJugA, HelBih, bird_mu=6.8e-8, plant_mu, plant_gen, bird_g=2.3)

recent_EU_HELCAR_diff <- Granger_psmc_entry_FUCKED_diff(EulJugA, HelCar, bird_mu=6.8e-8, plant_mu, plant_gen, bird_g=2.3, 100000)
#EU_HELBIH_diff <- Granger_psmc_diff(EulJugA, HelBih, bird_mu=6.8e-8, plant_mu, plant_gen, bird_g=2.3)

#I wanst able to run reagular Granger --> so i ran one that allows for NA.
recent_EU_HELBIH <- Granger_psmc_entry_FUCKED(EulJugA, HelBih, bird_mu=6.8e-8, plant_mu, plant_gen, bird_g=2.3, 100000)
recent_EU_HELBIH_diff <- Granger_psmc_entry_FUCKED_diff(EulJugA, HelBih, bird_mu=6.8e-8, plant_mu, plant_gen, bird_g=2.3, 100000)


recent_EU_HOLO_HELBIH_diff <- Granger_psmc_entry_FUCKED_diff(EulHol, HelBih, bird_mu=6.8e-8, plant_mu, plant_gen, bird_g=2.3, 100000)
recent_EU_HOLO_HELCAR_diff <- Granger_psmc_entry_FUCKED_diff(EulHol, HelCar, bird_mu=6.8e-8, plant_mu, plant_gen, bird_g=2.3, 100000)


recent_EU_HOLO_HELBIH <- Granger_psmc_entry_FUCKED(EulHol, HelBih, bird_mu=6.8e-8, plant_mu, plant_gen, bird_g=2.3, 100000)
recent_EU_HOLO_HELCAR <- Granger_psmc_entry_FUCKED(EulHol, HelCar, bird_mu=6.8e-8, plant_mu, plant_gen, bird_g=2.3, 100000)

V2_EU_HOLO_HELBIH <- create_matrix(recent_EU_HOLO_HELBIH_diff, "V2", "H.B effect on E.holosericeus, Order 1")
V3_EU_HOLO_HELBIH <- create_matrix(recent_EU_HOLO_HELBIH_diff, "V3", "H.B effect on E.holosericeus, Order 2")
V4_EU_HOLO_HELBIH <- create_matrix(recent_EU_HOLO_HELBIH_diff, "V4", "H.B effect on E.holosericeus, Order 3")
V5_EU_HOLO_HELBIH <- create_matrix(recent_EU_HOLO_HELBIH_diff, "V5", "H.B effect on E.holosericeus, Order 4")
V6_EU_HOLO_HELBIH <- create_matrix(recent_EU_HOLO_HELBIH_diff, "V6", "H.B effect on E.holosericeus, Order 5")
V7_EU_HOLO_HELBIH <- create_matrix(recent_EU_HOLO_HELBIH_diff, "V7", "E.holosericeus effect on H.B, Order 1")
V8_EU_HOLO_HELBIH <- create_matrix(recent_EU_HOLO_HELBIH_diff, "V8", "E.holosericeus effect on H.B, Order 2")
V9_EU_HOLO_HELBIH <- create_matrix(recent_EU_HOLO_HELBIH_diff, "V9", "E.holosericeus effect on H.B, Order 3")
V10_EU_HOLO_HELBIH <- create_matrix(recent_EU_HOLO_HELBIH_diff, "V10", "E.holosericeus effect on H.B, Order 4")
V11_EU_HOLO_HELBIH <- create_matrix(recent_EU_HOLO_HELBIH_diff, "V11", "E.holosericeus effect on H.B, Order 5")


V2_EU_HOLO_HELCAR <- create_matrix(recent_EU_HOLO_HELCAR_diff, "V2", "H.C effect on E.holosericeus, Order 1")
V3_EU_HOLO_HELCAR <- create_matrix(recent_EU_HOLO_HELCAR_diff, "V3", "H.C effect on E.holosericeus, Order 2")
V4_EU_HOLO_HELCAR <- create_matrix(recent_EU_HOLO_HELCAR_diff, "V4", "H.C effect on E.holosericeus, Order 3")
V5_EU_HOLO_HELCAR <- create_matrix(recent_EU_HOLO_HELCAR_diff, "V5", "H.C effect on E.holosericeus, Order 4")
V6_EU_HOLO_HELCAR <- create_matrix(recent_EU_HOLO_HELCAR_diff, "V6", "H.C effect on E.holosericeus, Order 5")
V7_EU_HOLO_HELCAR <- create_matrix(recent_EU_HOLO_HELCAR_diff, "V7", "E.holosericeus effect on H.C, Order 1")
V8_EU_HOLO_HELCAR <- create_matrix(recent_EU_HOLO_HELCAR_diff, "V8", "E.holosericeus effect on H.C, Order 2")
V9_EU_HOLO_HELCAR <- create_matrix(recent_EU_HOLO_HELCAR_diff, "V9", "E.holosericeus effect on H.C, Order 3")
V10_EU_HOLO_HELCAR <- create_matrix(recent_EU_HOLO_HELCAR_diff, "V10", "E.holosericeus effect on H.C, Order 4")
V11_EU_HOLE_HELCAR <- create_matrix(recent_EU_HOLO_HELCAR_diff, "V11", "E.holosericeus effect on H.C, Order 5")



V2_EU_HELBIH <- create_matrix(recent_EU_HELBIH, "V2", "H.B effect on E.Jugularis, Order 1")
V3_EU_HELBIH <- create_matrix(recent_EU_HELBIH, "V3", "H.B effect on E.Jugularis, Order 2")
V4_EU_HELBIH <- create_matrix(recent_EU_HELBIH, "V4", "H.B effect on E.Jugularis, Order 3")
V5_EU_HELBIH <- create_matrix(recent_EU_HELBIH, "V5", "H.B effect on E.Jugularis, Order 4")
V6_EU_HELBIH <- create_matrix(recent_EU_HELBIH, "V6", "H.B effect on E.Jugularis, Order 5")
V7_EU_HELBIH <- create_matrix(recent_EU_HELBIH, "V7", "E.jugularis effect on H.B, Order 1")
V8_EU_HELBIH <- create_matrix(recent_EU_HELBIH, "V8", "E.jugularis effect on H.B, Order 2")
V9_EU_HELBIH <- create_matrix(recent_EU_HELBIH, "V9", "E.jugularis effect on H.B, Order 3")
V10_EU_HELBIH <- create_matrix(recent_EU_HELBIH, "V10", "E.Jugularis effect on H.B, Order 4")
V11_EU_HELBIH <- create_matrix(recent_EU_HELBIH, "V11", "E.Jugularis effect on H.B, Order 5")


V2_EU_HELCAR <- create_matrix(recent_EU_HELCAR, "V2", "H.C effect on E.Jugularis, Order 1")
V3_EU_HELCAR <- create_matrix(recent_EU_HELCAR, "V3", "H.C effect on E.Jugularis, Order 2")
V4_EU_HELCAR <- create_matrix(recent_EU_HELCAR, "V4", "H.C effect on E.Jugularis, Order 3")
V5_EU_HELCAR <- create_matrix(recent_EU_HELCAR, "V5", "H.C effect on E.Jugularis, Order 4")
V6_EU_HELCAR <- create_matrix(recent_EU_HELCAR, "V6", "H.C effect on E.Jugularis, Order 5")
V7_EU_HELCAR <- create_matrix(recent_EU_HELCAR, "V7", "E.jugularis effect on H.C, Order 1")
V8_EU_HELCAR <- create_matrix(recent_EU_HELCAR, "V8", "E.jugularis effect on H.C, Order 2")
V9_EU_HELCAR <- create_matrix(recent_EU_HELCAR, "V9", "E.jugularis effect on H.C, Order 3")
V10_EU_HELCAR <- create_matrix(recent_EU_HELCAR, "V10", "E.jugularis effect on H.C, Order 4")
V11_EU_HELCAR <- create_matrix(recent_EU_HELCAR, "V11", "E.jugularis effect on H.C, Order 5")




# Convert pheatmaps to grobs
plot1 <- grid.grabExpr(print(V2_EU_HELBIH$plot))
plot2 <- grid.grabExpr(print(V3_EU_HELBIH$plot))
plot3 <- grid.grabExpr(print(V4_EU_HELBIH$plot))
plot4 <- grid.grabExpr(print(V5_EU_HELBIH$plot))
plot5 <- grid.grabExpr(print(V6_EU_HELBIH$plot))

plot6 <- grid.grabExpr(print(V2_EU_HELCAR$plot))
plot7 <- grid.grabExpr(print(V3_EU_HELCAR$plot))
plot8 <- grid.grabExpr(print(V4_EU_HELCAR$plot))
plot9 <- grid.grabExpr(print(V5_EU_HELCAR$plot))
plot10 <- grid.grabExpr(print(V6_EU_HELCAR$plot))

plot11 <- grid.grabExpr(print(V2_EU_HOLO_HELBIH$plot))
plot12 <- grid.grabExpr(print(V3_EU_HOLO_HELBIH$plot))
plot13 <- grid.grabExpr(print(V4_EU_HOLO_HELBIH$plot))
plot14 <- grid.grabExpr(print(V5_EU_HOLO_HELBIH$plot))
plot15 <- grid.grabExpr(print(V6_EU_HOLO_HELBIH$plot))

plot16 <- grid.grabExpr(print(V2_EU_HOLO_HELCAR$plot))
plot17 <- grid.grabExpr(print(V3_EU_HOLO_HELCAR$plot))
plot18 <- grid.grabExpr(print(V4_EU_HOLO_HELCAR$plot))
plot19 <- grid.grabExpr(print(V5_EU_HOLO_HELCAR$plot))
plot20 <- grid.grabExpr(print(V6_EU_HOLO_HELCAR$plot))



# Open a PDF file to save the plots
pdf("LAST100.000Diff_Plant_on_HOLO_vsJUR_7.pdf", width = 14, height = 10)

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


























#####Running PSMC, 
####Cutting to covered time period. 
###Fitting a line, 
##Predicting Ne every 1000 year 
#Rooting data by computing t0-t1,t1-t2...tn-1-tn, (diff($))
#Running a Granger causality Test order i=1-5.

##Below a function: 
  #To automate species comparisons and facilitate parameter exploration. :D


# ### First the manual run and plotting of data !!!


                ########################
                #  Running in terminal #
                ########################
# if Kronk avaliable - The lever will be pulled.
# else:


# Run PSMC for bird
psmcbird <- psmc.result(EulJugA, i.iteration=25, mu=6.8e-8, s=100, g=2.3)
subset_psmcbird <- psmcbird[-(1:12), ]
#fit a line (down-stream)
smooth_bird <- smooth.spline(subset_psmcbird)

# Run PSMC for plant with dynamic mu and g
psmcplant <- psmc.result(HelBih, i.iteration=25, mu=8e-8, s=100, g=4)
subset_psmcplant <- psmcplant[-(1:12), ]
#fit a line (down-stream)
smooth_plant <- smooth.spline(subset_psmcplant)


# Get filtering stats
min_plant <- min(subset_psmcplant$Time)
max_bird <- max(subset_psmcbird$Time)

# Merge the two datasets
merged_df <- merge(subset_psmcbird, subset_psmcplant, by = "Time",
                   suffixes = c("_bird", "_plant"), all = TRUE)
head(merged_df)

dev.new()


###CHECK out the real data !!

ggplot(merged_df, aes(x = Time)) +
  geom_line(aes(y = Ne_bird), color = "purple", na.rm = TRUE) +
  geom_line(aes(y = Ne_plant), color = "forestgreen", na.rm = TRUE) +  # Log scale for time
  scale_y_continuous(limits = c(0, 4e+4)) +
  labs(title = "Ne over Time",
       x = "Time",
       y = "Effective Population Size (Ne)") +
  theme_minimal()


##log (TIME)


ggplot(merged_df, aes(x = Time)) +
  geom_line(aes(y = Ne_bird), color = "purple", na.rm = TRUE) +
  geom_line(aes(y = Ne_plant), color = "forestgreen", na.rm = TRUE) +  # Log scale for time
  scale_y_continuous(limits = c(0, 5e+4)) +
  scale_x_log10(labels = scales::comma) +
  labs(title = "Ne over Time (Log Scale for Time)",
       x = "Time",
       y = "Effective Population Size (Ne)") +
  theme_minimal()



# Filter merged dataframe
cut_merged_df <- merged_df %>%
  filter(Time > (min_plant-1) & Time < max_bird)

###CHECK out the comparison data !!


ggplot(cut_merged_df, aes(x = Time)) +
  geom_point(aes(y = Ne_bird), color = "purple", size = 2) +
  geom_point(aes(y = Ne_plant), color = "forestgreen", size = 2) +
  scale_x_log10(labels = scales::comma) +
  labs(title = "Checking once cut",
       x = "Time",
       y = "Effective Population Size (Ne)") +
  theme_minimal()



####Predicting 

Newvals <- data.frame(Time=seq(min_plant, max(max_bird, na.rm=T), 1000))

Newvals$pred_bird <- predict(smooth_bird, Newvals$Time)$y

Newvals$pred_plant <- predict(smooth_plant, Newvals$Time)$y

head(Newvals)

ggplot(Newvals, aes(x = Time)) +
  geom_point(aes(y = pred_bird), color = "purple", size = 1) +
  geom_point(aes(y = pred_plant), color = "forestgreen", size = 1) +
  scale_x_log10(labels = scales::comma) +
  labs(title = "Checking once cut",
       x = "Time",
       y = "Effective Population Size (Ne)") +
  theme_minimal()


# Convert to time series object
bird_ts <- ts(Newvals$pred_bird, frequency = 1)
plant_ts <- ts(Newvals$pred_plant, frequency = 1)

adf.test(bird_ts) 
adf.test(plant_ts) 


# Apply first differencing for stationarity
bird_ts_diff <- diff(bird_ts, differences = 1)
plant_ts_diff <- diff(plant_ts, differences = 1)

adf.test(bird_ts_diff) 
adf.test(plant_ts_diff) 


BIRD_Test_result <- lmtest::grangertest(plant_ts_diff, bird_ts_diff, order = 2, test = "F")
PLANT_Test_result <- lmtest::grangertest(bird_ts_diff, plant_ts_diff, order = 2, test = "F")

# Initialize vectors to store p-values
p_values_plant <- numeric(5)
p_values_bird <- numeric(5)


for (i in 1:5) {
  tryCatch({ # Allow #simple - acceptable Errors to pass through :D with the error massage and NA as p-value 
    BIRD_Test_result <- lmtest::grangertest(plant_ts_diff, bird_ts_diff, order = i, test = "F")
    PLANT_Test_result <- lmtest::grangertest(bird_ts_diff, plant_ts_diff, order = i, test = "F")
    
    p_values_plant[i] <- PLANT_Test_result$`Pr(>F)`[2]
    p_values_bird[i] <- BIRD_Test_result$`Pr(>F)`[2]
  }, error = function(e) {
    message("Error in Granger test for order ", i, ": ", e$message)
    p_values_plant[i] <- NA
    p_values_bird[i] <- NA
  })
}

# Dynamic name for the column
plant_name <- sub(".*/PSMC/([^/]+)/.*", "\\1", HelBih)
param_name <- paste0(plant_name, "_mu", mu, "_g", g)

# Combine all results into one row
Result <- as.data.frame(t(c(param_name, p_values_bird, p_values_plant)))

# Accumulate results
All_Results <- rbind(All_Results, Result)






#AAA Alternative granger 

NEW_Granger_psmc <- function(bird, plant, plant_mu, bird_mu, bird_g, plant_gen, max_bird) {
  library(lmtest)
  library(imputeTS)
  library(dplyr)
  library(tseries)
  
  # Initialize an empty data frame to accumulate results
  All_Results <- data.frame()
  
  # Run PSMC for bird
  psmcbird <- psmc.result(EulJugA, i.iteration=25, mu=bird_mu, s=100, g=bird_g)
  subset_psmcbird <- psmcbird[-(1:12), ]
  #fit a line (down-stream)
  smooth_bird <- smooth.spline(subset_psmcbird)
  
  # Run PSMC for plant with dynamic mu and g
  psmcplant <- psmc.result(plant, i.iteration=25, mu=8e-8, s=100, g=4)
  subset_psmcplant <- psmcplant[-(1:12), ]
  #fit a line (down-stream)
  smooth_plant <- smooth.spline(subset_psmcplant)
  
  for (mu in plant_mu) {
    for (g in plant_gen) {
      
      # Run PSMC for plant with dynamic mu and g
      psmcplant <- psmc.result(plant, i.iteration=25, mu=8e-8, s=100, g=4)
      subset_psmcplant <- psmcplant[-(1:12), ]
      #fit a line (down-stream)
      smooth_plant <- smooth.spline(subset_psmcplant)
      
      # Get filtering stats
      min_plant <- min(subset_psmcplant$Time)
      max_bird <- max(subset_psmcbird$Time)
      
      # Merge the two datasets
      merged_df <- merge(subset_psmcbird, subset_psmcplant, by = "Time",
                         suffixes = c("_bird", "_plant"), all = TRUE)
      
      # Filter merged dataframe
      cut_merged_df <- merged_df %>%
        filter(Time > (min_plant-1) & Time < max_bird)
      
      ####Predicting 
      Newvals <- data.frame(Time=seq(min_plant, max(max_bird, na.rm=T), 1000))
      Newvals$pred_bird <- predict(smooth_bird, Newvals$Time)$y
      Newvals$pred_plant <- predict(smooth_plant, Newvals$Time)$y
      
      
      # Convert to time series object
      bird_ts <- ts(Newvals$pred_bird, frequency = 1)
      plant_ts <- ts(Newvals$pred_plant, frequency = 1)
      
      adf.test(bird_ts) 
      adf.test(plant_ts) 
      
      
      # Vectors to store p-values
      p_values_plant <- numeric(5)
      p_values_bird <- numeric(5)
      
      # Run the Granger causality test for different lags
      
      for (i in 1:5) {
        tryCatch({ # Allow #simple - acceptable Errors to pass through :D with the error massage and NA as p-value 
          BIRD_Test_result <- lmtest::grangertest(plant_ts, bird_ts, order = i, test = "F")
          PLANT_Test_result <- lmtest::grangertest(bird_ts, plant_ts, order = i, test = "F")
          
          p_values_plant[i] <- PLANT_Test_result$`Pr(>F)`[2]
          p_values_bird[i] <- BIRD_Test_result$`Pr(>F)`[2]
        }, error = function(e) {
          message("Error in Granger test for order ", i, ": ", e$message)
          p_values_plant[i] <- NA
          p_values_bird[i] <- NA
        })
      }
      
      # Dynamic name for the column
      plant_name <- sub(".*/PSMC/([^/]+)/.*", "\\1", HelBih)
      param_name <- paste0(plant_name, "_mu", mu, "_g", g)
      
      # Combine all results into one row
      Result <- as.data.frame(t(c(param_name, p_values_bird, p_values_plant)))
      
      # Accumulate results
      All_Results <- rbind(All_Results, Result)
      
    }
  }
  
  return(All_Results)
}



#AAA Alternative granger 

NEW_Granger_psmc_diff <- function(bird, plant, plant_mu, bird_mu, bird_g, plant_gen, max_bird) {
  library(lmtest)
  library(imputeTS)
  library(dplyr)
  library(tseries)
  
  # Initialize an empty data frame to accumulate results
  All_Results <- data.frame()
  
  # Run PSMC for bird
  psmcbird <- psmc.result(EulJugA, i.iteration=25, mu=bird_mu, s=100, g=bird_g)
  subset_psmcbird <- psmcbird[-(1:12), ]
  #fit a line (down-stream)
  smooth_bird <- smooth.spline(subset_psmcbird)
  
  # Run PSMC for plant with dynamic mu and g
  psmcplant <- psmc.result(plant, i.iteration=25, mu=8e-8, s=100, g=4)
  subset_psmcplant <- psmcplant[-(1:12), ]
  #fit a line (down-stream)
  smooth_plant <- smooth.spline(subset_psmcplant)
  
  for (mu in plant_mu) {
    for (g in plant_gen) {
      
      # Run PSMC for plant with dynamic mu and g
      psmcplant <- psmc.result(plant, i.iteration=25, mu=8e-8, s=100, g=4)
      subset_psmcplant <- psmcplant[-(1:12), ]
      #fit a line (down-stream)
      smooth_plant <- smooth.spline(subset_psmcplant)
      
      # Get filtering stats
      min_plant <- min(subset_psmcplant$Time)
      max_bird <- max(subset_psmcbird$Time)
      
      # Merge the two datasets
      merged_df <- merge(subset_psmcbird, subset_psmcplant, by = "Time",
                         suffixes = c("_bird", "_plant"), all = TRUE)
      
      # Filter merged dataframe
      cut_merged_df <- merged_df %>%
        filter(Time > (min_plant-1) & Time < max_bird)
      
      ####Predicting 
      Newvals <- data.frame(Time=seq(min_plant, max(max_bird, na.rm=T), 1000))
      Newvals$pred_bird <- predict(smooth_bird, Newvals$Time)$y
      Newvals$pred_plant <- predict(smooth_plant, Newvals$Time)$y
      
      
      # Convert to time series object
      bird_ts <- ts(Newvals$pred_bird, frequency = 1)
      plant_ts <- ts(Newvals$pred_plant, frequency = 1)
      
      # Apply first differencing for stationarity
      bird_ts_diff <- diff(bird_ts, differences = 1)
      plant_ts_diff <- diff(plant_ts, differences = 1)
      
      adf.test(bird_ts_diff) 
      adf.test(plant_ts_diff) 
      
      
      # Vectors to store p-values
      p_values_plant <- numeric(5)
      p_values_bird <- numeric(5)
      
      # Run the Granger causality test for different lags
      
      for (i in 1:5) {
        tryCatch({ # Allow #simple - acceptable Errors to pass through :D with the error massage and NA as p-value 
          BIRD_Test_result <- lmtest::grangertest(plant_ts_diff, bird_ts_diff, order = i, test = "F")
          PLANT_Test_result <- lmtest::grangertest(bird_ts_diff, plant_ts_diff, order = i, test = "F")
          
          p_values_plant[i] <- PLANT_Test_result$`Pr(>F)`[2]
          p_values_bird[i] <- BIRD_Test_result$`Pr(>F)`[2]
        }, error = function(e) {
          message("Error in Granger test for order ", i, ": ", e$message)
          p_values_plant[i] <- NA
          p_values_bird[i] <- NA
        })
      }
      
      # Dynamic name for the column
      plant_name <- sub(".*/PSMC/([^/]+)/.*", "\\1", plant)
      param_name <- paste0(plant_name, "_mu", mu, "_g", g)
      
      # Combine all results into one row
      Result <- as.data.frame(t(c(param_name, p_values_bird, p_values_plant)))
      
      # Accumulate results
      All_Results <- rbind(All_Results, Result)
      
    }
  }
  return(All_Results)
}





NEW_Granger_psmc_diff()


EU_HELCAR <- NEW_Granger_psmc(EulJugA, HelCar, bird_mu=6.8e-8, plant_mu, plant_gen, bird_g=2.3)
EU_HELBIH <- NEW_Granger_psmc(EulJugA, HelBih, bird_mu=6.8e-8, plant_mu, plant_gen, bird_g=2.3)

EU_HELCAR_diff <- NEW_Granger_psmc_diff(EulJugA, HelCar, bird_mu=6.8e-8, plant_mu, plant_gen, bird_g=2.3)
EU_HELBih_diff <- NEW_Granger_psmc_diff(EulJugA, HelBih, bird_mu=6.8e-8, plant_mu, plant_gen, bird_g=2.3)


V2_EU_HELBIH <- create_matrix(EU_HELBIH, "V2", "H.C effect on E.jugularis, Order 1")
V3_EU_HELBIH <- create_matrix(EU_HELBIH, "V3", "H.C effect on E.jugularis, Order 2")
V4_EU_HELBIH <- create_matrix(EU_HELBIH, "V4", "H.C effect on E.jugularis, Order 3")
V5_EU_HELBIH <- create_matrix(EU_HELBIH, "V5", "H.C effect on E.jugularis, Order 4")
V6_EU_HELBIH <- create_matrix(EU_HELBIH, "V6", "H.C effect on E.jugularis, Order 5")
V7_EU_HELBIH <- create_matrix(EU_HELBIH, "V7", "E.jugularis effect on H.C, Order 1")
V8_EU_HELBIH <- create_matrix(EU_HELBIH, "V8", "E.jugularis effect on H.C, Order 2")
V9_EU_HELBIH <- create_matrix(EU_HELBIH, "V9", "E.jugularis effect on H.C, Order 3")
V10_EU_HELBIH <- create_matrix(EU_HELBIH, "V10", "E.jugularis effect on H.C, Order 4")
V11_EU_HELBIH <- create_matrix(EU_HELBIH, "V11", "E.jugularis effect on H.C, Order 5")




V2_EU_HELBIH <- create_matrix(EU_HELBIH_diff, "V2", "H.B effect on E.Jugularis, Order 1")
V3_EU_HELBIH <- create_matrix(EU_HELBIH_diff, "V3", "H.B effect on E.Jugularis, Order 2")
V4_EU_HELBIH <- create_matrix(EU_HELBIH_diff, "V4", "H.B effect on E.Jugularis, Order 3")
V5_EU_HELBIH <- create_matrix(EU_HELBIH_diff, "V5", "H.B effect on E.Jugularis, Order 4")
V6_EU_HELBIH <- create_matrix(EU_HELBIH_diff, "V6", "H.B effect on E.Jugularis, Order 5")
V7_EU_HELBIH <- create_matrix(EU_HELBIH_diff, "V7", "E.jugularis effect on H.B, Order 1")
V8_EU_HELBIH <- create_matrix(EU_HELBIH_diff, "V8", "E.jugularis effect on H.B, Order 2")
V9_EU_HELBIH <- create_matrix(EU_HELBIH_diff, "V9", "E.jugularis effect on H.B, Order 3")
V10_EU_HELBIH <- create_matrix(EU_HELBIH_diff, "V10", "E.Jugularis effect on H.B, Order 4")
V11_EU_HELBIH <- create_matrix(EU_HELBIH_diff, "V11", "E.Jugularis effect on H.B, Order 5")


V2_EU_HELCAR <- create_matrix(EU_HELCAR_diff, "V2", "H.C effect on E.Jugularis, Order 1")
V3_EU_HELCAR <- create_matrix(EU_HELCAR_diff, "V3", "H.C effect on E.Jugularis, Order 2")
V4_EU_HELCAR <- create_matrix(EU_HELCAR_diff, "V4", "H.C effect on E.Jugularis, Order 3")
V5_EU_HELCAR <- create_matrix(EU_HELCAR_diff, "V5", "H.C effect on E.Jugularis, Order 4")
V6_EU_HELCAR <- create_matrix(EU_HELCAR_diff, "V6", "H.C effect on E.Jugularis, Order 5")
V7_EU_HELCAR <- create_matrix(EU_HELCAR_diff, "V7", "E.jugularis effect on H.C, Order 1")
V8_EU_HELCAR <- create_matrix(EU_HELCAR_diff, "V8", "E.jugularis effect on H.C, Order 2")
V9_EU_HELCAR <- create_matrix(EU_HELCAR_diff, "V9", "E.jugularis effect on H.C, Order 3")
V10_EU_HELCAR <- create_matrix(EU_HELCAR_diff, "V10", "E.jugularis effect on H.C, Order 4")
V11_EU_HELCAR <- create_matrix(EU_HELCAR_diff, "V11", "E.jugularis effect on H.C, Order 5")























#complex: 

library(vars)
combined_ts <- cbind(Bird = bird_ts, Plant = plant_ts)

lag_selection <- VARselect(combined_ts, lag.max = 10, type = "const")
optimal_lag <- lag_selection$selection["AIC(n)"]

# Fit VAR model
var_model <- VAR(combined_ts, p = optimal_lag, type = "const")
summary(var_model)


library(lmtest)
# Granger causality: Does Bird population predict Plant population?
causality_test1 <- causality(var_model, cause = "Bird")
print(causality_test1)

# Granger causality: Does Plant population predict Bird population?
causality_test2 <- causality(var_model, cause = "Plant")
print(causality_test2)

#complex: on the diff data


library(vars)
combined_ts <- cbind(Bird = bird_ts_diff, Plant = plant_ts_diff)

lag_selection <- VARselect(combined_ts, lag.max = 10, type = "const")
optimal_lag <- lag_selection$selection["AIC(n)"]

# Fit VAR model
var_model <- VAR(combined_ts, p = optimal_lag, type = "const")
summary(var_model)


library(lmtest)
# Granger causality: Does Bird population predict Plant population?
causality_test1 <- causality(var_model, cause = "Bird")
print(causality_test1)

# Granger causality: Does Plant population predict Bird population?
causality_test2 <- causality(var_model, cause = "Plant")
print(causality_test2)



























####Give the paths to The dir where PSMC is stored 


EulJugA <- dir("C:/Users/au601132/OneDrive - Aarhus universitet/Skrivebord/PhD/Kolibri_heliconia/PSMC/Auto/","psmc$", full.names=T) # main PSMC results
HelBih <- dir("C:/Users/au601132/OneDrive - Aarhus universitet/Skrivebord/PhD/Kolibri_heliconia/PSMC/HBih/","psmc$", full.names=T) # main PSMC results
HelCar <- dir("C:/Users/au601132/OneDrive - Aarhus universitet/Skrivebord/PhD/Kolibri_heliconia/PSMC/HCar/","psmc$", full.names=T) # main PSMC results
EulHol <- dir("C:/Users/au601132/OneDrive - Aarhus universitet/Skrivebord/PhD/Kolibri_heliconia/PSMC/Ehol/","psmc$", full.names=T) # main PSMC results



# # Input the timeintervals for scaleing. 
# #Obs this will be used for any run heron out (unless a new )
# newvals <- data.frame(Time=seq(5000, max(x[,1], na.rm=T), 3000))
# 




plant_mu = c(4.5e-9, 
             5e-9,
             5.5e-9,
             6e-9,
             6.5e-9,
             7e-9,
             7.5e-9,
             8e-9,
             8.5e-9,
             9e-9,
             9.5e-9,
             1e-8,
             1.5e-8,
             2e-8,
             2.5e-8,
             3e-8,
             3.5e-8,
             4e-8, # "esimated"  
             4.5e-8,
             5e-8,
             5.5e-8,
             6e-8,
             6.5e-8,
             7e-8,
             7.5e-8,
             8e-8,
             8.5e-8,
             9e-8,
             9.5e-8,
             1e-7)

plant_gen = c(3, 
              5, 
              7, 
              9, 
              11, 
              13, 
              15, # "15-18 H. Bihai and H. caribea" 
              17, 
              19, 
              21,
              23,
              25) # tilføj alle 3-25





####HELCAR


#Trying on one set one variable paramester space. 

Trial <- comparative_psmc(EulJugA, HelCar, bird_mu=6.8e-8, plant_mu, plant_gen, bird_g=2.3)

# Create an empty data frame to store the comparisons
comparison_df <- data.frame(matrix(ncol = ncol(Trial), nrow = nrow(Trial)))
colnames(comparison_df) <- colnames(Trial)

# Loop through all columns to apply the comparison
for (i in 1:nrow(Trial)) {
  for (j in 1:ncol(Trial)) {
    if (is.na(Trial$Bird_trend_comparison[i]) && is.na(Trial[i, j])) {
      comparison_df[i, j] <- "NA"
    } else if (is.na(Trial$Bird_trend_comparison[i]) || is.na(Trial[i, j])) {
      comparison_df[i, j] <- "*"
    } else if (Trial$Bird_trend_comparison[i] == Trial[i, j]) {
      comparison_df[i, j] <- "+"
    } else {
      comparison_df[i, j] <- "-"
    }
  }
}

# View the new comparison dataframe
head(comparison_df)


# Create an empty data frame to store the results
ratio_limEU_HelCar <- data.frame(
  Comparison = character(ncol(comparison_df)),  # Adjusted size
  Ratio = numeric(ncol(comparison_df)),        # Adjusted size
  stringsAsFactors = FALSE
)



for (i in 1:ncol(comparison_df)) {
  # Extract the current column's comparison results
  comparison_col <- comparison_df[[i]]
  
  # Count the number of "+" and "*"
  num_plus <- sum(comparison_col == "+")
  num_star <- sum(comparison_col == "*")
  num_NA  <- sum(comparison_col == "NA")
  num_minus <- sum(comparison_col == "-")
  
  # Calculate the ratio of + / (11576 - *)
  ratio <- num_plus / 98 
  
  # Store the column name and calculated ratio in the correct rows
  ratio_limEU_HelCar[i, "Comparison"] <- colnames(comparison_df)[i]
  ratio_limEU_HelCar[i, "Ratio"] <- ratio
  ratio_limEU_HelCar[i, "overlap"] <- 98 - (num_star + num_NA)
}






####HELBih

#Trying on chapped paramester space. 

Trial <- comparative_psmc(EulJugA, HelBih, bird_mu=6.8e-8, plant_mu, plant_gen, bird_g=2.3)


# Create an empty data frame to store the comparisons
comparison_df <- data.frame(matrix(ncol = ncol(Trial), nrow = nrow(Trial)))
colnames(comparison_df) <- colnames(Trial)

# Loop through all columns to apply the comparison
for (i in 1:nrow(Trial)) {
  for (j in 1:ncol(Trial)) {
    if (is.na(Trial$Bird_trend_comparison[i]) && is.na(Trial[i, j])) {
      comparison_df[i, j] <- "NA"
    } else if (is.na(Trial$Bird_trend_comparison[i]) || is.na(Trial[i, j])) {
      comparison_df[i, j] <- "*"
    } else if (Trial$Bird_trend_comparison[i] == Trial[i, j]) {
      comparison_df[i, j] <- "+"
    } else {
      comparison_df[i, j] <- "-"
    }
  }
}

# View the new comparison dataframe
head(comparison_df)


# Create an empty data frame to store the results
ratio_limEU_HelBih <- data.frame(
  Comparison = character(ncol(comparison_df)),  # Adjusted size
  Ratio = numeric(ncol(comparison_df)),        # Adjusted size
  stringsAsFactors = FALSE
)


for (i in 1:ncol(comparison_df)) {
  # Extract the current column's comparison results
  comparison_col <- comparison_df[[i]]
  
  # Count the number of "+" and "*"
  num_plus <- sum(comparison_col == "+")
  num_star <- sum(comparison_col == "*")
  num_NA  <- sum(comparison_col == "NA")
  num_minus <- sum(comparison_col == "-")
  
  # Calculate the ratio of + / (11576 - *)
  ratio <- num_plus / 98
  
  # Store the column name and calculated ratio in the correct rows
  ratio_limEU_HelBih[i, "Comparison"] <- colnames(comparison_df)[i]
  ratio_limEU_HelBih[i, "Ratio"] <- ratio
  ratio_limEU_HelBih[i, "overlap"] <- 98 - (num_star + num_NA)
}


#Plot these 

library(gridExtra)

library(ggplot2)

library(dbplyr)

plotHC <-ggplot(ratio_limEU_HelCar, aes(x=overlap, y=Ratio)) +
  geom_point(shape=1) +
  labs(x="Overlap", y="Ratio", title="Overlap vs Ratio - H. Car") +
  ylim(0, 1) +
  theme_minimal()

plotHB <- ggplot(ratio_limEU_HelBih, aes(x=overlap, y=Ratio)) +
  geom_point(shape=1) +
  labs(x="Overlap", y="Ratio", title="Overlap vs Ratio - H. Bih") +
  ylim(0, 1) +
  theme_minimal()

grid.arrange(plotHC, plotHB, nrow=2)






## 
#Calculate most frequent parameters: 

head(ratio_limEU_HelBih)


library(dplyr)
library(stringr)

# Filter out the unwanted rows
Transformed__HelBih_ratio <- ratio_limEU_HelBih %>%
  filter(Comparison != "Bird_trend_comparison") %>%
  # Extract mu value using regex
  mutate(mu = str_extract(Comparison, "(?<=mu)[0-9eE.-]+") %>% as.numeric(),
         # Extract g value using regex
         g = str_extract(Comparison, "(?<=g)[0-9]+") %>% as.numeric())

# View the cleaned data
head(Transformed__HelCar_ratio)


# Filter out the unwanted rows
Transformed__HelBih_ratio_clean <- Transformed__HelBih_ratio %>%
  filter(Ratio > 0.50)


table(Transformed__HelBih_ratio_clean$g)

table(Transformed__HelBih_ratio_clean$mu)



## 
#Calculate most frequent parameters: 




# Filter out the unwanted rows
Transformed__HelCar_ratio <- ratio_limEU_HelCar %>%
  filter(Comparison != "Bird_trend_comparison") %>%
  # Extract mu value using regex
  mutate(mu = str_extract(Comparison, "(?<=mu)[0-9eE.-]+") %>% as.numeric(),
         # Extract g value using regex
         g = str_extract(Comparison, "(?<=g)[0-9]+") %>% as.numeric())

# View the cleaned data
head(Transformed__HelCar_ratio)


# Filter out the unwanted rows
Transformed__HelCar_ratio_clean <- Transformed__HelCar_ratio %>%
  filter(Transformed__HelCar_ratio$Ratio > 0.50)


##Compar

table(Transformed__HelBih_ratio_clean$g)

table(Transformed__HelBih_ratio_clean$mu)


table(Transformed__HelCar_ratio_clean$g)

table(Transformed__HelCar_ratio_clean$mu)


df <- table(Transformed__HelCar_ratio_clean$mu)
df_plot <- as.data.frame(df)
colnames(df_plot) <- c("mu", "Frequency")
# Create the bar plot using ggplot2
mu_CAR <- ggplot(df_plot, aes(x = mu, y = Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Frequency of mu Values for H. Car", x = "mu", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  +
  ylim(0,19)#  # 



df <- table(Transformed__HelCar_ratio_clean$g)
df_plot <- as.data.frame(df)
colnames(df_plot) <- c("mu", "Frequency")
# Create the bar plot using ggplot2
g_CAR <-ggplot(df_plot, aes(x = mu, y = Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Frequency of g Values for H. Car", x = "g", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  +
  ylim(0,19)#   # 



df <- table(Transformed__HelBih_ratio_clean$g)
df_plot <- as.data.frame(df)
colnames(df_plot) <- c("mu", "Frequency")
# Create the bar plot using ggplot2
g_BIH <-ggplot(df_plot, aes(x = mu, y = Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Frequency of g Values for H. Bih", x = "g", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0,19)# 


df <- table(Transformed__HelBih_ratio_clean$mu)
df_plot <- as.data.frame(df)
colnames(df_plot) <- c("mu", "Frequency")
# Create the bar plot using ggplot2
mu_BIH <-ggplot(df_plot, aes(x = mu, y = Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Frequency of mu Values H. Bih", x = "mu", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0,19)#  # 


grid.arrange(g_CAR, g_BIH, nrow=2)

grid.arrange(mu_CAR, mu_BIH, nrow=2)








########    EULHOL
###
##
# Tryning on a different kolibri Ehol


EulHol <- dir("C:/Users/au601132/OneDrive - Aarhus universitet/Skrivebord/PhD/Kolibri_heliconia/PSMC/Ehol/","psmc$", full.names=T) # main PSMC results



Trial <- comparative_psmc(EulHol, HelBih, bird_mu=6.8e-8, plant_mu, plant_gen, bird_g=2.3)


# Create an empty data frame to store the comparisons
comparison_df <- data.frame(matrix(ncol = ncol(Trial), nrow = nrow(Trial)))
colnames(comparison_df) <- colnames(Trial)

# Loop through all columns to apply the comparison
for (i in 1:nrow(Trial)) {
  for (j in 1:ncol(Trial)) {
    if (is.na(Trial$Bird_trend_comparison[i]) && is.na(Trial[i, j])) {
      comparison_df[i, j] <- "NA"
    } else if (is.na(Trial$Bird_trend_comparison[i]) || is.na(Trial[i, j])) {
      comparison_df[i, j] <- "*"
    } else if (Trial$Bird_trend_comparison[i] == Trial[i, j]) {
      comparison_df[i, j] <- "+"
    } else {
      comparison_df[i, j] <- "-"
    }
  }
}

# View the new comparison dataframe
head(comparison_df)


# Create an empty data frame to store the results
ratio_limEulHol_HelBih <- data.frame(
  Comparison = character(ncol(comparison_df)),  # Adjusted size
  Ratio = numeric(ncol(comparison_df)),        # Adjusted size
  stringsAsFactors = FALSE
)


for (i in 1:ncol(comparison_df)) {
  # Extract the current column's comparison results
  comparison_col <- comparison_df[[i]]
  
  # Count the number of "+" and "*"
  num_plus <- sum(comparison_col == "+")
  num_star <- sum(comparison_col == "*")
  num_NA  <- sum(comparison_col == "NA")
  num_minus <- sum(comparison_col == "-")
  
  # Calculate the ratio of + / (11576 - *)
  ratio <- num_plus / (98 - num_star)
  
  # Store the column name and calculated ratio in the correct rows
  ratio_limEulHol_HelBih[i, "Comparison"] <- colnames(comparison_df)[i]
  ratio_limEulHol_HelBih[i, "Ratio"] <- ratio
  ratio_limEulHol_HelBih[i, "overlap"] <- 98
}


#Plot these 

library(gridExtra)

library(ggplot2)

library(dbplyr)

plotHCEulHol <-ggplot(ratio_limEulHol_HelCar, aes(x=overlap, y=Ratio)) +
  geom_point(shape=1) +
  labs(x="Overlap", y="Ratio", title="Overlap vs Ratio - H. Car") +
  ylim(0, 1) +
  theme_minimal()

plotHBEulHol <- ggplot(ratio_limEulHol_HelBih, aes(x=overlap, y=Ratio)) +
  geom_point(shape=1) +
  labs(x="Overlap", y="Ratio", title="Overlap vs Ratio - H. Bih") +
  ylim(0, 1) +
  theme_minimal()

grid.arrange(plotHCEulHol, plotHBEulHol, nrow=2)






## 
#Calculate most frequent parameters: 

head(ratio_limEulHol_HelBih)


library(dplyr)
library(stringr)

# Filter out the unwanted rows
Transformed__HelBih_ratio <- ratio_limEulHol_HelBih %>%
  filter(Comparison != "Bird_trend_comparison") %>%
  # Extract mu value using regex
  mutate(mu = str_extract(Comparison, "(?<=mu)[0-9eE.-]+") %>% as.numeric(),
         # Extract g value using regex
         g = str_extract(Comparison, "(?<=g)[0-9]+") %>% as.numeric())

# View the cleaned data
head(Transformed__HelBih_ratio)


# Filter out the unwanted rows
Transformed__HelBih_ratio_clean <- Transformed__HelBih_ratio %>%
  filter(overlap > 50)


table(Transformed__HelBih_ratio_clean$g)

table(Transformed__HelBih_ratio_clean$mu)



## 
#Calculate most frequent parameters: 

head(ratio_limEulHol_HelCar)



# Filter out the unwanted rows
Transformed__HelCar_ratio <- ratio_limEulHol_HelCar %>%
  filter(Comparison != "Bird_trend_comparison") %>%
  # Extract mu value using regex
  mutate(mu = str_extract(Comparison, "(?<=mu)[0-9eE.-]+") %>% as.numeric(),
         # Extract g value using regex
         g = str_extract(Comparison, "(?<=g)[0-9]+") %>% as.numeric())

# View the cleaned data
head(Transformed__HelCar_ratio)


# Filter out the unwanted rows
Transformed__HelCar_ratio_clean <- Transformed__HelCar_ratio %>%
  filter(overlap > 50)


##Compar

table(Transformed__HelBih_ratio_clean$g)

table(Transformed__HelBih_ratio_clean$mu)


table(Transformed__HelCar_ratio_clean$g)

table(Transformed__HelCar_ratio_clean$mu)


df <- table(Transformed__HelCar_ratio_clean$mu)
df_plot <- as.data.frame(df)
colnames(df_plot) <- c("mu", "Frequency")
# Create the bar plot using ggplot2
mu_CAR <- ggplot(df_plot, aes(x = mu, y = Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Frequency of mu Values for H. Car", x = "mu", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  +
  ylim(0,19)#  # 



df <- table(Transformed__HelCar_ratio_clean$g)
df_plot <- as.data.frame(df)
colnames(df_plot) <- c("mu", "Frequency")
# Create the bar plot using ggplot2
g_CAR <-ggplot(df_plot, aes(x = mu, y = Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Frequency of g Values for H. Car", x = "g", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  +
  ylim(0,19)#   # 



df <- table(Transformed__HelBih_ratio_clean$g)
df_plot <- as.data.frame(df)
colnames(df_plot) <- c("mu", "Frequency")
# Create the bar plot using ggplot2
g_BIH <-ggplot(df_plot, aes(x = mu, y = Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Frequency of g Values for H. Bih", x = "g", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0,19)# 


df <- table(Transformed__HelBih_ratio_clean$mu)
df_plot <- as.data.frame(df)
colnames(df_plot) <- c("mu", "Frequency")
# Create the bar plot using ggplot2
mu_BIH <-ggplot(df_plot, aes(x = mu, y = Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Frequency of mu Values H. Bih", x = "mu", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0,19)#  # 


grid.arrange(g_CAR, g_BIH, nrow=2)

grid.arrange(mu_CAR, mu_BIH, nrow=2)







### List the parameter space for the set of organisms

#Heliconia parameteres 
###OBS these are to be used for both species. 
H_mu = c(7.5e-7,
         8e-7,
         8.5e-7,
         9e-7,
         9.5e-7,
         1e-8,
         1.5e-8,
         2e-8,
         2.5e-8,
         3e-8,
         3.5e-8,
         4e-8, # "esimated"  
         4.5e-8,
         5e-8,
         5.5e-8,
         6e-8,
         6.5e-8)

H_gen = c(3, 
          5, 
          7, 
          9, 
          11, 
          13, 
          15, # "15-18 H. Bihai and H. caribea" 
          17, 
          19, 
          21,
          23,
          25)

#Eulampis jugularis parameters 
Eu_mu = c(4.8e-8,
          5.3e-8,
          5.8e-8,
          6.3e-8,
          6.8e-8, # "estimated"
          7.3e-8,
          7.8e-8,
          8.3e-8,
          8.8e-8,
          9.3e-8,
          9.8e-8,
          1.3e-9,
          1.8e-9)

Eu_gen = c(2.1,
           2.3, # " estimated" 
           2.5,
           2.7,
           2.9,
           3.1,
           3.3,
           3.5,
           3.7,
           3.9,
           4.1,
           4.3,
           4.5,
           4.7,
           4.9,
           5.1)





######################################RUNNING functions
##  run as loops through the parameter-space



# Running EulJugA with the defined parameter space

for (mu in Eu_mu) {
  for (g in Eu_gen) {
    assign(paste0("EulJugA", "_mu", mu, "_g", g), # because of for loop :D 
           temporal_change_psmc(EulJugA, mu, g))
  }
}


### combine the parameter space vectors 

# Find the maximum length across all vectors
max_len <- max(sapply(mget(ls(pattern = "^EulJugA_mu[0-9\\.e\\-]+_g[0-9\\.]+$")), function(x) length(x$comparison)))


result_df_EulJugA <- do.call(cbind, 
                             lapply(mget(ls(pattern = "^EulJugA_mu[0-9\\.e\\-]+_g[0-9\\.]+$")), 
                                    function(x) {
                                      vec <- x$comparison  # Extract comparison vector
                                      length(vec) <- max_len  # Pad with NA
                                      return(vec)
                                    }))

# Convert to data frame
result_df_EulJugA <- as.data.frame(result_df_EulJugA)


#getting the orginal Time max and min 
# input is path, mutation rates (list), & generations time (list)

TIME_EulJugA <- temporal_points_psmc(EulJugA, Eu_mu, Eu_gen) 
#Input Spp
TIME_EulJugA$Spp <- "EulJugA"







#################################################
# Running HelBih with the defined parameter space
for (mu in H_mu) {
  for (g in H_gen) {
    assign(paste0("HelBih", "_mu", mu, "_g", g), # because of for loop :D 
           temporal_change_psmc(HelBih, mu, g))
  }
}

### combine the parameter space vectors 

# Find the maximum length across all vectors
max_len <- max(sapply(mget(ls(pattern = "^HelBih_mu[0-9\\.e\\-]+_g[0-9]+$")), function(x) length(x$comparison)))

# collum bind to df, only comparison! 
result_df_HelBih <- do.call(cbind, 
                             lapply(mget(ls(pattern = "^HelBih_mu[0-9\\.e\\-]+_g[0-9]+$")), 
                                    function(x) {
                                      vec <- x$comparison  # Extract comparison vector
                                      length(vec) <- max_len  # Pad with NA
                                      return(vec)
                                    }))


#getting the orginal Time max and min 
# input is path, mutation rates (list), & generations time (list)

TIME_HelBih <- temporal_points_psmc(HelBih, H_mu, H_gen) 
#Input Spp
TIME_HelBih$Spp <- "HelBih"






##################################################
# Running HelCar with the defined parameter space
for (mu in H_mu) {
  for (g in H_gen) {
    assign(paste0("HelCar", "_mu", mu, "_g", g),  # because of for loop :D 
           temporal_change_psmc(HelCar, mu, g, newvals))
  }
}


### combine the parameter space vectors 
# Find the maximum length across all vectors
max_len <- max(sapply(mget(ls(pattern = "^HelCar_mu[0-9\\.e\\-]+_g[0-9]+$")), function(x) length(x$comparison)))

result_df_HelCar <- do.call(cbind, 
                            lapply(mget(ls(pattern = "^HelCar_mu[0-9\\.e\\-]+_g[0-9]+$")), 
                                   function(x) {
                                     vec <- x$comparison  # Extract comparison vector
                                     length(vec) <- max_len  # Pad with NA
                                     return(vec)
                                   }))

#getting the orginal Time max and min 
# input is path, mutation rates (list), & generations time (list)
TIME_HelCar <- temporal_points_psmc(HelCar, H_mu, H_gen) 
#Input Spp
TIME_HelCar$Spp <- "HelCar"






#We now have 3 data frames of delta Ne between time points of 3000 years. 


#Next the EulJugA spp will be compared to the plant species HelCar.
#if both show a decrease, increase, & stable trend = +,
#if one increase the other decrease input = -
#if one is NA the other is a number input = *
#if both is NA input = NA

#The resulting data frame: HeCar_EulJugA
#81×81=6561 comparision
#Input data.frames
#result_df_EulJugA
#result_df_HelCar

table(result_df_EulJugA$`EulJugA_mu1.3e-09_g2.1`)
dim(result_df_EulJugA)

#The no of rows [x,] = the now of temporal (3000 year) comparisons. 

table(result_df_HelCar)
dim(result_df_HelCar)



# loop through each row from 1 to 194, then for each pair of columns (81 columns in both dataframes)
# Column Naming:  combining the column names from both dataframes (result_df_EulJugA and result_df_HelCar) using an underscore _ separator.
# Comparison Logic: Based on the values in the two columns based on the chr of following conditions ar in put:
#   If both are 1, both are -1, or both are 0: Output +.
# If one is 1 and the other is -1, output -.
# If one is 0 and the other is 1 or -1, output -.
# If one is a number and the other is NA, output *.
# If both are NA, output NA.
# Assuming result_df_EulJugA and result_df_HelCar are already loaded

# Subset to to the max no of rows!
subset_result_df_EulJugA <- result_df_EulJugA[1:237, ]
subset_result_df_HelCar <- result_df_HelCar[1:237, ]

# Get column names of both data frames
columns_EulJugA <- colnames(result_df_EulJugA)
columns_HelCar <- colnames(result_df_HelCar)

# Create an empty list to store the results
result_list_E_HC <- list()

# Iterate over each pair of columns from both data frames
for (col_EulJugA in columns_EulJugA) {
  for (col_HelCar in columns_HelCar) {
    
    # Create an empty vector to store comparison results for the current column pair
    comparison <- character(237)
    
    # Perform the comparison for each row
    for (i in 1:237) {
      value_EulJugA <- subset_result_df_EulJugA[i, col_EulJugA]
      value_HelCar <- subset_result_df_HelCar[i, col_HelCar]
      
      # Comparison conditionss
      if (is.na(value_EulJugA) && is.na(value_HelCar)) {
        comparison[i] <- "NA"
      } else if (is.na(value_EulJugA) || is.na(value_HelCar)) {
        comparison[i] <- "*"
      } else if (value_EulJugA == value_HelCar) {
        comparison[i] <- "+"
      } else {
        comparison[i] <- "-"
      }
    }
    
    # Store the comparison result with a new column name as "col_EulJugA_col_HelCar"
    result_list_E_HC[[paste(col_EulJugA, col_HelCar, sep = "_")]] <- comparison
  }
}


# Convert the result list into a data frame
RESULT_df_E_HC <- as.data.frame(result_list_E_HC)

dim(RESULT_df_E_HC)




# Create an empty data frame to store the results
RESULT_ratio_df_E_HC <- data.frame(
  Comparison = character(ncol(RESULT_df_E_HC)),  # Adjusted size
  Ratio = numeric(ncol(RESULT_df_E_HC)),        # Adjusted size
  stringsAsFactors = FALSE
)



for (i in 1:ncol(RESULT_df_E_HC)) {
  # Extract the current column's comparison results
  comparison_col <- RESULT_df_E_HC[[i]]
  
  # Count the number of "+" and "*"
  num_plus <- sum(comparison_col == "+")
  num_star <- sum(comparison_col == "*")
  
  # Calculate the ratio of + / (11576 - *)
  ratio <- num_plus / (237 - num_star)
  
  # Store the column name and calculated ratio in the correct rows
  RESULT_ratio_df_E_HC[i, "Comparison"] <- colnames(RESULT_df_E_HC)[i]
  RESULT_ratio_df_E_HC[i, "Ratio"] <- ratio
  RESULT_ratio_df_E_HC[i, "overlap"] <- num_plus
}

# View the resulting dataframe
head(RESULT_df_E_HC)




###


# Subset to to the max no of rows!

subset_result_df_HelBih <- result_df_HelBih[1:11576, ]

# Get column names of both data frames
columns_HelBih <- colnames(result_df_HelBih)

# Create an empty list to store the results
result_list_E_HB <- list()

# Iterate over each pair of columns from both data frames
for (col_EulJugA in columns_EulJugA) {
  for (col_HelBih in columns_HelBih) {
    
    # Create an empty vector to store comparison results for the current column pair
    comparison <- character(11576)
    
    # Perform the comparison for each row
    for (i in 1:11576) {
      value_EulJugA <- subset_result_df_EulJugA[i, col_EulJugA]
      value_HelBih <- subset_result_df_HelBih[i, col_HelBih]
      
      # Comparison conditionss
      if (is.na(value_EulJugA) && is.na(value_HelBih)) {
        comparison[i] <- "NA"
      } else if (is.na(value_EulJugA) || is.na(value_HelBih)) {
        comparison[i] <- "*"
      } else if (value_EulJugA == value_HelBih) {
        comparison[i] <- "+"
      } else {
        comparison[i] <- "-"
      }
    }
    
    # Store the comparison result with a new column name as "col_EulJugA_col_HelCar"
    result_list_E_HB[[paste(col_EulJugA, col_HelBih, sep = "_")]] <- comparison
  }
}

# Convert the result list into a data frame
result_df_E_HB <- as.data.frame(result_list_E_HB)

dim(result_df_E_HB)



# Create an empty data frame to store the results
result_ratio_df_E_HC <- data.frame(
  Comparison = character(ncol(result_df_E_HC)),  # Adjusted size
  Ratio = numeric(ncol(result_df_E_HC)),        # Adjusted size
  stringsAsFactors = FALSE
)


for (i in 1:ncol(result_df_E_HC)) {
  # Extract the current column's comparison results
  comparison_col <- result_df_E_HC[[i]]
  
  # Count the number of "+" and "*"
  num_plus <- sum(comparison_col == "+")
  num_star <- sum(comparison_col == "*")
  
  # Calculate the ratio of + / (11576 - *)
  ratio <- num_plus / (11576 - num_star)
  
  # Store the column name and calculated ratio in the correct rows
  result_ratio_df_E_HC[i, "Comparison"] <- colnames(result_df_E_HC)[i]
  result_ratio_df_E_HC[i, "Ratio"] <- ratio
  result_ratio_df_E_HC[i, "overlap"] <- num_plus
}

# View the resulting dataframe
head(result_df_E_HC)










############
#All of the above has been run on a cluster - as there are aproxx. 42500 comparisions pr plant species.. 
# I import  ratio outputs and and plot them 

setwd("C:/Users/au601132/OneDrive - Aarhus universitet/Skrivebord/PhD/Kolibri_heliconia/PSMC")

Eu_HC_RATIO <- read.csv("Resultsresult_ratio_df_EulJug_HelCar.txt", sep = '\t')

Eu_HB_RATIO <- read.csv("Resultsresult_ratio_df_EulJug_HelBih.txt", sep = '\t')

EU_TIME <- read.csv("ResultsTIME_EulJugA.txt", sep = '\t')

HC_TIME <- read.csv("ResultsTIME_HelCar.txt", sep = '\t')

HB_TIME <- read.csv("ResultsTIME_HelBih.txt", sep = '\t')


head(Eu_HC_RATIO)

Eu_HC_RATIO <- as.data.frame(Eu_HC_RATIO)

Eu_HB_RATIO <- as.data.frame(Eu_HB_RATIO)


#plotting All the raw data: 


library(gridExtra)

library(ggplot2)

library(dbplyr)

plotHC <-ggplot(Eu_HC_RATIO, aes(x=overlap, y=Ratio)) +
  geom_point(shape=1) +
  labs(x="Overlap", y="Ratio", title="Overlap vs Ratio - H. Car") +
  ylim(0, 1) +
  theme_minimal()

plotHB <- ggplot(Eu_HB_RATIO, aes(x=overlap, y=Ratio)) +
  geom_point(shape=1) +
  labs(x="Overlap", y="Ratio", title="Overlap vs Ratio - H. Bih") +
  ylim(0, 1) +
  theme_minimal()

grid.arrange(plotHC, plotHB, nrow=2)


#### Filter based on the selected thresholds


filtered_Eu_HC_RATIO <- Eu_HC_RATIO %>% filter(Eu_HC_RATIO$Ratio > 0.5 & Eu_HB_RATIO$overlap > 1000)

filtered_Eu_HB_RATIO <- Eu_HB_RATIO %>% filter(Eu_HB_RATIO$Ratio > 0.5 & Eu_HB_RATIO$overlap > 1000)


##Spiltting names the the downstream mapping od the max time point

# Extract 'bird' (everything before the third underscore)
filtered_Eu_HC_RATIO$bird <- sub("^(.*?_.*?_.*?)_.*", "\\1", filtered_Eu_HC_RATIO$Comparison)

# Extract 'plant' (everything after the third underscore)
filtered_Eu_HC_RATIO$plant <- sub(".*?_.*?_.*?_(.*)", "\\1", filtered_Eu_HC_RATIO$Comparison)


# Extract 'bird' (everything before the third underscore)
filtered_Eu_HB_RATIO$bird <- sub("^(.*?_.*?_.*?)_.*", "\\1", filtered_Eu_HB_RATIO$Comparison)

# Extract 'plant' (everything after the third underscore)
filtered_Eu_HB_RATIO$plant <- sub(".*?_.*?_.*?_(.*)", "\\1", filtered_Eu_HB_RATIO$Comparison)

# Combine the columns to create the new 'bird' column in the time data frame. 
HC_TIME$plant <- paste(HC_TIME$Spp, "_mu", HC_TIME$mu, "_g", HC_TIME$g, sep = "")
HC_TIME$plant <- gsub("e-([0-9]+)", "e.\\1", HC_TIME$plant)

# Combine the columns to create the new 'bird' column in the time data frame. 
HB_TIME$plant <- paste(HB_TIME$Spp, "_mu", HB_TIME$mu, "_g", HB_TIME$g, sep = "")
HB_TIME$plant <- gsub("e-([0-9]+)", "e.\\1", HB_TIME$plant)

# Combine the columns to create the new 'bird' column in the time data frame. 
EU_TIME$bird <- paste(EU_TIME$Spp, "_mu", EU_TIME$mu, "_g", EU_TIME$g, sep = "")
EU_TIME$bird <- gsub("e-([0-9]+)", "e.\\1", EU_TIME$bird)

##
#Now we have two dataframes with the filtred Ratio ond overlaps. 
#We need to project the max_time value for the bird and the plant into the filtered data.frame

head(filtered_Eu_HB_RATIO)
dim(filtered_Eu_HB_RATIO)
head(HB_TIME)
dim(HB_TIME)
head(EU_TIME)

# Merge the two data frames on the 'plant' column
merged_data <- merge(filtered_Eu_HB_RATIO, HB_TIME[, c("plant", "max")], by = "plant", all.x = TRUE)
filtered_Eu_HB_RATIO$plant_max <- merged_data$max # Add the 'max' column to the original data frame as 'plant_max'

# Merge the two data frames on the 'plant' column
merged_data <- merge(filtered_Eu_HB_RATIO, EU_TIME[, c("bird", "max")], by = "bird", all.x = TRUE)
filtered_Eu_HB_RATIO$bird_max <- merged_data$max # Add the 'max' column to the original data frame as 'plant_max'


# Merge the two data frames on the 'plant' column
merged_data <- merge(filtered_Eu_HC_RATIO, HC_TIME[, c("plant", "max")], by = "plant", all.x = TRUE)
filtered_Eu_HC_RATIO$plant_max <- merged_data$max # Add the 'max' column to the original data frame as 'plant_max'

# Merge the two data frames on the 'plant' column
merged_data <- merge(filtered_Eu_HC_RATIO, EU_TIME[, c("bird", "max")], by = "bird", all.x = TRUE)
filtered_Eu_HC_RATIO$bird_max <- merged_data$max # Add the 'max' column to the original data frame as 'plant_max'


#HelBIH

# Step 1: Sort the data based on 'max_plant' and 'max_bird'
filtered_Eu_HB_RATIO_sorted <- filtered_Eu_HB_RATIO[order(filtered_Eu_HB_RATIO$bird_max), ]
filtered_Eu_HB_RATIO_sorted <- filtered_Eu_HB_RATIO[order(filtered_Eu_HB_RATIO$plant_max, filtered_Eu_HB_RATIO$bird_max), ]


# Step 2: Create a matrix of the ratio values where the rows are the plants and columns are the birds
# First, we extract the unique bird and plant names
bird_names <- unique(filtered_Eu_HB_RATIO_sorted$bird)
plant_names <- unique(filtered_Eu_HB_RATIO_sorted$plant)

# Now we create an empty matrix to fill in the ratio values
ratio_matrix <- matrix(NA, nrow = length(plant_names), ncol = length(bird_names))

# Step 3: Fill the matrix with the ratio values
for (i in 1:nrow(filtered_Eu_HB_RATIO_sorted)) {
  bird_index <- which(bird_names == filtered_Eu_HB_RATIO_sorted$bird[i])
  plant_index <- which(plant_names == filtered_Eu_HB_RATIO_sorted$plant[i])
  ratio_matrix[plant_index, bird_index] <- filtered_Eu_HB_RATIO_sorted$Ratio[i]
}

# Step 4: Plot the heatmap using ggplot2 or base R
library(ggplot2)
library(reshape2)

# Convert the matrix to a data frame for ggplot2
ratio_data <- melt(ratio_matrix)
colnames(ratio_data) <- c("Plant", "Bird", "Ratio")

# Create the heatmap
ggplot(ratio_data, aes(x = Bird, y = Plant, fill = Ratio)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "orange", midpoint = 0.75) +
  theme_minimal() +
  labs(title = "Heatmap of Ratio Values", x = "Bird", y = "Plant")




# Step 1: Sort the data based on 'max_plant' and 'max_bird'
filtered_Eu_HC_RATIO_sorted <- filtered_Eu_HC_RATIO[order(filtered_Eu_HC_RATIO$bird_max), ]
filtered_Eu_HC_RATIO_sorted <- filtered_Eu_HC_RATIO[order(filtered_Eu_HC_RATIO$plant_max, filtered_Eu_HC_RATIO$bird_max), ]

# Step 2: Create a matrix of the ratio values where the rows are the plants and columns are the birds
# First, we extract the unique bird and plant names
bird_names <- unique(filtered_Eu_HC_RATIO_sorted$bird)
plant_names <- unique(filtered_Eu_HC_RATIO_sorted$plant)

# Now we create an empty matrix to fill in the ratio values
ratio_matrix <- matrix(NA, nrow = length(plant_names), ncol = length(bird_names))

# Step 3: Fill the matrix with the ratio values
for (i in 1:nrow(filtered_Eu_HC_RATIO_sorted)) {
  bird_index <- which(bird_names == filtered_Eu_HC_RATIO_sorted$bird[i])
  plant_index <- which(plant_names == filtered_Eu_HC_RATIO_sorted$plant[i])
  ratio_matrix[plant_index, bird_index] <- filtered_Eu_HC_RATIO_sorted$Ratio[i]
}

# Step 4: Plot the heatmap using ggplot2 or base R
library(ggplot2)
library(reshape2)

# Convert the matrix to a data frame for ggplot2
ratio_data_HC <- melt(ratio_matrix)
colnames(ratio_data_HC) <- c("Plant", "Bird", "Ratio")

# Create the heatmap
ggplot(ratio_data_HC, aes(x = Bird, y = Plant, fill = Ratio)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "orange", midpoint = 0.75) +
  theme_minimal() +
  labs(title = "Heatmap of Ratio Values", x = "Bird", y = "Plant")

