library(ggplot2)
library(dplyr)



# Prepare heatmap with color_group logic
# 1) Prep heatmap colors: keep exact keys and align factors
prepare_heatmap_colors <- function(result_df, list_mu2, list_g2) {
  result_df %>%
    dplyr::filter(Method == "spearman") %>%
    dplyr::transmute(
      mu = list_mu2,                # <- no rounding
      g  = list_g2,
      color_group = dplyr::case_when(
        Correlation < 0 ~ "neg_corr",
        Correlation >= 0 & P_value >= 1e-5 ~ "gray_tile",
        TRUE ~ "heat"
      ),
      heat_val = ifelse(color_group == "heat", P_value, NA_real_)
    ) %>%
    dplyr::mutate(
      mu = factor(mu, levels = list_mu2),
      g  = factor(g,  levels = list_g2)
    ) %>%
    dplyr::distinct(mu, g, color_group, .keep_all = TRUE)  # ensure 1 row/facet
}



# 2) Plot: enforce identical facet keys and use full-panel rects
plot_psmc_with_color_bg <- function(psmc_df, heat_df, species_name, list_mu2, list_g2) {
  
  psmc_df <- psmc_df %>%
    dplyr::mutate(
      mu = factor(mu, levels = list_mu2),
      g  = factor(g,  levels = list_g2)
    )
  
  heat_df <- heat_df %>%
    dplyr::mutate(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
  
  # Custom labellers for facet labels
  mu_labeller <- function(values) sprintf("%.1e", as.numeric(values))  # scientific notation
  g_labeller  <- function(values) as.character(values)                 # keep as-is
  
  ggplot(psmc_df, aes(x = Time, y = Ne, color = species)) +
    geom_rect(
      data = dplyr::filter(heat_df, color_group == "neg_corr"),
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "lightgrey", inherit.aes = FALSE
    ) +
    geom_rect(
      data = dplyr::filter(heat_df, color_group == "gray_tile"),
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "white", inherit.aes = FALSE
    ) +
    geom_rect(
      data = dplyr::filter(heat_df, color_group == "heat"),
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = heat_val),
      inherit.aes = FALSE
    ) +
    geom_line(na.rm = TRUE, size = 0.7) +
    facet_grid(
      rows = vars(g),
      cols = vars(mu),
      labeller = labeller(mu = mu_labeller, g = g_labeller)
    ) +
    scale_color_manual(values = c("E. jugularis"="purple", "HelCar"="yellow3", "HelBih"="red2")) +
    scale_fill_gradient(
      low = "dodgerblue4", high = "white", na.value = "transparent",
      trans = "log10", limits = c(1e-27, 0.05), name = "P-value"
    ) +
    coord_cartesian(xlim = c(0, 6e6), ylim = c(0, 1e6), expand = FALSE) +
    theme_bw() +
    theme(
      strip.text.x = element_text(angle = 90),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    ) +
    labs(x = "Time", y = "Ne", color = "Species",
         title = paste("PSMC with background P-values -", species_name))
}




### Prep input data: 

#Data
datafolder <- "C:/Users/au601132/OneDrive - Aarhus universitet/Skrivebord/PhD/Kolibri_heliconia/PSMC/"

# Main PSMC results
EulJugA <- dir(paste0(datafolder, "Auto/"), pattern = "psmc$", full.names = TRUE)
HelBih  <- dir(paste0(datafolder, "HBih/"), pattern = "psmc$", full.names = TRUE)
HelCar  <- dir(paste0(datafolder, "HCar/"), pattern = "psmc$", full.names = TRUE)
EulHol  <- dir(paste0(datafolder, "Ehol/"), pattern = "psmc$", full.names = TRUE)

##Run a subset of comparisons for figure plotting:

#MUTATION RATE 
#a linear sequence within this range
list_mu2 <- seq(1e-8, 9e-8, length.out = 11)  

#GENERATION TIME 
list_g2 = c(3,6,9,12,15,18,21,24,27,30,33) # (as many as wanted)

#Reversing the list for downstream plotting 
list_g2 <- rev(list_g2)   # (optional - But more intuitive)



###Running the Bincorr function



#Pick out every 3rd 
#plant_g <- plant_gen[c(1, seq(3, length(plant_gen), by = 3))]
#plant_mu <- plant_mu_8[c(1, seq(3, length(plant_mu_8), by = 3))]

#E. jujularis vs. H. bihai 
df_EulJugA_HelBih_8 <- BinCorr_psmc(EulJugA, HelBih, mu1, g1, list_mu2, list_g2)

#E. jujularis vs. H. caribea
df_EulJugA_HelCar_8 <- BinCorr_psmc(EulJugA, HelCar, mu1, g1, list_mu2, list_g2)


#### less of a mutualist  
#E. holosericeus vs H. bihai 
df_EulHol_HelBih_8 <- BinCorr_psmc(EulHol, HelBih, mu1, g1, list_mu2, list_g2)

#E. holosericeus vs H. caribea 
df_EulHol_HelCar_8 <- BinCorr_psmc(EulHol, HelCar, mu1, g1, list_mu2, list_g2)


table(df_EulJugA_HelBih_8$Mu2)
table(df_EulJugA_HelBih_8$G2)


# Replicate bird across all mu × g combinations
bird_facets <- expand.grid(mu=list_mu2, g=list_mu2) %>%
  mutate(data = pmap(list(mu, g), function(mu, g) {
    psmcbird_Eu %>% mutate(species="E. jugularis", mu=mu, g=g)
  })) %>%
  pull(data) %>%
  bind_rows()

# --- Plant runs ---
psmc_plant <- expand.grid(mu=list_mu2, g=list_mu2) %>%
  mutate(data = pmap(list(mu, g), function(mu, g) {
    df1 <- psmc.result(HelCar, i.iteration=25, mu=mu, s=100, g=g) %>%
      mutate(species="HelCar", mu=mu, g=g)
    df2 <- psmc.result(HelBih, i.iteration=25, mu=mu, s=100, g=g) %>%
      mutate(species="HelBih", mu=mu, g=g)
    bind_rows(df1, df2)
  })) %>%
  pull(data) %>%
  bind_rows()

# --- Combine everything ---
all_psmc <- bind_rows(psmc_plant, bird_facets)

# --- Plot --- version 1

# make g a factor with levels in descending order
all_psmc$g <- factor(all_psmc$g, levels = plant_g)


heat_HelCar <- prepare_heatmap_colors(df_EulJugA_HelCar_8)
heat_HelBih <- prepare_heatmap_colors(df_EulJugA_HelBih_8)



# Separate plots for HelCar and HelBih P-values
plot_HelCar <- plot_psmc_with_color_bg(all_psmc, heat_HelCar, "HelCar", plant_mu, plant_g)
plot_HelBih <- plot_psmc_with_color_bg(all_psmc, heat_HelBih, "HelBih", plant_mu, plant_g)


plot_HelCar
plot_HelBih
