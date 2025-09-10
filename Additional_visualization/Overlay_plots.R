
library(ggplot2)
library(dplyr)
library(purrr)

#Run the analysis on very 3rd parameter for plottion overlay 
plant_mu_8 <- seq(1e-8, 9e-8, length.out = 30)  # Adjust length.out as needed

print(plant_mu_8)

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
              32,
              33)

plant_gen_f <- plant_gen[c(1, seq(3, length(plant_gen), by = 3))]
plant_mu_f <- plant_mu_8[c(1, seq(3, length(plant_mu_8), by = 3))]


figure_df_EulJugA_HelBih_8 <- BinCorr_psmc(EulJugA, HelBih, plant_mu_f, plant_gen_f, bird_mu, bird_g)

figure_df_EulJugA_HelCar_8 <- BinCorr_psmc(EulJugA, HelCar, plant_mu_f, plant_gen_f, bird_mu, bird_g)

# --- Bird reference ---
#E. Lampis
psmcbird_Eu <- psmc.result(EulJugA, i.iteration=25, mu=2.8e-9, s=100, g=2.5)

#Eu. hol
psmcbird_hol <- psmc.result(EulHol, i.iteration=25, mu=2.8e-9, s=100, g=2.5)





# Replicate bird across all mu × g combinations
bird_facets <- expand.grid(mu=plant_mu_f, g=plant_gen_f) %>%
  mutate(data = pmap(list(mu, g), function(mu, g) {
    psmcbird_Eu %>% mutate(species="E. jugularis", mu=mu, g=g)
  })) %>%
  pull(data) %>%
  bind_rows()

# --- Plant runs ---
psmc_plant_8 <- expand.grid(mu=plant_mu_f, g=plant_gen_f) %>%
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
all_psmc_8 <- bind_rows(psmc_plant_8, bird_facets)

# --- Plot --- version 1

# make g a factor with levels in descending order
all_psmc_8$g <- factor(all_psmc_8$g, levels = plant_g)





# Convert mu and g to factors matching facet grid
all_psmc_8 <- all_psmc_8 %>% 
  mutate(g = factor(g, levels = plant_g),
         mu = signif(mu, 2))

# Prepare heatmap with color_group logic
prepare_heatmap_colors <- function(result_df) {
  result_df %>%
    filter(Method == "spearman") %>%
    mutate(
      mu = signif(Plant_mu, 2),
      g = factor(Plant_g, levels = plant_g),
      color_group = case_when(
        Correlation < 0 ~ "neg_corr",
        Correlation >= 0 & P_value >= 1e-5 ~ "gray_tile",
        Correlation >= 0 & P_value < 1e-5 ~ "heat"
      ),
      heat_val = ifelse(color_group == "heat", P_value, NA)
    )
}

heat_HelCar <- prepare_heatmap_colors(figure_df_EulJugA_HelCar_8)
heat_HelBih <- prepare_heatmap_colors(figure_df_EulJugA_HelBih_8)

# Function to plot PSMC with color-coded background
plot_psmc_with_color_bg <- function(psmc_df, heat_df, species_name) {
  
  # For each facet, create a rectangle covering the full x/y axis
  heat_df <- heat_df %>%
    mutate(xmin = 0, xmax = 6e6, ymin = 0, ymax = 1e6)
  
  ggplot(psmc_df, aes(x = Time, y = Ne, color = species)) +
    # Black tiles for negative correlations
    geom_rect(
      data = filter(heat_df, color_group == "neg_corr"),
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "gray", inherit.aes = FALSE
    ) +
    # Gray tiles for non-significant positive correlations
    geom_rect(
      data = filter(heat_df, color_group == "gray_tile"),
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "white", inherit.aes = FALSE
    ) +
    # Heat gradient tiles for significant positive correlations
    geom_rect(
      data = filter(heat_df, color_group == "heat"),
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = heat_val),
      inherit.aes = FALSE
    ) +
    # Original PSMC lines
    geom_line(na.rm = TRUE) +
    facet_grid(rows = vars(g), cols = vars(mu)) +
    scale_color_manual(values = c("E. jugularis"="purple", "HelCar"="yellow3", "HelBih"="red2")) +
    scale_fill_gradient(
      low = "darkblue", high = "white", na.value = "transparent",
      trans = "log10", limits = c(1e-27, 0.05),
      name = "P-value"
    ) +
    xlim(0, 6e6) + ylim(0, 1e6) +
    theme_bw() +
    theme(
      strip.text.x = element_text(angle = 90),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    ) +
    labs(
      x = "Time", y = "Ne",
      color = "Species",
      title = paste("PSMC with background P-values -", species_name)
    )
}

# Separate plots for HelCar and HelBih P-values
plot_HelCar <- plot_psmc_with_color_bg(all_psmc_8, heat_HelCar, "HelCar")
plot_HelBih <- plot_psmc_with_color_bg(all_psmc_8, heat_HelBih, "HelBih")

plot_HelCar
plot_HelBih
