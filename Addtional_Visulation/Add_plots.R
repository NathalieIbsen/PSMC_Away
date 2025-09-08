#Additional plots



### General figure creation. . 


EulJugA <- dir("C:/Users/au601132/OneDrive - Aarhus universitet/Skrivebord/PhD/Kolibri_heliconia/PSMC/Auto/","psmc$", full.names=T) # main PSMC results
HelBih <- dir("C:/Users/au601132/OneDrive - Aarhus universitet/Skrivebord/PhD/Kolibri_heliconia/PSMC/HBih/","psmc$", full.names=T) # main PSMC results
HelCar <- dir("C:/Users/au601132/OneDrive - Aarhus universitet/Skrivebord/PhD/Kolibri_heliconia/PSMC/HCar/","psmc$", full.names=T) # main PSMC results
EulHol <- dir("C:/Users/au601132/OneDrive - Aarhus universitet/Skrivebord/PhD/Kolibri_heliconia/PSMC/Ehol/","psmc$", full.names=T) # main PSMC results
EulJugZ <- dir("C:/Users/au601132/OneDrive - Aarhus universitet/Skrivebord/PhD/Kolibri_heliconia/PSMC/Z/","psmc$", full.names=T) # main PSMC results
CyBi <- dir("C:/Users/au601132/OneDrive - Aarhus universitet/Skrivebord/PhD/Kolibri_heliconia/PSMC/CyBi/","psmc$", full.names=T) # main PSMC results
OrCr <- dir("C:/Users/au601132/OneDrive - Aarhus universitet/Skrivebord/PhD/Kolibri_heliconia/PSMC/OrCr/","psmc$", full.names=T) # main PSMC results


# Run the Function for a selected set of parameters: 
psmc_Car <- psmc.result(HelCar, i.iteration=25, mu=2.655172e-08, s=100, g=22)
psmc_Bih <- psmc.result(HelBih, i.iteration=25, mu=8.724138e-09, s=100, g=5)
psmc_EulJugAZ <- psmc.result(EulJugZ, i.iteration=25, mu=2.8e-9, s=100, g=2.5)
psmc_EulJugA <- psmc.result(EulJugA, i.iteration=25, mu=2.8e-9, s=100, g=2.5)
psmc_EulHol <- psmc.result(EulHol, i.iteration=25, mu=2.8e-9, s=100, g=2.5)
psmc_CyBi <- psmc.result(CyBi, i.iteration=25, mu=2.8e-9, s=100, g=2.5)
psmc_OrCr <- psmc.result(OrCr, i.iteration=25, mu=2.8e-9, s=100, g=2.5)


# log scaled 

par(mar=c(5,5,3,1)+0.1)

plot(1,1,type="n",log="x", xlab="Years before present", ylab="Effective population size (Ne)",xlim=c(25000,5e6),ylim=c(0,3e5),tck=-0.03, main = "E. jugularis")

legend("top", legend=c("Autosomes", "Z chromosomes"),
       
       col=c("darkorchid4", "mediumorchid"), lty=1, cex=0.5)

axis(side=1, at=c(1000,10000,100000,1000000,10000000),tck=-0.03)

for(file in EulJugA)lines(psmc.result(file,i.iteration=25,mu=2.8e-9,s=100,g=2.5),col="darkorchid4",lwd=2, xlab="Year before present")  #plot main PSMC trajectory - adjust mutation rate and generation time as needed

for(file in EulJugZ)lines(psmc.result(file,i.iteration=25,mu=2.8e-9,s=100,g=2.5),col="mediumorchid",lwd=2, xlab="Year before present")  #plot main PSMC trajectory - adjust mutation rate and generation time as needed

abline(v=2310000, lty=2)
abline(v=635000, lty=2)
text(280000, 15000, "female bias")
text(1200000, 15000, "male bias")
text(3700000, 15000, "female bias")


#non log scaled 
par(mar=c(5,5,3,1)+0.1)

plot(1, 1, type="n",
     xlab="Years before present",
     ylab="Effective population size (Ne)",
     xlim=c(25000, 5e6),
     ylim=c(0, 3e5),
     tck=-0.03,
     main = "E. jugularis")  # <-- removed log="x"

legend("top", legend=c("Autosomes", "Z chromosomes"),
       col=c("darkorchid4", "mediumorchid"), lty=1, cex=0.5)

axis(side=1, at=c(1000000,2000000,3000000,4000000,5000000), tck=-0.03)

for(file in EulJugA) {
  lines(psmc.result(file, i.iteration=25, mu=2.8e-9, s=100, g=2.5),
        col="darkorchid4", lwd=2)
}

for(file in EulJugZ) {
  lines(psmc.result(file, i.iteration=25, mu=2.8e-9, s=100, g=2.5),
        col="mediumorchid", lwd=2)
}

abline(v=2310000, lty=2)
abline(v=635000, lty=2)
text(280000, 15000, "female bias")
text(1200000, 15000, "male bias")
text(2750000, 15000, "female bias")


# Set up 1 row, 2 columns
par(mfrow = c(1, 2), mar=c(5,5,3,1)+0.1)

# --- Left: log-scaled x-axis ---
plot(1,1,type="n", log="x",
     xlab="Years before present",
     ylab="Effective population size (Ne)",
     xlim=c(25000,5e6),
     ylim=c(0,3e5),
     tck=-0.03,
     main = "E. jugularis (log x-axis)")

legend("top", legend=c("Autosomes", "Z chromosomes"),
       col=c("darkorchid4", "mediumorchid"), lty=1.5, cex=0.5, bty="n")  # no box

axis(side=1, at=c(1000,10000,100000,1000000,10000000), tck=-0.03)

for(file in EulJugA)
  lines(psmc.result(file,i.iteration=25,mu=2.8e-9,s=100,g=2.5),
        col="darkorchid4",lwd=2)

for(file in EulJugZ)
  lines(psmc.result(file,i.iteration=25,mu=2.8e-9,s=100,g=2.5),
        col="mediumorchid",lwd=2)

abline(v=2310000, lty=2)
abline(v=635000, lty=2)
text(280000, 15000, "female bias")
text(1200000, 15000, "male bias")
text(3700000, 15000, "female bias")

# --- Right: linear x-axis ---
plot(1, 1, type="n",
     xlab="Years before present",
     ylab="Effective population size (Ne)",
     xlim=c(25000,5e6),
     ylim=c(0,3e5),
     tck=-0.03,
     main = "E. jugularis (linear x-axis)")

legend("bottomright", legend=c("Autosomes", "Z chromosomes"),
       col=c("darkorchid4", "mediumorchid"), lty=1.5, cex=0.5, bty="n")  # no box

axis(side=1, at=c(1000000,2000000,3000000,4000000,5000000), tck=-0.03)

for(file in EulJugA)
  lines(psmc.result(h,i.iteration=25,mu=2.8e-09,s=100,g=2.5),
        col="darkorchid4",lwd=2)

for(file in EulJugZ)
  lines(psmc.result(file,i.iteration=25,mu=2.8e-9,s=100,g=2.5),
        col="mediumorchid",lwd=2)

abline(v=2310000, lty=2)
abline(v=635000, lty=2)
text(280000, 15000, "female bias")
text(1200000, 15000, "male bias")
text(2750000, 15000, "female bias")









#Best fittet 


# Set up 1 row, 2 columns
par(mfrow = c(1, 2), mar=c(5,5,3,1)+0.1)

# --- Left: log-scaled x-axis ---
plot(1,1,type="n", log="x",
     xlab="Years before present",
     ylab="Effective population size (Ne)",
     xlim=c(25000,5e6),
     ylim=c(0,3e5),
     tck=-0.03,
     main = "E. jugularis, H. Bihai, H. caribea (log x-axis)")

legend("top", legend=c("E. jugularis", "H. Bihai", "H. caribea"),
       col=c("darkorchid4", "red3", "yellow3"), lty=1.5, cex=0.5, bty="n")  # no box

axis(side=1, at=c(1000,10000,100000,1000000,10000000), tck=-0.03)

for(file in HelCar)
  lines(psmc.result(file,i.iteration=25,mu=2.655172e-08,s=100,g=22)[-(1:12), ],
        col="yellow3",lwd=2)

for(file in EulJugA)
  lines(psmc.result(file,i.iteration=25,mu=2.8e-9,s=100,g=2.5)[-(1:12), ],
        col="darkorchid4",lwd=2)

for(file in HelBih)
  lines(psmc.result(file,i.iteration=25,mu=4.862069e-08,s=100,g=31)[-(1:12), ],
        col="red3",lwd=2)



# --- Right: linear x-axis ---
plot(1, 1, type="n",
     xlab="Years before present",
     ylab="Effective population size (Ne)",
     xlim=c(25000,5e6),
     ylim=c(0,3e5),
     tck=-0.03,
     main = "E. jugularis, H. Bihai, H. caribea (linear x-axis)")


legend("top", legend=c("E. jugularis", "H. Bihai", "H. caribea"),
       col=c("darkorchid4", "red3", "yellow3"), lty=1.5, cex=0.5, bty="n")  # no box

axis(side=1, at=c(1000,10000,100000,1000000,10000000), tck=-0.03)

for(file in HelCar)
  lines(psmc.result(file,i.iteration=25,mu=2.655172e-08,s=100,g=22)[-(1:12), ],
        col="yellow3",lwd=2)

for(file in EulJugA)
  lines(psmc.result(file,i.iteration=25,mu=2.8e-9,s=100,g=2.5)[-(1:12), ],
        col="darkorchid4",lwd=2)

for(file in HelBih)
  lines(psmc.result(file,i.iteration=25,mu=4.862069e-08,s=100,g=31)[-(1:12), ],
        col="red3",lwd=2)







###
# Best fitted including the mean of the two plants. 
#we have to do aonther interpoltion to do this.


psmc_Car <- psmc.result(HelCar, i.iteration=25, mu=2.655172e-08, s=100, g=22)[-(1:12), ]
psmc_Bih <- psmc.result(HelBih, i.iteration=25,mu=4.862069e-08,s=100,g=31)[-(1:12), ]

head(psmc_Bih)
head(psmc_Car)

# Maximum time across both datasets
max_time <- max(psmc_Bih$Time, psmc_Car$Time)

# Create a regular time grid (e.g., every 1000 years)
time_grid <- seq(0, max_time, by = 1000)

# Interpolate Bih
Ne_Bih_interp <- approx(x = psmc_Bih$Time, y = psmc_Bih$Ne, xout = time_grid)$y

# Interpolate Car
Ne_Car_interp <- approx(x = psmc_Car$Time, y = psmc_Car$Ne, xout = time_grid)$y

#mean
Ne_avg <- (Ne_Bih_interp + Ne_Car_interp) / 2

psmc_avg <- data.frame(
  Time = time_grid,
  Ne = Ne_avg
)

head(psmc_avg)




# Set up 1 row, 2 columns
par(mfrow = c(1, 2), mar=c(5,5,3,1)+0.1)

# --- Left: log-scaled x-axis ---
plot(1,1,type="n", log="x",
     xlab="Years before present",
     ylab="Effective population size (Ne)",
     xlim=c(25000,5e6),
     ylim=c(0,3e5),
     tck=-0.03,
     main = "E. jugularis, H. Bihai, H. caribea (log x-axis)")

legend("top", legend=c("E. jugularis", "H. Bihai", "H. caribea","Mean Ne of H. Bihai & H. caribea"),
       col=c("darkorchid4", "red3", "yellow3","grey"), lty=1.5, cex=0.5, bty="n")  # no box

axis(side=1, at=c(1000,10000,100000,1000000,10000000), tck=-0.03)

for(file in HelCar)
  lines(psmc.result(file,i.iteration=25,mu=2.655172e-08,s=100,g=22)[-(1:12), ],
        col="yellow3",lwd=2)

for(file in EulJugA)
  lines(psmc.result(file,i.iteration=25,mu=2.8e-9,s=100,g=2.5)[-(1:12), ],
        col="darkorchid4",lwd=2)

for(file in HelBih)
  lines(psmc.result(file,i.iteration=25,mu=4.862069e-08,s=100,g=31)[-(1:12), ],
        col="red3",lwd=2)

lines(psmc_avg[-(1:12), ], col="grey",lwd=2)



# --- Right: linear x-axis ---
plot(1, 1, type="n",
     xlab="Years before present",
     ylab="Effective population size (Ne)",
     xlim=c(25000,5e6),
     ylim=c(0,3e5),
     tck=-0.03,
     main = "E. jugularis, H. Bihai, H. caribea (linear x-axis)")


legend("top", legend=c("E. jugularis", "H. Bihai", "H. caribea","Mean Ne of H. Bihai & H. caribea"),
       col=c("darkorchid4", "red3", "yellow3","grey"), lty=1.5, cex=0.5, bty="n")  # no box

axis(side=1, at=c(1000,10000,100000,1000000,10000000), tck=-0.03)

for(file in HelCar)
  lines(psmc.result(file,i.iteration=25,mu=2.655172e-08,s=100,g=22)[-(1:12), ],
        col="yellow3",lwd=2)

for(file in EulJugA)
  lines(psmc.result(file,i.iteration=25,mu=2.8e-9,s=100,g=2.5)[-(1:12), ],
        col="darkorchid4",lwd=2)

for(file in HelBih)
  lines(psmc.result(file,i.iteration=25,mu=4.862069e-08,s=100,g=31)[-(1:12), ],
        col="red3",lwd=2)
lines(psmc_avg[-(1:12), ], col="grey",lwd=2)

############
# all birds 


# Set up 1 row, 2 columns
par(mfrow = c(1, 2), mar=c(5,5,3,1)+0.1)


# --- Left: log-scaled x-axis ---
plot(1,1,type="n", log="x",
     xlab="Years before present",
     ylab="Effective population size (Ne)",
     xlim=c(25000,5e6),
     ylim=c(0,1.25e6),
     tck=-0.03,
     main = "A. jugularis, A. holosericeus, R. bicolor & O. cristatus (log x-axis)")

legend(x = 2e5, y = 1.2e6,
       legend=c("A. jugularis", "A. holosericeus", "R. bicolor","O. cristatus"),
       col=c("darkorchid4", "green4", "#0492C2", "goldenrod3"),
       lty=1,
       lwd=3,
       seg.len=0.5,      # line length
       cex=0.7,        # text size
       bty="n",
       x.intersp=0.5)  # reduces space between line and text


axis(side=1, at=c(1000,10000,100000,1000000,10000000), tck=-0.03)

for(file in OrCr)
  lines(psmc.result(file,i.iteration=25,mu=2.8e-9,s=100,g=2.5),
        col="goldenrod3",lwd=2)

for(file in EulJugA)
  lines(psmc.result(file,i.iteration=25,mu=2.8e-9,s=100,g=2.5)[-(1:12), ],
        col="darkorchid4",lwd=2)

for(file in CyBi)
  lines(psmc.result(file,i.iteration=25,mu=2.8e-9,s=100,g=2.5)[-(1:12), ],
        col="#0492C2",lwd=2)

for(file in EulHol)
  lines(psmc.result(file,i.iteration=25,mu=2.8e-9,s=100,g=2.5),
        col="green4",lwd=2)


# --- Right: linear x-axis ---
plot(1, 1, type="n",
     xlab="Years before present",
     ylab="Effective population size (Ne)",
     xlim=c(25000,5e6),
     ylim=c(0,1.25e6),
     tck=-0.03,
     main = "A. jugularis, A. holosericeus, R. bicolor & O. cristatus (linear x-axis)")


legend(x = 2e6, y = 1.2e6,
       legend=c("A. jugularis", "A. holosericeus", "R. bicolor","O. cristatus"),
       col=c("darkorchid4", "green4", "#0492C2", "goldenrod3"),
       lty=1,
       lwd=3,
       seg.len=0.5,      # line length
       cex=0.7,        # text size
       bty="n",
       x.intersp=0.5)  # reduces space between line and text


for(file in OrCr)
  lines(psmc.result(file,i.iteration=25,mu=2.8e-9,s=100,g=2.5),
        col="goldenrod3",lwd=2)

for(file in EulJugA)
  lines(psmc.result(file,i.iteration=25,mu=2.8e-9,s=100,g=2.5)[-(1:12), ],
        col="darkorchid4",lwd=2)

for(file in CyBi)
  lines(psmc.result(file,i.iteration=25,mu=2.8e-9,s=100,g=2.5)[-(1:12), ],
        col="#0492C2",lwd=2)

for(file in EulHol)
  lines(psmc.result(file,i.iteration=25,mu=2.8e-9,s=100,g=2.5),
        col="green4",lwd=2)






#### MATRIX styled plots: 


# --- Bird reference ---
#E. Lampis
psmcbird_Eu <- psmc.result(EulJugA, i.iteration=25, mu=2.8e-9, s=100, g=2.5)


#################### For Mu ranging = 1-e8, 9-e8



# Replicate bird across all mu × g combinations
bird_facets <- expand.grid(mu=plant_mu_8, g=plant_g) %>%
  mutate(data = pmap(list(mu, g), function(mu, g) {
    psmcbird_Eu %>% mutate(species="E. jugularis", mu=mu, g=g)
  })) %>%
  pull(data) %>%
  bind_rows()

# --- Plant runs ---
psmc_plant_8 <- expand.grid(mu=plant_mu_8, g=plant_g) %>%
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

# now plot
Matrix_EUJU_8 <- ggplot(all_psmc_8, aes(x=Time, y=Ne, color=species)) +
  geom_line(na.rm=TRUE) +
  xlim(0, 6e6) +
  ylim(0, 1e6) +
  facet_grid(rows = vars(g), cols = vars(signif(mu,2))) +
  scale_color_manual(values=c("E. jugularis"="purple", "HelCar"="yellow3", "HelBih"="red2")) +
  theme_bw() +
  theme(
    strip.text.x = element_text(angle=90),
    axis.text.x  = element_text(angle=90, vjust=0.5, hjust=1)
  ) +
  labs(
    x="Time", y="Ne", color="Species",
    title="PSMC with varying plant mu (columns) and generationtime (rows)"
  )


#no y 0r x ticks

library(ggh4x)


# now plot
ggplot(all_psmc, aes(x=Time, y=Ne, color=species)) +
  geom_line(na.rm=TRUE) +
  xlim(0, 6e6) +
  ylim(0, 1e6) +
  facet_grid(rows = vars(g), cols = vars(signif(mu,2))) +  # use g factor directly
  scale_color_manual(values=c("Bird"="green4", "HelCar"="yellow3", "HelBih"="red2")) +
  theme_bw() +
  theme(
    strip.text.x = element_text(angle=90),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(
    x="Time", y="Ne", color="Species",
    title="PSMC with varying plant mu (columns) and generationtime (rows)"
  )











