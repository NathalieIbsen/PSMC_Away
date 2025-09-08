# List of required packages
packages <- c(
  "lmtest",
  "imputeTS",
  "dplyr",
  "tseries",
  "BINCOR",
  "ggplot2"
)

# Install any missing packages
installed <- rownames(installed.packages())
for (pkg in packages) {
  if (!(pkg %in% installed)) {
    install.packages(pkg)
  }
}

# Load the packages
lapply(packages, library, character.only = TRUE)


#Some err messages are to be expected. 

