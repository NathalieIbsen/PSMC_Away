# List of required packages
packages <- c(
  "lmtest",
  "imputeTS",
  "dplyr",
  "tseries",
  "BINCOR",
  "ggplot2",
  "flextable",
  "webshot2"
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

#The libraries will loaded by the functions when the package is needed ! 
#purrr tends to be overroad alot depending on your local R set up.
# - This is fine. 

