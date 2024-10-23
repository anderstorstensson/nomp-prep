library(tidyverse)
library(readxl)

# Select year
year <- 2024

# Find the latest bvol file
bvol_filename <- list.files("data") %>%
  as_tibble() %>%
  filter(agrepl("NOMP_BVOL", value)) %>%
  filter(agrepl(".xlsx", value)) %>%
  filter(!agrepl("~$", value)) %>%
  filter(agrepl(year, value)) %>%
  arrange(value)

# Read current NOMP list
bvol_nomp <- read_excel(file.path("data", bvol_filename$value[nrow(bvol_filename)])) %>%
  select(-contains("NOT IMPORTED"))

# Read current changelog
changelog <- read_excel(file.path("data", bvol_filename$value[nrow(bvol_filename)]), sheet = 2) 

# Filter current changelog
current_changelog <- changelog %>%
  filter(Year == year)

# Filter new species and size classes
new_taxa <- current_changelog %>%
  filter(`Change search` %in% c("New Species"))

# Filter new species and size classes
new_size_class <- current_changelog %>%
  filter(`Change search` %in% c("New Size class")) %>%
  mutate(species_size = paste(Species, `Size Class`, sep = "_"))

# Filter SMHI and NIVA list
smhi_niva <- bvol_nomp %>%
  filter(!List == paste0("PEG_BVOL", year))

new_species <- unique(new_taxa$Species)[unique(new_taxa$Species) %in% smhi_niva$Species]

cat("Number of new species already present in SMHI and NIVA:", length(new_species), "\n")

if (length(new_species) > 0) {
  print(new_species)
}

duplicates <- smhi_niva %>%
  mutate(species_size = paste(Species, SizeClassNo, sep = "_")) %>%
  filter(species_size %in% new_size_class$species_size)

cat("Number of new size classes already present in SMHI and NIVA:", nrow(duplicates), "\n")

if (nrow(duplicates) > 0) {
  print(duplicates$species_size)
}
