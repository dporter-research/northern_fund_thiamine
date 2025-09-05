library(tidyverse)
library(googlesheets4)
library(googledrive)

egg_thiamine_url <- "https://docs.google.com/spreadsheets/d/1xZgxUHShmJiPbeIi8wHqr_Qyw8aWAC5rch3kxgNNpaw/edit?gid=2376641#gid=2376641"
dipac_meta_url <- "https://docs.google.com/spreadsheets/d/10xZojflgULdqBCIukSCz_ZDYc-3QowlFEWhUzosxTQo/edit?gid=0#gid=0"

dipac_egg_thiamine <- read_sheet(egg_thiamine_url, sheet = "collate")
dipac_meta <- read_sheet(dipac_meta_url, sheet = "fish_data")

dipac_meta_f <- dipac_meta |> 
  filter(sex == "f")

dipac_f_meta_thiamine <- dipac_meta_f |> 
  left_join(dipac_egg_thiamine, by = join_by (fish_id == id))

# Define BKD fish
bkd_fish <- c("f13", "f34")

dipac_females <- dipac_f_meta_thiamine |> 
  filter(!(fish_id %in% bkd_fish))

# Exploratory plots ------------------------------------------------------------

## Histogram -------------------------------------------------------------------
nmolT_g_IQR <- IQR(dipac_females$nmolT_g)
nmolT_g_n <- length(dipac_females$nmolT_g)

nmolT_g_binwidth <- 2 * nmolT_g_IQR / (nmolT_g_n^(1/3))
bins_nmolT_g <- round(1 + log2(nmolT_g_n), 0)


ggplot(dipac_females, aes(x = nmolT_g)) +
  geom_histogram(
    aes(y = after_stat(density)),
    binwidth = nmolT_g_binwidth,
    fill = "salmon",
    color = "black"
    ) +
  geom_density(color = "red", linewidth = 1) +
  theme_minimal()
