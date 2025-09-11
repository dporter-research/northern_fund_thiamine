library(tidyverse)

# Global ggplot options --------------------------------------------------------

color_palette <- c("#1F77B4FF", "#FF7F0EFF", "#2CA02CFF", "#D62728FF", "#9467BDFF",
                   "#8C564BFF", "#E377C2FF", "#7F7F7FFF", "#BCBD22FF", "#17BECFFF")

options(ggplot2.discrete.colour = color_palette)
options(ggplot2.discrete.fill = color_palette)

theme_set(theme_bw())

jitter_width = 0.1
multifig_point_size = 0.75
multifig_point_alpha = 0.5


# Read in data, clean and join -------------------------------------------------
pre25 <- read_csv("data/raw/SEAK_cleaned_egg_thiamine_pre2025.csv",
                  col_types = "cfffddff") |> 
  mutate(location = if_else(location == "Macauly", "Macaulay", location)) |> 
  mutate(year_location = paste0(year, "_", location)) |> 
  filter(stock == "Andrew Creek") |> 
  rename(
    fish_id = id,
    nmolT_g = egg_nmol_thia_g,
    nmolT_egg = egg_nmol_thia_egg
  )
  

dipac25 <- read_rds("data/processed/dipac_females.rds") |>
  select(fish_id, nmolT_g, nmolT_egg) |> 
  mutate(
    year = "2025",
    stock = "Andrew Creek",
    location = "Macaulay",
    year_stock = "2025_Andrew Creek",
    year_location = "2025_Macaulay"
  ) |> 
  mutate(
    year = factor(year),
    stock = factor(stock),
    location = factor(location),
    year_stock = factor(year_stock),
    year_location = factor(year_location)
  )

andrew_creek_fish <- pre25 |> 
  full_join(dipac25)

# Violin Plot ------------------------------------------------------------------  

## Create sample sizes and label positions -------------------------------------
sample_sizes <- andrew_creek_fish %>%
  group_by(year, location) %>%
  summarise(
    n = n(),
    y_position_egg_conc = max(nmolT_g, na.rm = TRUE) + 0.5,
    y_position_total_egg_thia = max(nmolT_egg, na.rm = TRUE) + 0.5
)

## Egg thiamine concentration --------------------------------------------------
egg_conc_violin <- ggplot(data = andrew_creek_fish, aes(x = year, y = nmolT_g,
                                                        fill = location)) +
  geom_violin() +
  geom_jitter(position = position_jitterdodge(
    jitter.width = jitter_width, dodge.width = 0.9
    )) +
  geom_text(
    data = sample_sizes,
    aes(y = y_position_egg_conc, label = paste("n =", n)),
    position = position_dodge(width = 0.9)
  ) +
  geom_hline(yintercept = 5, lty = 2, color = "red") +
  labs(
    x = "Year",
    y = "Egg Thiamine (nmol/g)"
  )

egg_conc_violin

ggsave(
  "output/plots/andrew_creek_thiamine_conc_yearly.png",
  egg_conc_violin,
  width = 7,
  height = 5,
  dpi = 300,
  units = "in"
)

## Total egg thiamine nmols ----------------------------------------------------