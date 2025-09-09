library(tidyverse)
library(googlesheets4)
library(googledrive)
library(patchwork)
library(knitr)
library(ggpmisc)

egg_thiamine_url <- "https://docs.google.com/spreadsheets/d/1xZgxUHShmJiPbeIi8wHqr_Qyw8aWAC5rch3kxgNNpaw/edit?gid=2376641#gid=2376641"
dipac_meta_url <- "https://docs.google.com/spreadsheets/d/10xZojflgULdqBCIukSCz_ZDYc-3QowlFEWhUzosxTQo/edit?gid=0#gid=0"

gs4_auth(email = "drew.porter@noaa.gov")
dipac_egg_thiamine <- read_sheet(egg_thiamine_url, sheet = "collate", na = "NA")
gs4_auth(email = "dporter13@alaska.edu")
dipac_meta <- read_sheet(dipac_meta_url, sheet = "fish_data", na = "NA")

dipac_meta_f <- dipac_meta |> 
  filter(sex == "f")

dipac_f_meta_thiamine <- dipac_meta_f |> 
  left_join(dipac_egg_thiamine, by = join_by (fish_id == id))

dipac_females <- dipac_f_meta_thiamine |>
  mutate(
    mate_group = factor(paste0(mate1, "+", mate2)),
    avg_egg_mass_g = egg_mass_g/n_eggs,
    est_fecund_n = (est_pre_hardened_egg_mass_g) / avg_egg_mass_g,
    deficient_conc = nmolT_g < 5.0,
    deficient_totalT = nmolT_egg < 1.0
  ) |> 
  select(
    fish_id, sex, `mid-eye-fork-length`, n_eggs, egg_mass_g, avg_egg_mass_g,
    total_egg_mass_g, est_pre_hardened_egg_mass_g, est_fecund_n, nmolT_g, 
    deficient_conc, nmolT_egg, deficient_totalT, mate1, mate2, mate_group, 
    whatman_card_number, card_position, comments
  )

#write_csv(dipac_females, file = "data/processed/dipac_females.csv")
#write_rds(dipac_females, file = "data/processed/dipac_females.rds")

# Define BKD fish from Steve's email
bkd_fish <- c("f13", "f34")

dipac_females_bkd_trimmed <- dipac_females |> 
  filter(!(fish_id %in% bkd_fish))


# Summary stats ----------------------------------------------------------------
summary(dipac_females_bkd_trimmed[c(
  "mid-eye-fork-length", "avg_egg_mass_g", "nmolT_g", "nmolT_egg",
  "deficient_conc", "deficient_totalT", "est_fecund_n"
)])

## Concentration ---------------------------------------------------------------
eggT_conc <- dipac_females_bkd_trimmed$nmolT_g
eggT_conc_breaks <- quantile(eggT_conc, probs = seq(0, 1, 0.25), na.rm = TRUE)

eggT_conc_quartile_summary <- dipac_females_bkd_trimmed |> 
  mutate(
    quartile = cut(
      eggT_conc,
      eggT_conc_breaks,
      labels = c("Q1 (Lowest 25%)", "Q2 (25-50%)", "Q3 (50-75%)", "Q4 (Highest 25%)"),
      include.lowest = TRUE
    )
  ) |> 
  group_by(quartile) |> 
  summarise(
    n_individuals = n(),
    min_thiamine = min(nmolT_g, na.rm = TRUE),
    max_thiamine = max(nmolT_g, na.rm = TRUE),
    count_below_5_nmol_T_g = sum(nmolT_g < 5.0, na.rm = TRUE)
  ) |> 
  mutate(thiamine_range = paste(round(min_thiamine, 2), "-", round(max_thiamine, 2))) |> 
  select(
    Quartile = quartile,
    `Thiamine Range (nmol/g)` = thiamine_range,
    `Total Individuals` = n_individuals,
    `Count < 5.0 nmol/g` = count_below_5_nmol_T_g
  )

eggT_conc_quartile_summary 


### Take bottom and top 16 fish ------------------------------------------------
bottom_16 <- dipac_females_bkd_trimmed |> 
  arrange(desc(nmolT_g)) |> 
  slice_tail(n = 16)

summary(bottom_16[c(
  "mid-eye-fork-length", "avg_egg_mass_g", "nmolT_g", "nmolT_egg",
  "deficient_conc", "deficient_totalT", "est_fecund_n"
)])

top_16 <- dipac_females_bkd_trimmed |> 
  arrange(desc(nmolT_g)) |> 
  slice_head(n = 16)

summary(top_16[c(
  "mid-eye-fork-length", "avg_egg_mass_g", "nmolT_g", "nmolT_egg",
  "deficient_conc", "deficient_totalT", "est_fecund_n"
)])

top_bottom_16 <- rbind(top_16, bottom_16) |> 
  mutate(group = factor(if_else(nmolT_g > 5, "replete", "deficient")))

str(top_bottom_16)

summary_top_bottom_16 <- top_bottom_16 |> 
  group_by(group) |> 
  summarise(
    n = n(),
    mean = round(mean(nmolT_g), 1),
    sd = round(sd(nmolT_g), 1),
    min = round(min(nmolT_g), 1),
    max = round(max(nmolT_g), 1),
    est_eggs = round(sum(est_fecund_n), 0)
  )

summary_top_bottom_16

# Exploratory plots ------------------------------------------------------------

## Concentration Histograms ----------------------------------------------------
nmolT_g_IQR <- IQR(dipac_females_bkd_trimmed$nmolT_g)
nmolT_g_n <- length(dipac_females_bkd_trimmed$nmolT_g)

nmolT_g_binwidth <- 2 * nmolT_g_IQR / (nmolT_g_n^(1/3))
bins_nmolT_g <- round(1 + log2(nmolT_g_n), 0)


dipac_conc_histogram <- ggplot(dipac_females_bkd_trimmed, aes(x = nmolT_g)) +
  geom_histogram(
    aes(),
    binwidth = nmolT_g_binwidth,
    fill = "salmon",
    color = "black"
    ) +
  labs(title = "Distribution of Thiamine per Gram",
       x = "Thiamine (nmol/g)", 
       y = "Count") +
  theme_minimal()

dipac_conc_histogram

dipac_conc_density <- ggplot(dipac_females_bkd_trimmed, aes(x = nmolT_g)) +
  geom_histogram(
    aes(y = after_stat(density)),
    binwidth = nmolT_g_binwidth,
    fill = "salmon",
    color = "black"
  ) +
  geom_density(color = "red", linewidth = 1) +
  labs(title = "Distribution of Thiamine per Gram",
       x = "Thiamine (nmol/g)", 
       y = "Density") +
  theme_minimal()

dipac_conc_density

## Per egg Histograms ----------------------------------------------------
nmolT_egg_IQR <- IQR(dipac_females_bkd_trimmed$nmolT_egg)
nmolT_egg_n <- length(dipac_females_bkd_trimmed$nmolT_egg)

nmolT_egg_binwidth <- 2 * nmolT_egg_IQR / (nmolT_egg_n^(1/3))
bins_nmolT_egg <- round(1 + log2(nmolT_egg_n), 0)


dipac_egg_histogram <- ggplot(dipac_females_bkd_trimmed, aes(x = nmolT_egg)) +
  geom_histogram(
    aes(),
    binwidth = nmolT_egg_binwidth,
    fill = "#96DED1",
    color = "black"
  ) +
  labs(title = "Distribution of Thiamine per Egg",
       x = "Thiamine (nmol/egg)", 
       y = "Count") +
  theme_minimal()

dipac_egg_histogram

dipac_egg_density <- ggplot(dipac_females_bkd_trimmed, aes(x = nmolT_egg)) +
  geom_histogram(
    aes(y = after_stat(density)),
    binwidth = nmolT_egg_binwidth,
    fill = "#96DED1",
    color = "black"
  ) +
  geom_density(color = "red", linewidth = 1) +
  labs(title = "Distribution of Thiamine per Egg",
       x = "Thiamine (nmol/egg)", 
       y = "Density") +
  theme_minimal()

dipac_egg_density

## Combined histograms ---------------------------------------------------------

# Egg thiamine count

combined_conc <- dipac_conc_histogram + dipac_egg_histogram +
  plot_layout(axes = "collect")

combined_conc

# Egg thiamine density 
combined_density <- dipac_conc_density + dipac_egg_density +
  plot_layout(axes = "collect")

combined_density

# Female size vs. egg thiamine -------------------------------------------------

## Concentration ---------------------------------------------------------------

egg_conc_lm <- lm(nmolT_g ~ `mid-eye-fork-length`, data = dipac_females_bkd_trimmed)

summary(egg_conc_lm)

# no significance, p-value = 0.9412

egg_conc_length <- ggplot(dipac_females_bkd_trimmed, 
                          aes(x = `mid-eye-fork-length`,
                              y = nmolT_g)) +
  geom_point(color = "salmon") +
  stat_poly_eq(use_label(c("r2", "p"))) +
  labs(x = "Mid-Eye-Fork-Length (mm)",
       y = "Thiamine (nmol/g)")

egg_conc_length

## Per Egg ---------------------------------------------------------------------

per_egg_lm <- lm(nmolT_egg ~ `mid-eye-fork-length`, data = dipac_females_bkd_trimmed)

summary(per_egg_lm)

# no significance, p-value = 0.9412

per_egg_length <- ggplot(dipac_females_bkd_trimmed, 
                          aes(x = `mid-eye-fork-length`,
                              y = nmolT_egg)) +
  geom_point(color = "#96DED1") +
  stat_poly_eq(use_label(c("r2", "p"))) +
  labs(x = "Mid-Eye-Fork-Length (mm)",
       y = "Thiamine (nmol/egg)")

per_egg_length

# Female length vs. egg size -----------------------------------------------------

length_egg_mass_lm <- lm(`mid-eye-fork-length` ~ avg_egg_mass_g, data = dipac_females)
summary(length_egg_mass_lm)

ggplot(data = dipac_females, aes(x = `mid-eye-fork-length`, y = avg_egg_mass_g)) +
  geom_point() +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "r2", "p"))) +
  labs(x = "Mid-Eye-Fork-Length (mm)",
       y = "Average Egg Mass (g)")

# Female length vs. estimated fecundity ----------------------------------------

length_fecundity_lm <- lm(est_fecund_n ~ `mid-eye-fork-length`, data = dipac_females)
summary(length_fecundity_lm)

ggplot(data = dipac_females, aes(x = `mid-eye-fork-length`, y = est_fecund_n)) +
  geom_point() +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "r2", "p")), coef.digits = 4) +
  labs(x = "Mid-Eye-Fork-Length (mm)",
       y = "Estimated Fecundity (eggs)")

# Egg thiamine concentration vs. estimated fecundity ---------------------------

thia_conc_fecundity_lm <- lm(est_fecund_n ~ nmolT_g, data = dipac_females)
summary(thia_conc_fecundity_lm)

ggplot(data = dipac_females, aes(x = nmolT_g, y = est_fecund_n)) +
  geom_point() +
  stat_poly_eq(use_label(c("r2", "p")), coef.digits = 4) +
  labs(x = "Egg Thiamine Concentration (nmol/g)",
       y = "Estimated Fecundity (eggs)")


# Total egg thiamine vs. estimated fecundity ---------------------------

total_thia_fecundity_lm <- lm(est_fecund_n ~ nmolT_egg, data = dipac_females)
summary(total_thia_fecundity_lm)

ggplot(data = dipac_females, aes(x = nmolT_egg, y = est_fecund_n)) +
  geom_point() +
  stat_poly_eq(use_label(c("r2", "p")), coef.digits = 4) +
  labs(x = "Total Egg Thiamine (nmol/egg)",
       y = "Estimated Fecundity (eggs)")
