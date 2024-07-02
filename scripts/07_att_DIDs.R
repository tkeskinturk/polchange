
# - life-course transitions and political beliefs ---------------------------- #
# - present treatment effects ------------------------------------------------ #

### note: this script
###       (a) examines aggregated ATTs for each model across events,
###       (b) exports tables for use in the manuscript.

# ---------------------------------------------------------------------------- #

rm(list = ls()) # clean-up
pacman::p_load(
  tidyverse, did, purrr, broom, hrbrthemes, patchwork, xtable)
theme_set(theme_ipsum_rc())

my_oka <- c("#EEBD64", "#C8D9F0", "#B4D3B2", "#414C66")
`%nin%` = Negate(`%in%`)

# --- DATA FRAMES
d <- readRDS("./data/outputs/models_did_ATT.rds")

# PART 1: ITEM ANALYSES: FAMILY ---------------------------------------------- #

# --- MARRIAGE
p1 <- d |>
  ## preprocess
  filter(events %in% c("Marriage")) |>
  mutate(label = factor(label)) |>
  ## draw
  ggplot(aes(
    x = reorder(label, estimate),
    y = estimate,
    ymin = estimate - 1.96 * sd,
    ymax = estimate + 1.96 * sd
  )) +
  geom_hline(yintercept = 0,
             col = "black",
             linewidth = 0.25) +
  geom_linerange(col = my_oka[1],
                 alpha = 0.5,
                 linewidth = 1.5) +
  geom_point() +
  scale_y_continuous(limits = c(-0.2, 0.2)) +
  labs(y = "Difference in Differences Estimates",
       x = "",
       subtitle = "Marriage") +
  theme_ipsum_rc(
    grid = "XY",
    axis_title_size = 12,
    plot_margin =
      margin(10, 10, 10, 10)
  ) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# --- FIRST-PARENTHOOD
p2 <- d |>
  ## preprocess
  filter(events %in% c("First-Parenthood")) |>
  mutate(label = factor(label)) |>
  ## draw
  ggplot(aes(
    x = reorder(label, estimate),
    y = estimate,
    ymin = estimate - 1.96 * sd,
    ymax = estimate + 1.96 * sd
  )) +
  geom_hline(yintercept = 0,
             col = "black",
             linewidth = 0.25) +
  geom_linerange(col = my_oka[1],
                 alpha = 0.5,
                 linewidth = 1.5) +
  geom_point() +
  scale_y_continuous(limits = c(-0.2, 0.2)) +
  labs(y = "",
       x = "",
       subtitle = "First-Parenthood") +
  theme_ipsum_rc(
    grid = "XY",
    axis_title_size = 12,
    plot_margin =
      margin(10, 10, 10, 10)
  ) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# --- DISSOLUTION
p3 <- d |>
  ## preprocess
  filter(events %in% c("Marriage Dissolution")) |>
  mutate(label = factor(label)) |>
  ## draw
  ggplot(aes(
    x = reorder(label, estimate),
    y = estimate,
    ymin = estimate - 1.96 * sd,
    ymax = estimate + 1.96 * sd
  )) +
  geom_hline(yintercept = 0,
             col = "black",
             linewidth = 0.25) +
  geom_linerange(col = my_oka[1],
                 alpha = 0.5,
                 linewidth = 1.5) +
  geom_point() +
  scale_y_continuous(limits = c(-0.2, 0.2)) +
  labs(y = "",
       x = "",
       subtitle = "Marriage Dissolution") +
  theme_ipsum_rc(
    grid = "XY",
    axis_title_size = 12,
    plot_margin =
      margin(10, 10, 10, 10)
  ) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

png(
  "./figures/did01_outcomesA.png",
  w = 12,
  h = 15,
  units = "in",
  res = 500
)
(p1 / p2) / p3
dev.off()

# PART 2: ITEM ANALYSES: WORK ------------------------------------------------ #

# --- ENTRY TO LABOR
p1 <- d |>
  ## preprocess
  filter(events %in% c("Entry to Labor")) |>
  mutate(label = factor(label)) |>
  ## draw
  ggplot(aes(
    x = reorder(label, estimate),
    y = estimate,
    ymin = estimate - 1.96 * sd,
    ymax = estimate + 1.96 * sd
  )) +
  geom_hline(yintercept = 0,
             col = "black",
             linewidth = 0.25) +
  geom_linerange(col = my_oka[1],
                 alpha = 0.5,
                 linewidth = 1.5) +
  geom_point() +
  scale_y_continuous(limits = c(-0.2, 0.2)) +
  labs(y = "Difference in Differences Estimates",
       x = "",
       subtitle = "Entry to Labor") +
  theme_ipsum_rc(
    grid = "XY",
    axis_title_size = 12,
    plot_margin =
      margin(10, 10, 10, 10)
  ) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# --- UNEMPLOYMENT
p2 <- d |>
  ## preprocess
  filter(events %in% c("Unemployment")) |>
  mutate(label = factor(label)) |>
  ## draw
  ggplot(aes(
    x = reorder(label, estimate),
    y = estimate,
    ymin = estimate - 1.96 * sd,
    ymax = estimate + 1.96 * sd
  )) +
  geom_hline(yintercept = 0,
             col = "black",
             linewidth = 0.25) +
  geom_linerange(col = my_oka[1],
                 alpha = 0.5,
                 linewidth = 1.5) +
  geom_point() +
  scale_y_continuous(limits = c(-0.2, 0.2)) +
  labs(y = "",
       x = "",
       subtitle = "Unemployment") +
  theme_ipsum_rc(
    grid = "XY",
    axis_title_size = 12,
    plot_margin =
      margin(10, 10, 10, 10)
  ) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# --- RETIREMENT
p3 <- d |>
  ## preprocess
  filter(events %in% c("Retirement")) |>
  mutate(label = factor(label)) |>
  ## draw
  ggplot(aes(
    x = reorder(label, estimate),
    y = estimate,
    ymin = estimate - 1.96 * sd,
    ymax = estimate + 1.96 * sd
  )) +
  geom_hline(yintercept = 0,
             col = "black",
             linewidth = 0.25) +
  geom_linerange(col = my_oka[1],
                 alpha = 0.5,
                 linewidth = 1.5) +
  geom_point() +
  scale_y_continuous(limits = c(-0.2, 0.2)) +
  labs(y = "",
       x = "",
       subtitle = "Retirement") +
  theme_ipsum_rc(
    grid = "XY",
    axis_title_size = 12,
    plot_margin =
      margin(10, 10, 10, 10)
  ) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

png(
  "./figures/did02_outcomesB.png",
  w = 12,
  h = 15,
  units = "in",
  res = 500
)
(p1 / p2) / p3
dev.off()

# PART 3: EXPORT ESTIMATES --------------------------------------------------- #

# table 1:
# the individual effect sizes
print(
  xtable::xtable(
    d |>
      select(survey,
             label,
             events,
             estimate) |>
      pivot_wider(names_from = events, values_from = estimate) |>
      ## reorder columns
      select(
        survey, label,
        #### events
        Marriage,
        `First-Parenthood`,
        `Marriage Dissolution`,
        `Entry to Labor`,
        Unemployment,
        Retirement), caption = "Table X"),
  include.rownames = FALSE,
  file = "./tables/table_estimates.txt",
  tabular.environment = "longtable",
  floating = FALSE
)

# PART 4: ALTERNATIVE DIDs 1 ------------------------------------------------- #

# --- PANEL 1
p1 <- d |>
  ggplot(aes(x = estimate, y = estimate_alt1)) +
  geom_point(size = 0.5, col = my_oka[4], alpha = 0.75) +
  geom_abline(
    intercept = 0,
    slope = 1,
    color = "black",
    linetype = "dashed"
  ) +
  labs(
    subtitle =
      "Conditional Estimates with Never-Treated",
    caption =
      "Pearson Correlation = 0.91",
    x = "Main Estimates",
    y = "Comparison Estimates"
  ) + xlim(-0.12, 0.12) + ylim(-0.12, 0.12)

# --- PANEL 2
p2 <- d |>
  ggplot(aes(x = estimate, y = estimate_alt2)) +
  geom_point(size = 0.5, col = my_oka[4], alpha = 0.75) +
  geom_abline(
    intercept = 0,
    slope = 1,
    color = "black",
    linetype = "dashed"
  ) +
  labs(
    subtitle =
      "Conditional Estimates with Covariate Interactions",
    caption =
      "Pearson Correlation = 0.99",
    x = "Main Estimates",
    y = "Comparison Estimates"
  ) + xlim(-0.12, 0.12) + ylim(-0.12, 0.12)

# --- EXPORT
png(
  "./figures/supp01_specificationsA.png",
  w = 12,
  h = 6,
  units = "in",
  res = 500
)
p1 + p2
dev.off()

# PART 4: ALTERNATIVE DIDs 2 ------------------------------------------------- #

# --- PANEL 1
p1 <- d |>
  ggplot(aes(x = estimate, y = estimate_min5)) +
  geom_point(size = 0.5, col = my_oka[4], alpha = 0.75) +
  geom_abline(
    intercept = 0,
    slope = 1,
    color = "black",
    linetype = "dashed"
  ) +
  labs(
    subtitle =
      "Estimates with A 5-Year Window",
    caption =
      "Pearson Correlation = 0.95",
    x = "Main Estimates",
    y = "Comparison Estimates"
  ) + xlim(-0.12, 0.12) + ylim(-0.12, 0.12)

# --- PANEL 2
p2 <- d |>
  ggplot(aes(x = estimate, y = estimate_min10)) +
  geom_point(size = 0.5, col = my_oka[4], alpha = 0.75) +
  geom_abline(
    intercept = 0,
    slope = 1,
    color = "black",
    linetype = "dashed"
  ) +
  labs(
    subtitle =
      "Estimates with A 10-Year Window",
    caption =
      "Pearson Correlation = 0.99",
    x = "Main Estimates",
    y = "Comparison Estimates"
  ) + xlim(-0.12, 0.12) + ylim(-0.12, 0.12)

# --- EXPORT
png(
  "./figures/supp02_specificationsB.png",
  w = 12,
  h = 6,
  units = "in",
  res = 500
)
p1 + p2
dev.off()

# PART 5: ALTERNATIVE DIDs 3 ------------------------------------------------- #

png(
  "./figures/supp03_specificationsC.png",
  w = 8,
  h = 6,
  units = "in",
  res = 500
)
d |>
  ggplot(aes(x = estimate, y = estimate_alt3)) +
  geom_point(size = 0.5, col = my_oka[4], alpha = 0.75) +
  geom_abline(
    intercept = 0,
    slope = 1,
    color = "black",
    linetype = "dashed"
  ) +
  labs(
    subtitle =
      "Estimates with 1-Year Anticipation",
    caption =
      "Pearson Correlation = 0.89",
    x = "Main Estimates",
    y = "Comparison Estimates"
  ) + xlim(-0.12, 0.12) + ylim(-0.12, 0.12)
dev.off()

# ---------------------------------------------------------------------------- #
