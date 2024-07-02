
# - life-course transitions and political orientations ----------------------- #
# - data description: outcomes ----------------------------------------------- #

### note: this script describes the outcomes by analyzing their trajectory.

# ---------------------------------------------------------------------------- #

rm(list = ls()) # clean-up
pacman::p_load(
  tidyverse, purrr, furrr, broom, hrbrthemes)
theme_set(theme_ipsum_rc())

my_oka <- c("#EEBD64", "#C8D9F0", "#B4D3B2", "#414C66")
`%nin%` = Negate(`%in%`)

d <- readRDS("./data/data.rds") |>
  ## get unique observations
  distinct(survey, 
           variable, 
           pid,
           wave, 
           y) |>
  ## change `wave` to a normalized `time`
  mutate(time = scales::rescale(wave),
         .by = c("survey", "variable", "pid")) |>
  ## nest at the level of individuals
  nest(.by = c("survey", "variable", "pid"))

# --- ESTIMATE CHANGE

plan(multisession)
d <- d |>
  mutate(m = future_map(
    .x = data,
    .f = ~ lm(y ~ time, data = .) |> broom::tidy(),
    .progress = TRUE
  )) |>
  select(-data) |>
  unnest(m) |>
  filter(term == "time") |>
  select(survey, variable, pid, estimate) |>
  mutate(change = abs(estimate))

# --- DRAW THE DISTRIBUTION

png(
  "./figures/desc01_polchange.png",
  w = 8,
  h = 4,
  units = "in",
  res = 500
)
d |>
  filter(change < quantile(change, 0.99)) |>
  ggplot(aes(x = change, y = after_stat(ncount))) +
  geom_histogram(
    binwidth = 0.05,
    color = "black",
    fill = my_oka[1],
    alpha = 0.8
  ) +
  geom_vline(
    xintercept = d |> pull(change) |> median(), ## median = 0.14
    linetype = "dashed") +
  theme_ipsum_rc(grid = "XY", axis_title_size = 12) +
  labs(x = "Absolute Individual Slopes", y = "Relative Frequency")
dev.off()

# ---------------------------------------------------------------------------- #
