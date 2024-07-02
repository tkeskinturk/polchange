
# - life-course transitions and political orientations ----------------------- #
# - data description: selection ---------------------------------------------- #

### note: this script describes exogeneity by analyzing selection into D.

# ---------------------------------------------------------------------------- #

rm(list = ls()) # clean-up
pacman::p_load(
  tidyverse, purrr, furrr, broom, hrbrthemes, tidytext)
theme_set(theme_ipsum_rc())

my_oka <- c("#EEBD64", "#C8D9F0", "#B4D3B2", "#414C66")
`%nin%` = Negate(`%in%`)

d <- readRDS("./data/data.rds")

# --- DATA PREPS

d <- d |>
  filter(treatment == 0) |> ## pre-treatment
  mutate(ever = ifelse(wave_at_treated == 0, 0, 1)) |> ## ever treated
  summarize(ymean = mean(y),
            .by = c("survey", "variable", "events", "pid", "ever"))

# --- NEST AND ESTIMATE

plan(multisession)
d <- d |>
  nest(.by = c("survey", "variable", "events")) |>
  mutate(m = future_map(
    .x = data,
    .f = ~ lm(ymean ~ ever, data = .) |> broom::tidy(),
    .progress = TRUE
  )) |> 
  select(-data) |>
  unnest(m) |>
  filter(term == "ever") |>
  select(survey, variable, events, estimate, std.error)

# --- ADD LABELS

d <- d |>
  left_join(read_csv("./misc/var_labels.csv"),
            by = c("survey", 
                   "variable" = "item")) |> 
  mutate(events = factor(
    events,
    levels = c("nvmarr",
               "kids_any",
               "sep",
               "entry",
               "unemp",
               "retir"),
    labels = c("Marriage",
               "First-Parenthood",
               "Marriage Dissolution",
               "Entry to Labor",
               "Unemployment",
               "Retirement")
  ))

# --- DRAW DIFFERENCES

png(
  "./figures/desc02_selection.png",
  w = 8,
  h = 4,
  units = "in",
  res = 500
)
d |>
  ggplot(
    aes(x = tidytext::reorder_within(label,
                                     estimate,
                                     events),
        y = estimate,
        ymin = estimate - 1.96 * std.error,
        ymax = estimate + 1.96 * std.error)
  ) +
  facet_wrap(~events, nrow = 2, scales = "free_x") +
  theme_ipsum_rc(grid = "Y",
                 axis_title_size = 12) +
  theme(axis.text.x = element_blank()) +
  scale_y_continuous(limits = c(-0.225, 0.225),
                     breaks = c(-0.2, 0, 0.2)) +
  geom_hline(yintercept = 0, linewidth = 0.25) +
  geom_linerange(col = my_oka[1],
                 alpha = 0.5,
                 linewidth = 0.75) +
  geom_point(size = 0.25) +
  labs(x = "Political Orientations", 
       y= "Difference in Pre-Treatment Y")
dev.off()

# ---------------------------------------------------------------------------- #
