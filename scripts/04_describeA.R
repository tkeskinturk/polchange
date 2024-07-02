
# - life-course transitions and political orientations ----------------------- #
# - data description: age at treatment --------------------------------------- #

### note: this script describes treatment by plotting age at first treatment.

# ---------------------------------------------------------------------------- #

rm(list = ls()) # clean-up
pacman::p_load(
  tidyverse, haven, did, hrbrthemes, patchwork, panelView)
theme_set(theme_ipsum_rc())

my_oka <- c("#EEBD64", "#C8D9F0", "#B4D3B2", "#414C66")
`%nin%` = Negate(`%in%`)

d <- readRDS("./data/data.rds")
vars <- read_csv("./misc/var_labels.csv")

# --- ORGANIZE DATA

d <- d |>
  ## survey
  mutate(survey = factor(
    survey,
    levels = c("bhps",
               "shp",
               "soep",
               "ukhls"),
    labels = c("BHPS",
               "SHP",
               "SOEP",
               "UKHLS")
  )) |>
  ## events
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
  )) |>
  ## constrain age to minimum
  mutate(age = min(age), .by = c("survey",
                                 "events",
                                 "variable",
                                 "pid")) |> 
  ## get distinct stuff
  distinct(survey, events, variable, pid, .keep_all = TRUE) |> 
  select(survey,
         events,
         variable,
         pid,
         t_time = 
           wave_at_treated,
         a_time =
           ages_at_treated,
         female, age, cohort, edu, migr) |> 
  ## note treatment status
  mutate(treatment = ifelse(t_time == 0,
                            0,
                            1))

# --- DRAW DISTRIBUTIONS

png(
  "./figures/desc03_ageattreatment.png",
  w = 8,
  h = 4,
  units = "in",
  res = 500
)
d |>
  filter(a_time != 0) |>
  summarize(a_time = min(a_time),
            .by = c("survey", "events", "pid")) |>
  ggplot(aes(x = a_time)) +
  geom_histogram(
    binwidth = 5,
    color = "black",
    fill = my_oka[1],
    alpha = 0.8
  ) +
  facet_wrap(~ events, nrow = 2, scales = "free_y") +
  theme_ipsum_rc(grid = "Xx", axis_title_size = 12) +
  labs(x = "Age at Treatment",
       y = "Relative Frequency") +
  theme(axis.text.y = element_blank())
dev.off()

# ---------------------------------------------------------------------------- #
