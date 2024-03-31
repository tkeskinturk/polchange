
# - life-course transitions and political beliefs ---------------------------- #
# - data descriptions -------------------------------------------------------- #

### note: this script 
###       (a) turns the variable list into a pretty latex format,
###       (b) generates several descriptive tables and plots for the study.
###       ~~~ note that I modify the latex outputs in the `rmd` files.

# ---------------------------------------------------------------------------- #

rm(list = ls()) # clean-up
pacman::p_load(
  tidyverse, haven, did, hrbrthemes, patchwork, panelView)
theme_set(theme_ipsum_rc())

my_oka <- c("#EEBD64", "#C8D9F0", "#B4D3B2", "#414C66")
`%nin%` = Negate(`%in%`)

# dataframe
d <- readRDS("./data/data.rds")

# variable labels
vars <- read_csv("./misc/var_labels.csv")

# PART 1: AGE AT TREATMENT --------------------------------------------------- #

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
  "./figures/supp_ageheteroA.png",
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

# PART 2: ITEMS -------------------------------------------------------------- #

print(
  xtable::xtable(
    vars |>
      ## organize
      select(survey, label, desc, item) |>
      ## filter
      filter(label %nin%
               c(
                 vars |> pull(label) |>
                   str_subset("public expenses")
               )) |>
      filter(desc %nin%
               c(
                 vars |> pull(desc) |>
                   str_subset("trustworthiness\\:")
               )) |>
      ## prettify
      mutate(desc =
               str_to_sentence(desc)) |>
      rename(
        Survey = survey,
        Labels = label,
        Description = desc,
        Item = item,
      ),
    caption = "Table X"
  ),
  include.rownames = FALSE,
  file = "./tables/table_outcomes.txt"
)

# PART 3: STAGGERED ---------------------------------------------------------- #

# generate an idealized example
set.seed(11235)
staggered <-
  ## build the data
  build_sim_dataset(reset.sim(n = 100, 
                              time.periods = 10)) |>
  ## assign treatment status
  mutate(treatment =
           ifelse(period < G, 0, 1))

png(
  "./figures/supp_staggered-ex.png",
  w = 8,
  h = 5,
  units = "in",
  res = 500
)
panelview(
  1 ~ treatment,
  data = staggered, index = c("id", "period"),
  by.timing = TRUE, collapse.history = "TRUE",
  display.all = TRUE, gridOff = FALSE,
  legend.labs = c("Under Control", "Under Treatment"),
  color = c(my_oka[1], my_oka[4]), cex.main = 0) +
  theme_ipsum_rc(axis_title_size = 12,
                 plot_margin = margin(0, 15, 15, 15)) +
  theme(legend.position = "top", legend.title=element_blank(),
        axis.text.y = element_blank()) +
  labs(x = "Wave", y = "Groups", title = "")
dev.off()

# ---------------------------------------------------------------------------- #
