
# - life-course transitions and political beliefs ---------------------------- #
# - heterogeneity in age at treatment ---------------------------------------- #

### note: this script
###       (a) disaggregates the sample into three age-groups,
###       (b) estimates age-group specific treatment effects for transitions,
###       (c) aggregates ATTs and present the comparable distributions.

# ---------------------------------------------------------------------------- #

rm(list = ls()) # clean-up
pacman::p_load(
  tidyverse, did, lme4, purrr, furrr, broom, hrbrthemes, Matrix)
theme_set(theme_ipsum_rc())

my_oka <- c("#EEBD64", "#C8D9F0", "#B4D3B2", "#414C66")
`%nin%` = Negate(`%in%`)

d <- readRDS("./data/data.rds") # dataframe

# PART 1: AGE GROUPS --------------------------------------------------------- #

# --- SUBSET
d <- d |>
  
  ## assign non-treated units to buckets
  mutate(age_median =
           median(age), .by = c("survey",
                                "events",
                                "pid",
                                "variable")) |>
  mutate(ages_at_treated =
           ifelse(ages_at_treated != 0,
                  ages_at_treated,
                  age_median)) |> ## assign positions
  
  ## reasonable bins!
  left_join(
    d |>
      filter(ages_at_treated != 0) |>
      distinct(survey, events, pid, ages_at_treated) |>
      summarize(
        lower = quantile(ages_at_treated, 1 / 3),
        upper = quantile(ages_at_treated, 2 / 3),
        .by = c("survey", "events")
      ),
    by = c("survey", "events")
  ) |>
  
  ## reasonable ages!
  mutate(
    age_groups =
      case_when(
        ages_at_treated <= lower ~ "Early",
        ages_at_treated >= upper ~ "Late",
        TRUE ~ "Modal"
      ),
    age_groups = factor(age_groups,
                        levels = c("Early",
                                   "Modal",
                                   "Late")))

# --- FILTER
d <- d |> 
  filter(is.na(age) == FALSE) |> ## just checking
  nest(.by = c("survey",
               "events",
               "age_groups",
               "variable")) |> 
  ## drop low-count cases
  mutate(counts = 
           map_dbl(.x = data, .f = ~nrow(.))) |>
  mutate(counts = ifelse(counts < 250, 1, 0)) |>
  filter(counts != 1) |> 
  mutate(n = n(), .by = c("survey", "variable", "events")) |> 
  filter(n == 3)

# PART 2: ESTIMATE DID MODEL ------------------------------------------------- #

# --- ESTIMATE THE DID

## estimation function
did_function <- function(df) {
  did_out <-
    att_gt(data = df,
           yname = "y", tname = "wave", idname = "pid",
           allow_unbalanced_panel = TRUE,
           gname = "wave_at_treated",
           xformla = ~female,
           base_period = "universal",
           control = "notyettreated", est_method = "ipw")
  return(did_out)
}

## estimates
set.seed(112358) ## set seed for the bootstrapped intervals
plan(multisession)
d <- d |>
  mutate(model = 
           future_map(.x = data, 
                      .f = did_function,
                      .options = furrr_options(seed = TRUE),
                      .progress = TRUE))

# PART 3: AGGREGATE THE ATTs ------------------------------------------------- #

# --- AGGREGATED ATTs
d <- d |>
  mutate(att =
           purrr::map(
             .x = model,
             .f = purrr::safely
             ( ~ aggte(., type = "simple",
                       na.rm = TRUE)),
             .progress = TRUE
           ))

# --- TIDY THE ESTIMATIONS
d <- d |>
  mutate(tidy = map(
    .x = att,
    .f = purrr::possibly( ~ tidy(.$result),
                          "Can't Retireve")
  )) |>
  unnest(tidy) |>
  ## add information for items
  left_join(read_csv("./misc/var_labels.csv"),
            by = c("survey", "variable" = "item")) |>
  ## cosmetic changes
  mutate(survey = factor(
    survey,
    levels = c("bhps",
               "shp",
               "soep",
               "ukhls"),
    labels = c("BHPS",
               "SHP",
               "SOEP",
               "UKHLS"))) |>
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
               "Retirement"))) |> 
  select(survey,
         events,
         variable,
         label,
         age_groups,
         estimate,
         sd = std.error) |> 
  mutate(age_groups = factor(age_groups, levels = c("Early",
                                                    "Modal",
                                                    "Late")))

# PART 4: DRAW THE ESTIMATES ------------------------------------------------- #

# --- GET THE REFERENCE
reference <- readRDS("./data/outputs/models_did_ATT.rds")

# --- ORGANIZE THE DATA
d <- d |> 
  left_join(reference |> 
              select(survey,
                     events,
                     variable,
                     reference = estimate),
            by = c("survey",
                   "events",
                   "variable"))

png(
  "./figures/did03_heterogeneityAge.png",
  w = 10,
  h = 4,
  units = "in",
  res = 500
)
d |>
  ## get the absolutes
  mutate(estimate = abs(estimate), reference = abs(reference)) |>
  
  ggplot() +
  
  ## age groups
  geom_histogram(
    aes(
      x = estimate,
      y = after_stat(ncount),
      color = age_groups,
      fill = age_groups
    ),
    stat = "bin",
    bins = 30,
    linewidth = 0.5,
    alpha = 0.75
  ) +
  
  ## reference stuff
  geom_step(
    aes(x = reference,
        y = after_stat(ncount)),
    stat = "bin",
    bins = 30,
    linewidth = 0.5,
    col = "gray30",
    direction = "mid"
  ) +
  
  ## spread out for groups
  facet_wrap(~ age_groups, nrow = 1) +
  
  ## theme stuff
  theme_ipsum_rc(grid = "XY",
                 axis_title_size = 12) +
  theme(legend.position = "none") +
  
  ## nicely color stuff
  scale_fill_manual(
    values = my_oka[1:3]) +
  scale_color_manual(
    values = my_oka[1:3]) +
  
  ## nice labels
  labs(x = "DID Estimates", y = "Relative Frequency")
dev.off()

# ---------------------------------------------------------------------------- #
