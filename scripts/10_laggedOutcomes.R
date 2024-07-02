
# - life-course transitions and political beliefs ---------------------------- #
# - conditioning on lagged outcomes ------------------------------------------ #

### note: this script
###       (a) calculates, for each id x item, a central tendency,
###       (b) estimates DID models by including central tendency in PS model,
###       (c) aggregates ATTs and present the comparable distributions.

# ---------------------------------------------------------------------------- #

rm(list = ls()) # clean-up
pacman::p_load(
  tidyverse, did, purrr, furrr, broom, hrbrthemes, Matrix)
theme_set(theme_ipsum_rc())

my_oka <- c("#EEBD64", "#C8D9F0", "#B4D3B2", "#414C66")
`%nin%` = Negate(`%in%`)

d <- readRDS("./data/data.rds") # dataframe

# PART 1: LAGGED OUTCOMES ---------------------------------------------------- #

# --- CENTRAL TENDENCY
tendency <- d |>
  
  ## drop "treated" times
  filter(treatment == 0) |> 
  
  ## get unique pid x item observations
  distinct(survey,
           variable,
           pid,
           wave,
           y) |>
  
  ## extract one's central tendency
  summarize(meany = mean(y), .by = c("survey",
                                     "variable",
                                     "pid"))

# --- MERGE AND BE HAPPY!
d <- d |> 
  left_join(tendency, by = c("survey",
                             "variable",
                             "pid"))

# PART 2: ESTIMATE DID MODEL ------------------------------------------------- #

# --- ORGANIZE THE DATA
d <- d |>
  nest(.by = c("survey",
               "events",
               "variable"))
l <- d$data ## pull the list of dataframes to loop over

# --- ESTIMATE THE DID

## estimation function
did_function <- function(df) {
  did_out <-
    att_gt(data = df,
           yname = "y", tname = "wave", idname = "pid",
           allow_unbalanced_panel = TRUE,
           gname = "wave_at_treated",
           xformla = ~meany + female + edu + age + I(age^2) + cohort + migr,
           base_period = "universal",
           control = "notyettreated", est_method = "ipw")
  return(did_out)
}

## estimates
set.seed(1123) ## set seed for the bootstrapped intervals
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
         estimate,
         sd = std.error)

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
  "./figures/supp04_specificationsD.png",
  w = 8,
  h = 6,
  units = "in",
  res = 500
)
d |>
  ggplot(aes(x = estimate, y = reference)) +
  geom_point(size = 0.5, col = my_oka[4], alpha = 0.75) +
  geom_abline(
    intercept = 0,
    slope = 1,
    color = "black",
    linetype = "dashed"
  ) +
  labs(
    subtitle =
      "Conditional Estimates with Lagged Outcomes",
    caption =
      "Pearson Correlation = 0.99",
    x = "Main Estimates",
    y = "Comparison Estimates"
  ) + xlim(-0.12, 0.12) + ylim(-0.12, 0.12)
dev.off()

# ---------------------------------------------------------------------------- #
