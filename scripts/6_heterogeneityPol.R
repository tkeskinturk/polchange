
# - life-course transitions and political beliefs ---------------------------- #
# - heterogeneity in interest ------------------------------------------------ #

### note: this script
###       (a) disaggregates the sample into three interest-groups,
###       (b) estimates interest specific treatment effects for transitions,
###       (c) aggregates ATTs and present the comparable distributions.

# ---------------------------------------------------------------------------- #

rm(list = ls()) # clean-up
pacman::p_load(
  tidyverse, did, lme4, purrr, broom, parallel, hrbrthemes, Matrix)
theme_set(theme_ipsum_rc())

my_oka <- c("#EEBD64", "#C8D9F0", "#B4D3B2", "#414C66")
`%nin%` = Negate(`%in%`)

d <- readRDS("./data/data.rds") # dataframe

# PART 1: POLITICAL INTEREST ------------------------------------------------- #

# --- GET INTEREST
d_interest <-
  
  ## organize sample
  d |> filter(variable %in% c("vote6",
                              "pp01",
                              "plh0007")) |>
  filter(treatment == 0) |>
  distinct(survey, pid, wave, y) |>
  
  ## center time at respondent midpoint
  mutate(
    wave_up = max(wave),
    wave_lo = min(wave),
    .by = c("survey", "pid")
  ) |>
  mutate(time =
           wave - (wave_up + wave_lo) / 2,
         .by = c("survey", "pid")) |>
  mutate(time = time / 10) |>
  
  ## estimate growth-curve models
  nest(.by = "survey") |>
  mutate(model = map(.x = data,
                     .f = ~ lmer(y ~ 1 + time + (1 + time |
                                                   pid),
                                 data = .))) |>
  ## extract the estimates
  mutate(coefs = purrr::map(
    .x = model,
    .f = ~ tibble(
      pid = coef(.)$pid |> rownames() |> as.integer(),
      int = coef(.)$pid |> pull(`(Intercept)`) # baseline interest level
    )
  )) |>
  unnest(coefs) |> select(survey, pid, int)

# --- ORGANIZE THE MAIN DATAFRAME
d <- d |>
  ## append
  left_join(d_interest, by = c("survey", "pid")) |>
  filter(variable %nin% c("vote6", 
                          "pp01", 
                          "plh0007")) |>
  ## get 25% lower and 25% upper
  mutate(
    lower =
      quantile(int, 1 / 3, na.rm = TRUE),
    upper =
      quantile(int, 2 / 3, na.rm = TRUE),
    .by = c("survey", "variable", "events")
  ) |>
  mutate(interest =
           case_when(int <= lower ~ "Lower",
                     int >= upper ~ "Upper",
                     TRUE ~ "Middle"))

# PART 2: ESTIMATE DID MODEL ------------------------------------------------- #

# --- PREPARE THE DATA
d <- d |> nest(.by = c("survey",
                       "interest",
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
           xformla = 
             ~female + edu + age + I(age^2) + cohort + migr,
           base_period = "universal",
           control = "notyettreated", est_method = "ipw")
  return(did_out)
}

## estimates
set.seed(112358) ## set seed for the bootstrapped intervals
estimates <- mclapply(l,
                      did_function,
                      mc.cores = 7)
d <- d |> mutate(model = estimates) ## append models

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
         interest,
         estimate,
         sd = std.error) |> 
  mutate(interest = factor(interest, levels = c("Lower",
                                                "Middle",
                                                "Upper")))

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
  "./figures/supp_pol-interest.png",
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
      color = interest,
      fill = interest
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
  facet_wrap(~ interest, nrow = 1) +
  
  ## drop legends
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
