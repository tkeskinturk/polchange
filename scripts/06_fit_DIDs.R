
# - life-course transitions and political orientations ----------------------- #
# - estimate diff and diffs -------------------------------------------------- #

### note: this script 
###       (a) estimates difference-in-differences models,
###       (b) estimates alternative specifications of the main DID models,
###       (c) stores the full model estimates for later use.

# ---------------------------------------------------------------------------- #

rm(list = ls()) # clean-up
pacman::p_load(
  tidyverse, did, purrr, furrr, broom, parallel, Matrix)

# dataframe
d <- readRDS("./data/data.rds") |>
  nest(.by = c("survey", 
               "events",
               "variable")) ## nest everything

# PART 1: ESTIMATE DID ------------------------------------------------------- #

# --- ESTIMATION FUNCTION

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

# --- ESTIMATE DID MODELS

set.seed(1123) ## set seed for the bootstrapped intervals
plan(multisession)
d <- d |>
  mutate(model = 
           future_map(.x = data, 
                      .f = did_function,
                      .options = furrr_options(seed = TRUE),
                      .progress = TRUE))

# PART 2: ALTERNATIVES ------------------------------------------------------- #

# --- ALTERNATIVE 1
# --- conditional DID with `never treated`
sensitive_did1 <- function(df) {
  did_out <- 
    att_gt(data = df,
           yname = "y", tname = "wave", idname = "pid",
           allow_unbalanced_panel = TRUE,
           gname = "wave_at_treated",
           xformla = 
             ~female + age + I(age^2) + edu + cohort + migr,
           base_period = "universal",
           control = "nevertreated", est_method = "ipw")
  return(did_out)
}
set.seed(1123) ## set seed for the bootstrapped intervals
d <- d |>
  mutate(model_alt1 = 
           future_map(.x = data, 
                      .f = sensitive_did1,
                      .options = furrr_options(seed = TRUE),
                      .progress = TRUE))

# --- ALTERNATIVE 2
# --- conditional DID with new specs
sensitive_did2 <- function(df) {
  did_out <- 
    att_gt(data = df,
           yname = "y", tname = "wave", idname = "pid",
           allow_unbalanced_panel = TRUE,
           gname = "wave_at_treated",
           xformla = 
             ~female + age + I(age^2) + edu + cohort + migr +
             ## additional interactions
             female * age + female * cohort,
           base_period = "universal",
           control = "notyettreated", est_method = "ipw")
  return(did_out)
}
set.seed(1123) ## set seed for the bootstrapped intervals
d <- d |>
  mutate(model_alt2 = 
           future_map(.x = data, 
                      .f = sensitive_did2,
                      .options = furrr_options(seed = TRUE),
                      .progress = TRUE))

# --- ALTERNATIVE 3
# --- adding 1-year anticipation effects
sensitive_did3 <- function(df) {
  did_out <- 
    att_gt(data = df,
           yname = "y", tname = "wave", idname = "pid",
           allow_unbalanced_panel = TRUE,
           gname = "wave_at_treated",
           xformla = 
             ~female + age + I(age^2) + edu + cohort + migr,
           anticipation = 1,
           base_period = "universal",
           control = "notyettreated", est_method = "ipw")
  return(did_out)
}
set.seed(1123) ## set seed for the bootstrapped intervals
d <- d |>
  mutate(model_alt3 = 
           future_map(.x = data, 
                      .f = sensitive_did3,
                      .options = furrr_options(seed = TRUE),
                      .progress = TRUE))

# PART 3: STORE & REINITIALIZE ----------------------------------------------- #

# --- STORE
saveRDS(d |>
          select(survey,
                 events,
                 variable,
                 model,
                 model_alt1,
                 model_alt2,
                 model_alt3),
        file = "./data/outputs/models_did.rds")

# PART 4: CALCULATE ATTs ----------------------------------------------------- #

# --- AGGREGATED ATTs: MAIN MODEL
d <- d |>
  mutate(att =
           purrr::map(
             .x = model,
             .f = purrr::safely
             (~ aggte(., type = "simple",
                      na.rm = TRUE)),
             .progress = TRUE
           ))

# --- AGGREGATED ATTs: CONSTRAINED MODEL WITH 5 YEARS
d <- d |>
  mutate(att_min5 =
           purrr::map(
             .x = model,
             .f = purrr::safely
             (~ aggte(., type = "simple",
                      na.rm = TRUE,
                      min_e = -5,
                      max_e =  5)),
             .progress = TRUE
           ))

# --- AGGREGATED ATTs: CONSTRAINED MODEL WITH 10 YEARS
d <- d |>
  mutate(att_min10 =
           purrr::map(
             .x = model,
             .f = purrr::safely
             (~ aggte(., type = "simple",
                      na.rm = TRUE,
                      min_e = -10,
                      max_e =  10)),
             .progress = TRUE
           ))

# --- AGGREGATED ATTs: ALTERNATIVE MODEL 1
d <- d |>
  mutate(att_alt1 =
           purrr::map(
             .x = model_alt1,
             .f = purrr::safely
             (~ aggte(., type = "simple",
                      na.rm = TRUE)),
             .progress = TRUE
           ))

# --- AGGREGATED ATTs: ALTERNATIVE MODEL 2
d <- d |>
  mutate(att_alt2 =
           purrr::map(
             .x = model_alt2,
             .f = purrr::safely
             (~ aggte(., type = "simple",
                      na.rm = TRUE)),
             .progress = TRUE
           ))

# --- AGGREGATED ATTs: ALTERNATIVE MODEL 3
d <- d |>
  mutate(att_alt3 =
           purrr::map(
             .x = model_alt3,
             .f = purrr::safely
             (~ aggte(., type = "simple",
                      na.rm = TRUE)),
             .progress = TRUE
           ))

# PART 5: ORGANIZE AGGREGATED ATTs ------------------------------------------- #

# --- AGGREGATED ATTs: EXTRACT

m_ATT <- 
  bind_cols(
    ## bare bone
    d |> 
      select(survey, events, variable),
    ## tidy the estimates 1
    d |>
      mutate(
        tidy = map(
          .x = att,
          .f = purrr::possibly( ~ tidy(.$result),
                                "Can't Retireve")
        )) |> 
      unnest(tidy) |> 
      select(estimate, sd = std.error),
    ## tidy the estimates 2
    d |>
      mutate(
        tidy = map(
          .x = att_min5,
          .f = purrr::possibly( ~ tidy(.$result),
                                "Can't Retireve")
        )) |> 
      unnest(tidy) |> 
      select(estimate_min5 = estimate, 
             sd_min5 = std.error),
    ## tidy the estimates 3
    d |>
      mutate(
        tidy = map(
          .x = att_min10,
          .f = purrr::possibly( ~ tidy(.$result),
                                "Can't Retireve")
        )) |> 
      unnest(tidy) |> 
      select(estimate_min10 = estimate, 
             sd_min10 = std.error),    
    ## tidy the estimates 4
    d |>
      mutate(
        tidy = map(
          .x = att_alt1,
          .f = purrr::possibly( ~ tidy(.$result),
                                "Can't Retireve")
        )) |> 
      unnest(tidy) |> 
      select(estimate_alt1 = estimate, 
             sd_alt1 = std.error),
    ## tidy the estimates 5
    d |>
      mutate(
        tidy = map(
          .x = att_alt2,
          .f = purrr::possibly( ~ tidy(.$result),
                                "Can't Retireve")
        )) |> 
      unnest(tidy) |> 
      select(estimate_alt2 = estimate,
             sd_alt2 = std.error),
    ## tidy the estimates 5
    d |>
      mutate(
        tidy = map(
          .x = att_alt3,
          .f = purrr::possibly( ~ tidy(.$result),
                                "Can't Retireve")
        )) |> 
      unnest(tidy) |> 
      select(estimate_alt3 = estimate,
             sd_alt3 = std.error)
  )

# --- PRETTIFY

m_ATT <- m_ATT |>
  ## add information for items
  left_join(read_csv("./misc/var_labels.csv"),
            by = c("survey", "variable"="item")) |>
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
               "Retirement")))

saveRDS(m_ATT, 
        file = "./data/outputs/models_did_ATT.rds")

# ---------------------------------------------------------------------------- #
