
# - life-course transitions and political orientations ----------------------- #
# - prepare data ------------------------------------------------------------- #

### note: this script 
###       (a) gathers BHPS, SHP, SOEP and UKHLS data files,
###       (b) appends the CPF-generated files,
###       (c) generates the analytical sample by subsetting the full data.

# ---------------------------------------------------------------------------- #

rm(list = ls()) # clean-up
pacman::p_load(tidyverse, haven, psych)

# PART 1: SURVEY DATA -------------------------------------------------------- #

# --- BHPS

bhps <- 
  read_dta("./data/surveys/bhps/bhps.dta") |>
  zap_labels() |> # remove Stata labels
  mutate(wave = svyyear - min(svyyear) + 1) |>
  select(-kids_any) # gonna merge later

bhps <- bhps |>
  
  ## directional coding 
  ## (higher: conservative OR high in scale)
  mutate(
    opfamf = 5 - opfamf,
    opfamr = 5 - opfamr,
    opsoca = 5 - opsoca,
    opsocc = 5 - opsocc,
    oppolb = 5 - oppolb,
    trust = 3 - trust, 
    vote6 = 4 - vote6) |> 
  ## go long
  pivot_longer(
    cols = -c(pid, svyyear, wave),
    names_to = "variable",
    values_to = "y"
  ) |>
  ## drop if y empty
  filter(is.na(y) == FALSE) |>
  ## normalize
  mutate(y = (y - min(y)) / (max(y) - min(y)), .by = "variable") |>
  ## add survey identifier
  mutate(survey = "bhps") |>
  ## organize and save
  select(survey, variable, pid, wave, y)

# --- SHP

shp <- 
  read_dta("./data/surveys/shp/shp.dta") |>
  zap_labels() |> # remove Stata labels
  mutate(wave = svyyear - min(svyyear) + 1)

## spending scale
shp_pca <-
  pca(shp[, c("pp53",
              "pp54",
              "pp55",
              "pp56",
              "pp57",
              "pp58",
              "pp59",
              "pp60",
              "pp61",
              "pp62",
              "pp63")] |>
        mutate(across(
          everything(),
          ~ case_when(. == 1 ~ -1,
                      . == 2 ~  0,
                      . == 3 ~  1,
                      TRUE ~ NA_real_)
        )),
      missing = FALSE,
      nfactors = 2)
shp$spendgover <- shp_pca$scores[, 1]
shp$spendshare <- shp_pca$scores[, 2]

cor(shp$pp10, shp$spendgover, use = "pairwise",
    method = "spearman") # p = 0.28 with left-right identification
cor(shp$pp10, shp$spendshare, use = "pairwise",
    method = "spearman") # p = 0.51 with left-right identification

## organize
shp <- shp |>
  select(-c(pp53,
            pp54,
            pp55,
            pp56,
            pp57,
            pp58,
            pp59,
            pp60,
            pp61,
            pp62,
            pp63)) |>
  
  ## directional coding 
  ## (higher: conservative OR high in scale)
  mutate(
    pp12 = 3 - pp12,
    pp13 = 3 - pp13,
    pp18 = 3 - pp18,
    pp20 = 10 - pp20,
    pp22 = 10 - pp22) |> 
  
  ## go long
  pivot_longer(
    cols = -c(pid, svyyear, wave),
    names_to = "variable",
    values_to = "y"
  ) |>
  ## drop if y empty
  filter(is.na(y) == FALSE) |>
  ## normalize
  mutate(y = (y - min(y)) / (max(y) - min(y)), .by = "variable") |>
  ## add survey identifier
  mutate(survey = "shp") |>
  ## organize and save
  select(survey, variable, pid, wave, y)

# --- SOEP

soep <- 
  read_dta("./data/surveys/soep/soep.dta") |>
  zap_labels() |> # remove Stata labels
  mutate(wave = svyyear - min(svyyear) + 1) |>
  select(-kids_any) # gonna merge later

## trust scale
soep_pca <-
  pca(soep[, c('plh0192',
               'plh0193',
               'plh0194',
               'plh0195',
               'plh0196')] |>
        mutate(plh0192 = 
                 (plh0192 - 1) / 3,
               plh0193 = 
                 (plh0193 - 1) / 3,
               plh0193 = 1 - plh0193,
               plh0194 = 
                 (plh0194 - 1) / 3,
               plh0194 = 1 - plh0194,
               plh0196 = 1 - plh0196),
      missing = FALSE,
      nfactors = 1)
soep$trustscale <- soep_pca$scores[, 1]

cor(soep$trustscale, soep$plj0046, use = "pairwise",
    method = "spearman") # p = -0.27 with  immigration

## organize
soep <- soep |>
  select(-c(plh0192,
            plh0193,
            plh0194,
            plh0195,
            plh0196)) |> 
  
  ## directional coding 
  ## (higher: conservative OR high in scale)
  mutate(
    plh0007 = 4 - plh0007,
    plj0046 = 3 - plj0046,
    plj0047 = 3 - plj0047) |> 
  
  ## go long
  pivot_longer(
    cols = -c(pid, svyyear, wave),
    names_to = "variable",
    values_to = "y"
  ) |>
  ## drop if y empty
  filter(is.na(y) == FALSE) |>
  ## normalize
  mutate(y = (y - min(y)) / (max(y) - min(y)), .by = "variable") |>
  ## add survey identifier
  mutate(survey = "soep") |>
  ## organize and save
  select(survey, variable, pid, wave, y)

# --- UKHLS

ukhls <- 
  read_dta("./data/surveys/ukhls/ukhls.dta") |>
  rename(wave = ukhls_wave) |> 
  zap_labels() # remove Stata labels

## organize
ukhls <- ukhls |>
  
  ## directional coding 
  ## (higher: conservative OR high in scale)
  mutate(
    vote6 = 4 - vote6,
    scenv_grn = 4 - scenv_grn,
    scenv_ftst = 3 - scenv_ftst,
    scopfamf = 5 - scopfamf,
    demorient = 4 - demorient) |>
  ## go long
  pivot_longer(
    cols = -c(pid, svyyear, wave),
    names_to = "variable",
    values_to = "y"
  ) |>
  ## drop if y empty
  filter(is.na(y) == FALSE) |>
  ## standardize
  mutate(y = (y - min(y)) / (max(y) - min(y)), .by = "variable") |>
  ## add survey identifier
  mutate(survey = "ukhls") |>
  ## organize and save
  select(survey, variable, pid, wave, y)

# --- BIND DATASETS

c <-
  ## bind
  bind_rows(bhps,
            shp,
            soep,
            ukhls) |>
  arrange(survey, variable, pid, wave) # nicely organize

# PART 2: CPF DATA ----------------------------------------------------------- #

# --- LOAD CPF

t <-
  bind_rows(
    # bhps
    read_dta("./data/surveys/bhps/bhps.dta") |>
      zap_labels() |> mutate(wave = svyyear - min(svyyear) + 1) |>
      select(pid, svyyear, wave, kids_any) |>
      ## add CPF data
      left_join(
        read_dta("./data/cpf/cpf_bhps.dta") |>
          zap_labels() |>
          filter(wave >= 1 & wave <= 18) |> # BHPS data
          filter(respstat == 1) |> # interviews with full information
          select(
            pid,
            wave,
            female,
            age,
            edu = edu5,
            yborn,
            migr,
            ml = mlstat5,
            nvmarr,
            widow,
            divor,
            separ,
            # kids_any, # children info comes from original fertility files
            emp = emplst5,
            retir = retf
          ),
        by = c("pid", "wave")) |> 
      mutate(survey = "bhps"),
    
    # shp
    read_dta("./data/surveys/shp/shp.dta") |>
      zap_labels() |> mutate(wave = svyyear - min(svyyear) + 1) |>
      select(pid, svyyear, wave) |>
      ## add CPF data
      left_join(
        read_dta("./data/cpf/cpf_shp.dta") |>
          zap_labels() |>
          filter(respstat2 == 1) |> # interviews with full information
          select(
            pid,
            wave,
            female,
            age,
            edu = edu5,
            yborn,
            migr,
            ml = mlstat5,
            nvmarr,
            widow,
            divor,
            separ,
            kids_any,
            emp = emplst5,
            retir = retf
          ),
        by = c("pid", "wave")) |> 
      mutate(survey = "shp"),
    
    # soep
    read_dta("./data/surveys/soep/soep.dta") |>
      zap_labels() |> mutate(wave = svyyear - min(svyyear) + 1) |>
      select(pid, svyyear, wave, kids_any) |>
      ## add CPF data
      left_join(
        read_dta("./data/cpf/cpf_soep.dta") |>
          zap_labels() |>
          filter(respstat == 1) |> # interviews with full information
          select(
            pid,
            wave,
            female,
            age,
            edu = edu5,
            yborn,
            migr,
            ml = mlstat5,
            nvmarr,
            widow,
            divor,
            separ,
            # kids_any, # children info comes from original fertility files
            emp = emplst5,
            retir = retf
          ),
        by = c("pid", "wave")) |> 
      mutate(survey = "soep"),
    
    # ukhls
    read_dta("./data/surveys/ukhls/ukhls.dta") |>
      zap_labels() |> mutate(wave = ukhls_wave) |>
      select(pid, svyyear, wave) |> 
      ## add CPF data
      left_join(
        read_dta("./data/cpf/cpf_bhps.dta") |>
          zap_labels() |>
          filter(wave >= 19) |> # UKHLS data
          mutate(wave = wave - 18) |> # wave adjustment
          filter(respstat == 1) |> # interviews with full information
          select(
            pid,
            wave,
            female,
            age,
            edu = edu5,
            yborn,
            migr,
            ml = mlstat5,
            nvmarr,
            widow,
            divor,
            separ,
            kids_any,
            emp = emplst5,
            retir = retf
          ),
        by = c("pid", "wave")) |> 
      mutate(survey = "ukhls")
    )

# --- ORGANIZE THE DATA

t <- t |> 
  mutate(across(
    c(
      'age',
      'female',
      'edu',
      'yborn',
      'migr',
      'ml',
      'nvmarr',
      'widow',
      'divor',
      'separ',
      'kids_any',
      'emp',
      'retir'),
    ~ case_when(. < 0 ~ NA_real_, TRUE ~ .)
  )) |>
  ## marriages
  mutate(
    ml = case_when(
      ml == 1 ~ "Married",
      ml == 2 ~ "Never Married",
      ml == 3 ~ "Widowed",
      ml == 4 ~ "Divorced",
      ml == 5 ~ "Separated"),
    sep = case_when(
      widow == 1 ~ 1,
      divor == 1 ~ 1,
      separ == 1 ~ 1,
      is.na(widow) == TRUE &
        is.na(divor) == TRUE &
        is.na(separ) == TRUE ~
        NA_real_,
      TRUE ~ 0
    )
  ) |>
  select(-c(widow, divor, separ)) |> # drop unnecessary items
  ## employment status
  mutate(emp = factor(
    emp,
    levels = c(1, 2, 3, 4, 5),
    labels = c("Employed",
               "Unemployed",
               "Retired",
               "Not Active",
               "Education")
  )) |>
  ## cohort structure
  mutate(
    cohort =
      case_when(
        yborn < 1940 ~ 
          "1939 and Before",
        yborn >= 1940 & yborn < 1960 ~ 
          "1940-1959",
        yborn >= 1960 & yborn < 1970 ~
          "1960-1969",
        yborn >= 1970 & yborn < 1980 ~ 
          "1970-1979",
        yborn >= 1980 ~ 
          "After 1980")) |>
  mutate(
    cohort = factor(cohort,
                    levels = c("1939 and Before",
                               "1940-1959",
                               "1960-1969",
                               "1960-1979",
                               "After 1980"))) |>
  drop_na()
  
# PART 3: COUNTERFACTUALS ---------------------------------------------------- #

t <- t |> arrange(survey, pid, wave)

# --- MARRIAGE
t <- t |>
  mutate(nvmarr = 1 - nvmarr) |>
  mutate(ml_lag = lag(ml), .by = c("survey", "pid")) |>
  mutate(ml_lag = ifelse(ml_lag == "Never Married",
                         1,
                         ifelse(is.na(ml_lag) == TRUE,
                                NA_real_,
                                0))) |>
  mutate(nvmarr = ifelse(nvmarr == 1 & ml_lag == 1,
                         1,
                         0)) |>
  mutate(nvmarr = cummax(coalesce(nvmarr, 0)) + nvmarr * 0,
         .by = c("survey", "pid")) |> 
  ## keep "always never married" as the counterfactual
  mutate(counterfactual =
           ifelse(ml == "Never Married", 1, 0)) |>
  mutate(counterfactual = mean(counterfactual), .by = "pid") |>
  mutate(treatment_share = mean(nvmarr), .by = "pid") |> 
  mutate(
    nvmarr = case_when(treatment_share > 0 ~ nvmarr,
                       treatment_share == 0 & counterfactual == 1 ~ nvmarr,
                       TRUE ~ NA_real_)) |> 
  ## clean-up for the subsequent iteration
  select(-ml_lag, -counterfactual, -treatment_share)

# --- FIRST-PARENTHOOD
t <- t |>
  mutate(kids_any = cummax(coalesce(kids_any, 0)) + kids_any * 0,
         .by = c("survey", "pid")) |>
  ## keep "no children" or "soon-to-have-children" as the counterfactual
  mutate(treatment_share = mean(kids_any), .by = "pid") |>
  mutate(kids_any = case_when(treatment_share == 1 ~ NA_real_,
                              TRUE ~ kids_any)) |>
  ## clean-up for the subsequent iteration
  select(-treatment_share)

# --- MARRIAGE DISSOLUTION
t <- t |> 
  mutate(ml_lag = lag(ml), .by = c("survey", "pid")) |>
  mutate(ml_lag = ifelse(ml_lag == "Married",
                         1,
                         ifelse(is.na(ml_lag) == TRUE,
                                NA_real_,
                                0))) |>
  mutate(sep = ifelse(sep == 1 & ml_lag == 1,
                      1,
                      0)) |>
  mutate(sep = cummax(coalesce(sep, 0)) + sep * 0,
         .by = c("survey", "pid")) |>
  ## keep "always married" as the counterfactual
  mutate(counterfactual =
           ifelse(ml == "Married", 1, 0)) |>
  mutate(counterfactual = mean(counterfactual), .by = "pid") |>
  mutate(treatment_share = mean(sep), .by = "pid") |> 
  mutate(
    sep = case_when(treatment_share > 0 ~ sep,
                    treatment_share == 0 & counterfactual == 1 ~ sep,
                    TRUE ~ NA_real_)) |> 
  ## clean-up for the subsequent iteration
  select(-ml_lag, -counterfactual, -treatment_share)

# --- ENTRY TO LABOR
t <- t |> 
  mutate(em_lag = lag(emp), .by = c("survey", "pid")) |> 
  mutate(em_lag = ifelse(em_lag %in% c("Not Active", "Education"),
                         1,
                         ifelse(is.na(em_lag) == TRUE,
                                NA_real_,
                                0))) |> 
  mutate(entry = ifelse(emp == "Employed" & em_lag == 1,
                        1,
                        0)) |> 
  mutate(entry = cummax(coalesce(entry, 0)) + entry * 0,
         .by = c("survey", "pid")) |> 
  ## keep "education" or "not active" as the counterfactual
  mutate(counterfactual =
           ifelse(emp %in% c("Not Active", "Education"), 1, 0)) |>
  mutate(counterfactual = mean(counterfactual), .by = "pid") |>
  mutate(treatment_share = mean(entry), .by = "pid") |> 
  mutate(
    entry = case_when(treatment_share > 0 ~ entry,
                      treatment_share == 0 & counterfactual == 1 ~ entry,
                      TRUE ~ NA_real_)) |> 
  ## clean-up for the subsequent iteration
  select(-em_lag, -counterfactual, -treatment_share)

# --- UNEMPLOYMENT
t <- t |> 
  mutate(em_lag = lag(emp), .by = c("survey", "pid")) |> 
  mutate(em_lag = ifelse(em_lag == "Employed",
                         1,
                         ifelse(is.na(em_lag) == TRUE,
                                NA_real_,
                                0))) |> 
  mutate(unemp = ifelse(emp == "Unemployed" & em_lag == 1,
                        1,
                        0)) |> 
  mutate(unemp = cummax(coalesce(unemp, 0)) + unemp * 0,
         .by = c("survey", "pid")) |> 
  ## keep "always employed" as the counterfactual
  mutate(counterfactual =
           ifelse(emp == "Employed", 1, 0)) |>
  mutate(counterfactual = mean(counterfactual), .by = "pid") |>
  mutate(treatment_share = mean(unemp), .by = "pid") |> 
  mutate(
    unemp = case_when(treatment_share > 0 ~ unemp,
                      treatment_share == 0 & counterfactual == 1 ~ unemp,
                      TRUE ~ NA_real_)) |> 
  ## clean-up for the subsequent iteration
  select(-em_lag, -counterfactual, -treatment_share)

# --- RETIREMENT
t <- t |> 
  mutate(em_lag = lag(emp), .by = c("survey", "pid")) |> 
  mutate(em_lag = ifelse(em_lag == "Employed",
                         1,
                         ifelse(is.na(em_lag) == TRUE,
                                NA_real_,
                                0))) |> 
  mutate(retir = ifelse(retir == 1 & em_lag == 1,
                        1,
                        0)) |> 
  mutate(retir = cummax(coalesce(retir, 0)) + retir * 0,
         .by = c("survey", "pid")) |> 
  ## keep "always employed" as the counterfactual
  mutate(counterfactual =
           ifelse(emp == "Employed", 1, 0)) |>
  mutate(counterfactual = mean(counterfactual), .by = "pid") |>
  mutate(treatment_share = mean(retir), .by = "pid") |> 
  mutate(
    retir = case_when(treatment_share > 0 ~ retir,
                      treatment_share == 0 & counterfactual == 1 ~ retir,
                      TRUE ~ NA_real_)) |> 
  ## clean-up for the subsequent iteration
  select(-em_lag, -counterfactual, -treatment_share)

# order the relevant variables
t <- t |> 
  select(
    survey,
    pid,
    wave,
    female,
    age,
    cohort,
    edu,
    migr,
    nvmarr,
    sep,
    kids_any,
    unemp,
    entry,
    retir)

# PART 4: MERGE & ORGANIZE --------------------------------------------------- #

# --- CODING DECISIONS

# decision 1:
# keep if respondent age is A{16, 79}
t <- t |> 
  filter(age >= 16 & age <= 79)
# decision 2:
# get maximum education a respondent will get over time
t <- t |>
  mutate(
    edu = max(edu), .by = c("survey", "pid"))
# decision 3:
# drop 2 observations where gender is non-binary
t <- t |> filter(female != 3)

# --- MERGE DATAFILES AND SUBSET

# decision 1:
# merge outcomes and treatments
d <- left_join(c,
               t,
               by = c("survey",
                      "pid",
                      "wave")) |>
  pivot_longer(
    cols = c(nvmarr,
             sep,
             kids_any,
             unemp,
             entry,
             retir),
    ## our lovely events
    names_to = "events",
    values_to = "treatment"
  ) |>
  filter(is.na(treatment) == FALSE)
# decision 2:
# drop all-treated respondents
d <- d |>
  mutate(
    treatment_avg = mean(treatment),
    .by = c("survey",
            "events",
            "pid",
            "variable")
  )
d <- d |> filter(treatment_avg != 1)
# decision 3:
# keep if outcome is observed at least 3 times
d <- d |>
  mutate(n = n(),
         .by = c("survey",
                 "pid",
                 "variable",
                 "events")) |>
  filter(n >= 3) |> select(-n)

# --- IDENTIFY TREATMENT GROUPS

d <- d |> 
  ## wave at treated
  left_join(
    d |> 
      filter(treatment == 1) |> 
      summarize(wave_at_treated = min(wave),
                .by = c("survey",
                        "events",
                        "pid",
                        "variable")),
    by = c("survey", "events", "pid", "variable")) |>
  mutate(wave_at_treated = 
           ifelse(is.na(wave_at_treated) == TRUE,
                  0,
                  wave_at_treated)) |> 
  ## ages at treated
  left_join(
    d |> 
      filter(treatment == 1) |> 
      summarize(ages_at_treated = min(age),
                .by = c("survey",
                        "events",
                        "pid",
                        "variable")),
    by = c("survey", "events", "pid", "variable")) |>
  mutate(ages_at_treated = 
           ifelse(is.na(ages_at_treated) == TRUE,
                  0,
                  ages_at_treated))

# PART 4: ORGANIZE & SAVE ---------------------------------------------------- #

d <- d |>
  select(survey,
         variable,
         events,
         pid,
         wave,
         treatment,
         wave_at_treated,
         ages_at_treated,
         y,
         female,
         age,
         cohort,
         edu,
         migr) |>
  arrange(survey, variable, events, pid, wave)

saveRDS(d, file = "./data/data.rds")

# ---------------------------------------------------------------------------- #
