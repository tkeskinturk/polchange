
# - life-course transitions and political orientations ----------------------- #
# - data descriptions: potpourri --------------------------------------------- #

### note: this script 
###       (a) turns the variable list into a pretty latex format,
###       (b) generates an illustrative DID study example.

# ---------------------------------------------------------------------------- #

rm(list = ls()) # clean-up
pacman::p_load(
  tidyverse, purrr, furrr, broom, hrbrthemes, did, panelView)
theme_set(theme_ipsum_rc())

my_oka <- c("#EEBD64", "#C8D9F0", "#B4D3B2", "#414C66")
`%nin%` = Negate(`%in%`)

# dataframe
d <- readRDS("./data/data.rds")

# variable labels
vars <- read_csv("./misc/var_labels.csv")

# PART 1: ITEMS -------------------------------------------------------------- #

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

# PART 2: STAGGERED ---------------------------------------------------------- #

# generate an idealized example
set.seed(11235)
staggered <-
  ## build the data
  build_sim_dataset(reset.sim(n = 100, time.periods = 10)) |>
  ## assign treatment status
  mutate(treatment =
           ifelse(period < G, 0, 1))

png(
  "./figures/desc04_staggeredys.png",
  w = 8,
  h = 5,
  units = "in",
  res = 500
)
panelview(
  1 ~ treatment,
  data = staggered,
  index = c("id", "period"),
  by.timing = TRUE,
  collapse.history = "TRUE",
  display.all = TRUE,
  gridOff = FALSE,
  legend.labs = c("Under Control", "Under Treatment"),
  color = c(my_oka[1], my_oka[4]),
  cex.main = 0
) +
  theme_ipsum_rc(axis_title_size = 12,
                 plot_margin = margin(0, 15, 15, 15)) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    axis.text.y = element_blank()
  ) +
  labs(x = "Wave", y = "Groups", title = "")
dev.off()

# ---------------------------------------------------------------------------- #
