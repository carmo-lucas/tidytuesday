# Setup ----

library(tidyverse)
library(rvest)
library(colorspace)

# Read data ----

pride_aggregates <-
    readr::read_csv(
        "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/pride_aggregates.csv"
    ) %>%
    janitor::clean_names()

fortune_aggregates <-
    readr::read_csv(
        "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/fortune_aggregates.csv"
    ) %>%
    janitor::clean_names()

static_list <-
    readr::read_csv(
        "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/static_list.csv"
    ) %>%
    janitor::clean_names()

pride_sponsors <-
    readr::read_csv(
        "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/pride_sponsors.csv"
    ) %>%
    janitor::clean_names()

corp_by_politician <-
    readr::read_csv(
        "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/corp_by_politician.csv"
    ) %>%
    janitor::clean_names()

donors <-
    readr::read_csv(
        "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/donors.csv"
    ) %>%
    janitor::clean_names()

# Analysis ----

saturated_palette <- c(
    "#fe0000",
    "#fe8d00",
    "#ffee00",
    "#018114",
    "#014cff",
    "#8a018c"
)

desaturated <- desaturate(palette, amount = .45)

df <- pride_aggregates %>%
    select(company, total_contributed) %>%
    head(6) %>%
    mutate(companies = fct_reorder(company, total_contributed)) %>%
    bind_cols("saturated" = saturated_palette)



ggplot() +
    geom_col(
        aes(
            x = df$companies,
            y = Inf,
            fill = desaturated
        ), width = 1
    ) +
    geom_col(
        data = df,
        aes(
            x = companies,
            y = total_contributed,
            fill = saturated
        ),
        width = 1
    ) +
    scale_fill_identity() +
    coord_flip() +
    theme_void()
