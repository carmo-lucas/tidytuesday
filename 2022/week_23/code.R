# Setup ----

library(tidyverse)
library(rvest)
library(ggsvg)
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

desaturated <- desaturate(saturated_palette, amount = .45)





parse_svg <- function(x) {
# Function that parses svg files into text.
    svg_string <- paste(
        readLines(x),
        collapse = "\n"
    )
    return(svg_string)
}


# List svg files from the directory
path_logos <- dir("2022/week_23/logo/",
    pattern = "*.svg", 
    full.names = TRUE,
)

# Create an object that contains the svg text of the logos
image_logos <- map(path_logos, parse_svg)

# Company names in the correct order for later matching
company_names <- c("Amazon", "AT&T", "Comcast", "FedEx", "State Farm", "Toyota")

# Set names for the logos
names(image_logos) <- company_names

# Create a dataframe with the information
logos_df <- bind_rows(image_logos) %>%
    pivot_longer(1:6) %>%
    rename("company" = name)


# Binds columns
df <- pride_aggregates %>%
    select(company, total_contributed) %>%
    head(6) %>%
    mutate(companies = fct_reorder(company, total_contributed)) %>%
    bind_cols("saturated" = saturated_palette) %>%
    left_join(logos_df)


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
    geom_point_svg(data = df,
    aes(x = companies,
    y = 100000,svg = value)) +
    scale_fill_identity() +
    coord_flip()
