# Setup ----

library(tidyverse)
library(rvest)
library(ggsvg)
library(colorspace)
library(glue)
library(ggtext)
library(ragg)

# Functions ----


agora <- function() {
    x <- format(Sys.time(), "%Y-%m-%d %H%M%S")
    return(x)
}


parse_svg <- function(x) {
    # Function that parses svg files into text.
    svg_string <- paste(
        readLines(x),
        collapse = "\n"
    )
    return(svg_string)
}


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

desaturated <- darken(saturated_palette, amount = .45)

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
        ),
        width = 1
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
    geom_point_svg(
        data = df,
        aes(
            x = companies,
            y = total_contributed + 80000,
            svg = value
        ),
        size = 10,
        hjust = 1
    ) +
        geom_text(
            data = df,
            aes(
                x = companies,
                y = total_contributed - 5000,
                label = paste0("U$", round(total_contributed / 1e3, 0), " K")
            ),
            color = "white",
            hjust = 1,
            size = 3
        ) +
    scale_fill_identity() +
        coord_flip() +
        labs(
            title =
                "<span style = 'color:#FFFFFF'> Companies that donated money to </span>
        <span style = 'color:#fe0000';>P</span>
        <span style = 'color:#fe8d00';>R</span>
        <span style = 'color:#ffee00';>I</span>
        <span style = 'color:#018114';>D</span>
        <span style = 'color:#014cff';>E</span>
        <span style = 'color:#8a018c';>!</span>"
        ) +
        theme(
            text = element_text(family = "Roboto"),
            plot.title.position = "panel",
            plot.title = element_textbox_simple(margin = margin(5, 0, 5, 0, "mm")),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid.major = element_blank(),
            panel.background = element_blank(),
            plot.background = element_rect(fill = "#333333", color = "#333333")
        )



ggsave(
    glue("2022/week_23/plot/{agora()}.png"),
    device = ragg::agg_png(),
    dpi = 300,
    width = 1400,
    height = 800,
    units = "px"
)
