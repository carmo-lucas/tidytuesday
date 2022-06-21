# Setup ----

library(tidyverse)
library(tidycensus)
library(mapproj)
library(glue)


# Loading data ----

data(fips_codes)
data(county.fips)

county_fips <- county.fips %>%
    mutate(FIPS = str_pad(fips, 5L, "left", "0")) %>%
    as_tibble()

# drought <- readr::read_csv(
#     "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-14/drought.csv"
# ) %>% as_tibble()

drought_fips <- readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-14/drought-fips.csv"
) %>% as_tibble()

# Cleaning data ----

drought_clean <- drought %>%
    mutate(DATE = str_remove_all(DATE, "d_") %>% lubridate::ymd())

counties_coord <- map_data("county", ) %>%
    as_tibble() %>%
    rename("state" = region, "county" = subregion) %>%
    unite(polyname, c("state", "county"), sep = ",", remove = FALSE)

coordinates <-
    full_join(county_fips, counties_coord) %>%
    select(FIPS, long, lat, state, county)

drought_fips_summary <-
    drought_fips %>%
    mutate(month = lubridate::month(date), year = lubridate::year(date)) %>%
    group_by(month, year, FIPS) %>%
    summarize(mean_DSCI = mean(DSCI, rm.na = FALSE)) %>%
    ungroup() %>%
    mutate(cut_DSCI = cut(mean_DSCI, 7)) %>%
    group_by(year, month) %>%
    nest()





df <- left_join(filtered_more, coordinates) %>% drop_na()

# Generate plots ----

p <- ggplot(
    data = df,
    mapping = aes(x = long, y = lat, fill = cut_DSCI, group = FIPS)) +
    geom_polygon() +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    scale_fill_brewer(type = "div", palette = "RdYlBu") +
    theme_void()

ggsave(p, glue("2022/week_24/plot/{year}-{month}.png"))

drought_fips_summary$data
