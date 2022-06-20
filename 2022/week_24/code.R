# Setup ----

library(tidyverse)
library(tidycensus)
library(mapproj)


# Loading data ----

data(fips_codes)
data(county.fips)

county_fips <- county.fips %>%
    mutate(FIPS = str_pad(fips, 5L, "left", "0")) %>%
    as_tibble()

drought <- readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-14/drought.csv"
) %>% as_tibble()

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

DF_2000 <-
    filter(drought_fips, lubridate::year(date) == 2000) %>%
    mutate(DSCI = cut(DSCI, 5))

df <-
    left_join(DF_2000, coordinates) %>% drop_na()



# Plotting ----

p <- ggplot(
    data = df,
    mapping = aes(x = long, y = lat, fill = DSCI, group = FIPS)
) +
    geom_polygon() +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    scale_fill_brewer(type = "div", palette =  "RdYlBu")

ggsave(p, "2022/week_24/plot.png")
