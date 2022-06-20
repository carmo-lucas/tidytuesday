# Setup ----

library(tidyverse)



# Loading data ----


drought <- readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-14/drought.csv"
)
drought_fips <- readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-14/drought-fips.csv"
)

# Cleaning data ---- 

drought_clean <- drought %>%
    mutate(DATE = str_remove_all(DATE, "d_") %>% lubridate::ymd())

str(drought_clean)
str(drought_fips)

fips_split <-
    drought_fips %>%
    mutate(
        state = str_sub(FIPS, 1L, 2L),
        county = str_sub(FIPS, 3L, 5L)
        )

str(drought_fips)

state <- map_data("state")
county <- map_data("county")

# Plotting ----

ggplot(
    data = county, mapping = aes(
        x = long, y = lat,
        group = group, fill = region
    ),
    color = "white", size = 0.5
) +
    geom_polygon() +
    guides(fill = "none") +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    theme_void()
