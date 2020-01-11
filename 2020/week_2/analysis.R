library(tidyverse)
library(here)
library(lubridate)

# Data -------------------------------------------------------

temperature <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/temperature.csv')

# Wrangle ------------------------------------------------------- 

city_temp <- temperature %>% 
	filter(
		city_name %in% c("MELBOURNE", "PERTH", "SYDNEY","BRISBANE"),
		date >= as_date("1970-01-01"),
		date < as_date("2019-01-01")) %>% 
	group_by(city_name, year = year(date), month = month(date,label = T)) %>% 
	summarise(temp = mean(temperature, na.rm = T))

# Plot -------------------------------------------------------

ggplot(city_temp, aes(
	x = year,
	y = forcats::fct_rev(month),
	fill = temp
)) +
	geom_raster() +
	scale_fill_viridis_c(option = "inferno") +
	theme_minimal() +
	theme(text = element_text(family = "Fira Sans", size = 5),
				panel.grid = element_blank(),
				plot.caption = element_text(hjust = 0)) + 
	facet_wrap(~city_name) +
	labs(
		title = "Australian Cities Temperature Over Time",
		subtitle = "Melbourne has lower mean temperatures in comparison",
		y = "",
		x = "",
		fill = "Average \ntemperature \n(°C)",
		caption = "Data: Australian Bureau of Meteorology\nVis: Lucas C.L. do Carmo"
	) +
	coord_equal()

ggsave(filename = here::here("2020", "week_2", "tt_week2_plot.png"),
			 device = "png",
			 dpi = "retina",
			 width = 20,
			 height = 8,
			 units = "cm")
