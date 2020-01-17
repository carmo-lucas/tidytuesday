library(tidyverse)
library(stringr)
library(ggraph)
library(janitor)
# Data -------

passwords <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv') %>% 
	remove_empty("rows")

# Wrangle -----

pass <- passwords %>% 
	mutate(char_category = case_when(
		str_detect(.$password, pattern = "^[a-z]+$") ~ "Only Lowercase",
		str_detect(.$password, pattern = "^[0-9]+$") ~ "Only Numeric",
		str_detect(.$password, pattern = "[:alnum:]") ~ "Alphanumeric",
		str_detect(.$password, pattern = "[:punct:]") ~ "With Punctuation",
	)) %>% 
	select(password, char_category, strength)

# Plot -----

ggplot(pass,aes(x = char_category,
								y = strength,
								color = char_category)) +
	geom_violin(fill = "none") +
	geom_jitter(width = 0.1, alpha = 0.3, size = 1) +
	theme(
		panel.background = element_rect(fill = "#132125", color= NULL),
		panel.grid = element_blank(),
		panel.border = element_blank(),
		legend.position = "none",
		text = element_text(family = "Courier", color = "white"),
		axis.text = element_text(color = "white"),
		plot.title = element_text(lineheight = 1),
		plot.background = element_rect(fill = "#132125", color = NULL),
		plot.caption = element_text(hjust = .5)
		) +
	scale_color_manual(values = c("#d33682","#268bd2","#2aa198"))+
	coord_flip() +
	labs(
		y = "Strength",
		x = "",
		title = "Top 500 passwords do not \nfollow security reccomendations",
		caption = "Data: Information is beautiful | Viz: @carmo_lcl"
	) +
	annotate(geom = "text", x = 2, y = 20, label = "Passwords containing only letters \nor words are not as strong and more \nfrequent than passwords containing \na combination of these elements", 
					 family = "Courier", color = "white", hjust = "left", size = 3)

# Save ----

ggsave(filename = here::here("2020", "week_3", "tt_week3_plot.png"),
			 device = "png",
			 dpi = "retina",
			 width = 15,
			 height = 16,
			 units = "cm")

