require(tidyverse)
require(ggpubr)

# https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-08-23

# variables
ROOT = here::here()
plot_file = file.path(ROOT,"plots","2022-08-23-chips.png")

# formatting
theme_set(theme_light(base_size = 14, base_family = "Roboto Condensed"))

theme_update(
  panel.grid.minor = element_blank(),
  plot.title = element_text(face = "bold"),
  legend.position = "top",
  plot.title.position = "plot"
)

# theme_update(
#   axis.ticks = element_blank(),
#   axis.text = element_blank(),
#   panel.grid.major = element_blank(),
#   panel.grid.minor = element_blank(),
#   panel.border = element_rect(color = NA),
#   plot.title = element_text(family = "Bitter", size = 32, hjust = 0.5),
#   plot.subtitle = element_text(family = "Montserrat", color = "grey80", size = 18, face = "bold", hjust = 0.5, margin = margin(b = 6)),
#   plot.caption = element_text(family = "Bitter", color = "grey60", size = 14, hjust = 0.5, lineheight = 1.2),
#   legend.position = "bottom",
#   legend.title = element_text(family = "Bitter", color = "grey60", face = "bold", size = 14),
#   legend.text = element_text(family = "Roboto Mono", color = "grey60", size = 10)
# )

# load data
df = read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-23/chips.csv")

# preprocess data


# plot
plt = df %>%
    ggplot(aes(x=year, y=transistors_million)) +
    geom_bar(fill="grey", stat="identity") +
    labs(
        x="Year",
        y="Millions of Transistors Produced",
        title="Moore Law",
        subtitle="Exponential increase of transistors.",
        caption="Transistors have increased"
    )


# save
ggsave(plot_file, plt, width=11, height=14, dpi=350)

print("Done!")
