require(tidyverse)
require(ggpubr)
require(ggtext)
require(latex2exp)

# https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-08-23

# variables
ROOT = here::here()
plot_file = file.path(ROOT,"plots","2022-08-23-chips.png")

# formatting
FONT_SIZE = 12
FONT_H1 = "Montserrat"
FONT_H2 = "Bitter"
FONT_H3 = "Roboto Condensed"

theme_set(theme_light(base_size = FONT_SIZE, base_family = FONT_H3))

theme_update(
    # plot
    panel.grid.major = element_line(linetype="dashed", size=1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = NA),
    
    panel.background = element_rect(fill = "grey98", colour = NA),
    plot.background = element_rect(fill = "grey98"), 
    plot.margin = unit(rep(3, times=4), "cm"),
    
    # text
    plot.title = element_text(size=FONT_SIZE+4, family=FONT_H1, face="bold", hjust = 0.5, margin = margin(-1, -2, 1, -2, "cm")),
    plot.subtitle = element_textbox_simple(family=FONT_H2, size = FONT_SIZE+2, hjust = 0.5, margin = margin(0, -2, 1, -2, "cm")),
    plot.caption = element_text(family=FONT_H2, size = FONT_SIZE, hjust = 0.5, lineheight = 1.2),
    axis.title.x = element_text(color="black", family=FONT_H3, size=FONT_SIZE+1),
    axis.title.y = element_text(color="black", family=FONT_H3, size=FONT_SIZE+1),
    axis.text.x = element_text(color="black", family=FONT_H3, face="bold", size=FONT_SIZE),
    axis.text.y = element_text(color="black", family=FONT_H3, face="bold", size=FONT_SIZE),
    #aspect.ratio = 1
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
subtitle = "According to *Moore's Law* the number of transistors in chips doubles about every year."

plt = df %>%
    ggplot(aes(x=year, y=transistors_million)) +
    geom_point(fill="orange", color="orange", shape=21, size=4) +
    geom_point(fill=NA, color="black", shape=21, size=4) +
    geom_smooth(method="lm", linetype="88", color="black", fill="grey80", size=1.5, alpha=0.5) +
    scale_y_continuous(
        trans = scales::log2_trans(), 
        breaks = scales::trans_breaks("log2", function(x){ 2^x }), 
        labels = scales::comma_format()
    ) +
    annotate(
        geom = "curve",
        x = 2010, xend = 2010,
        y = 256, yend = 650,
        curvature = +.1,
        arrow = arrow(
          length = unit(5, "pt"),
          type = "closed",
          ends = "both"
        )
    ) +
    annotate(
        geom = "curve",
        x = 2008, xend = 2010,
        y = 240, yend = 240,
        curvature = +.1,
        arrow = arrow(
          length = unit(5, "pt"),
          type = "closed",
          ends = "both"
        )
    ) +
    stat_regline_equation(size=4, family="Roboto Condensed") +
    labs(
        x="\nYear",
        y=TeX("log_2(Millions of Transistors Produced)"),
        title="Does Moore's Law hold?",
        subtitle = subtitle,
        caption=NULL
    )


# save
ggsave(plot_file, plt, width=20, height=20, dpi=350, units="cm")

print("Done!")
