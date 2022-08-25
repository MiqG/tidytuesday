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

PAL_BACKGROUND = "grey97"

theme_set(theme_light(base_size = FONT_SIZE, base_family = FONT_H3))

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
    geom_smooth(method="lm", linetype="dashed", color="black", fill="grey80", size=1, alpha=0.5) +
    scale_y_continuous(
        trans = scales::log2_trans(), 
        breaks = scales::trans_breaks("log2", function(x){ 2^x }), 
        labels = scales::comma_format()
    ) +
    ggrepel::geom_text_repel(
        aes(label=round(transistors_million)),
        . %>% filter(year %in% c(2008, 2010)),
        box.padding = 0,
        point.padding = 0.5,
        min.segment.length = 0,
        xlim=c(2005, 2006),
        hjust=-0.5,
        segment.size=0.5,
        segment.linetype="dashed", 
        family=FONT_H3,
    ) +
    annotate(
        geom = "segment",
        x = 2010, xend = 2010,
        y = 256, yend = 630,
        linetype="dashed"
    ) +
    annotate(
        geom = "segment",
        x = 2008, xend = 2008,
        y = 256, yend = 400,
        linetype="dashed"
    ) +
    annotate(
        geom = "segment",
        x = 2008, xend = 2010,
        y = 240, yend = 240,
        arrow = arrow(
          length = unit(4, "pt"),
          type = "closed",
          ends = "both"
        )
    ) +
    geom_text(
        aes(x=2009, y=200, label="2 years"),
        family=FONT_H3,
        size=4
    ) +
    #stat_regline_equation(size=4, family="Roboto Condensed") +
    labs(
        x="Year",
        y=TeX("log_2(Millions of Transistors Produced)"),
        title="Does Moore's Law hold?",
        subtitle = subtitle,
        caption=NULL
    ) +
    theme(
        # plot
        panel.grid.major = element_line(color="white", linetype="dashed", size=1),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color=PAL_BACKGROUND),

        panel.background = element_rect(fill = PAL_BACKGROUND, colour=PAL_BACKGROUND),
        plot.background = element_rect(fill = PAL_BACKGROUND, colour=PAL_BACKGROUND), 
        plot.margin = unit(rep(1, times=4), "cm"),

        # text
        plot.title = element_text(size=FONT_SIZE+4, family=FONT_H1, face="bold", hjust = -0.25, margin = margin(-1, -2, 1, -4, "cm")),
        plot.subtitle = element_textbox_simple(family=FONT_H2, size = FONT_SIZE+2, hjust = -0.5, margin = margin(0, -2, 1, -4, "cm")),
        plot.caption = element_text(family=FONT_H2, size = FONT_SIZE, hjust = 0.5, lineheight = 1.2),
        axis.title.x = element_text(color="black", family=FONT_H3, size=FONT_SIZE+1, margin=margin(t=1, unit="cm")),
        axis.title.y = element_text(color="black", family=FONT_H3, size=FONT_SIZE+1, margin=margin(r=1, unit="cm")),
        axis.text.x = element_text(color="black", family=FONT_H3, face="bold", size=FONT_SIZE),
        axis.text.y = element_text(color="black", family=FONT_H3, face="bold", size=FONT_SIZE),
        aspect.ratio = 1
    )


plt = ggarrange(plt) +
    theme(
        plot.background = element_rect(fill = PAL_BACKGROUND, colour=NA), 
        plot.margin = margin(2, 2, 2, 2, "cm")
    ) 

# save
ggsave(plot_file, plt, width=20, height=20, dpi=350, units="cm")

print("Done!")
