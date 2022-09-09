require(tidyverse)
require(ggpubr)
require(extrafont)
require(brickr)
require(ggtext)
require(grid)
require(gridtext)

# https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-09-06

# variables
ROOT = here::here()
plot_file = file.path(ROOT,"plots","2022-09-06-lego.png")

# formatting
FONT_SIZE = 12
FONT_H1 = "Impact"
FONT_H2 = "Georgia"

MATERIALS = c('Plastic','Rubber','Cloth','Metal','Cardboard/Paper','Foam')
PAL_MATERIALS = setNames(get_palette("npg", length(MATERIALS)), MATERIALS)
PAL_BACKGROUND = "#FFF7AB"#"#F0E68C"

# load data
inventories = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventories.csv.gz')
sets = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/sets.csv.gz')
colors = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/colors.csv.gz')
inventory_parts = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventory_parts.csv.gz')
parts = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/parts.csv.gz')

# preprocess
df = inventories %>%
    left_join(sets %>% dplyr::rename(set_name=name, set_img_url=img_url), by="set_num") %>%
    left_join(inventory_parts %>% dplyr::rename(inventory_img_url=img_url), by=c("id"="inventory_id")) %>%
    left_join(parts %>% dplyr::rename(part_name=name), by="part_num")

##### what are the colors' diversity by material? ######
# get counts of colors per material per year
X = df %>% 
    distinct(year, part_material, color_id) %>% 
    drop_na() %>% 
    count(year, part_material)

# create a function to plot a single material (instead of faceting)
make_plot = function(X, material_oi) {
    plt = X %>%
        filter(year %in% seq(1950, 2022, 3) & part_material==material_oi) %>%
        ggplot(aes(x=year, y=n)) +
        geom_brick_col(
            aes(fill=part_material), position="stack", 
            split_bricks=FALSE, label="LEGO"
        ) +
        fill_palette(PAL_MATERIALS[material_oi]) +
        labs(
            x=NULL,
            y=NULL,
            fill=NULL
        ) +
        scale_x_continuous(limits = c(1950, 2022)) +
        scale_y_continuous(breaks = seq(0,100,25), limits = c(0,110)) +
        theme_pubr(x.text.angle = 45) +
        theme(
            aspect.ratio = 0.45,
            
            legend.position = c(0.01,0.83),
            legend.justification = "left",
            
            axis.line = element_blank(),
            axis.ticks = element_line(size=0.75),
            axis.ticks.length=unit(0.25, "cm"),

            # plot
            panel.background = element_rect(fill = PAL_BACKGROUND, colour=PAL_BACKGROUND),
            plot.background = element_rect(fill = PAL_BACKGROUND, colour=PAL_BACKGROUND), 
            plot.margin = unit(rep(0.2, times=4), "cm"),

            # text
            plot.title = element_blank(),
            plot.subtitle = element_blank(),
            plot.caption = element_blank(),
            
            legend.background = element_blank(),
            legend.title = element_blank(),
            legend.text = element_text(size=FONT_SIZE-1, family=FONT_H2),
            axis.text = element_text(size=FONT_SIZE-1, family=FONT_H2)
        )
    
    return(plt)
}

# combine the plots into a single figure, emulating the faceting
# this allows hacing the legend icons in the facets
plts = lapply(
    X %>% pull(part_material) %>% unique(),
    function(material_oi){ make_plot(X, material_oi) }
)

# I tried using patchwork, but it did not allow to easily add labels to the whole figure
fig = ggarrange(plotlist=plts, ncol=2, nrow=3) %>%
    annotate_figure(
        # title
        top = richtext_grob(
            "Color diversity in *LEGO* materials", 
            gp=gpar(fontsize=FONT_SIZE+20, fontfamily=FONT_H1), vjust=-0.1
        ),
        # y-axis common label
        left = text_grob(
            "N. Different Colors", size=FONT_SIZE-1, family=FONT_H2, 
            rot=90, face="bold", vjust=0.2
        ),
        # x-axis common label
        bottom = text_grob("Year", size=FONT_SIZE-1, family=FONT_H2, face="bold")
    ) +
    theme(
        plot.background = element_rect(fill = PAL_BACKGROUND, colour=NA), 
        plot.margin = margin(1, 1, 1, 1, "cm"),
        panel.background = element_blank()
    )

# save
ggsave(plot_file, fig, width=20, height=20, dpi=350, units="cm")

print("Done!")
