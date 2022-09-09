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
FONT_H2 = "Georgia"#"Times New Roman"
FONT_H3 = "Courier New"

MATERIALS = c('Plastic','Rubber','Cloth','Metal','Cardboard/Paper','Foam')
PAL_MATERIALS = setNames(get_palette("npg", length(MATERIALS)), MATERIALS)
PAL_BACKGROUND = "khaki"

# load data
inventories = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventories.csv.gz')
inventory_sets = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventory_sets.csv.gz')
sets = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/sets.csv.gz')
colors = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/colors.csv.gz')
elements = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/elements.csv.gz')
inventories = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventories.csv.gz')
inventory_sets = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventory_sets.csv.gz')
inventory_parts = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventory_parts.csv.gz')
parts = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/parts.csv.gz')
part_categories = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/part_categories.csv.gz')
part_rel = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/part_relationships.csv.gz')


# preprocess
df = inventories %>%
    left_join(sets %>% dplyr::rename(set_name=name, set_img_url=img_url), by="set_num") %>%
    left_join(inventory_parts %>% dplyr::rename(inventory_img_url=img_url), by=c("id"="inventory_id")) %>%
    left_join(parts %>% dplyr::rename(part_name=name), by="part_num")

# how did the use of materials change along time?
X = df %>% 
    count(year, part_material) %>%
    drop_na() %>% 
    group_by(part_material) %>%
    mutate(prop = n/sum(n)) %>%
    ungroup() %>%
    group_by(year) %>%
    mutate(rel_prop = prop/sum(prop)) %>%
    ungroup() 

plt = X %>%
    filter(year %in% seq(1950, 2022, 8)) %>%
    ggplot(aes(x=factor(year), y=rel_prop)) +
    geom_brick_col(aes(fill=part_material), position="stack", split_bricks=FALSE, label="LEGO") +
    #geom_bar(aes(fill=part_material), stat="identity") +
    fill_palette("npg") +
    theme_minimal()

# what are the colors diversity by material?
X = df %>% 
    distinct(year, part_material, color_id) %>% 
    drop_na() %>% 
    count(year, part_material)

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
            
            legend.position = c(0.01,1),
            legend.justification = "left",
            
            axis.line = element_blank(),
            axis.ticks.length=unit(.25, "cm"),

            # plot
            panel.background = element_rect(fill = PAL_BACKGROUND, colour=PAL_BACKGROUND),
            plot.background = element_rect(fill = PAL_BACKGROUND, colour=PAL_BACKGROUND), 
            plot.margin = unit(rep(0.3, times=4), "cm"),

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
plts = lapply(
    X %>% pull(part_material) %>% unique(),
    function(material_oi){ make_plot(X, material_oi) }
)
fig = ggarrange(plotlist=plts, ncol=2, nrow=3) %>%
    annotate_figure(
        top = richtext_grob("Color diversity in *LEGO* materials", gp=gpar(fontsize=FONT_SIZE+20, fontfamily=FONT_H1), vjust=-0.1),
        left = text_grob("N. Different Colors", size=FONT_SIZE-1, family=FONT_H2, rot=90, face="bold", vjust=0.5),
        bottom = text_grob("Year", size=FONT_SIZE-1, family=FONT_H2, face="bold")
    ) +
    theme(
        plot.background = element_rect(fill = PAL_BACKGROUND, colour=NA), 
        plot.margin = margin(1, 1, 1, 1, "cm"),
        panel.background = element_blank()
    )

ggsave(plot_file, fig, width=20, height=20, dpi=350, units="cm")

print("Done!")


# make lego logo
font_file = system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
font = read_hex(font_file)
bml = as_bm_list("LEGO", font = font)
bm = (3 * bml) %>%
    bm_pad(sides = 2L) %>%
    bm_shadow(value = 2L) %>%
    bm_call(cbind) %>%
    bm_extend(sides = 1L, value = 1L)

col = apply(bm + 1L, c(1, 2), function(i) {
        switch(i, "white", "grey20", "lightblue", "darkblue")
})

coords = xyz_heightmap(bm, col=col, flipy = FALSE)

logo = coords %>%
    ggplot(aes(x, y, z = z, fill = fill)) +
    geom_oblicuboids(light = FALSE) +
    coord_fixed() +
    theme_void() +
    guides(fill="none") +

ggsave("logo.jpeg", logo, width=5, height=5, units="cm", bg="white")
img = jpeg::readJPEG("logo.jpeg")
img %>% image_to_mosaic(img_size=48) %>% build_mosaic()

# plot
subtitle = "The **Pell Grants**"

plt = df %>% 
    
    group_by(part_material) %>%
    mutate(freq = row_number()) %>%
    ungroup() %>%
    ggplot(aes(x=part_material, y=freq)) +
    coord_fixed() +
    geom_oblicubes(angle = -45) +
    yscale("log10") +
    labs(
        x=NULL,
        y=NULL,
        fill = "Amount per Recipient and State (USD)",
        title = "How were Pell Grants distributed along time?",
        subtitle = subtitle,
        caption=NULL
    ) +
    theme_void() +
    theme(
        # plot
        panel.background = element_rect(fill = PAL_BACKGROUND, colour=PAL_BACKGROUND),
        plot.background = element_rect(fill = PAL_BACKGROUND, colour=PAL_BACKGROUND), 
        plot.margin = unit(rep(1, times=4), "cm"),
        
        # text
        plot.title = element_text(size=FONT_SIZE+4, family=FONT_H1, face="bold", hjust = 0.5, margin = margin(0, 0, 1, 0, "cm")),
        plot.subtitle = element_textbox_simple(family=FONT_H2, size = FONT_SIZE+2, hjust = -0.5, margin = margin(0, 0, 1, 0, "cm")),
        plot.caption = element_text(family=FONT_H2, size = FONT_SIZE, hjust = 0.5, lineheight = 1.2),
        
        legend.title = element_text(size=FONT_SIZE, family=FONT_H2),
        legend.text = element_text(size=FONT_SIZE-1, family=FONT_H2),
        legend.box.margin = unit(c(0.5,0,0,0), "cm"),
        legend.position = "bottom",
        legend.justification = "right",
        
        strip.text = element_text(size=FONT_SIZE-1, family=FONT_H3, margin=margin(0.5, 0, 0.25, 0, "cm"))
    )

plt = ggarrange(plt) +
    theme(
        plot.background = element_rect(fill = PAL_BACKGROUND, colour=NA), 
        plot.margin = margin(1, 1, 1, 1, "cm")
    )

# save


# example
library(ggplot2)
library(viridis)
library(tidyverse)

# Create dataset
data = data.frame(
  individual=paste( "Mister ", seq(1,60), sep=""),
  group=c( rep('Group A', 10), rep('Group B', 30), rep('Group C', 14), rep('Group D', 6)) ,
  value=sample( seq(10,100), 60, replace=T), stringsAsFactors = TRUE)

# Set a number of 'empty bar' to add at the end of each group
empty_bar = 3
to_add = data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
colnames(to_add) = colnames(data)
to_add$group = rep(levels(data$group), each=empty_bar)
data = rbind(data, to_add)
data = data %>% arrange(group)
data$id = seq(1, nrow(data))

# Get the name and the y position of each label
label_data = data
number_of_bar = nrow(label_data)
angle = 90 - 360 * (label_data$id-0.5) /number_of_bar 
label_data$hjust = ifelse( angle < -90, 1, 0)
label_data$angle = ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data = data %>% 
  group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

angle = 90 - 360 * (base_data$title-0.5)/number_of_bar  
base_data$angle = ifelse(angle < -90, angle+180, angle)

# prepare a data frame for grid (scales)
grid_data = base_data
grid_data$end = grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start = grid_data$start - 1
grid_data = grid_data[-1,]

# Make the plot
ggplot(data, aes(x=as.factor(id), y=value, fill=group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar

geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +

# # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
#   geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
#   geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
#   geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
#   geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +

#   # Add text showing the value of each 100/75/50/25 lines
#   annotate("text", x = rep(max(data$id),4), y = c(20, 40, 60, 80), label = c("20", "40", "60", "80") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +

  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
  ylim(-50,120) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
#    plot.margin = unit(rep(-1,4), "cm") 
  ) +
geom_segment(data=base_data, aes(x = start, y = -1, xend = end, yend = -1), colour = "black", alpha=0.6, size=0.8, inherit.aes = F ) +
  geom_text(data=base_data, aes(x = title, y = -6, label=group), 
            hjust=c(1,1,0,0), colour = "black", alpha=0.7, size=2, fontface="bold", inherit.aes = FALSE) +

  coord_polar()
  #geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +

# Add base line information

