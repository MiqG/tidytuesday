require(tidyverse)
require(ggpubr)
require(extrafont)
require(sf)
require(spData)

# https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-30

# variables
ROOT = here::here()
plot_file = file.path(ROOT,"plots","2022-08-30-pell.png")

# formatting
FONT_SIZE = 12
FONT_H1 = "Georgia"
FONT_H2 = "Times New Roman"
FONT_H3 = "Courier New"

PAL_BACKGROUND = "#F6FFED"
PAL_POINTS = "#ffba49"

# load data
## grants
pell = read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-30/pell.csv")
## state abbreviations
state_abbr = read_csv("http://goodcsv.com/wp-content/uploads/2020/08/us-states-territories.csv")
## US map with states
sf_map = us_states %>% st_simplify(preserveTopology=TRUE, dTolerance=10000) %>% st_as_sf() # reduce resolution

# preprocess data
df = pell %>%
    drop_na() %>%
    mutate(
        money_per_recipient = AWARD / RECIPIENT
    ) %>%
    left_join(
        state_abbr %>% 
            distinct(Name, Abbreviation) %>%
            mutate(Name = gsub("\\[E\\]","",Name)),
        by = c("STATE"="Abbreviation")
    ) %>%
    group_by(YEAR, STATE, Name) %>%
    summarize(
        money_per_recipient = sum(AWARD) / sum(RECIPIENT),
        n_universities = n(),
        total_recipients = sum(RECIPIENT),
        total_money = sum(AWARD)
    ) %>%
    ungroup() %>%
    group_by(STATE) %>%
    mutate(
        scaling_factor = money_per_recipient / sum(money_per_recipient)
    ) %>%
    ungroup()


df = sf_map %>%
    left_join(df, by = c("NAME"="Name")) %>%
    drop_na(YEAR)

nz_sfc = st_geometry(df)
scaling_factor = 1 #13 * df %>% pull(scaling_factor)
nz_centroid_sfc = st_centroid(nz_sfc)
nz_scale = (nz_sfc - nz_centroid_sfc) * scaling_factor + nz_centroid_sfc
df_scaled = st_set_geometry(df, nz_scale)

# plot
subtitle = "The **Pell Grants** are federal US grants for undergraduate students in financial need to continue their studies. Overall, across the different states, the granted amounts per recipient have increased steadily with peaks in 2010 and 2017."

plt = ggplot() +
    geom_sf(aes(geometry=geometry, fill=money_per_recipient), df_scaled, color=NA) +
    facet_wrap(~YEAR, nrow=3, strip.position=) +
    scale_fill_gradient(
        low="grey80", high="tan4",
        label=scales::comma, 
        guide=guide_legend(
            keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), 
            label.position = "bottom", title.position = 'top', title.hjust=1, nrow=1
        )
    ) +
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
ggsave(plot_file, plt, width=28, height=18, dpi=350, units="cm")

print("Done!")
