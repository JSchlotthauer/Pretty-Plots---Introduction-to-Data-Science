library(dplyr)
library(countrycode)
library(ggplot2)

# World expenditure on health per capita in US$ - 2000 a 2019
# WORLD_EXP_YEAR
WORLD_EXP_YEAR = WORLD_EXP_YEAR

# World suicide rate: total and by gender - 2000 a 2019
# WORLD_SUIC_SEX
WORDL_SUIC_SEX = SUIC.RATES

### Plot SUICIDE RATE PER GENDER
WORDL_SUIC_SEX_graph = 
  WORDL_SUIC_SEX_EXP %>% 
  filter(`Indicador` != "Total") %>% 
  filter(`Year` %in%  c(2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018)) %>%
  ggplot(aes(x = Year, y = Value, fill = Indicador)) +
  scale_x_continuous(breaks = seq(min(WORDL_SUIC_SEX_EXP$Year), 
                                  max(WORDL_SUIC_SEX_EXP$Year), 
                                  by = 2)) +
  geom_bar(stat = "identity", position = position_dodge(width = 2)) +
  theme_bw() +
  theme(plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14), 
        plot.caption = element_text(size = 12)) +
  labs(title = "World suicide rate by sex", 
       subtitle = "Annual number of suicides per 100,000 males or females",
       y = "Suicide rate", 
       x = " ",
       fill = "",
       caption = "Source: World Health Organization")

### Plot SUICIDE RATE AND HEALTH EXPENDITURE 
WORDL_SUIC_SEX_EXP = 
  merge(WORDL_SUIC_SEX, WORLD_EXP_YEAR, by = "Year")

WORDL_SUIC_EXP_graph = 
  WORDL_SUIC_SEX_EXP %>% 
  filter(`Indicador` == "Total") %>% 
  ggplot(aes(x = Year)) +
  geom_line(aes(y = scale(Value), 
                color = "Suicide rate"), 
            linetype = "dashed", size = 1) +
  geom_line(aes(y = scale(Expenditure), 
                color = "Health expenditure"), 
            size = 1) +
  scale_x_continuous(breaks = seq(min(WORDL_SUIC_SEX_EXP$Year), 
                                  max(WORDL_SUIC_SEX_EXP$Year), 
                                  by = 2)) +
  scale_color_manual(values = c("black", "red")) +
  theme_bw() +
  theme(plot.title = element_text(size = 18, 
                                  face = "bold"),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14), 
        plot.caption = element_text(size = 12)) +
  labs(title = "World suicide rate and health expenditure", 
       subtitle = "Number of suicides per 100,000 people compare to health expenditure per capita",
       y = "Z - Score",
       x = " ",
       color = "",
       caption = "Source: World Development Indicators") 

### Plot RELATION SUIC x EXP
REGRESION_EXPxSUI = 
  WORDL_SUIC_SEX_EXP %>% 
  filter(`Indicador` == "Total") %>% 
  ggplot(aes(x = Expenditure, y = Value)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme(plot.title = element_text(size = 18, 
                                  face = "bold"),
        plot.subtitle = element_text(size = 14),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14), 
        plot.caption = element_text(size = 12)) +
  labs(title = "Relation between health expenditure and suicide rate", 
       subtitle = "Time frame: 2000 to 2019",
       y = "Suicide rate", 
       x = "Health expenditure",
       caption = "Scale(Expenditure): -9.631e-01")

## CORRELATION VALUES
REGRESION_EXPxSUI_values = 
  WORDL_SUIC_SEX_EXP %>% filter(`Indicador` == "Total")

lm(scale(Value) ~ scale(Expenditure), REGRESION_EXPxSUI_values)

### Plot WORDL EXP
HEALTHmapaPLOT = 
  ggplot(HEALTHmapa, aes(x = long, y = lat, group = group, order = order, fill = TOTAL))+
  geom_polygon (color = "black") +
  coord_map(xlim = c(-180, 180)) +
  scale_fill_gradientn(colors = c("#e9eb96", "#ebbe96", "#db8c8c")) +
  labs(fill = "US$ per capita") +
  theme_bw() +
  labs(x = " ", 
       y = " ", 
       title = "Health expenditure in the world", 
       subtitle = "Sum of annual health expenditure from 2000 to 2019",
       caption = "Source: World Development Indicators") +
  theme(plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 14),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        plot.caption = element_text(size = 12))

# FINAL PANEL
### WORDL_SUIC_SEX_graph
### WORDL_SUIC_EXP_graph
### REGRESION_EXPxSUI
### HEALTHmapaPLOT

PANEL =
  (WORDL_SUIC_SEX_graph + 
     WORDL_SUIC_EXP_graph + 
     REGRESION_EXPxSUI + 
     HEALTHmapaPLOT +
     plot_annotation(tag_levels = "A"))

PANEL + plot_layout(nrow = 2, 
                    ncol = 2,
                    widths = c(1, 1), 
                    heights = c(0.5, 0.5))