library(tidyverse)
data(mpg)


library(ggstatsplot)

gghistostats(mpg, cty, 
             test.value = 16,
             bin.args = list(color = "black",
                             fill = "cyan")) +
  theme_classic(base_size = 15)


gghistostats(mpg, hwy, 
             test.value = 22,
             bin.args = list(color = "grey",
                             fill = "purple"),
             centrality.line.args = list(color = "red", 
                                         linewidth = 1, 
                                         linetype = "dashed")) +
  theme_classic(base_size = 15)

table(mpg$drv)

mpg %>%
  filter(drv %in% c("4", "f")) %>%
  ggbetweenstats(drv, 
                 hwy,
                 p.adjust.method = "none",
                 xlab = "drive",
                 package = "RColorBrewer",
                 palette = "Set1",
                 violin.args = list(width = 0)
                 ) +
  theme_classic(base_size = 15)



mpg %>%
  filter(drv %in% c("4", "f")) %>%
  ggbetweenstats(drv, 
                 cty,
                 p.adjust.method = "none",
                 xlab = "drive",
                 package = "RColorBrewer",
                 palette = "Set1",
                 violin.args = list(width = 0)
  ) +
  theme_classic(base_size = 15)

ggpiestats(mpg,
           drv,
           ratio = c(0.45, 0.45, 0.1),
           package = "RColorBrewer",
           palette = "Set2")+
  theme_bw(base_size = 15)

ggpiestats(mpg,
           drv,
           cyl,
           package = "RColorBrewer",
           palette = "Paired")+
  theme_bw(base_size = 15)

ggbarstats(mpg,
           drv,
           cyl,
           package = "RColorBrewer",
           palette = "Paired")+
  theme_bw(base_size = 15)


mpg %>% 
  mutate(transmission = if_else(substr(trans, 1, 4) == "auto", 
                                "automatic", 
                                "manual")) %>% 
  ggbarstats(drv,
             transmission,
             package = "RColorBrewer",
             palette = "Paired")+
  theme_bw(base_size = 15)
  







