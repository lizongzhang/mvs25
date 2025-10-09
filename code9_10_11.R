
# chap 9 goodness of fit------------------------------------------------------------------

obs_freq <- c(17, 27, 10, 28, 18)

chisq.test(obs_freq)

exp_freq <- c(15, 30, 10, 30, 15)

chisq.test(obs_freq, p = exp_freq, rescale.p = TRUE)

chisq.test(obs_freq, p = exp_freq, rescale.p = TRUE) %>% 
  str()


library(ggstatsplot)
library(tidyverse)

df <- data.frame
df <- data.frame(id = 1:100, 
                 weekday = c(rep("Mon", 17),
                             rep("Tue", 27),
                             rep("Wed", 10),
                             rep("Thu", 28),
                             rep("Fri", 18)))

df %>% 
  ggpiestats(weekday,
             ratio = c(0.15, 0.3, 0.1, 0.3, 0.15),
             package = "RColorBrewer",
             palette = "Set3")+
  theme_bw(base_size = 12)

#查看调色板颜色
library(RColorBrewer)
display.brewer.all()


# chap 9 independent test -------------------------------------------------

# 例题9.3, P169
# 创建数据矩阵
sales <- matrix(c(52, 64, 24, 60, 59, 52, 50, 65, 74), 
                     nrow = 3, 
                     byrow = TRUE)

rownames(sales) <- c("甲地区", "乙地区", "丙地区")
colnames(sales) <- c("一级", "二级", "三级")

chisq.test(sales)

chisq.test(sales) %>% str()

chisq.test(sales)$expected

#计算百分比
sales %>% 
  prop.table()

sales %>% 
  prop.table(1)

sales %>% 
  prop.table(2)


# data frame
table(mpg$drv, mpg$cyl) %>% 
  chisq.test() 


table(mpg$drv, mpg$cyl) %>% 
  chisq.test(simulate.p.value = TRUE) 

table(mpg$cyl)

df <- mpg %>% 
  filter(cyl != 5)


table(df$drv, df$cyl) %>% 
  chisq.test() 


# ggbarstats --------------------------------------------------------------


ggbarstats(mpg,
           drv,
           cyl,
           package = "RColorBrewer",
           palette = "Paired")+
  theme_bw(base_size = 15)






# chap 10 anova -----------------------------------------------------------


# one-way ANOVA -----------------------------------------------------------
library(readxl)
eg10_1 <- read_excel("例10.1.xlsx")

#wide to long
eg10_1_df <- pivot_longer(
  eg10_1,
  cols = everything(),  # 或指定列名 c(零售业, 旅游业, 航空公司, 家电制造业)
  names_to = "industry",  # 新的列名，用于存放原来的列名（行业名称）
  values_to = "value"  # 新的列名，用于存放对应的值
) %>% 
  na.omit()

eg10_1_df %>% 
  aov(value ~ industry, data = .) %>% 
  summary()



# ggbetweenstats ----------------------------------------------------------


library(showtext)
showtext_auto()

eg10_1_df %>% 
  ggbetweenstats(industry, 
                 value,
                 violin.args = list(width = 0))



# two-way anova -----------------------------------------------------------



mpg %>% 
  mutate(trans = ifelse(substr(trans, 1, 4) == "auto", 
                        "automatic", 
                        "manual")) %>% 
  aov(cty ~ trans + drv, data = .) %>% 
  summary()

mpg %>% 
  mutate(trans = ifelse(substr(trans, 1, 4) == "auto", 
                        "automatic", 
                        "manual")) %>% 
  aov(cty ~ trans + drv + trans:drv, data = .) %>% 
  summary()


# chap 11 correlation -----------------------------------------------------
install.packages("corrplot")
library(corrplot)

mpg %>% 
  select_if(is.numeric) %>% 
  cor()

mpg %>% 
  select_if(is.numeric) %>% 
  cor() %>% 
  round(3) %>% 
  corrplot(addCoef.col = "white",
           number.cex = 0.8,
           number.digits = 3,
           tl.cex = 0.8,
           cl.length = 11,
           type = "upper")


# chap 11 regression ------------------------------------------------------
data(mpg)

lm(mpg$cty ~ mpg$displ) %>% summary()

mpg %>% 
  lm(cty ~ displ, data = .)


lm(mpg$cty[mpg$cyl == 4] ~ mpg$displ[mpg$cyl == 4]) %>% summary()

mpg %>% 
  ggplot(aes(displ, cty, color = as.factor(cyl))) +
  geom_point() +
  geom_smooth(method = "lm")


install.packages("ggpubr")
library(ggpubr)

mpg %>% 
  ggscatter("displ", "cty",
            add = "reg.line", 
            color = "red") +
  stat_regline_equation(formula = mpg$cty ~ poly(mpg$displ,1))

mpg %>% 
  ggscatter("displ", "cty",
            add = "reg.line", 
            color = "red") +
  stat_regline_equation(formula = mpg$cty ~ mpg$displ,
                        aes(label =  paste(after_stat(eq.label), 
                                           ..adj.rr.label.., 
                                           sep = "~~~~")))



