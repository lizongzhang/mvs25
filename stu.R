data(mpg)
# mpg %>% 
#   ggplot(aes(displ, hwy, col = drv)) +
#   geom_point(aes(shape = cyl)) # 形状必须用离散型变量来映射，否则会报错

mpg %>% 
  ggplot(aes(displ, hwy, col = drv)) +
  geom_point(aes(shape = fl)) #chr类型就是离散型变量

mpg %>% 
  ggplot(aes(displ, hwy, col = drv, shape = fl)) + # col = drv放在ggplot画布上，
  #会对下面的所有图层生效
  geom_point() +
  geom_smooth(method = "lm", se = F) 

mpg %>% 
  ggplot(aes(displ, hwy, shape = fl)) + 
  #会对下面的所有图层生效
  geom_point(aes(color = drv)) + # col = drv放在点图图层，只对点图图层生效
  geom_smooth(method = "lm", se = F)

mpg %>% 
  ggplot(aes(displ, hwy, shape = fl)) + 
  #会对下面的所有图层生效
  geom_point() +
  geom_smooth(aes(color = drv),      # col = drv放在平滑线图层，只对平滑线图层生效
              method = "lm", se = F)

mpg %>% 
  ggplot(aes(hwy, fill = substr(trans, 1, 4))) +
  geom_histogram() +
  facet_wrap(~ substr(trans, 1, 4),
             labeller = labeller(`auto` = "Automatic", `manu` = "Manual")) +
  labs(title = "Highway Mileage Distribution by Transmission Type",
       x = "Highway Mileage", 
       y = "Frequency",
       fill = "Transmission")  


data(mtcars)

table(mtcars$vs, mtcars$am) %>% 
  chisq.test()

mtcars <- mtcars %>%
  mutate(vs = factor(vs, labels = c("V", "S")),
         am = factor(am, labels = c("Automatic", "Manual"))) 

table(mtcars$vs, mtcars$am) %>% 
  chisq.test()


install.packages("e1071")
library(e1071)

mpg %>% 
  select(hwy, cty) %>% 
  summarise(
    mean_hwy = mean(hwy),
    mean_cty = mean(cty),
    sd_hwy = sd(hwy),
    sd_cty = sd(cty),
    max_hwy = max(hwy),
    max_cty = max(cty),
    min_hwy = min(hwy),
    min_cty = min(cty),
    median_hwy = median(hwy),
    median_cty = median(cty),
    skew_hwy = e1071::skewness(hwy),
    skew_cty = e1071::skewness(cty),
    kurtosis_hwy = e1071::kurtosis(hwy),
    kurtosis_cty = e1071::kurtosis(cty)
    ) %>% 
  print(width = Inf)


# 定义x和y的值
x <- seq(-pi, pi, length.out = 50)
y <- x

# 生成网格数据
z <- outer(x, y, function(x, y) sin(x) * cos(y))

# 绘制3D曲面图
persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue")


# 安装plotly包，如果已经安装则可以跳过这一步
# install.packages("plotly")

# 加载plotly包
library(plotly)

# 使用plot_ly函数创建3D曲面图
plot_ly(x = ~x, y = ~y, z = ~z) %>% add_surface()


# 定义网格
x <- seq(-3, 3, length = 30)
y <- seq(-3, 3, length = 30)

# 创建网格的组合
grid <- expand.grid(x=x, y=y)

# 计算二元正态分布的密度
z <- matrix((1/(2*pi)) * exp(-0.5 * (grid$x^2 + grid$y^2)), 
            nrow = 30, ncol = 30, byrow = TRUE)

# 使用persp函数绘制3D图
persp(x, y, z, theta = 30, phi = 30, expand = 0.5, 
      col = "blue", xlab = "X", ylab = "Y", zlab = "Density")

# 使用plotly包下的plot_ly函数绘制3D图
library(plotly)
plot_ly(x = x, y = y, z = z) %>% add_surface()



library(tidyverse)

mpg %>% 
  group_by(drv) %>% 
  summary()

mpg %>% 
  group_by(drv) %>% 
  summarize(mean = format(round(mean(cty), 3), nsmall = 3), 
            sd = format(round(sd(cty), 3), nsmall = 3))

mean(mpg$cty[mpg$drv == "4"])

display.brewer.all()

library(tidyverse)
library(RColorBrewer)

# 查看所有可用的调色板
display.brewer.all()

ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point(size = 3) +
  scale_color_manual(values = brewer.pal(n = 3,    #所需的颜色数量     
                                         name = "Blues"), #调色板名称
                     labels = c("4-wheels", 
                                "front-wheel", "rear-wheel")) +
  theme_minimal() +
  labs(title = "Displacement vs Highway MPG",
       x = "Displacement",
       y = "Highway MPG",
       color = "Drive Type")

library(tidyverse)
library(RColorBrewer)
# 查看所有可用的调色板
display.brewer.all()

# 使用 ggplot2 绘制散点图
ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  # 绘制点图，点的大小为3
  geom_point(size = 3) +
  # 手动设置颜色，并使用 RColorBrewer 包中的 "Blues" 调色板
  scale_color_manual(values = brewer.pal(n = 3, name = "Paired"),
                     # 修改图例标签
                     labels = c("4-wheels", "front-wheel", "rear-wheel")) +
  # 使用极简主题
  theme_minimal() +
  # 设置标题和轴标签
  labs(title = "Displacement vs Highway MPG",
       x = "Displacement",
       y = "Highway MPG",
       color = "Drive Type")







