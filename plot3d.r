# 安装并加载必要的包
# install.packages("MASS")
# install.packages("scatterplot3d")

par(family  = 'STKaiti')
#install.packages("showtext")
library(showtext)
showtext_auto()

library(MASS)
library(scatterplot3d)

set.seed(123)

# 均值向量
mean1 <- c(0, 0, 0)
mean2 <- c(4, 4, 4)

# 相同协方差矩阵（形状相同，方向一致）
cov_shared <- matrix(c(3, 0.8, 0.6,
                      0.8, 3, 0.7,
                      0.6, 0.7, 3), 
                     nrow = 3, byrow = TRUE)

data1_shared <- mvrnorm(n = 300, mu = mean1, Sigma = cov_shared)
data2_shared <- mvrnorm(n = 300, mu = mean2, Sigma = cov_shared)

# 不同协方差矩阵（形状和方向差异显著）
mean1 <- c(0, 0, 0)
mean2 <- c(2, 2, 2)


cov1 <- matrix(c(3, 0.8, 0.6,
                 0.8, 3, 0.7,
                 0.6, 0.7, 3), nrow = 3, byrow = TRUE)
cov2 <- matrix(c(1, -0.8, -0.6,
                 -0.8, 1, -0.7,
                 0.6, -0.7, 1), nrow = 3, byrow = TRUE)

data1_diff <- mvrnorm(n = 300, mu = mean1, Sigma = cov1)
data2_diff <- mvrnorm(n = 300, mu = mean2, Sigma = cov2)


# 绘图：相同协方差矩阵
par(mfrow = c(1, 2)) # 1行2列布局

# 左图：相同协方差矩阵
s3d1 <- scatterplot3d(data1_shared, color = rgb(0, 0, 1, 0.5), pch = 16,
                      xlab = "X1", ylab = "X2", zlab = "X3",
                      main = "协方差矩阵相等",
                      xlim = range(c(data1_shared[,1], data2_shared[,1])),
                      ylim = range(c(data1_shared[,2], data2_shared[,2])),
                      zlim = range(c(data1_shared[,3], data2_shared[,3])))

s3d1$points3d(data2_shared, col = rgb(1, 0, 0, 0.5), pch = 16)

# 右图：不同协方差矩阵
s3d2 <- scatterplot3d(data1_diff, color = rgb(0, 0, 1, 0.5), pch = 16,
                      xlab = "X1", ylab = "X2", zlab = "X3",
                      main = "协方差矩阵不等",
                      xlim = range(c(data1_diff[,1], data2_diff[,1])),
                      ylim = range(c(data1_diff[,2], data2_diff[,2])),
                      zlim = range(c(data1_diff[,3], data2_diff[,3])))

s3d2$points3d(data2_diff, col = rgb(1, 0, 0, 0.5), pch = 16)


# 多元正态分布 ------------------------------------------------------------------

# a simulation of 5 variable data distribution.

# load library MASS
library(MASS)

# set seed and create data vectors
set.seed(98989)
sample_size <- 1000                                       
sample_meanvector <- c(10, 5, 7, 9, 20)                                   
sample_covariance_matrix <- matrix(c(5, 4, 3, 2, 1, 4, 5, 4, 3, 2,
                                     3, 4, 5, 4, 3, 2, 3, 4, 5, 4, 1, 
                                     2, 3, 4, 5), ncol = 5)

# create multivariate normal distribution
sample_distribution <- mvrnorm(n = sample_size,
                               mu = sample_meanvector, 
                               Sigma = sample_covariance_matrix)

# print top of distribution
head(sample_distribution)



# simulation --------------------------------------------------------------

# 安装必要的包
install.packages(c("MASS", "mvtnorm", "plotly", "ggplot2"))
library(MASS)
library(mvtnorm)
library(plotly)
library(ggplot2)
library(tidyverse)

# -------------------
# 1. 一元正态
# -------------------
set.seed(123)
x1 <- rnorm(1000, mean = 0, sd = 1)

# 绘制概率密度
ggplot(data.frame(x=x1), aes(x=x)) +
  geom_density(fill="skyblue", alpha=0.5) +
  labs(title="一元正态分布", x="X", y="Density")

# -------------------
# 2. 二元正态
# -------------------
mu2 <- c(0, 0)
Sigma2 <- matrix(c(1, 0.6, 0.6, 1), 2, 2)
set.seed(123)
x2 <- mvrnorm(500, mu2, Sigma2)
df2 <- data.frame(x=x2[,1], y=x2[,2])

# 散点图
ggplot(df2, aes(x=x, y=y)) +
  geom_point(alpha=0.5, color="blue") +
  stat_density_2d(aes(fill = ..level..), geom="polygon", alpha=0.3) +
  labs(title="二维正态分布", x="X1", y="X2")

# -------------------
# 3. 三元正态
# -------------------
mu3 <- c(0, 0, 0)
Sigma3 <- matrix(c(1,0.5,-0.3, 
                   0.5,1,0.4, 
                   -0.3,0.4,1), 3, 3)
set.seed(123)
x3 <- mvrnorm(500, mu3, Sigma3)
df3 <- data.frame(x=x3[,1], y=x3[,2], z=x3[,3])

# 三维散点 + 密度表面
plot_ly(df3, 
        x=~x, y=~y, z=~z, 
        type="scatter3d", 
        mode="markers", 
        marker=list(size=3)) %>%
  layout(title="三维正态分布散点图")


# multi-normal pdf --------------------------------------------------------

# 安装必要的包
install.packages(c("MASS", "mvtnorm", "plotly", "ggplot2"))
library(MASS)
library(mvtnorm)
library(plotly)
library(ggplot2)

# ---------------------------
# 设置随机种子
set.seed(1000)

# 均值
mean_vec <- c(0, 0)

# 协方差列表
cov_vals <- c(-0.8, 0, 0.8)

# 存储密度
pdf_list <- list()

# 网格点
x <- seq(-3, 3, length.out = 100)
y <- seq(-3, 3, length.out = 100)
grid <- expand.grid(x=x, y=y)

# ---------------------------
# 绘制三维曲面
fig3d <- plot_ly()

for (idx in seq_along(cov_vals)) {
  val <- cov_vals[idx]
  Sigma <- matrix(c(1, val, val, 1), 2, 2)
  
  # 计算每个网格点的密度
  pdf <- matrix(dmvnorm(grid, mean=mean_vec, sigma=Sigma), nrow=100, ncol=100)
  pdf_list[[idx]] <- pdf
  
  fig3d <- fig3d %>% add_surface(
    x = x, y = y, z = pdf,
    showscale = FALSE
  ) %>% layout(
    scene = list(
      xaxis = list(title="x1"),
      yaxis = list(title="x2"),
      zaxis = list(title="", showticklabels=FALSE)
    ),
    title = paste("Covariance =", val)
  )
}
fig3d

# ---------------------------
# 绘制等高线图
par(mfrow=c(1,3))  # 三列并排
for (idx in seq_along(pdf_list)) {
  filled.contour(
    x, y, pdf_list[[idx]],
    color.palette = terrain.colors,
    plot.title = title(main=paste("Covariance =", cov_vals[idx]),
                       xlab="x1", ylab="x2")
  )
}




