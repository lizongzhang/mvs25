library(readxl)
eg9_1 <- read_excel("eg9.1.xls")

#重命名
library(tidyverse)
data <- eg9_1 %>% rename(
  weight = x1,  waist = x2,  pulse = x3, 
  chinup = y1,   situp = y2,   jump = y3) %>% 
  dplyr::select(-1)

#定义两组变量
physical <- data[,1:3]
train <- data[,4:6]

#计算协方差矩阵
cov(data) %>% round(3)

#剖分协方差矩阵
Rxx <- cov(data)[1:3, 1:3]
Rxy <- cov(data)[1:3, 4:6]
Ryy <- cov(data)[4:6, 4:6]
Ryx <- cov(data)[4:6, 1:3]

#计算Rx
Rx <- solve(Rxx) %*% Rxy %*% solve(Ryy) %*% Ryx

# 计算Rx的特征值和特征向量
# Rx的特征向量: 第1典型变量u1与原始变量的线性组合
eigen(Rx)

#计算典型变量u1的原始得分
physical_score <- eigen(Rx)$vectors[,1] %*% t(physical)

# 计算典型变量u1原始得分的标准差
sd(physical_score)

# 标准化第1典型变量u1的系数: v1与原始变量的线性组合
eigen(Rx)$vectors[,1]/sd(physical_score)

#计算Ry
Ry <- solve(Ryy) %*% Ryx %*% solve(Rxx) %*% Rxy

#计算Ry的特征值和特征向量
eigen(Ry)

#计算典型变量v1的原始得分
train_score <- eigen(Ry)$vectors[,1] %*% t(train)

# 计算典型变量v1原始得分的标准差·
sd(train_score)


# 标准化典型变量v1的系数: v1与原始变量的线性组合
eigen(Ry)$vectors[,1]/sd(train_score)

library(CCA)
res.cc <- cc(physical, train)

#查看典型变量u1与原始变量的线性组合表达式
res.cc$xcoef

# 查看典型相关系数
res.cc$cor

# u1和v1的相关系数
cor(res.cc$scores$xscores[,1], res.cc$scores$yscores[,1])

# u2和v2的相关系数
cor(res.cc$scores$xscores[,2], res.cc$scores$yscores[,2])

# u3和v3的相关系数
cor(res.cc$scores$xscores[,3], res.cc$scores$yscores[,3])

# 保存典型变量得分scores

res.cc.score <- data.frame(
  physical_u1 = res.cc$scores$xscores[,1], 
  train_v1 = res.cc$scores$yscores[,1]
)

res.cc.score %>% 
  ggplot(aes(physical_u1,train_v1)) +
  geom_point()


# yacca::cca --------------------------------------------------------------

library(yacca)
res.cca <- cca(physical, train)

#查看结果
res.cca

#检验典型相关系数的显著性
F.test.cca(res.cca)

plot(res.cca)

# helio plot 典型变量与原始变量之间的相关系数
helio.plot(res.cca,
           x.name = "Physical Variables",
           y.name = "Train Variables")

helio.plot(res.cca,
           x.name = "Physical Variables",
           y.name = "Train Variables",
           type = "variance")

res.cca$xstructcorrsq
res.cca$ystructcorrsq



