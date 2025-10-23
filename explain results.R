#P146，例题7.1 52名学生的6科目成绩
library(psych)

library(readr)
eg7_1 <- read_csv("eg6.1.csv")

library(tidyverse)
eg7_1 <- eg7_1 %>% rename(数学 = x1,
                          物理 = x2,
                          化学 = x3,
                          语文 = x4,
                          历史 = x5,
                          英语 = x6)

fa.pc.none <- principal(eg7_1, nfactors = 2,
                        rotate = "none")

fa.pc.none

#计算原始变量相关系数矩阵的特征值和特征向量
eg7_1.ev <- eg7_1 %>% cor() %>% eigen()
eg7_1.ev

#计算因子载荷
eg7_1.ev$vectors %*% diag(sqrt(eg7_1.ev$values))

eg7_1.ev$vectors[,1]*sqrt(eg7_1.ev$values[1])

fa.pc.none
# com的含义
# com : Hoffman's index of complexity for each item，
#该值越接近于1，代表该原始变量主要由某个因子代表。 

#h2，Communality， 共同度h2=每一行loadings的平方再相加
#共同度：公共因子对某个变量的方差贡献
#u2, uniquenesses, 唯一性/特殊度u2=1-h2，特殊因子对某个变量的方差贡献
sum(fa.pc.none$loadings[1,]^2)

sum(fa.pc.none$loadings[2,]^2)

#SS loadings, 某个公共因子对所有变量的方差贡献（公共因子与所有原始变量的关系）
#SS loadings = 每一列loadings的平方再相加

sum(fa.pc.none$loadings[,1]^2)
sum(fa.pc.none$loadings[,2]^2)

#公共因子的方差贡献Proportion Var
sum(fa.pc.none$loadings[,1]^2)/6
sum(fa.pc.none$loadings[,2]^2)/6

sum(fa.pc.none$uniquenesses)/6







