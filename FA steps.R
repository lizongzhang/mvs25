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

# 第1步：评估数据是否适合做因子分析 -------------------------------------------------------
#计算KMO值
KMO(eg7_1)

#BARTLETT检验
#原假设：原始变量的相关系数矩阵是单位阵（相互独立）
cortest.bartlett(eg7_1)

# 第2步：绘制碎石图,确定因子个数 -----------------------------------------------------------
scree(eg7_1, factors = FALSE)

# 第3步：估计因子载荷 -------------------------------------------------------------

#主成分法 Principal Component Method
fa.pc.none <- principal(eg7_1, 
                        nfactors = 2,
                        rotate = "none")

fa.pc.none$values
fa.pc.none$loadings
fa.pc.none$scores

fa.pc.none$loadings %>% print(digits = 3, 
                              cut = 0.5, 
                              sort = TRUE)

#正交旋转varimax(因子之间保持独立)
fa.pc.varimax <- principal(eg7_1, 
                           nfactors = 2,
                           rotate = "varimax")

fa.pc.varimax$loadings %>% print(digits = 3, 
                        cut = 0.5, 
                        sort = TRUE)

#斜交旋转promax(因子之间是相关的)
fa.pc.promax <- principal(eg7_1, 
                           nfactors = 2,
                           rotate = "promax")

fa.pc.promax$loadings %>% print(digits = 3, 
                      cut = 0.5, 
                      sort = TRUE)

#斜交旋转oblimin
fa.pc.oblimin <- principal(eg7_1, 
                           nfactors = 2,
                           rotate = "oblimin")

fa.pc.oblimin$loadings %>% print(digits = 3, 
                       cut = 0.5, 
                       sort = TRUE)




