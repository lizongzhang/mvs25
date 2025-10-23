#P146，例题7.1 52名学生的6科目成绩
library(tidyverse)
library(readr)
library(psych)

eg7_1 <- read_csv("eg6.1.csv")

eg7_1 <- eg7_1 %>% rename(数学 = x1,
                          物理 = x2,
                          化学 = x3,
                          语文 = x4,
                          历史 = x5,
                          英语 = x6)

#提取的因子个数
#特征值大于1
#因子的累积贡献达到70%-80%以上

# 因子载荷估计方法
# 主成分法 principal()
# 极大似然法 fa()
# 主轴因子法 fa()

#因子旋转方法
#正交旋转：varimax, quartimax 因子相互独立的
#斜交旋转：oblimin, promax 因子之间是相关的

#正交旋转varimax(因子之间保持独立)
fa.pc.varimax <- principal(eg7_1, 
                           nfactors = 2,
                           rotate = "varimax")

fa.pc.varimax$loadings %>% print(digits = 3, 
                                 cut = 0.5, 
                                 sort = TRUE)

#斜交旋转promax 因子之间是相关的
fa.pc.promax <- principal(eg7_1, 
                           nfactors = 2,
                           rotate = "promax")
fa.pc.promax

fa.pc.promax$loadings %>% print(digits = 3, 
                                 cut = 0.5, 
                                 sort = TRUE)

# 极大似然法 maximum likelihood-------------------------------------------------------------------

#极大似然法,正交varimax旋转  
fa.ml.varimax <- fa(eg7_1, 
                 nfactors = 2, 
                 fm = "ml",
                 rotate = "varimax")

fa.ml.varimax$loadings %>%  print(digits =3, 
                    cut = 0.5, 
                    sort = TRUE)

#极大似然法, 斜交promax旋转  
fa.ml.promax <- fa(eg7_1, 
                    nfactors = 2, 
                    fm = "ml",
                    rotate = "promax")

fa.ml.promax$loadings %>%  print(digits =3, 
                         cut = 0.5, 
                         sort = TRUE)

# 主轴因子法 principal axis -------------------------------------------------------------------

#主轴因子法,正交varimax旋转 
fa.pa.varimax <- fa(eg7_1, 
                 nfactors = 2, 
                 fm = "pa",
                 rotate = "varimax")

fa.pa.varimax$loadings %>%  print(digits =3, 
                                  cut = 0.5, 
                                  sort = TRUE)

#主轴因子法, 斜交varimax旋转
fa.pa.promax <- fa(eg7_1, 
                    nfactors = 2, 
                    fm = "pa",
                    rotate = "promax")

fa.pa.varimax$loadings %>%  print(digits =3, 
                                  cut = 0.5, 
                                  sort = TRUE)










