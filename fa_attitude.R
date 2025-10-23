library(readxl)
attitude <- read_excel("attitude.xlsx") 

library(tidyverse)
fa_df <- attitude %>% 
  dplyr::select(您人缘关系有多好分:子女有出息)

# 因子分析

fa_result <- principal(fa_df, 
                      nfactors = 10, 
                      rotate = "promax")

fa_result

#按因子载荷系数降序排列
print(fa_result$loadings, 
      digits = 3, cutoff = 0.5,sort = T)

# 第一轮调整：减少原始变量个数

# 把载荷变为普通矩阵
L <- as.matrix(fa_result$loadings)   # rows = variables, cols = factors
# 计算每个变量在所有因子上的最大绝对载荷
max_abs_loading <- apply(abs(L), 1, max)

# 找出所有最大绝对载荷 < 0.5 的变量
threshold <- 0.5
remove_vars <- names(max_abs_loading)[max_abs_loading < threshold]

remove_vars

# 从 fa_df 中剔除这些变量，得到新的数据集
fa_df_trim <- fa_df[, !(names(fa_df) %in% remove_vars), drop = FALSE]

# 重新做主成分/因子分析
fa_result_trim <- principal(fa_df_trim, 
                            nfactors = 7, 
                            rotate = "promax", 
                            scores = TRUE)

# 查看结果
print(fa_result_trim, cutoff = 0.3)
print(fa_result_trim$loadings, 
      digits = 3, cutoff = 0.5,sort = T)

# 第2轮调整：减少原始变量个数

# 把载荷变为普通矩阵
L <- as.matrix(fa_result_trim$loadings)   # rows = variables, cols = factors
# 计算每个变量在所有因子上的最大绝对载荷
max_abs_loading <- apply(abs(L), 1, max)

# 找出所有最大绝对载荷 < 0.5 的变量
threshold <- 0.5
remove_vars <- names(max_abs_loading)[max_abs_loading < threshold]

remove_vars

# 从 fa_df 中剔除这些变量，得到新的数据集
fa_df_trim2 <- fa_df_trim[, !(names(fa_df_trim) %in% remove_vars), drop = FALSE]

# 重新做主成分/因子分析
fa_result_trim2 <- principal(fa_df_trim2, 
                            nfactors = 6, 
                            rotate = "promax", 
                            scores = TRUE)

# 查看结果
print(fa_result_trim2$loadings, 
      digits = 3, cutoff = 0.5,sort = T)

# 绘制因子载荷系数图
fa.diagram(fa_result_trim2$loadings, digits = 3)

# 保存最终数据集
final_data <- cbind(attitude, fa_result_trim2$scores)

# 分析男性和女性在各个因子上的得分差异
final_data %>%
  group_by(male) %>%
  summarise(across(starts_with("RC"), 
                   mean, na.rm = TRUE))


# 分析男性和女性在因子1上的得分的分布的直方图
final_data %>%
  ggplot(aes(RC1, fill = as.factor(male))) +
  geom_histogram(col = 1, position = "identity", alpha = 0.5) +
  labs(fill = "Male") +
  scale_fill_manual(values = c("blue", "pink")) +
  theme_minimal() +
  ggtitle("Distribution of RC1 by Gender")

# 分析不同教育水平在各个因子上的得分差异因1的分布的直方图

final_data %>%
  ggplot(aes(RC1, fill = as.factor(EDU))) +
  geom_histogram(col = 1, position = "identity", alpha = 0.5) +
  labs(fill = "EDU") +
  theme_minimal() +
  ggtitle("Distribution of RC1 by Education")

# 利用6个因子得分进行K-means聚类分析

# K-means聚类分析
set.seed(123)  # For reproducibility

kmeans_result <- kmeans(final_data %>% select(starts_with("RC"),
                                              年龄, 
                                              主要工作总收入), 
                        centers =3, 
                        nstart = 25)

final_data$cluster <- as.factor(kmeans_result$cluster)

# 查看各聚类中心
kmeans_result$centers










