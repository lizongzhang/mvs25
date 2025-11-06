# UCBAdmissions 示例 —— 表示院系、性别与是否录取（含频数）
library(FactoMineR)
library(factoextra)

data(UCBAdmissions)
head(as.data.frame(UCBAdmissions))

# 展开频数表
df_tab2 <- as.data.frame(UCBAdmissions)    # Admit, Gender, Dept, Freq

df2 <- df_tab2[rep(1:nrow(df_tab2), df_tab2$Freq), 1:3]


# MCA 需要输入每一行为一个观测（个体），每列为一个分类变量（factor 或 character）
# 运行 MCA
res2 <- MCA(df2, graph = FALSE)

# 特征值 / 惯性
print(res2$eig)

# 类别坐标与贡献
res2$var$coord
res2$var$contrib

# 对应分析/MCA 通过多个维度捕捉数据中分类变量之间的结构。
# 特征值表示信息量，坐标表示类别在维度上的位置，贡献度表示哪些类别主导了维度的解释。

# 绘图
fviz_mca_var(res2, repel = TRUE, col.var = "contrib")

fviz_mca_var(res.mca, 
             axes = c(3, 4),
             repel = TRUE, col.var = "contrib")

fviz_mca_ind(res.mca, 
             geom = "point", 
             habillage = df2$Gender,      # 按性别上色
             addEllipses = TRUE,           # 添加椭圆
             ellipse.type = "confidence",  # 置信椭圆
             palette = c("#0072B2", "#D55E00"), # 自定义颜色（可选）
             repel = TRUE)



fviz_mca_ind(res.mca, 
             geom = "point", 
             habillage = interaction(df2$Gender, df2$Admit),  
             palette = brewer.pal(4, "Set3"), # 自定义颜色（可选）
             repel = TRUE)

fviz_mca_ind(res.mca, 
             geom = "point", 
             habillage = interaction(df2$Gender, df2$Admit,
                                     df2$Dept),      # 按性别上色
            palette = colorRampPalette(brewer.pal(9, "Blues"))(24), # 自定义颜色（可选）
             repel = TRUE)


# 假设 res.mca 是你的 MCA 结果，df2 是用于绘图的数据框（含 Gender, Admit, Dept）
groups <- interaction(df2$Gender, df2$Admit, df2$Dept)
coords <- res.mca$ind$coord[, 1:2]   # 每个个体在前两维的坐标（Dim1, Dim2）
library(dplyr)

df_coords <- data.frame(group = groups, coords)
centroids <- df_coords %>%
  group_by(group) %>%
  summarize(Dim1 = mean(Dim.1), Dim2 = mean(Dim.2), n = n()) %>%
  mutate(
    quadrant = case_when(
      Dim1 > 0 & Dim2 > 0  ~ "Q1 (右上)",
      Dim1 < 0 & Dim2 > 0  ~ "Q2 (左上)",
      Dim1 < 0 & Dim2 < 0  ~ "Q3 (左下)",
      Dim1 > 0 & Dim2 < 0  ~ "Q4 (右下)",
      TRUE ~ "On axis/near axis"
    )
  )

# 查看结果
print(centroids)
# 按象限列出组
split(centroids, centroids$quadrant)
