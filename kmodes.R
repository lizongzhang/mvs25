install.packages("klaR")


# 指定图形的中文字体
par(family  = 'STKaiti')
install.packages("showtext")
library(showtext)
showtext_auto()

# 加载必要的包
library(klaR)      # 用于 kmodes
library(tidyverse) # 用于数据处理和可视化
library(readr)     # 用于读取 CSV 文件

# 读取数据
library(readxl)
supermarket <- read_excel("supermarket.xlsx")

# 数据预处理
supermarket <- supermarket %>%
  rename(Marital = `Marital status`, Settlement = `Settlement size`) %>%
  mutate(
    Age_group = cut(Age, breaks = c(0,20,30,40,50,60,70,80), right = FALSE, 
                    labels = c("[0,20)","[20,30)", "[30,40)", "[40,50)", "[50,60)", "[60,70)", "[70,80)")),
    Income_group = cut(Income, breaks = c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000), 
                       right = FALSE, labels = c("[0,50k)", "[50k,100k)", "[100k,150k)", "[150k,200k)", 
                                                 "[200k,250k)", "[250k,300k)", "[300k,350k+)"))
  ) %>%
  select(Sex, Marital, Age_group, Education, Income_group, Occupation, Settlement) %>%
  mutate(across(.cols = everything(), .fns = as.factor)) %>%
  na.omit() %>% 
  as.data.frame()

# 重命名因子水平
supermarket$Sex <- factor(supermarket$Sex, levels = c(0, 1), labels = c("Male", "Female"))
supermarket$Marital <- factor(supermarket$Marital, levels = c(0, 1), labels = c("Single", "Non-single"))
supermarket$Education <- factor(supermarket$Education, levels = c(0, 1, 2, 3), 
                                labels = c("Other/Unknown", "High School", "University", "Graduate School"))
supermarket$Occupation <- factor(supermarket$Occupation, levels = c(0, 1, 2), 
                                 labels = c("Unemployed/Unskilled", "Skilled Employee/Official", 
                                            "Management/Self-employed/Highly Qualified"))
supermarket$Settlement <- factor(supermarket$Settlement, levels = c(0, 1, 2), 
                                 labels = c("Small City", "Mid-sized City", "Big City"))

# kmodes 聚类
set.seed(123)
kmodes_result <- kmodes(supermarket, modes = 4, iter.max = 10)

# 添加簇标签
supermarket$Cluster <- as.factor(kmodes_result$cluster)

# 概括每个类别的特征
cluster_summary <- supermarket %>%
  group_by(Cluster) %>%
  summarise(
    Sex = names(which.max(table(Sex))),
    Marital = names(which.max(table(Marital))),
    Age_group = names(which.max(table(Age_group))),
    Education = names(which.max(table(Education))),
    Income_group = names(which.max(table(Income_group))),
    Occupation = names(which.max(table(Occupation))),
    Settlement = names(which.max(table(Settlement))),
    Size = n()
  )

# 输出簇特征
cluster_summary

# 可视化：年龄组和收入组的簇分布
ggplot(supermarket, aes(x = Age_group, fill = Cluster)) +
  geom_bar(position = "dodge") +
  labs(title = "Age Group Distribution by Cluster", x = "Age Group", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(supermarket, aes(x = Income_group, fill = Cluster)) +
  geom_bar(position = "dodge") +
  labs(title = "Income Group Distribution by Cluster", x = "Income Group", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 输出簇大小
table(kmodes_result$cluster)



# k-prototype -------------------------------------------------------------

install.packages("clustMixType")
library(clustMixType)

# 读取数据
supermarket <- read_csv("supermarket.csv")

# 数据预处理
# 读取数据
supermarket <- read_csv("supermarket.csv")

# 数据预处理
supermarket <- supermarket %>%
  rename(Marital = `Marital status`, Settlement = `Settlement size`) %>%
  select(Sex, Marital, Education, Occupation, Settlement, Income, Age) %>%
  mutate(across(c(Sex, Marital, Education, Occupation, Settlement), 
                as.factor)) %>%
  na.omit() %>% 
  as.data.frame()

# 设置随机种子
set.seed(123)

# 选择混合类型变量
mix_data <- supermarket %>%
  select(Age, Income, Sex, Marital, Education, Occupation, Settlement)

# 运行 k-prototypes 聚类，设定 4 个簇
kproto_result <- kproto(mix_data, k = 4, verbose = TRUE)

# 查看聚类中心
kproto_result$centers

# 每个样本的簇标签
head(kproto_result$cluster)

# 各簇样本数量
table(kproto_result$cluster)

# 将簇标签添加到原始数据
supermarket$Cluster <- as.factor(kproto_result$cluster)


# 1️⃣ 数值变量在各簇的均值
num_summary <- supermarket %>%
  group_by(Cluster) %>%
  summarise(
    Mean_Age = mean(Age),
    Mean_Income = mean(Income),
    .groups = "drop"
  )
print(num_summary)

# 2️⃣ 分类变量在各簇的分布
cat_vars <- c("Sex","Marital","Education","Occupation","Settlement")

cat_summary <- supermarket %>%
  group_by(Cluster) %>%
  summarise(across(all_of(cat_vars), ~paste(names(sort(table(.), decreasing = TRUE))[1])), 
            .groups = "drop")
print(cat_summary)

# 3️⃣ 可视化各簇样本数量
cluster_count <- supermarket %>%
  count(Cluster)

ggplot(cluster_count, aes(x = Cluster, y = n, fill = Cluster)) +
  geom_bar(stat = "identity") +
  labs(title = "各簇样本数量", x = "簇编号", y = "样本数量") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

# 4️⃣ 可视化分类变量在各簇的分布（气泡图示例）
# 将分类变量展开
supermarket_long <- supermarket %>%
  pivot_longer(cols = all_of(cat_vars), names_to = "Variable", values_to = "Category")

ggplot(supermarket_long, aes(x = Variable, fill = Category)) +
  geom_bar(position = "fill") +
  facet_wrap(~Cluster) +
  labs(title = "各簇分类变量分布", y = "比例", x = "分类变量") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1")



  
