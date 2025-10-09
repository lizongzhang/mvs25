# 安装与加载所需包
# install.packages("e1071")
# install.packages("ggplot2")

library(e1071)
library(ggplot2)

# 取两个变量做演示
iris_sub <- iris[, c("Sepal.Length", "Sepal.Width", "Species")]

# 建立 SVM 模型（径向基核函数 RBF）
svm_model <- svm(Species ~ ., data = iris_sub, 
                 kernel = "radial", 
                 cost = 1, 
                 gamma = 0.5)

# 生成网格点用于预测
xrange <- seq(min(iris_sub$Sepal.Length) - 0.5, 
              max(iris_sub$Sepal.Length) + 0.5, 
              by = 0.02)

yrange <- seq(min(iris_sub$Sepal.Width) - 0.5, 
              max(iris_sub$Sepal.Width) + 0.5, 
              by = 0.02)

grid <- expand.grid(Sepal.Length = xrange, 
                    Sepal.Width = yrange)

# 对网格点分类预测
grid$Species <- predict(svm_model, grid)


# 画出分类区域和样本点
ggplot() +
  geom_tile(data = grid, 
            aes(x = Sepal.Length, 
                y = Sepal.Width, 
                fill = Species), 
            alpha = 0.3) +
  geom_point(data = iris_sub, 
             aes(x = Sepal.Length, 
                 y = Sepal.Width, 
                 color = Species), 
             size = 2) +
  labs(title = "SVM on Iris Dataset (3-class)",
       x = "Sepal Length", 
       y = "Sepal Width") +
  theme_minimal()


# 计算预测准确率
pred <- predict(svm_model, iris_sub)
accuracy <- mean(pred == iris_sub$Species)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))

# 改进SVM模型
# 1. 调整参数
svm_model_tuned <- svm(Species ~ ., data = iris_sub,
                       kernel = "radial", cost = 10, gamma = 0.8)
pred_tuned <- predict(svm_model_tuned, iris_sub)
accuracy_tuned <- mean(pred_tuned == iris_sub$Species)
print(paste("Tuned Accuracy:", round(accuracy_tuned * 100, 2), "%"))

# 2. 使用不同核函数（线性核）
svm_model_linear <- svm(Species ~ ., data = iris_sub,
                        kernel = "linear", cost = 10)

pred_linear <- predict(svm_model_linear, iris_sub)

accuracy_linear <- mean(pred_linear == iris_sub$Species)

print(paste("Linear Kernel Accuracy:", round(accuracy_linear * 100, 2),
            "%"))

# 3. 数据预处理（标准化）
iris_sub_scaled <- iris_sub

iris_sub_scaled[, 1:2] <- scale(iris_sub_scaled[, 1:2])

svm_model_scaled <- svm(Species ~ ., 
                        data = iris_sub_scaled,
                        kernel = "radial", 
                        cost = 1, 
                        gamma = 0.5)

pred_scaled <- predict(svm_model_scaled, iris_sub_scaled)

accuracy_scaled <- mean(pred_scaled == iris_sub_scaled$Species)

print(paste("Scaled Data Accuracy:", round(accuracy_scaled * 100, 2),
            "%"))

# 4. 交叉验证选择最佳参数
tune_result <- tune(svm, Species ~ ., 
                    data = iris_sub,
                    ranges = list(cost = 10^(-1:2), 
                                  gamma = c(0.1, 0.5, 1)))

best_model <- tune_result$best.model

pred_best <- predict(best_model, iris_sub)

accuracy_best <- mean(pred_best == iris_sub$Species)

print(paste("Best Model Accuracy:", round(accuracy_best * 100, 2),
            "%"))

# 输出最佳参数
print(tune_result$best.parameters)

# 输出调参结果
print(tune_result)

# 画出最佳模型的分类区域和样本点
grid$Species <- predict(best_model, grid)
ggplot() +
  geom_tile(data = grid, 
            aes(x = Sepal.Length, 
                y = Sepal.Width, 
                fill = Species), 
            alpha = 0.3) +
  geom_point(data = iris_sub, 
             aes(x = Sepal.Length, 
                 y = Sepal.Width, 
                 color = Species), 
             size = 2) +
  labs(title = "Tuned SVM on Iris Dataset (3-class)",
       x = "Sepal Length", y = "Sepal Width") +
  theme_minimal()

# 5. 使用更多特征（这里仅用两个特征做可视化，实际可用全部特征）
# 由于可视化限制，无法直接展示多维特征空间的分类边
# 界，但可以通过准确率来评估模型性能
svm_model_full <- svm(Species ~ ., 
                      data = iris,
                      kernel = "radial", 
                      cost = 1, 
                      gamma = 0.5)

pred_full <- predict(svm_model_full, iris)

accuracy_full <- mean(pred_full == iris$Species)

print(paste("Full Feature Set Accuracy:", 
            round(accuracy_full * 100, 2), "%"))




# decathlon ---------------------------------------------------------------

library(dplyr)
library(readxl)
library(caret)
library(e1071)

# 导入数据
decathlon <- read_excel("decathlon.xlsx", 
                        col_types = c("numeric", "text", "numeric", 
                                      "numeric", rep("text", 3), rep("numeric", 3), "text", 
                                      rep("numeric", 11)))

# 创建 tier 分类变量
decathlon <- decathlon %>% mutate(tier = case_when(
  rank %in% 1:33 ~ "first",
  rank %in% 34:66 ~ "second",
  rank %in% 67:100 ~ "third",
  TRUE ~ NA_character_
))

# 预处理
decathlon <- na.omit(decathlon)
dummy <- dummyVars(" ~ tier", data = decathlon)
tier_encoded <- data.frame(predict(dummy, newdata = decathlon))
decathlon_encoded <- cbind(decathlon[, 8:17], tier_encoded)  # 8:17 为 M100 到 javelin_throw
str(decathlon_encoded)

# 数据分割
set.seed(123)
train_index <- createDataPartition(decathlon_encoded$tier.first, p = 0.7, list = FALSE)
train_data <- decathlon_encoded[train_index, ]
test_data <- decathlon_encoded[-train_index, ]
x_train <- train_data[, 1:10]
y_train <- factor(apply(train_data[, 11:13], 1, which.max) - 1)
x_test <- test_data[, 1:10]
y_test <- factor(apply(test_data[, 11:13], 1, which.max) - 1)

# 训练模型
svm_linear <- svm(y_train ~ ., data = cbind(x_train, y_train), kernel = "linear", cost = 1)
svm_rbf <- svm(y_train ~ ., data = cbind(x_train, y_train), kernel = "radial", cost = 1, gamma = 0.1)

# 预测和评估
pred_linear <- predict(svm_linear, newdata = x_test)
acc_linear <- mean(pred_linear == y_test)
cat("Linear SVM Accuracy:", acc_linear, "\n")

pred_rbf <- predict(svm_rbf, newdata = x_test)
acc_rbf <- mean(pred_rbf == y_test)
cat("RBF SVM Accuracy:", acc_rbf, "\n")

table(Predicted = pred_rbf, Actual = y_test)

# 可视化
plot(svm_rbf, data = cbind(x_train, y_train), M100 ~ long_jump)

# 预测新个案
new_case <- data.frame(
  M100 = 10.73,
  Hurdles_M100 = 15.34,
  M400 = 47.93,
  M1500 = 288.58, 
  long_jump = 7.58,
  high_jump = 2.06,
  pole_vault = 5.10,
  shot_put = 14.28,
  discus_thow = 36.93,  # 需确认是否为 discus_throw
  javelin_throw = 61.43
)
new_pred <- predict(svm_rbf, newdata = new_case)
cat("New Case Predicted Class:", new_pred, "\n")
