df <- read.csv("D:/Courses/Udemy - Complete Machine Learning with R studio - ML for 2020/Files-section8/House-Price.csv", header = TRUE)
boxplot(df$n_hot_rooms)

uv <- 3*quantile(df$n_hot_rooms, 0.99)
df$n_hot_rooms[df$n_hot_rooms > uv] <- uv
summary(df$n_hot_rooms)

lv <- 0.3*quantile(df$rainfall, 0.01)
df$rainfall[df$rainfall < lv] <- lv
summary(df$rainfall)

mean_value <- mean(df$n_hos_beds, na.rm = TRUE)
df$n_hos_beds[is.na(df$n_hos_beds)] <- mean_value
summary(df$n_hos_beds)

df$avg_dist <- (df$dist1 + df$dist2 + df$dist3 + df$dist4)/4
df2 <- df[,-6:-9]
df <- df2
rm(df2)
df <- df[,-13]

df <- dummy.data.frame(df)
df <- df[,-8]
df <- df[,-13]

# generalized linear model
#logistic regression with single predictor
glm_fit <- glm(Sold~price, data = df, family = binomial)
summary(glm_fit)

#logistic regression with multiple predictor
glm_fit_m <- glm(Sold~., data = df, family = binomial)
summary(glm_fit_m)

glm_probs <- predict(glm_fit_m, type = "response")
glm_probs[1:10]

glm_pred <- rep("NO",506)
glm_pred[glm_probs>0.5] <- "YES"
# confusion matrix
table(glm_pred, df$Sold)
# glm_pred   0   1
# NO  197  81
# YES  79 149

# linear discriminant analysis
lda_fit <- lda(Sold~., data = df)
summary(lda_fit)
lda_pred <- predict(lda_fit,df)
lda_class <- lda_pred$class
lda_pred$posterior
# confusion matrix
table(lda_class, df$Sold)
# lda_class   0   1
# 0 192  79
# 1  84 151

# test train split
set.seed(0)
split <- sample.split(df, SplitRatio = 0.8)
training_set <- subset(df, split == TRUE)
test_set <- subset(df, split == FALSE)

# prediction and confusion matrix for glm(logistic regression)
train_fit <- glm(Sold~., data = training_set, family = binomial)
test_probs <- predict(train_fit, test_set, type = "response")
test_pred <- rep("NO",120)
test_pred[test_probs>0.5] <- "YES"
table(test_pred, test_set$Sold)
# test_pred  0  1
# NO  42 16
# YES 26 36

# prediction and confusion matrix for lda
train_lda_fit <- lda(Sold~., data = training_set)
lda_test_probs <- predict(train_lda_fit, test_set)
lda_test_class <- lda_test_probs$class
table(lda_test_class, test_set$Sold)
# lda_test_class  0  1
# 0 44 16
# 1 24 36

# KNN classifier
trainX = training_set[,-16]
testX = test_set[,-16]
trainY = training_set$Sold
testY = test_set$Sold
k=3

trainX_s = scale(trainX)
testX_s = scale(testX)
set.seed(0)

knn.pred = knn(trainX_s, testX_s, trainY, k=k)
table(knn.pred, testY)
# knn.pred  0  1
# 0 38 24
# 1 30 28