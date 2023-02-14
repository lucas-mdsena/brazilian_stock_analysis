# PROJECT - BRAZILIAN STOCK ANALYSIS


# PACKAGES
library(tidyverse) # Data manipulation and exploration
library(gmodels)
library(class) # KNN
library(C50) # Decision trees

# READING DATA
stocks <- read.csv(file = 'statusinvest.csv',
                   sep = ';',
                   dec = ',')

# BASIC INFO
stocks %>% str()

# PICKING FEATURES FOR EDA
stocks_eda <- stocks %>% select(1:5, 7:11, 18:20, 27:28)
stocks_eda %>% str()

# MISSING VALUES
stocks_eda %>% is.na() %>% sum()
# There are 390 missing values.

# The NAs in DY can be replaced by 0 and the others will be droped.
stocks_eda$DY[is.na(stocks_eda$DY)] <- 0

stocks_NA <- stocks_eda[rowSums(is.na(stocks_eda))>0, ]

stocks_eda <- stocks_eda %>% drop_na()

stocks_eda %>% is.na() %>% sum()


# DATA EXPLORATION AND VISUALIZATION

# Dividend Yield
stocks_eda %>% 
  ggplot() +
  geom_boxplot(aes(y = DY),
               fill = "lightblue",
               outlier.colour = "red",
               outlier.shape = 15,
               outlier.size = 1.6) +
  geom_hline(yintercept = mean(stocks_eda$DY, na.rm = T),
             color = "purple") +
  coord_flip() +
  labs(title = "Dividend Yield Distribution - DY",
       y = "Dividend Yield (%)",
       x = "") +
  theme_classic(base_size = 16) +
  theme(panel.background = element_rect(fill = "gray97"))



# CLASSIFICATION

# KNN
stocks_knn <- stocks_eda

# The average DY paid by brazilian stocks in 2022 was 9.25%. Base on that we will create a colum with 2 levels (high and low) and build a
# classification model.

stocks_knn$DY_CLASS <- with(stocks_knn, ifelse(DY>8, 'High', 'Low')) %>% 
  factor(levels = c('High', 'Low'),
         labels = c('High', 'Low'))
stocks_knn %>% str()
stocks_knn$DY_CLASS %>% table() %>% prop.table() %>% round(4) * 100

stocks_knn_scale <- stocks_knn[2:15] %>% scale()

# DIVIND INTO TRAIN AND TEST

knn_train <- stocks_knn_scale[1:400,]
knn_test <- stocks_knn_scale[401:514,]

knn_train_labels <- stocks_knn[1:400, 16]
knn_teste_labels <- stocks_knn[401:514, 16]

# TRAINING THE MODEL (KNN ALREADY TESTS)
model_knn <- knn(knn_train,
                 knn_test,
                 knn_train_labels,
                 k = 21)

# PREDICTING
CrossTable(knn_teste_labels,
           model_knn,
           prop.chisq = F)

# The model has 97.36% of accuracy.

# DECISION TREES

stocks_tree <- stocks_eda

stocks_tree$DY_CLASS <- with(stocks_tree, ifelse(DY>8, 'High', 'Low')) %>% 
  factor(levels = c('High', 'Low'),
         labels = c('High', 'Low'))

# TRAIN AND TEST DATASET
tree_train <- stocks_tree[1:450,-1]
tree_test <- stocks_tree[451:514,-1]

tree_test$DY_CLASS %>% table() %>% prop.table()
tree_train$DY_CLASS %>% table() %>% prop.table()

# TRAINING THE MODEL
tree_model <- C5.0(tree_train[,-15],
                   tree_train$DY_CLASS)
tree_model
tree_model %>% summary()






