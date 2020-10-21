#[1] "rattle"            "bitops"            "tibble"            "correlationfunnel"
#[5] "corrplot"          "caret"             "lattice"           "dplyr"            
#[9] "ggplot2"           "stats"             "graphics"          "grDevices"        
#[13] "utils"             "datasets"          "methods"           "base" 
library(ggplot2)
library(dplyr)
library(caret)
library(rattle)
setwd("F:/DS/ASS/ML/Input")
train <- read.csv("pml-training.csv")
temp <- train %>%   select(which(colMeans(is.na(.)) < 0.5)) 
temp2 <- temp[,-grep("kurtosis",names(temp))]
temp3 <- temp2[,-grep("skewness",names(temp2))]
temp3 <- temp3[,-grep("max",names(temp3))]
temp3 <- temp3[,-grep("min",names(temp3))]
temp3 <- temp3[,-grep("amplitude",names(temp3))]
temp3 <- temp3[,-grep("timestamp",names(temp3))]
train <- train[,-grep("window",names(train))]
train <- temp3
rm(temp,temp2,temp3)
cv_train <-train[train_p,]
cv_test <- train[-train_p,]
table(cv_train$classe)/nrow(cv_train)
#From the above it is clear that there are not that much 
#bias in the data in term of different "classe". 

cor_cv_train <- cor(cv_train[,3:54])
diag(cor_cv_train) <- 0
cor_cv_train  <- which(abs(cor_cv_train)>0.8,arr.ind = T)
cor_cv_train <-unique(row.names(cor_cv_train))
corrplot(cor(select(cv_train,cor_cv_train)),type="upper", 
         order="hclust",method = "number")

bi_cv_train <- cv_train %>% binarize(n_bins = 4, thresh_infreq = 0.01)
corr_a <- bi_cv_train %>% correlate(target = classe__A) 
corr_a %>% plot_correlation_funnel(interactive = T,limits = c(-0.5,0.5))
corr_a_sub <- head(corr_a %>% mutate(corr = abs(correlation)) %>% 
                       arrange(desc(corr)) %>% select(feature) %>% unique(),20)
corr_a_sub$feature[which(temp_a$feature %in% cor_cv_train)]

corr_b <- bi_cv_train %>% correlate(target = classe__B) 
corr_b %>% plot_correlation_funnel(interactive = T,limits = c(-0.5,0.5))
corr_b_sub <- head(corr_b %>% mutate(corr = abs(correlation)) %>% 
                           arrange(desc(corr)) %>% select(feature) %>% unique(),20)
corr_b_sub$feature[which(temp_a$feature %in% cor_cv_train)]

corr_c <- bi_cv_train %>% correlate(target = classe__C) 
corr_c %>% plot_correlation_funnel(interactive = T,limits = c(-0.5,0.5))
corr_c_sub <- head(corr_c %>% mutate(corr = abs(correlation)) %>% 
                           arrange(desc(corr)) %>% select(feature) %>% unique(),20)
corr_c_sub$feature[which(temp_a$feature %in% cor_cv_train)]

corr_d <- bi_cv_train %>% correlate(target = classe__D) 
corr_d %>% plot_correlation_funnel(interactive = T,limits = c(-0.5,0.5))
corr_d_sub <- head(corr_d %>% mutate(corr = abs(correlation)) %>% 
                           arrange(desc(corr)) %>% select(feature) %>% unique(),20)
corr_d_sub$feature[which(temp_a$feature %in% cor_cv_train)]

corr_e <- bi_cv_train %>% correlate(target = classe__E) 
corr_e %>% plot_correlation_funnel(interactive = T,limits = c(-0.5,0.5))
corr_e_sub <- head(corr_e %>% mutate(corr = abs(correlation)) %>% 
                           arrange(desc(corr)) %>% select(feature) %>% unique(),20)
corr_e_sub$feature[which(temp_a$feature %in% cor_cv_train)]


# Decision Tree Model and Prediction
DTM <- train(classe ~. , data = cv_train,method = "rpart")
fancyRpartPlot(DTM$finalModel)
set.seed(21243)
DTP<- predict(DTM, cv_test)
confusionMatrix(DTP, cv_test$classe)


#Random Forest Model and Prediction
set.seed(26817)
RFM <- train(classe ~. , data = cv_train,method = "rf")
 




