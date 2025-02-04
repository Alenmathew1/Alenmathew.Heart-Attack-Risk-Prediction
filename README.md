# Alenmathew.Heart-Attack-Risk-Prediction
# Load necessary libraries
library(dplyr)
library(tidyr)
library(ranger)
library(ggplot2)
library(caret)
library(vip)
library(rsample)
library(rattle)
library(rpart)
library(broom)
library(pROC)
 
# Data loading and cleaning
## Load dataset
heart_attack_dataset <- read.csv("C:/Users/midhu/OneDrive/Documents/dsci 401/project/heart_attack_prediction_dataset.csv")
 
## Explore dataset summary
summary(heart_attack_dataset)
 
## Clean dataset by removing duplicate rows
dataset <- distinct(heart_attack_dataset)
 
# Data transformation
## Split the blood pressure into systolic and diastolic columns
heart_dataset <- separate(dataset, Blood.Pressure, into = c("BP_systolic", "BP_diastolic"), sep = "/")
heart_dataset$BP_systolic <- as.numeric(heart_dataset$BP_systolic)
heart_dataset$BP_diastolic <- as.numeric(heart_dataset$BP_diastolic)
 
# Data exploration
## Number of heart attack risk by continent
ggplot(heart_dataset, aes(x =Continent,y=Heart.Attack.Risk , fill = Continent)) +
  geom_bar(stat = "identity") +
  labs(x = 'Continents', y = 'Count') 
x1 <- ggplot(heart_set, aes(x = factor(Smoking), fill = factor(Heart.Attack.Risk))) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Heart Attack Occurrence by Smoking Status",
       x = "Smoking Status",
       y = "Count") +
  scale_x_discrete(labels = c("Non-Smoker", "Smoker")) +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "salmon"))

x2 <- ggplot(heart_set, aes(x = factor(Sex), fill = factor(Heart.Attack.Risk))) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Heart Attack Occurrence by Gender",
       x = "Gender",
       y = "Count") +
  scale_x_discrete(labels = c("male", "female")) +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "salmon"))

x3 <- ggplot(heart_set, aes(x = factor(Diabetes), fill = factor(Heart.Attack.Risk))) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Heart Attack Occurrence based on diabetes status",
       x = "Diabetes condition",
       y = "Count") +
  scale_x_discrete(labels = c("Diabetes", "No Diabetes")) +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "salmon"))


x4 <- ggplot(heart_set, aes(x = factor(Family.History), fill = factor(Heart.Attack.Risk))) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Heart Attack Occurrence based on Family History",
       x = "Family history",
       y = "Count") +
  scale_x_discrete(labels = c("Previous family history", "No previous history")) +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "salmon"))

library(gridExtra)

grid.arrange(x1,x2,x3,x4, ncol = 2)

## Remove unnecessary columns
heart_set <- as.data.frame(heart_dataset[-c(1, 24, 25, 26)])
 
## Feature encoding
## Encode sex column
heart_set$Sex <- ifelse(heart_set$Sex == "Male", 1, 0)
 
## Encode diet column using ordinal encoding
mapping <- c("Unhealthy" = 1, "Average" = 2, "Healthy" = 3)
heart_set$Diet <- mapping[heart_set$Diet]
 
## Random forest model
 
## number of features
n_features <- length(setdiff(names(heart_set),heart_set$Heart.Attack.Risk))
heart_set$Heart.Attack.Risk<-as.factor(heart_set$Heart.Attack.Risk)
 
## Train a default random forest model
heart_rf1 <- ranger(
  Heart.Attack.Risk ~ ., 
  data = heart_set,
  num.trees = 500,
  importance = "permutation",
  mtry = floor(n_features / 3),
  seed = 310
)
heart_rf1
 
default_rmse <- sqrt(heart_rf1$prediction.error)
default_rmse
 
## Create hyperparameter grid
hyper_grid <- expand.grid(
  mtry = floor(n_features * c(.05, .15, .25, .333, .4)),
  min.node.size = c(1, 3, 5, 10), 
  replace = c(TRUE, FALSE),                               
  sample.fraction = c(.5, .63, .8),                       
  rmse = NA                                               
)
 
## Execute full cartesian grid search
for(i in seq_len(nrow(hyper_grid))) {
## Fit model for ith hyperparameter combination
  fit <- ranger(
    formula         = Heart.Attack.Risk ~ ., 
    data            = heart_set, 
    num.trees       = n_features * 10,
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$min.node.size[i],
    replace         = hyper_grid$replace[i],
    sample.fraction = hyper_grid$sample.fraction[i],
    verbose         = FALSE,
    seed            = 123,
    respect.unordered.factors = 'order',
  )
  ## Export OOB error 
  hyper_grid$rmse[i] <- sqrt(fit$prediction.error)
  
  
}
 
## Assess top 10 models
hyper_grid %>%
  arrange(rmse) %>%
  mutate(perc_gain = (default_rmse - rmse) / default_rmse * 100) %>%
  head(10)
 
## Final model
 
rf_impurity <- ranger(
  formula = Heart.Attack.Risk~ ., 
  data = heart_set, 
  num.trees = 230,
  mtry = 1,
  min.node.size = 5,
  sample.fraction = .50,
  replace = TRUE,
  importance = "impurity",
  respect.unordered.factors = "order",
  verbose = FALSE,
  seed  = 123
)
 
rf_impurity
 
p1 <- vip::vip(rf_impurity, num_features = 22, bar = FALSE)
p1
 
 
## Assuming rf_impurity is your trained Random Forest model
 
## Make predictions on the test set
predictions <- predict(rf_impurity, data = heart_set)
 
## Extract predicted values
predicted_values <- predictions$predictions
 
## Extract true values from the test set
true_values <- heart_set$Heart.Attack.Risk
 
## Calculate accuracy
accuracy <- mean(predicted_values == true_values)
print(paste("Accuracy:", accuracy))
 
 
 
 
## Logistic Regression
 
## 70 :30 split
set.seed(310)  # for reproducibility
data_split <- initial_split(heart_set, prop = .7, strata = "Heart.Attack.Risk")
data_train <- training(data_split)
data_test  <- testing(data_split)
 
model3 <- glm(
  Heart.Attack.Risk~.,
  family = "binomial", 
  data = imp_features
)
tidy(model3)
glance(model3)
## Assuming 'model3' is the logistic regression model
 
## Predict probabilities using the logistic regression model
predicted_probs <- predict(model1, type = "response")
 
## Create ROC curve object
roc_curve <- roc(data_train$Heart.Attack.Risk, predicted_probs)
 
## Plot ROC curve
plot(roc_curve, col = "blue", main = "ROC Curve", lwd = 2)
 
model1 <- glm(
  Heart.Attack.Risk~.,
  family = "binomial", 
  data = data_train
)
library(tidyverse)
tidy(model1)
glance(model3)
 
summary(model1)
vif(model3)
 
set.seed(310)
 
## Train a logistic regression model using cross-validation
cv_model3 <- train(
  Heart.Attack.Risk ~ ., 
  data = data_train, 
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10)
)
 
 
## Predict class using the cross-validated model
pred_class <- predict(cv_model3, newdata = data_test, type = "raw")  # Assuming "raw" probabilities
 
## Create confusion matrix
conf_matrix <- confusionMatrix(
  data = pred_class,  # Predicted classes
  reference = data_test$Heart.Attack.Risk  # True classes
)
logistic regression
library(mlr)
library(tidyverse)

dataset <- heart_set
data <- as_tibble(dataset)
dataUntidy <- gather(data, key = "Variable", value = "Value",
                        -Heart.Attack.Risk)
heartTask <- makeClassifTask(data = data, target = "Heart.Attack.Risk")
logReg <- makeLearner("classif.logreg", predict.type = "prob")
logRegModel <- train(logReg, heartTask)
#cv
kFold <- makeResampleDesc(method = "RepCV", folds = 10, reps = 50, 
                          stratify = TRUE)

kFoldCV <- resample(learner = logReg, task = heartTask, 
                    resampling = kFold, measures = list(mmce, acc))

kFoldCV$aggr

kFoldCV$measures.test

calculateConfusionMatrix(kFoldCV$pred, relative = TRUE)

logRegModelData <- getLearnerModel(logRegModel)
coef(logRegModelData)
exp(cbind(Odds_Ratio = coef(logRegModelData), confint(logRegModelData)))


testdata <-  heart_set[c(1,3,4,5,6,12,17,18,19,20)]
testdata <- head(testdata,500)
predict(logRegModel, newdata = testdata)
 
 
## Partial Least Squares (PLS) Model
 
set.seed(310)
cv_model_pls <- train(
  Heart.Attack.Risk ~ ., 
  data = data_train, 
  method = "pls",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10),
  preProcess = c("zv", "center", "scale"),
  tuneLength = 16
)
 
## Model with lowest RMSE
cv_model_pls$bestTune
 
## Results for model with lowest loss
cv_model_pls$results %>%
  dplyr::filter(ncomp == pull(cv_model_pls$bestTune))
 
## Plot cross-validated RMSE
ggplot(cv_model_pls)
 
## K-Nearest Neighbors (KNN)
 
 
knndata<-heart_set[c(3,4,7,22,23)]
 
hearttibknn<-as_tibble(knndata)
hearttibknn
 
data_task<-makeClassifTask(data = hearttibknn,target = "Heart.Attack.Risk")
knn<-makeLearner("classif.knn",par.vals = list("k"= 13))
 
## K fold cv
kfold<-makeResampleDesc(method = "RepCV",folds = 10 ,reps =20 ,stratify = TRUE)
kfoldcv<-resample(learner = knn,task=data_task,resampling = kfold,measures = acc)
 
kfoldcv$aggr
 
## Tune
 
knnparamspace<-makeParamSet(makeDiscreteParam("k",values = 1:10))
gridsearch<-makeTuneControlGrid()
cvfortunning<-makeResampleDesc("RepCV",folds= 10 ,reps = 20)
tunedk<-tuneParams("classif.knn",task = data_task,resampling = cvfortunning,par.set = knnparamspace,control = gridsearch)
 
tunedk
 
knntunningdata<-generateHyperParsEffectData(tunedk)
 
plotHyperParsEffect(knntunningdata, x="k" , y = "mmce.test.mean", plot.type = "line") + theme_bw()
 
tunedknn<-setHyperPars(makeLearner("classif.knn"),par.vals = tunedk$x)
 
tunnedknnmodel<-train(tunedknn,data_task)

## Applying pca to reduce dimention

heart_set1 <- heart_set[-23]

heart_PCA  <-  heart_set1%>% prcomp(center = TRUE, scale = TRUE)

heart_PCA

summary(heart_PCA)


library(dplyr)
library(purrr)

map_dfc(1:17, ~heart_PCA$rotation[, .] * sqrt(heart_PCA$sdev ^ 2)[.])


# Plotting pca results ----
#install.packages("factoextra")

library(factoextra)

pcaDat <- get_pca(heart_PCA)

pcaDat$coord


fviz_pca_biplot(heart_PCA, label = "var")

fviz_pca_var(heart_PCA, axes = c(3,4))


fviz_screeplot(heart_PCA, addlabels = TRUE, choice = "eigenvalue")


fviz_screeplot(heart_PCA, addlabels = TRUE, choice = "variance", ncomp = 17)
fviz_screeplot(heart_PCA, addlabels = TRUE, choose = "variance", ncp = 17)


PCA1 = SP_PCA$x[, 1]
dim(heart_set)
heart_PCA



swissPcaheart <- heart_set %>%  mutate(PC1 = heart_PCA$x[, 1], PC2 = heart_PCA$x[, 2],
                             PC3 = heart_PCA$x[, 3],PC4 = heart_PCA$x[, 4],
                             PC5 = heart_PCA$x[, 5],PC6 = heart_PCA$x[, 6],
                             PC7 = heart_PCA$x[, 7],PC8 = heart_PCA$x[, 8],
                             PC9 = heart_PCA$x[, 9],PC10 = heart_PCA$x[, 10],
                             PC11 = heart_PCA$x[, 11],PC12 = heart_PCA$x[, 12],PC13 = heart_PCA$x[, 13],PC14 = heart_PCA$x[, 14],
                             PC15 = heart_PCA$x[, 15],PC16 = heart_PCA$x[, 16],PC17 = heart_PCA$x[, 17])

## Here the 'x' values are the original features expressed in
## Terms of the principal components!
ggplot(swissPcaheart, aes(PC1, PC2, PC3,PC4,PC5,PC6,PC7,PC8,PC10,PC11,PC12,PC13,PC14,PC15,PC16,PC17col=Heart.Attack.Risk)) +
  stat_ellipse() + geom_point() + theme_bw() 
#stat_ellipse(level = 0.99)
library(ggplot2)
library(factoextra)
loadings <- get_pca_var(heart_PCA)$contrib[, 1:3]
top_vars <- apply(loadings, 2, function(x) names(x)[order(x, decreasing = TRUE)[1:4]])
## Create a bar plot of the top variables for each component
top_vars_df <- data.frame(component = rep(1:4, each = 3), variable = unlist(top_vars))
#untidy format for easy plot
titanicUntidy <- gather(top_vars_df, key = "variable", value = "component")
## Define a custom color palette with four colors
my_palette <- c("#004c6d","#004c6d" ,"#004c6d","#004c6d", "#004c6d", "#004c6d", "#004c6d","#a7d5ed", "#e2e2e2"
                , "#22a7f0", "#1984c5")

## important variables are triglyceride,Stress level, Smoking, sleep hours
ggplot(titanicUntidy, aes(x = variable, y = component, fill = component)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = my_palette) +  # Use the new color palette
  theme_classic() +
  labs(title = 'Top 3 Influential Variables of Top 3 Principal Components', x = 'Variable', y = 'Component')

ggplot(titanicUntidy, aes(x = variable, y = component, fill = component)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = my_palette) +  # Use the new color palette
  theme_classic() +
  labs(title = 'Top 4 Influential Variables of Top 3 Principal Components', x = 'Variable', y = 'Component') +
  theme(axis.text.y = element_blank())
