Sys.setlocale(category = "LC_ALL", locale = "Polish")
install.packages("pacman")
library(pacman)
pacman::p_load(dplyr, 
               scales, 
               tidymodels, 
               tune, 
               workflows, 
               tictoc, 
               themis, 
               reshape2, 
               glmnet, 
               tensorflow, 
               keras,
               mlbench,
               caret,
               e1071,
               randomForest,
               ranger)
def_par = par(no.readonly = TRUE)
#data loading
if (!grepl('src$', getwd())) {
  setwd("src")
}
source("common.r")
load("rdata/datadf.Rdata")

data_df <- data_df %>% mutate_at(c("Amount", "Time"), ~ (scale(.) %>% as.vector()))

undersample_recipe <- recipe(Class~., data = data_df) %>%
  themis::step_downsample(Class)

undersampled_df <-undersample_recipe %>% prep(training = data_df) %>% bake(new_data=NULL)

oversample_recipe <- recipe(Class~., data = data_df) %>%
  themis::step_smote(Class)

oversampled_df <- oversample_recipe %>% prep(training = data_df) %>% bake(new_data=NULL)

# feature selection
set.seed(7)
trControl = trainControl(method="cv", number = 5, allowParallel = TRUE, verbose = TRUE)
rf_fit <- train(Class ~ ., data = data_df, method = "ranger", num.trees = 50, trControl = trControl, importance = 'impurity')

save(rf_fit, file = "rdata/rf_fit.Rdata")

imp <- varImp(rf_fit, scale = FALSE)
imp <- data.frame(imp[1])
rownames(imp)[imp$Overall > mean(imp$Overall)]
features <- rownames(imp)[imp$Overall > mean(imp$Overall)]

data_df_selected <- data_df[,which(names(data_df) %in% c(features, "Class"))]

selected_undersample_recipe <- recipe(Class~., data = data_df_selected) %>%
  themis::step_downsample(Class) %>%
  prep()

selected_undersampled_df <- bake(selected_undersample_recipe, new_data=NULL)

selected_oversample_recipe <- recipe(Class~., data = data_df_selected) %>%
  themis::step_smote(Class) %>%
  prep()

selected_oversampled_df <- bake(selected_oversample_recipe, new_data=NULL)

# splitting data into train and test sets
data_split <- initial_split(data_df, strata = Class)
data_train <- training(data_split)
data_test <- testing(data_split)

selected_data_split <- initial_split(data_df_selected, strata = Class)
selected_data_train <- training(selected_data_split)
selected_data_test <- testing(selected_data_split)

log_reg_tune_spec <- create_log_tune_spec()
tree_tune_spec <- create_tree_tune_spec()
undersample_neural_net_spec <- create_neural_net_spec(epochs = 300)
oversample_neural_net_spec <- create_neural_net_spec(epochs = 50)

# LOGISTIC REGRESSION
# training and testing on dataset with all features and using undersample
log_reg_undersample_workflow <- tune_with_data(log_reg_tune_spec, data_train, undersample_recipe)
log_req_undersample_final_fit <- fit_and_eval(log_reg_undersample_workflow, data_split)
log_req_undersample_final_fit %>% collect_metrics()

save(log_req_undersample_final_fit, file = "rdata/log_req_undersample_final_fit.Rdata")

# training and testing on dataset with selected features and using undersample
log_reg_undersample_selected_workflow <- tune_with_data(log_reg_tune_spec, selected_data_train, selected_undersample_recipe)
log_req_undersample_selected_final_fit <- fit_and_eval(log_reg_undersample_selected_workflow, selected_data_split)
log_req_undersample_selected_final_fit %>% collect_metrics()

save(log_req_undersample_selected_final_fit, file = "rdata/log_req_undersample_selected_final_fit.Rdata")

# training and testing on dataset with all features and using oversample
log_reg_oversample_workflow <- tune_with_data(log_reg_tune_spec, data_train, oversample_recipe)
log_req_oversample_final_fit <- fit_and_eval(log_reg_oversample_workflow, data_split)
log_req_oversample_final_fit %>% collect_metrics()

save(log_req_oversample_final_fit, file = "rdata/log_req_oversample_final_fit.Rdata")

# r training and testing on dataset with selected features and using oversample
log_reg_oversample_selected_workflow <- tune_with_data(log_reg_tune_spec, selected_data_train, selected_oversample_recipe)
log_req_oversample_selected_final_fit <- fit_and_eval(log_reg_oversample_selected_workflow, selected_data_split)
log_req_oversample_selected_final_fit %>% collect_metrics()

save(log_req_oversample_selected_final_fit, file = "data/log_req_oversample_selected_final_fit.Rdata")


# DECISION TREE

tree_undersample_workflow <- tune_with_data(tree_tune_spec, data_train, undersample_recipe)
tree_undersample_final_fit <- fit_and_eval(tree_undersample_workflow, data_split)
tree_undersample_final_fit %>% collect_metrics()

save(tree_undersample_final_fit, file = "rdata/tree_undersample_final_fit.Rdata")

tree_undersample_selected_workflow <- tune_with_data(tree_tune_spec, selected_data_train, selected_undersample_recipe)
tree_undersample_selected_final_fit <- fit_and_eval(tree_undersample_selected_workflow, selected_data_split)
tree_undersample_selected_final_fit %>% collect_metrics()

save(tree_undersample_selected_final_fit, file = "rdata/tree_undersample_selected_final_fit.Rdata")

tree_oversample_workflow <- tune_with_data(tree_tune_spec, data_train, oversample_recipe)
tree_oversample_final_fit <- fit_and_eval(tree_oversample_workflow, data_split)
tree_oversample_final_fit %>% collect_metrics()

save(tree_oversample_final_fit, file = "rdata/tree_oversample_final_fit.Rdata")

tree_oversample_selected_workflow <- tune_with_data(tree_tune_spec, selected_data_train, selected_oversample_recipe)
tree_oversample_selected_final_fit <- fit_and_eval(tree_oversample_selected_workflow, selected_data_split)
tree_oversample_selected_final_fit %>% collect_metrics()

save(tree_oversample_selected_final_fit, file = "rdata/tree_oversample_selected_final_fit.Rdata")


# NEURAL NETWORK

neural_net_undersample_workflow <- workflow() %>% add_model(undersample_neural_net_spec) %>% add_recipe(undersample_recipe)
neural_net_undersample_final_fit <- fit_and_eval(neural_net_undersample_workflow, data_split)
neural_net_undersample_final_fit %>% collect_metrics()

save(neural_net_undersample_final_fit, file = "rdata/neural_net_undersample_final_fit.Rdata")

neural_net_undersample_selected_workflow <- workflow() %>% add_model(undersample_neural_net_spec) %>% add_recipe(selected_undersample_recipe)
neural_net_undersample_selected_final_fit <- fit_and_eval(neural_net_undersample_selected_workflow, selected_data_split)
neural_net_undersample_selected_final_fit %>% collect_metrics()

save(neural_net_undersample_selected_final_fit, file = "rdata/neural_net_undersample_selected_final_fit.Rdata")

neural_net_oversample_workflow <- workflow() %>% add_model(oversample_neural_net_spec) %>% add_recipe(oversample_recipe)
neural_net_oversample_final_fit <- fit_and_eval(neural_net_oversample_workflow, data_split)
neural_net_oversample_final_fit %>% collect_metrics()

save(neural_net_oversample_final_fit, file = "rdata/neural_net_oversample_final_fit.Rdata")

neural_net_oversample_selected_workflow <- workflow() %>% add_model(oversample_neural_net_spec) %>% add_recipe(selected_oversample_recipe)
neural_net_oversample_selected_final_fit <- fit_and_eval(neural_net_oversample_selected_workflow, selected_data_split)
neural_net_oversample_selected_final_fit %>% collect_metrics()

save(neural_net_oversample_selected_final_fit, file = "rdata/neural_net_oversample_selected_final_fit.Rdata")
