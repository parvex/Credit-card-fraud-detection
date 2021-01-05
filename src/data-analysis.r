checkNans <- function(data) {
  nans <- sum(is.na(data))
  
  if (nans > 0) {
    sprintf("Zbiór danych ma %d wartości NaN", nans)
    # TODO eliminate NaNs in this case
  } else {
    print("Zbiór nie ma wartości NaN")
  }
}

corMatrix <- function (data, title) {
  data$Class <- as.numeric(data$Class) - 1
  cor_matrix <- cor(data)
  corrplot(cor_matrix,
           type = "lower",
           tl.col = "black", tl.srt = 45,
           main = title,
           mar=c(0,0,1,0)
  )
  return(cor_matrix)
}

bigCorrsBoxPlot <- function (data, cor_matrix) {
  class_corrs <- cor_matrix["Class", ][-ncol(data)]
  big_corrs <- names(class_corrs)[abs(class_corrs) > 0.5]
  
  melted <- data %>% select(big_corrs, Class) %>% melt(id.var = "Class")
  ggplot(melted, aes(x=variable, y=value)) + 
    geom_boxplot(aes(fill=Class)) + 
    facet_wrap( ~ variable, scales="free")
  data$Class <- as.factor(data$Class)
}

create_log_tune_spec <- function () {
  return(logistic_reg(penalty = 10, mixture = 0.1) %>%
          set_engine("glmnet") %>%
          set_mode("classification"))
}

create_tree_tune_spec <- function () {
  return(decision_tree(
          cost_complexity = tune(),
          tree_depth = tune(),
          min_n = tune()
         ) %>% 
          set_engine("rpart") %>% 
          set_mode("classification")
        )
}
create_neural_net_spec <- function (epochs=20) {
  return(mlp(epochs = epochs, 
               hidden_units = 64)%>%
             set_mode("classification") %>% 
             set_engine("keras", verbose = 1)
         )
}

tune_with_data <- function (tune_spec, data, recipes) {
  folds <- vfold_cv(data, strata = Class)  
  
  tune_workflows <- list()
  
  for (recipe in recipes) {
      new_workflow <- workflow() %>% add_model(tune_spec) %>% add_recipe(recipe)
      tune_workflows <- append(tune_workflows, list(new_workflow))
  }
  
  final_workflows <- list()
  i <- 1
  for (workflow in tune_workflows) {
    HP_set <- parameters(workflow)
    
    result <- workflow %>% 
      tune_bayes(
        resamples = folds,
        # To use non-default parameter ranges
        param_info = HP_set,
        # Generate five at semi-random to start
        initial = 5,
        iter = 10,
        # How to measure performance?
        metrics = metric_set(roc_auc),
        control = control_bayes(no_improve = 5, verbose = TRUE)
      )
    
    best <- result %>% select_best("roc_auc")
    final_workflows[[i]] <- workflow %>% finalize_workflow(best)
    i <- i + 1
  }
  
  return(final_workflows)
}

fit_and_eval <- function(workflow, data_split) {
  final_fit <- last_fit(workflow, split = data_split)
  
  final_fit %>%
    collect_metrics()

  final_fit %>%
    collect_predictions() %>% 
    roc_curve(Class, .pred_1) %>% 
    autoplot()
}

