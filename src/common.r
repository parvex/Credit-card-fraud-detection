# Plik zawiera specyfikacje funkcji używanych zarówno w pliku models.r oraz notebook.Rmd,
# a także funkcje, które pozwalają skrócić i uporządkować kod we wspomnianych plikach.

# Sprawdź, czy zbiór danych zawiera wartosci NaN
checkNans <- function(data) {
  nans <- sum(is.na(data))
  
  if (nans > 0) {
    sprintf("Zbior danych ma %d wartosci NaN", nans)
    # TODO eliminate NaNs in this case
  } else {
    print("W zbiorze nie znajduja sie wartosci NaN")
  }
}

# Funkcja tworzy i zwraca macierz korelacji dla danego zbioru danych. Funkcja oczekuje, że w zbiorze będzie atrybut Class, jest więc 
# podporządkowana pod nasze zadanie 
corMatrix <- function (data, title) {
  data$Class <- as.numeric(data$Class) - 1
  cor_matrix <- cor(data)
  corrplot(cor_matrix,
           type = "lower",
           tl.col = "black", tl.srt = 45,
           main = title,
           mar=c(0,0,1,0),
           tl.cex=1
  )
  return(cor_matrix)
}

# Funkcja pomocnicza do tworzenia specyfikacji modelu regresji logistycznej, która następnie zostanie użyta w
# strojeniu i trenowaniu modelu.
create_log_tune_spec <- function () {
  return(logistic_reg(penalty = tune(), mixture = 1) %>%
          set_engine("glmnet") %>%
          set_mode("classification"))
}

# Funkcja pomocnicza do tworzenia specyfikacji modelu drzewa klasyfikacji. 
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

#Funkcja pomocnicza do tworzenia specyfikacji sieci neuronowej. Tworzy dwuwarstwowy perceptron z 64 neuronami.
create_neural_net_spec <- function (epochs=20) {
  return(mlp(epochs = epochs, 
               hidden_units = 64,
              activation = "relu")%>%
             set_mode("classification") %>% 
             set_engine("keras", verbose = 1)
         # uses Adam optimizer by default
         )
}

# Wykonuje strojenie modeli i zwraca workflow zawierający model z najlepszymi hiperparametrami.
tune_with_data <- function (tune_spec, data, recipe, kfolds = 10) {
  folds <- vfold_cv(data, v=kfolds, strata = Class)  
  
  tune_workflow <- workflow() %>% add_model(tune_spec) %>% add_recipe(recipe)
  
  HP_set <- parameters(tune_workflow)
    
  result <- tune_workflow %>% 
    tune_bayes(
      resamples = folds,
      # To use non-default parameter ranges
      param_info = HP_set,
      # Generate five at semi-random to start
      initial = 5,
      iter = 10,
      # How to measure performance?
      metrics = metric_set(roc_auc),
      control = control_bayes(no_improve = 5, verbose = TRUE))
      
    
    best <- result %>% select_best("roc_auc")
    final_workflow <- tune_workflow %>% finalize_workflow(best)
  
  return(final_workflow)
}

# Funkcja wykonująca trenowanie modelu zawartego w podanym workflow na zbiorze podzielonym na zbiór treningowy i testowy,
# zgodnie z parametrem data_split.
fit_and_eval <- function(workflow, data_split) {
  return(last_fit(workflow, split = data_split, metrics = metric_set(roc_auc, accuracy, f_meas)))
}

# Funkcja pomocnicza dodająca do ramki danych z wynikami uczenia modeli kolejny wynik.
add_metrics_to_results <- function(metrics, data_frame, dataset_type) {
  new_row <- data.frame(dataset_type, metrics[1,".estimate"], metrics[2,".estimate"], metrics[3,".estimate"])
  names(new_row) <- names(data_frame)
  return(rbind(data_frame, new_row))
}
