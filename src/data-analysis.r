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