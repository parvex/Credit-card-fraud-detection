install.packages("skimr")
install.packages("corrplot")
library(skimr)
library(dplyr)
library(ggplot2)
library(scales)
library(corrplot)

print("Reading csv")
ccfd_dframe <- read.csv("creditcard.csv")
print("Basic information about dataset:")
print("Head:")
head(ccfd_dframe)
print("Summatry info of columns:")
skim(ccfd_dframe)

sprintf("This dataset contains %d rows and %d columns", dimension[1], dimension[2])

nans <- sum(is.na(ccfd_dframe))

if (nans > 0) {
  sprintf("Dataset contains %d NaN values", nans)
  # TODO eliminate NaNs in this case
} else {
  print("Dataset does not contain any NaN values")
}

ccfd_dframe$Class = as.factor(ccfd_dframe$Class)

ggplot(ccfd_dframe, aes(x = Class)) +  
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=percent)+
  labs(y="Percentage", x = "Class")+
  ggtitle("Percentage of classes in a dataset")

ccfd_dframe$Class = as.numeric(ccfd_dframe$Class)

hist(ccfd_dframe$Amount,
     breaks = 500,
     xlim=c(0,max(ccfd_dframe$Amount)),
     xlab = 'Amount',
     ylab = 'Frequency',
     main = 'Histogram of amount of transactions')


#scale Amount and Time
ccfd_dframe <- ccfd_dframe %>% mutate_at(c("Amount", "Time"), ~(scale(.) %>% as.vector))
cor_matrix <- cor(ccfd_dframe)
corrplot(cor_matrix, type = "lower", 
         tl.col = "black", tl.srt = 45)


