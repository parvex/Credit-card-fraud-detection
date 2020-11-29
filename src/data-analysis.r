install.packages("skimr")
install.packages("corrplot")
install.packages("unbalanced")
library(skimr)
library(dplyr)
library(ggplot2)
library(scales)
library(corrplot)
library(unbalanced)

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

hist(ccfd_dframe$Amount,
     breaks = 500,
     xlim=c(0,max(ccfd_dframe$Amount)),
     xlab = 'Amount',
     ylab = 'Frequency',
     main = 'Histogram of amount of transactions')


#scale Amount and Time
ccfd_dframe <- ccfd_dframe %>% mutate_at(c("Amount", "Time"), ~(scale(.) %>% as.vector))

# Perform undersampling and plot correlation matrix

n_columns<-ncol(ccfd_dframe)
unbalanced_class<-ccfd_dframe$Class
variables<-ccfd_dframe[ ,-n_columns]

data_X_and_Y <- ubUnder(variables, unbalanced_class)
ccfd_dframe_sub <- cbind(data_X_and_Y$X, data_X_and_Y$Y)
names(ccfd_dframe_sub)[n_columns] <- "Class"

skim(ccfd_dframe_sub)

ccfd_dframe_sub$Class = as.numeric(ccfd_dframe_sub$Class)

cor_matrix <- cor(ccfd_dframe_sub)
corrplot(cor_matrix, type = "lower", 
         tl.col = "black", tl.srt = 45)

# Perform oversampling and plot correlation matrix
# The parameters perc.over and perc.under control the amount of over-sampling of the minority class and under-sampling of the majority classes, respectively.
# See also https://www.rdocumentation.org/packages/unbalanced/versions/2.0/topics/ubSMOTE

data_X_and_Y <- ubSMOTE(variables, unbalanced_class, perc.over = 100)
ccfd_dframe_over <- cbind(data_X_and_Y$X, data_X_and_Y$Y)
names(ccfd_dframe_over)[n_columns] <- "Class"

skim(ccfd_dframe_over)

ggplot(ccfd_dframe_over, aes(x = Class)) +  
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=percent)+
  labs(y="Percentage", x = "Class")+
  ggtitle("Percentage of classes in an oversampled dataset")

ccfd_dframe_over$Class = as.numeric(ccfd_dframe_over$Class)

cor_matrix <- cor(ccfd_dframe_over)
corrplot(cor_matrix, type = "lower", 
         tl.col = "black", tl.srt = 45)