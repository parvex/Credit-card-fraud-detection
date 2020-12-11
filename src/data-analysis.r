# install.packages("skimr")
# install.packages("corrplot")
# install.packages("ROSE")
library(skimr)
library(dplyr)
library(ggplot2)
library(scales)
library(corrplot)
library(ROSE)

print("Reading csv")
ccfd_dframe <- read.csv("creditcard.csv")
print("Basic information about dataset:")
print("Head:")
head(ccfd_dframe)
print("Summary info of columns:")
skim(ccfd_dframe)


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

ccfd_dframe_sub = ovun.sample(Class~., data=ccfd_dframe, method="under")[["data"]]
table(ccfd_dframe_sub$Class)

skim(ccfd_dframe_sub)

ccfd_dframe_sub$Class = as.numeric(ccfd_dframe_sub$Class)

cor_matrix <- cor(ccfd_dframe_sub)
corrplot(cor_matrix, type = "lower", 
         tl.col = "black", tl.srt = 45)


n_columns<-ncol(ccfd_dframe)

# Perform oversampling and plot correlation matrix
# The parameters perc.over and perc.under control the amount of over-sampling of the minority class and under-sampling of the majority classes, respectively.
# See also https://www.rdocumentation.org/packages/unbalanced/versions/2.0/topics/ubSMOTE

ccfd_dframe_over <- ovun.sample(Class~., data=ccfd_dframe, method="over")[["data"]]

table(ccfd_dframe$Class)
table(ccfd_dframe_over$Class)


skim(ccfd_dframe_over)

ggplot(ccfd_dframe_over, aes(x = Class)) +  
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=percent)+
  labs(y="Percentage", x = "Class")+
  ggtitle("Percentage of classes in an oversampled dataset")

ccfd_dframe_over_numeric_class <- ccfd_dframe_over
ccfd_dframe_over_numeric_class$Class = as.numeric(ccfd_dframe_over_numeric_class$Class) - 1

cor_matrix <- cor(ccfd_dframe_over_numeric_class)
corrplot(cor_matrix, type = "lower", 
         tl.col = "black", tl.srt = 45)

# Take variables which have significant correlation with Class. Use oversampled set.

class_corrs <- cor_matrix["Class",][-n_columns]
big_positive_corrs <- names(class_corrs)[class_corrs > 0.5]
big_negative_corrs <- names(class_corrs)[class_corrs < -0.5]

par(mfrow=c(2, floor((length(big_positive_corrs) + length(big_negative_corrs)/2))))
par(mar=c(7,5,1,1))
par(cex.lab=1.5)

for (big_corrs in c(big_negative_corrs, big_positive_corrs)) {
  boxplot(ccfd_dframe_over[ccfd_dframe_over$Class == 0,][,big_corrs], ccfd_dframe_over[ccfd_dframe_over$Class == 1,][,big_corrs],
          names = c(0,1),
          xlab = 'Class',
          ylab='Value',
          main = big_corrs)
}

# small_corrs <- names(class_corrs)[class_corrs < 0.1 & class_corrs > -0.1]
# small_corrs