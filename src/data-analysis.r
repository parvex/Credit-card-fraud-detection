#install.packages('pacman')
library(pacman)
options(vsc.plot = "Two")
pacman::p_load(skimr, dplyr, ggplot2, scales, corrplot, ROSE)
dev.off()
def_par = par()

print("Reading csv")
data_df <- read.csv("creditcard.csv")
print("Basic information about dataset:")
print("Head:")
head(data_df)
print("Summary info of columns:")
skim(data_df)

nans <- sum(is.na(data_df))

if (nans > 0) {
  sprintf("Dataset contains %d NaN values", nans)
  # TODO eliminate NaNs in this case
} else {
  print("Dataset does not contain any NaN values")
}

data_df$Class <- as.factor(data_df$Class)

ggplot(data_df, aes(x = Class)) +
  geom_bar(aes(y = (..count..) / sum(..count..))) +
  scale_y_continuous(labels = percent) +
  labs(y = "Percentage", x = "Class") +
  ggtitle("Percentage of classes in a dataset")

hist(data_df$Amount,
  breaks = 500,
  xlim = c(0, max(data_df$Amount)),
  xlab = "Amount",
  ylab = "Frequency",
  main = "Histogram of amount of transactions"
)


# scale Amount and Time
data_df <- data_df %>% mutate_at(c("Amount", "Time"), ~ (scale(.) %>% as.vector()))
n_columns <- ncol(data_df)

data_df$Class <- as.numeric(data_df$Class) - 1
#correlation matrix for unbalanced set
cor_matrix <- cor(data_df)
corrplot(cor_matrix,
         type = "lower",
         tl.col = "black", tl.srt = 45,
         main = "Correlation matrix for unbalanced set"
)

# Perform undersampling and plot correlation matrix

undersampled_df <- ovun.sample(Class ~ ., data = data_df, method = "under")[["data"]]
table(undersampled_df$Class)

skim(undersampled_df)

undersampled_df$Class <- as.numeric(undersampled_df$Class) - 1

cor_matrix <- cor(undersampled_df)
corrplot(cor_matrix,
  type = "lower",
  tl.col = "black", tl.srt = 45,
  main = "Correlation matrix for undersampled set"
)

# Take variables which have significant correlation with Class. Use oversampled set.

class_corrs <- cor_matrix["Class", ][-n_columns]
big_positive_corrs <- names(class_corrs)[class_corrs > 0.5]
big_negative_corrs <- names(class_corrs)[class_corrs < -0.5]

par(mfrow = c(2, floor((length(big_positive_corrs) + length(big_negative_corrs) / 2))))
par(mar = c(7, 5, 1, 1))
par(cex.lab = 1.5)

for (big_corrs in c(big_negative_corrs, big_positive_corrs)) {
  boxplot(undersampled_df[undersampled_df$Class == 0, ][, big_corrs],
    undersampled_df[undersampled_df$Class == 1, ][, big_corrs],
    names = c(0, 1),
    xlab = "Class",
    ylab = "Value",
    main = big_corrs
  )
}

par(def_par)

# Perform oversampling and plot correlation matrix
# The parameters perc.over and perc.under control the amount of over-sampling of the minority class and under-sampling of the majority classes, respectively.
# See also https://www.rdocumentation.org/packages/unbalanced/versions/2.0/topics/ubSMOTE

oversampled_df <- ovun.sample(Class ~ ., data = data_df, method = "over")[["data"]]

table(data_df$Class)
table(oversampled_df$Class)


skim(oversampled_df)

ggplot(oversampled_df, aes(x = Class)) +
  geom_bar(aes(y = (..count..) / sum(..count..))) +
  scale_y_continuous(labels = percent) +
  labs(y = "Percentage", x = "Class") +
  ggtitle("Percentage of classes in an oversampled dataset")

oversampled_numveric_class_df <- oversampled_df
oversampled_numveric_class_df$Class <- as.numeric(oversampled_numveric_class_df$Class) - 1

cor_matrix <- cor(oversampled_numveric_class_df)
corrplot(cor_matrix,
  type = "lower",
  tl.col = "black", tl.srt = 45,
  main = "Correlation matrix for oversampled set"
)

# Take variables which have significant correlation with Class. Use oversampled set.

class_corrs <- cor_matrix["Class", ][-n_columns]
big_positive_corrs <- names(class_corrs)[class_corrs > 0.5]
big_negative_corrs <- names(class_corrs)[class_corrs < -0.5]

par(mfrow = c(2, floor((length(big_positive_corrs) + length(big_negative_corrs) / 2))))
par(mar = c(7, 5, 1, 1))
par(cex.lab = 1.5)

for (big_corrs in c(big_negative_corrs, big_positive_corrs)) {
  boxplot(oversampled_df[oversampled_df$Class == 0, ][, big_corrs],
    oversampled_df[oversampled_df$Class == 1, ][, big_corrs],
    names = c(0, 1),
    xlab = "Class",
    ylab = "Value",
    main = big_corrs
  )
}
par(def_par)

# small_corrs <- names(class_corrs)[class_corrs < 0.1 & class_corrs > -0.1]
# small_corrs