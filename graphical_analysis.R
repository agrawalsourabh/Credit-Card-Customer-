# installing packages
install.packages("corrplot")
install.packages("GGally")

# load packages
library(ggplot2)
library(naniar) # for missing values
library(corrplot)
library(GGally)

# check missing values
vis_miss(our.data)
ggsave("/Plots/missing_data.png")

# Check correlation
our.data$DEFAULT = as.numeric(our.data$DEFAULT)
r = cor(our.data[-c(3, 4, 5, 7, 26, 27)])
corrplot(r, method = "circle")

ggcorr(our.data)

# Sex
# Bar Graph for gender
ggplot(data = our.data, mapping = aes(x = GENDER, fill = DEFAULT)) +
  geom_bar() +
  ggtitle("Gender") +
  stat_count(aes(label = ..count..), geom = "label")

# Education
# Bar Graph for Education
ggplot(data = our.data, mapping = aes(x = EDUCATION, fill = DEFAULT)) +
  geom_bar() +
  ggtitle("EDUCATION") +
  stat_count(aes(label = ..count..), geom = "label")

# Bar Graph
ggplot(data = default_count_perc, mapping = aes(x = reorder(level, percentage), y = percentage)) +
  geom_bar(stat = "identity", fill = "#b3e569") +
  coord_flip() +
  xlab("Education") +
  ylab("Percentage of Defaulters") +
  ggtitle("Education level vs default") +
  geom_label(label = paste(default_count_perc$percentage, "%"))

# Age
# Density plot for Age feature
ggplot(data = our.data, mapping = aes(x = AGE, fill = DEFAULT)) +
  geom_density() +
  ggtitle("Age") +
  xlab("Age")

# Histogram for age
ggplot(data = our.data, mapping = aes(x = AGE, fill = DEFAULT)) +
  geom_bar() +
  xlab("Age") +
  ggtitle("Age") 

# MARITALSTATUS
# Bar graph for marital status

ggplot(data = our.data, mapping = aes(x = MARITALSTATUS, fill = DEFAULT)) +
  geom_bar() +
  xlab("Marital status") +
  ggtitle(" Defaulters on Marital Status") +
  stat_count(aes(label = ..count..), geom = "label")

# Bar Graph
ggplot(data = marital_default_per_df, mapping = aes(x = reorder(level, percentage), y = percentage)) +
  geom_bar(stat = "identity", fill = "#f47d42") +
  coord_flip() +
  xlab("Marital Status") +
  ylab("Percentage of Defaulters") +
  ggtitle("Marital Status vs default") +
  geom_label(label = paste(marital_default_per_df$percentage, "%"))

#PAY_1
ggplot(data = pay_1_default_perc.df, mapping = aes(x = reorder(level, percentage), y = percentage)) +
  geom_bar(stat = "identity", fill = "#ce9b2d") +
  coord_flip() +
  xlab("PAY_1") +
  ylab("Percentage of Defaulters") +
  ggtitle("Credit Behaviour(PAY_1) vs default") +
  geom_label(label = paste(pay_1_default_perc.df$percentage, "%"))

#REMAINING_AMT_PER_RANGE
ggplot(data = RemainingAmtPerc_per_df, mapping = aes(x = reorder(level, percentage), y = percentage)) +
  geom_bar(stat = "identity", fill = "#ce9b2d") +
  coord_flip() +
  xlab("REMAINING_AMT_PER_RANGE") +
  ylab("Percentage of Defaulters") +
  ggtitle("Credit Behaviour(Remaining amount) vs default") +
  geom_label(label = paste(RemainingAmtPerc_per_df$percentage, "%"))

# -----------------------------------------------------------------------------------
# ------------------------------WORKPLACE BACKUP-------------------------------------
# -----------------------------------------------------------------------------------

# LIMIT_BAL density plot
ggplot(data = our.data, mapping = aes(x = LIMIT_BAL)) + 
  geom_density(fill = "#f0f9a7") +
  ggtitle("LIMIT_BAL Distribution") +
  xlab("LIMIT_BAL") +
  geom_vline(xintercept = mean(our.data$LIMIT_BAL), col = "red", 
             linetype = "dashed", size = 0.6) +
  annotate("text", 
           x = -Inf, y = Inf, 
           label = paste("Mean:", round(mean(our.data$LIMIT_BAL), digits = 2)), 
           hjust = 0, vjust = 1, col = "red", size = 3)

# -----------------------------------------------------------------------------------
# ------------------------------WORKPLACE BACKUP-------------------------------------
# -----------------------------------------------------------------------------------


# plot a line graph for accuracy
ggplot(data = model_accuracy_80, mapping = aes(x = Model.Name, y = Accuracy )) +
  geom_point(col = "red") +
  geom_line(aes(group = 1), col = "tomato") +
  ggtitle("Accuracy") +
  xlab(" Model Name ") +
  ylab("Accuracy") +
  geom_label(label = round(model_accuracy_80$Accuracy, digits = 2)) +
  scale_y_continuous(limits = c(0:1))

# plot a line graph for accuracy - k-fold
ggplot(data = model_accuracy_80_cv, mapping = aes(x = Model.Name, y = Accuracy )) +
  geom_point(col = "red") +
  geom_line(aes(group = 1), col = "tomato") +
  ggtitle("Accuracy K-fold") +
  xlab(" Model ") +
  ylab("Accuracy") +
  geom_label(label = round(model_accuracy_80_cv$Accuracy, digits = 2)) +
  scale_y_continuous(limits = c(0:1))

# -----------------------------------------------------------------------------------
# ------------------------------WORKPLACE BACKUP-------------------------------------
# -----------------------------------------------------------------------------------