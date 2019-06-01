# installing packages

# load packages
library(ggplot2)
library(naniar)

# check missing values
vis_miss(our.data)
ggsave("/Plots/missing_data.png")

# Sex
# Bar Graph for gender
ggplot(data = our.data, mapping = aes(x = GENDER, fill = default.payment.next.month)) +
  geom_bar() +
  ggtitle("Gender") +
  stat_count(aes(label = ..count..), geom = "label")

# Education
# Bar Graph for gender
ggplot(data = our.data, mapping = aes(x = EDUCATION, fill = default.payment.next.month)) +
  geom_bar() +
  ggtitle("EDUCATION") +
  stat_count(aes(label = ..count..), geom = "label")

# Age
# Density plot for Age feature
ggplot(data = our.data, mapping = aes(x = AGE, fill = default.payment.next.month)) +
  geom_density() +
  ggtitle("Age") +
  xlab("Age")

# Histogram for age
ggplot(data = our.data, mapping = aes(x = AGE, fill = default.payment.next.month)) +
  geom_bar() +
  xlab("Age") +
  ggtitle("Age") 

# MARITALSTATUS
# Bar graph for marital status

ggplot(data = our.data, mapping = aes(x = MARITALSTATUS, fill = default.payment.next.month)) +
  geom_bar() +
  xlab("Marital status") +
  ggtitle(" Defaulters on Marital Status") +
  stat_count(aes(label = ..count..), geom = "label")
