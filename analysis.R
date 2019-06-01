# installing packages

# load packages

# importing data set
our.data = read.csv("data/UCI_Credit_Card.csv")
str(our.data)

# Renameing column PAY_0
colnames(our.data)[colnames(our.data) == "PAY_0"] = "PAY_1"

# Convert default payment next month to factor
our.data$default.payment.next.month = as.factor(our.data$default.payment.next.month)
table(our.data$default.payment.next.month)

# Convert SEX to factor
our.data$SEX = as.factor(our.data$SEX)
table(our.data$SEX)

# Add one feature GENDER
our.data$GENDER = ifelse(our.data$SEX == 1, "Male", "Female")

# Education
# Convert EDUCATION to factor

# Merging 0, 5 and 6 to 4(others)
our.data$EDUCATION = ifelse(our.data$EDUCATION == 0 |our.data$EDUCATION == 5 | our.data$EDUCATION == 6,
       4, our.data$EDUCATION)

unique(our.data$EDUCATION)
our.data$EDUCATION = as.factor(our.data$EDUCATION)
table(our.data$EDUCATION)

# Marriage
unique(our.data$MARRIAGE)

# Merging 0 to 3(others)
our.data$MARRIAGE = ifelse(our.data$MARRIAGE == 0, 3, our.data$MARRIAGE)

# Convert to factors
our.data$MARRIAGE = as.factor(our.data$MARRIAGE)

table(our.data$MARRIAGE)

# Adding one feature marital status
addMaritalStatus = function(x, colInd, newColInd){
  for (i in 1: nrow(x)) {
    if(x[i, colInd] == 1){
      x[i, newColInd] = "Married"
    }
    else if (x[i, colInd] == 2) {
      x[i, newColInd] = "Not Married"
    }
    else{
      x[i, newColInd] = "Others"
    }
      
  }
  return(x)
}
our.data$MARITALSTATUS = NA
our.data = addMaritalStatus(our.data, which(colnames(our.data) == "MARRIAGE"), 
                            which(colnames(our.data) == "MARITALSTATUS"))