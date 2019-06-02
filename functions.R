# Function to calculate no of defaults per education level
calDefaultPerc = function(x, colInd, data.level){
  default_count = c()
  default_perc = c()
  
  for (i in 1:length(data.level)) {
    print("start of for...")
    total_count = count(x[x[colInd] == data.level[i], ], vars = "DEFAULT")
    count_def = total_count[total_count$DEFAULT == 1, 2]
    total = total_count[total_count$DEFAULT == 1, 2] + total_count[total_count$DEFAULT == 0, 2]
    print(i)
    perc = round(count_def/total * 100, digits = 2)
    
    default_count = c(default_count, count_def)
    default_perc = c(default_perc, perc)
    print("end of for...")
  }
  print("outer for....")
  #data.level
  length(default_count)
  length(default_perc)
  
  paste("data.level: ", length(data.level))
  paste("default_count: ", length(default_count))
  paste("default_perc: ", length(default_perc))
  
  default_perc_df = data.frame(level = data.level, default.count = default_count,
                               percentage = default_perc)
  print("data frame created....")
  return(default_perc_df)
}

# calculate diff of each row of total BILL_AMT and total PAY_AMT
calDiffBillAndPayPer = function(x, bill_amt_ind, pay_amt_ind){
  TOTAL_PAYABLE_PERC = c()
  
  for (i in 1:nrow(x)) {
    sum_bill_amt = sum(x[i, bill_amt_ind])
    sum_pay_amt = sum(x[i, pay_amt_ind])
    
    payable_amt =  sum_bill_amt - sum_pay_amt
    if(sum_bill_amt != 0){
      payable_perc = round(payable_amt / sum_bill_amt * 100, digits = 2)
    }
    else{
      payable_perc = 0.00
    }
    
    TOTAL_PAYABLE_PERC = c(TOTAL_PAYABLE_PERC, payable_perc)
  }
  return(TOTAL_PAYABLE_PERC)
}

# function to find the range of REMAINING_AMT_PER_RANGE
getRemPerRange = function(x, colInd){
  all_per_range = c()
  
  for (i in 1:nrow(x)) {
    per_range = -1
    
    if(x[i, colInd] >=0 & x[i, colInd] <= 10)
      per_range = 0
    else if(x[i, colInd] >10 & x[i, colInd] <= 20)
      per_range = 1
    else if(x[i, colInd] >20 & x[i, colInd] <= 30)
      per_range = 2
    else if(x[i, colInd] >30 & x[i, colInd] <= 40)
      per_range = 3
    else if(x[i, colInd] >40 & x[i, colInd] <= 50)
      per_range = 4
    else if(x[i, colInd] >50 & x[i, colInd] <= 60)
      per_range = 5
    else if(x[i, colInd] >60 & x[i, colInd] <= 70)
      per_range = 6
    else if(x[i, colInd] >70 & x[i, colInd] <= 80)
      per_range = 7
    else if(x[i, colInd] >80 & x[i, colInd] <= 90)
      per_range = 8
    else if(x[i, colInd] >90 & x[i, colInd] <= 100)
      per_range = 9
    else
      per_range = -1
    
    all_per_range = c(all_per_range, per_range)
  }
  
  return(all_per_range)
}

# -----------------------------------------------------------------------------------
# ------------------------------WORKPLACE BACKUP-------------------------------------
# -----------------------------------------------------------------------------------

# Function to find the Limit Balance Range
getLimitBalRange = function(x, colInd){
  limit_bal_seg = c()
  
  for (i in 1:nrow(x)) {
    bal_seg = 0
    if (10000 >= x[i, colInd] & x[i, colInd] < 100000) 
      bal_seg = 0
    else if (100000 >= x[i, colInd] & x[i, colInd] < 200000) 
      bal_seg = 1
    else if (200000 >= x[i, colInd] & x[i, colInd] < 300000) 
      bal_seg = 2
    else if (300000 >= x[i, colInd] & x[i, colInd] < 400000) 
      bal_seg = 3
    else if (400000 >= x[i, colInd] & x[i, colInd] < 500000) 
      bal_seg = 4
    else if (500000 >= x[i, colInd] & x[i, colInd] < 600000) 
      bal_seg = 5
    else if (600000 >= x[i, colInd] & x[i, colInd] < 700000) 
      bal_seg = 6
    else if (700000 >= x[i, colInd] & x[i, colInd] < 800000) 
      bal_seg = 7
    else if (800000 >= x[i, colInd] & x[i, colInd] < 900000) 
      bal_seg = 8
    else
      bal_seg = 9
    
    limit_bal_seg = c(limit_bal_seg, bal_seg)
  }
  
  return(limit_bal_seg)
}

# -----------------------------------------------------------------------------------
# ------------------------------WORKPLACE BACKUP-------------------------------------
# -----------------------------------------------------------------------------------