#PRICING CODE
install.packages("readxl")
install.packages("writexl")
library(readxl)
library(writexl)

#PROFIT and PREMIUM
#The above code calculates both profit and premium. To calculate premium change the return of each formula to return(total_premium)

#20 Year Term Life Insurance

#The below function is used to calculate profit for policies that are written before year 11.  
#The costs of the program are charged but the mortality savings are staggered as we don't expect to fully be realised until year 10.
#It calculates the present value of the total premium an individual will for the whole policy at time 0 (Year 2024). 

program_model_TERM_before_year_11 <- function(age, mortality_table, face_amount, yearly_expense, smoker_expense){
  #set amounts
  discount_rate <- 0.1
  investment_return <- 0.08
  lapse_rate <- 0.02
  
  # Initialize vectors
  expected_death_payment <- numeric(20)  
  expenses <- numeric(20)
  interest <- numeric(20)
  survival_probability <- numeric(20)
  premium <- numeric(20)
  profit <- numeric(20)
  probability_of_death <- numeric(20)
  death_probability_per_year <- numeric(20)
  
  #calculate the probability of death
  for (i in 1:20){
    if (i ==1){
      survival_probability[1] <- 1
      index <- mortality_table[mortality_table$Age == age + i,i+1]
      probability_of_death[i] <- as.numeric(index)
    } else {
      index <- mortality_table[mortality_table$Age == age + i,i+1]
      probability_of_death[i] <- as.numeric(index)
      survival_probability[i] <- survival_probability[i-1] - (probability_of_death[i]+ lapse_rate)*survival_probability[i-1]
    }
  }
  
  for (i in 1:20){
    index <- mortality_table[mortality_table$Age == age + i,i+1]
    death_probability_per_year[i] <- as.numeric(index)
    expected_death_payment[i] <- face_amount*survival_probability[i]*death_probability_per_year[i]*(1/(1+discount_rate)^i)
    expenses[i] <- yearly_expense*survival_probability[i]*(1/(1+discount_rate)^i)
    premium[i] <- (expenses[i] + expected_death_payment[i])*1.10 
    interest[i] <- (premium[i])*investment_return*(1/(1+discount_rate)^i)
    profit[i] <- premium[i] + interest[i] - (expenses[i] + expected_death_payment[i])
  }
  
  total_premium <- sum(premium) + smoker_expense #smoker expense is a one off payment
  
  # for Profit
  return(sum(profit))
  
  # for premium
  # return(total_premium)
}

#The below function is used to calculate profit for policies that are written after year 11. 
#The costs of the program are charged and mortality savings are fully realised 
#It calculates the present value of the total premium an individual will for the whole policy at time 0 (Year 2024). 

program_model_TERM_after_year_11 <- function(age, mortality_table, face_amount, yearly_expense, smoker_expense){
  #set amounts
  discount_rate <- 0.1
  investment_return <- 0.08
  lapse_rate <- 0.02
  
  # Initialize vectors
  expected_death_payment <- numeric(20)  
  expenses <- numeric(20)
  interest <- numeric(20)
  survival_probability <- numeric(20)
  premium <- numeric(20)
  profit <- numeric(20)
  probability_of_death <- numeric(20)
  death_probability_per_year <- numeric(20)
  
  #calculate the probability of death
  for (i in 1:20){
    if (i ==1){
      survival_probability[1] <- 1
      index <- mortality_table[mortality_table$Age == age + i,13]
      probability_of_death[i] <- as.numeric(index)
    } else {
      index <- mortality_table[mortality_table$Age == age + i,13]
      probability_of_death[i] <- as.numeric(index)
      survival_probability[i] <- survival_probability[i-1] - (probability_of_death[i]+ lapse_rate)*survival_probability[i-1]
    }
  }
  
  for (i in 1:20){
    index <- mortality_table[mortality_table$Age == age + i,13]
    death_probability_per_year[i] <- as.numeric(index)
    expected_death_payment[i] <- face_amount*survival_probability[i]*death_probability_per_year[i]*(1/(1+discount_rate)^i)
    expenses[i] <- yearly_expense*survival_probability[i]*(1/(1+discount_rate)^i)
    premium[i] <- (expenses[i] + expected_death_payment[i])*1.10 
    interest[i] <- (premium[i])*investment_return*(1/(1+discount_rate)^i)
    profit[i] <- premium[i] + interest[i] - (expenses[i] + expected_death_payment[i])
  }
  
  total_premium <- sum(premium) + smoker_expense #smoker expense is a one off payment
  
  
  # for Profit
  return(sum(profit))
  
  # for premium
  # return(total_premium)
}

#The below function is used to calculate profit for policies with no program in place
no_program_model_TERM <- function(age, mortality_table, face_amount, yearly_expense, smoker_expense){
  #set amounts
  discount_rate <- 0.1
  investment_return <- 0.08
  lapse_rate <- 0.02
  
  # Initialize vectors
  expected_death_payment <- numeric(20)  
  expenses <- numeric(20)
  interest <- numeric(20)
  survival_probability <- numeric(20)
  premium <- numeric(20)
  profit <- numeric(20)
  probability_of_death <- numeric(20)
  death_probability_per_year <- numeric(20)
  
  #calculate the probability of death
  for (i in 1:20){
    if (i ==1){
      survival_probability[1] <- 1
      index <- mortality_table[mortality_table$Age == age + i,2]
      probability_of_death[i] <- as.numeric(index)
    } else {
      index <- mortality_table[mortality_table$Age == age + i,2]
      probability_of_death[i] <- as.numeric(index)
      survival_probability[i] <- survival_probability[i-1] - (probability_of_death[i]+ lapse_rate)*survival_probability[i-1]
    }
  }
  
  for (i in 1:20){
    index <- mortality_table[mortality_table$Age == age + i,2]
    death_probability_per_year[i] <- as.numeric(index)
    expected_death_payment[i] <- face_amount*survival_probability[i]*death_probability_per_year[i]*(1/(1+discount_rate)^i)
    expenses[i] <- yearly_expense*survival_probability[i]*(1/(1+discount_rate)^i)
    premium[i] <- (expenses[i] + expected_death_payment[i])*1.10 
    interest[i] <- (premium[i])*investment_return*(1/(1+discount_rate)^i)
    profit[i] <- premium[i] + interest[i] - (expenses[i] + expected_death_payment[i])
  }
  
  total_premium <- sum(premium) + smoker_expense #smoker expense is a one off payment
  
  
  # for Profit
  return(sum(profit))
  
  # for premium
  # return(total_premium)
}


# The below calculates profit/premium by age for each case 
no_program_Term <- function(mortality_table){
  Average_output <- numeric(100)
  for (i in 1:100){
    Average_output[i] <- no_program_model_TERM(i,mortality_table,250000, 0, 0)
  }
  return(Average_output)
}

program_Term_before_year_11 <- function(mortality_table){
  Average_output <- numeric(100)
  for (i in 1:100){
    Average_output[i] <- program_model_TERM_before_year_11(i,mortality_table,250000, 181.63, 0)
  }
  return(Average_output)
}


program_Term_after_year_11 <- function(mortality_table){
  Average_output <- numeric(100)
  for (i in 1:100){
    Average_output[i] <- program_model_TERM_after_year_11(i,mortality_table,250000, 181.63, 0)
  }
  return(Average_output)
}

#F_NS_High
F_NS_High_T20 <- data.frame(c(1:100),no_program_Term(F_NS_High), program_Term_before_year_11(F_NS_High), program_Term_after_year_11(F_NS_High))
colnames(F_NS_High_T20) <- c("Age", "No Program", "Before yr 11", "After yr 11")

#F_NS_Mod
F_NS_Mod_T20 <- data.frame(c(1:100),no_program_Term(F_NS_Mod), program_Term_before_year_11(F_NS_Mod), program_Term_after_year_11(F_NS_Mod))
colnames(F_NS_Mod_T20) <- c("Age", "No Program", "Before yr 11", "After yr 11")

#F_NS_L
F_NS_L_T20 <- data.frame(c(1:100),no_program_Term(F_NS_L), program_Term_before_year_11(F_NS_L), program_Term_after_year_11(F_NS_L))
colnames(F_NS_L_T20 ) <- c("Age", "No Program", "Before yr 11", "After yr 11")

#F_NS_VL
F_NS_VL_T20 <- data.frame(c(1:100),no_program_Term(F_NS_VL), program_Term_before_year_11(F_NS_VL), program_Term_after_year_11(F_NS_VL))
colnames(F_NS_VL_T20 ) <- c("Age", "No Program", "Before yr 11", "After yr 11")

#M_NS_High
M_NS_High_T20 <- data.frame(c(1:100),no_program_Term(M_NS_High), program_Term_before_year_11(M_NS_High), program_Term_after_year_11(M_NS_High))
colnames(M_NS_High_T20 ) <- c("Age", "No Program", "Before yr 11", "After yr 11")

#M_NS_Mod
M_NS_Mod_T20 <- data.frame(c(1:100),no_program_Term(M_NS_Mod), program_Term_before_year_11(M_NS_Mod), program_Term_after_year_11(M_NS_Mod))
colnames(M_NS_Mod_T20 ) <- c("Age", "No Program", "Before yr 11", "After yr 11")

#M_NS_L
M_NS_L_T20 <- data.frame(c(1:100),no_program_Term(M_NS_L), program_Term_before_year_11(M_NS_L), program_Term_after_year_11(M_NS_L))
colnames(M_NS_L_T20 ) <- c("Age", "No Program", "Before yr 11", "After yr 11")

#M_NS_VL
M_NS_VL_T20 <- data.frame(c(1:100),no_program_Term(M_NS_VL), program_Term_before_year_11(M_NS_VL), program_Term_after_year_11(M_NS_VL))
colnames(M_NS_VL_T20 ) <- c("Age", "No Program", "Before yr 11", "After yr 11")


##SMOKERS
#M_S_High
M_S_High_T20 <- data.frame(c(1:100),no_program_Term(M_S_High), program_Term_before_year_11(M_S_High), program_Term_after_year_11(M_S_High))
colnames(M_S_High_T20 ) <- c("Age", "No Program", "Before yr 11", "After yr 11")

#M_S_Mod
M_S_Mod_T20 <- data.frame(c(1:100),no_program_Term(M_S_Mod), program_Term_before_year_11(M_S_Mod), program_Term_after_year_11(M_S_Mod))
colnames(M_S_Mod_T20 ) <- c("Age", "No Program", "Before yr 11", "After yr 11")

#F_S_High
F_S_High_T20 <- data.frame(c(1:100),no_program_Term(F_S_High), program_Term_before_year_11(F_S_High), program_Term_after_year_11(F_S_High))
colnames(F_S_High_T20 ) <- c("Age", "No Program", "Before yr 11", "After yr 11")

#F_S_Mod
F_S_Mod_T20 <- data.frame(c(1:100),no_program_Term(F_S_Mod), program_Term_before_year_11(F_S_Mod), program_Term_after_year_11(F_S_Mod))
colnames(F_S_Mod_T20 ) <- c("Age", "No Program", "Before yr 11", "After yr 11")

#This code creates a spreadsheet of the entire results
library(writexl)
write_xlsx(list("M_S_High" = M_S_High_T20, "M_S_Mod" = M_S_Mod_T20, "F_S_High" = F_S_High_T20,
                "F_S_Mod" = F_S_Mod_T20, "M_NS_High" = M_NS_High_T20, "M_NS_Mod" = M_NS_Mod_T20, 
                "M_NS_L" = M_NS_L_T20, "M_NS_VL" = M_NS_VL_T20, "F_NS_High" = F_NS_High_T20,
                "F_NS_Mod" = F_NS_Mod_T20, "F_NS_L" = F_NS_L_T20, "F_NS_VL" = F_NS_VL_T20), "BEM_Profit_by_Age_Term_250000.xlsx") #adjust spreadsheet name to match inputs

#WHOLE LIFE SINGLE PROFIT 

library(readxl)

#The below function should be used for policies that are written before year 11

program_model_SPWL_before_year_11 <- function(age, mortality_table, face_amount, yearly_expense, smoker_expense){
  #set amounts
  discount_rate <- 0.1
  investment_return <- 0.08
  
  # Initialize vectors
  expected_death_payment <- numeric((100-age))  # Initialize vectors
  expenses <- numeric((100-age))
  interest <- numeric((100-age))
  survival_probability <- numeric((100-age))
  premium <- numeric((100-age))
  profit <- numeric((100-age))
  probability_of_death <- numeric((100-age))
  death_probability_per_year <- numeric((100-age))
  
  #calculate the probability of death
  for (i in 1:(100-age)){
    if (i ==1){
      survival_probability[1] <- 1
      index <- mortality_table[mortality_table$Age == age + i,i+1]
      probability_of_death[i] <- as.numeric(index)
    } else {
      index <- mortality_table[mortality_table$Age == age + i,i+1]
      probability_of_death[i] <- as.numeric(index)
      survival_probability[i] <- survival_probability[i-1] - (probability_of_death[i])*survival_probability[i-1]
    }
  }
  
  for (i in 1:(100-age)){
    index <- mortality_table[mortality_table$Age == age + i,i+1]
    death_probability_per_year[i] <- as.numeric(index)
    expected_death_payment[i] <- face_amount*survival_probability[i]*death_probability_per_year[i]*(1/(1+discount_rate)^i)
    expenses[i] <- yearly_expense*survival_probability[i]*(1/(1+discount_rate)^i)
    premium[i] <- (expenses[i] + expected_death_payment[i])*1.10 
    interest[i] <- (premium[i])*investment_return*(1/(1+discount_rate)^i)
    profit[i] <- premium[i] + interest[i] - (expenses[i] + expected_death_payment[i])
  }
  
  total_premium <- sum(premium) + smoker_expense #smoker expense is a one off payment
  
  # for Profit
  return(sum(profit))
  
  # for premium
  # return(total_premium)
}


#The below function should be used for policies that are written after year 11

program_model_SPWL_after_year_11 <- function(age, mortality_table, face_amount, yearly_expense, smoker_expense){
  #set amounts
  discount_rate <- 0.1
  investment_return <- 0.08
  
  # Initialize vectors
  expected_death_payment <- numeric((100-age))  # Initialize vectors
  expenses <- numeric((100-age))
  interest <- numeric((100-age))
  survival_probability <- numeric((100-age))
  premium <- numeric((100-age))
  profit <- numeric((100-age))
  probability_of_death <- numeric((100-age))
  death_probability_per_year <- numeric((100-age))
  
  #calculate the probability of death
  for (i in 1:(100-age)){
    if (i ==1){
      survival_probability[1] <- 1
      index <- mortality_table[mortality_table$Age == age + i,13]
      probability_of_death[i] <- as.numeric(index)
    } else {
      index <- mortality_table[mortality_table$Age == age + i,13]
      probability_of_death[i] <- as.numeric(index)
      survival_probability[i] <- survival_probability[i-1] - (probability_of_death[i])*survival_probability[i-1]
    }
  }
  
  for (i in 1:(100-age)){
    index <- mortality_table[mortality_table$Age == age + i,13]
    death_probability_per_year[i] <- as.numeric(index)
    expected_death_payment[i] <- face_amount*survival_probability[i]*death_probability_per_year[i]*(1/(1+discount_rate)^i)
    expenses[i] <- yearly_expense*survival_probability[i]*(1/(1+discount_rate)^i)
    premium[i] <- (expenses[i] + expected_death_payment[i])*1.10 
    interest[i] <- (premium[i])*investment_return*(1/(1+discount_rate)^i)
    profit[i] <- premium[i] + interest[i] - (expenses[i] + expected_death_payment[i])
  }
  
  total_premium <- sum(premium) + smoker_expense #smoker expense is a one off payment
  
  # for Profit
  return(sum(profit))
  
  # for premium
  # return(total_premium)
}

##no program
no_program_model_SPWL <- function(age, mortality_table, face_amount, yearly_expense, smoker_expense){
  #set amounts
  discount_rate <- 0.1
  investment_return <- 0.08
  
  # Initialize vectors
  expected_death_payment <- numeric((100-age))  # Initialize vectors
  expenses <- numeric((100-age))
  interest <- numeric((100-age))
  survival_probability <- numeric((100-age))
  premium <- numeric((100-age))
  profit <- numeric((100-age))
  probability_of_death <- numeric((100-age))
  death_probability_per_year <- numeric((100-age))
  
  #calculate the probability of death
  for (i in 1:(100-age)){
    if (i ==1){
      survival_probability[1] <- 1
      index <- mortality_table[mortality_table$Age == age + i,2]
      probability_of_death[i] <- as.numeric(index)
    } else {
      index <- mortality_table[mortality_table$Age == age + i,2]
      probability_of_death[i] <- as.numeric(index)
      survival_probability[i] <- survival_probability[i-1] - (probability_of_death[i])*survival_probability[i-1]
    }
  }
  
  for (i in 1:(100-age)){
    index <- mortality_table[mortality_table$Age == age + i,2]
    death_probability_per_year[i] <- as.numeric(index)
    expected_death_payment[i] <- face_amount*survival_probability[i]*death_probability_per_year[i]*(1/(1+discount_rate)^i)
    expenses[i] <- yearly_expense*survival_probability[i]*(1/(1+discount_rate)^i)
    premium[i] <- (expenses[i] + expected_death_payment[i])*1.10 
    interest[i] <- (premium[i])*investment_return*(1/(1+discount_rate)^i)
    profit[i] <- premium[i] + interest[i] - (expenses[i] + expected_death_payment[i])
  }
  
  total_premium <- sum(premium) + smoker_expense #smoker expense is a one off payment
  
  # for Profit
  return(sum(profit))
  
  # for premium
  # return(total_premium)
}



# calculate profit/premium by age
no_program_SPWL <- function(mortality_table){
  Average_output <- numeric(100)
  for (i in 1:100){
    Average_output[i] <- no_program_model_SPWL(i,mortality_table,250000, 0, 0)
  }
  return(Average_output)
}

program_SPWL_before_year_11 <- function(mortality_table){
  Average_output <- numeric(100)
  for (i in 1:100){
    Average_output[i] <- program_model_SPWL_before_year_11(i,mortality_table,250000, 181.63, 0)
  }
  return(Average_output)
}


program_SPWL_after_year_11 <- function(mortality_table){
  Average_output <- numeric(100)
  for (i in 1:100){
    Average_output[i] <- program_model_SPWL_after_year_11(i,mortality_table,250000, 181.63, 0)
  }
  return(Average_output)
}

#F_NS_High
F_NS_High_SPWL <- data.frame(c(1:100),no_program_SPWL(F_NS_High), program_SPWL_before_year_11(F_NS_High), program_SPWL_after_year_11(F_NS_High))
colnames(F_NS_High_SPWL) <- c("Age", "No Program", "Before yr 11", "After yr 11")

#F_NS_Mod
F_NS_Mod_SPWL <- data.frame(c(1:100),no_program_SPWL(F_NS_Mod), program_SPWL_before_year_11(F_NS_Mod), program_SPWL_after_year_11(F_NS_Mod))
colnames(F_NS_Mod_SPWL) <- c("Age", "No Program", "Before yr 11", "After yr 11")

#F_NS_L
F_NS_L_SPWL <- data.frame(c(1:100),no_program_SPWL(F_NS_L), program_SPWL_before_year_11(F_NS_L), program_SPWL_after_year_11(F_NS_L))
colnames(F_NS_L_SPWL ) <- c("Age", "No Program", "Before yr 11", "After yr 11")

#F_NS_VL
F_NS_VL_SPWL <- data.frame(c(1:100),no_program_SPWL(F_NS_VL), program_SPWL_before_year_11(F_NS_VL), program_SPWL_after_year_11(F_NS_VL))
colnames(F_NS_VL_SPWL ) <- c("Age", "No Program", "Before yr 11", "After yr 11")

#M_NS_High
M_NS_High_SPWL <- data.frame(c(1:100),no_program_SPWL(M_NS_High), program_SPWL_before_year_11(M_NS_High), program_SPWL_after_year_11(M_NS_High))
colnames(M_NS_High_SPWL ) <- c("Age", "No Program", "Before yr 11", "After yr 11")

#M_NS_Mod
M_NS_Mod_SPWL <- data.frame(c(1:100),no_program_SPWL(M_NS_Mod), program_SPWL_before_year_11(M_NS_Mod), program_SPWL_after_year_11(M_NS_Mod))
colnames(M_NS_Mod_SPWL ) <- c("Age", "No Program", "Before yr 11", "After yr 11")

#M_NS_L
M_NS_L_SPWL <- data.frame(c(1:100),no_program_SPWL(M_NS_L), program_SPWL_before_year_11(M_NS_L), program_SPWL_after_year_11(M_NS_L))
colnames(M_NS_L_SPWL ) <- c("Age", "No Program", "Before yr 11", "After yr 11")

#M_NS_VL
M_NS_VL_SPWL <- data.frame(c(1:100),no_program_SPWL(M_NS_VL), program_SPWL_before_year_11(M_NS_VL), program_SPWL_after_year_11(M_NS_VL))
colnames(M_NS_VL_SPWL ) <- c("Age", "No Program", "Before yr 11", "After yr 11")


##SMOKERS
#M_S_High
M_S_High_SPWL <- data.frame(c(1:100),no_program_SPWL(M_S_High), program_SPWL_before_year_11(M_S_High), program_SPWL_after_year_11(M_S_High))
colnames(M_S_High_SPWL ) <- c("Age", "No Program", "Before yr 11", "After yr 11")

#M_S_Mod
M_S_Mod_SPWL <- data.frame(c(1:100),no_program_SPWL(M_S_Mod), program_SPWL_before_year_11(M_S_Mod), program_SPWL_after_year_11(M_S_Mod))
colnames(M_S_Mod_SPWL ) <- c("Age", "No Program", "Before yr 11", "After yr 11")

#F_S_High
F_S_High_SPWL <- data.frame(c(1:100),no_program_SPWL(F_S_High), program_SPWL_before_year_11(F_S_High), program_SPWL_after_year_11(F_S_High))
colnames(F_S_High_SPWL ) <- c("Age", "No Program", "Before yr 11", "After yr 11")

#F_S_Mod
F_S_Mod_SPWL <- data.frame(c(1:100),no_program_SPWL(F_S_Mod), program_SPWL_before_year_11(F_S_Mod), program_SPWL_after_year_11(F_S_Mod))
colnames(F_S_Mod_SPWL ) <- c("Age", "No Program", "Before yr 11", "After yr 11")

library(writexl)
write_xlsx(list("M_S_High" = M_S_High_SPWL, "M_S_Mod" = M_S_Mod_SPWL, "F_S_High" = F_S_High_SPWL,
                "F_S_Mod" = F_S_Mod_SPWL, "M_NS_High" = M_NS_High_SPWL, "M_NS_Mod" = M_NS_Mod_SPWL, 
                "M_NS_L" = M_NS_L_SPWL, "M_NS_VL" = M_NS_VL_SPWL, "F_NS_High" = F_NS_High_SPWL,
                "F_NS_Mod" = F_NS_Mod_SPWL, "F_NS_L" = F_NS_L_SPWL, "F_NS_VL" = F_NS_VL_SPWL), "BEM_Profit_by_Age_SPWL_250000.xlsx") #adjust spreadsheet name to match inputs

