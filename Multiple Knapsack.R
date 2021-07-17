#Set your own working directory
setwd("C:/Users/diego/Documents/R/Projects/GitHub_Projects/Optimization/Multiple Knapsack")

# Import lpSolve package
library(lpSolve)

#Import required packages
library(dplyr)
library(ROI)
library(ROI.plugin.symphony)
library(ompr)
library(ompr.roi)

#Set weights
w <-c(48, 30, 42, 36, 36, 48, 42, 42, 36, 24, 30, 30, 42, 36, 36)

#Set values
v <- c(10, 30, 25, 50, 35, 30, 15, 40, 30, 35, 45, 10, 20, 30, 25)

#Set capacities
k <- c(100, 100, 100, 100, 100)

#Set number of Bins
m <- length(k)

#Set number of Items
n <- length(w)

#Build Model
Model <- MIPModel() %>%
  add_variable(x[i, j], i = 1:n , j = 1:m, type = "binary") %>%       #define variables
  set_objective(sum_expr(x[i,j]*v[i], j = 1:m, i = 1:n), "max") %>%   #define objective function
  add_constraint(sum_expr(x[i, j], j = 1:m) <= 1, i = 1:n) %>%        #define constraints
  add_constraint(sum_expr(x[i, j]*w[i], i = 1:n) <= k[j] , j = 1:m) %>%
  solve_model(with_ROI(solver = "symphony", verbosity = 1))


#Model summary
##Status
print(paste("Model status is:", Model$status))

##Objective Function
print(paste("Objective value:", objective_value(Model)))

## X variables
for (r in 1:n) {
  for (c in 1:m) {
    tmp <- get_solution(Model, x[i, j]) %>%
      filter(variable == "x", i == r , j == c) %>%
      select(value)
    
    if (tmp > 0) {
      print(paste("x[", r, ",", c, "] = ", tmp))
    }
  }
}