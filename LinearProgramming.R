#### Batch 70 Linear Programming Activity  
####---- install.packages("lpSolve") ----####

library(lpSolve)

#ARGUMENTS

#lp(direction = "min", objective.in, const.mat, const.dir, const.rhs,
#int.vec)

# direction - character string: direction of optimization: "min"(default) or "max."
# objective.in - numeric vector: coefficients of objective function
# const.mat - numeric matrix: coefficients of constraints, one row per constraint, one column per variable (unless transpose.constraints = FALSE; see below).
# const.dir - character string vector:direction of the constraint: 
#each value should be one of "<," "<=," "=," "==," ">," or ">=".
#(In each pair the two values are identical.)
# const.rhs - numeric vector: RHS of constraints.
# int.vec -   numeric vector: indices of variables that are required to be integer.
#The length of this vector will therefore be the number of integer variables.


####---- Problem : ----####
# A farmer has 240 acres to plant.
# He needs to decide how many acres of corn to plant and how many acres of oats. 
# He can make $40.acre for corn and $30/acre for oats. 
# However, corn takes 2 hours of labor/acre to harvest, and the oats take 1hr labor/acre.
# He has only 320 hours of labor he can invest. 
# To maximize his profit, how many acres of each should he plant?


# Decision Variables
#Number of acres of corn: Nc 
#Number of acres of oats : No


#Objective Function
#Total profit  => 4000*Nc + 3000*No

#Constraints
#Constraint 1: Nc + No <=240
#Constraint 2: 2Nc+ No <=320

#Implicit Constraints
# Nc and No >= 0



f.obj = c(4000, 3000)
f.con = matrix (c(1,1,2,1), nrow=2, byrow=TRUE)
f.dir = c("<=", "<=")
f.rhs = c(240,320)

lp (direction = "max",f.obj, f.con, f.dir, f.rhs,int.vec = 1:2)
lp ("max", f.obj, f.con, f.dir, f.rhs)$solution




####---- Problem 1: ----####
# A furniture maker has 6 units of wood and 28 hours of free time.
# Two models were sold well in the past.
# Model 1 requires 2 units of wood and 7 hours of time. 
# Model 2 requires 1 unit of wood and 8 hours of time.
# Selling Prices are Rs. 200 and 150 each. 
# How many of each should he make to maximize the revenues?



# Decision Variables
#Number of pieces of model 1: M1 
#Number of pieces of model 2: M2

#Objective Function
# Revenue earned from making M1 pieces of model 1: 200*M1 
# Revenues earned from making M2 pieces of model 2: 150*M2
#Total Revenue R = 200*M1 + 150*M2

#Explicit Constraints

#The total wood needed to make M1 & M2 pieces: 2M1 + M2
#Available wood: 6
#Constraint 1: 2M1 + M2 <=6

#The total time needed to make M1 & M2 pieces: 7M1 + 8M2
#Available time: 28
#Constraint 2: 7M1 + 8M2 <= 28

#Implicit Constraints
# M1 and M2 >= 0
library(lpSolve)
f.obj = c(200, 150)
f.con = matrix (c(2, 1, 7, 8), nrow=2, byrow=TRUE)
f.dir = c("<=", "<=")
f.rhs = c(6,28)

lp(direction = "max", objective.in = f.obj, const.mat = f.con, 
   const.dir = f.dir, const.rhs = f.rhs)

a=lp ("max", f.obj, f.con, f.dir, f.rhs)
a$solution


lp ("max", f.obj, f.con, f.dir, f.rhs)$solution
#lp ("max", f.obj, f.con, f.dir, f.rhs, int.vec=1:2)$solution
lp ("max", f.obj, f.con, f.dir, f.rhs, int.vec=1:2)$solution  

####---- Problem 2  --####
# A  manufacturer  of printed circuit boards has 200 resistors, 120 transistors and 150 capacitors.
# The company is required to produce two different circuit boards  that requires:
# Type A board: 20 resistors, 10 transistors and 5 capacitors 
# Type B board: 10 resistors, 12 transistors and 30 capacitors 
# If the profit on type A board is Rs. 5 and type B board is 12,
# How many of each should he manufacture to maximize the profits?

f.obj = c(5, 12)
f.con = matrix (c(20,10, 10, 12,5,30), nrow=3, byrow=TRUE)
f.dir = c("<=", "<=","<=")
f.rhs = c(200,120,150)

lp ("max", f.obj, f.con, f.dir, f.rhs, int.vec=1:2)
lp ("max", f.obj, f.con, f.dir, f.rhs, int.vec=1:2)$solution


####---- Problem 3:Transportation problem --####

# Tropicsun currently has 275,000 bags of citrus at Mt. Dora,
# 400,000 bags at Eustis, and 300,000 bags at Clermont. Tropicsun has
# citrus processing plants in Ocala, Orlando, and Leesburg with processing 
# capacities to handle 200,000, 600,000, and 225,000 bags, respectively.
# Tropicsun contracts with a local trucking company to transport its fruit 
# from the groves to the processing plants. The trucking company charges a 
# flat rate for every mile that each bushel of fruit must be transported. 
# Each mile a bushel of fruit travels is known as a bushelmile. 
# The following table summarizes the distances (in miles) between the groves 
# and processing plants:

#Solving it using lp.transport
rm(list=ls(all=TRUE))
library(lpSolve)
obj <- c(21,50,40,35,30,22,55,20,25)
cost.mat <- matrix(obj,nrow=3,byrow=TRUE)
rowdir=c("=", "=", "=")
coldir=c("<=", "<=", "<=")
rowRhs=c(275000,400000,300000)
colRhs=c(200000, 600000, 225000)
trans <-lp.transport (cost.mat, direction="min", 
                      rowdir, rowRhs, coldir,
                      colRhs,
                      integers=1:nrow(cost.mat)*ncol(cost.mat))

options(scipen = 100)
trans$solution

#Or Solving it using lp, by explicitly mentioning constraints
#l <- lp ("max", f.obj, f.con, f.dir, f.rhs, int.vec=1:2)
#lp ("max", f.obj, f.con, f.dir, f.rhs, int.vec=1:2)$solution
#l$solution
obj=c(21, 50, 40, 35, 30, 
      22, 55, 20, 25) 
con=rbind(c(1,1,1,0,0,0,0,0,0), 
          c(0,0,0,1,1,1,0,0,0), 
          c(0,0,0,0,0,0,1,1,1), 
          c(1,0,0,1,0,0,1,0,0), 
          c(0,1,0,0,1,0,0,1,0), 
          c(0,0,1,0,0,1,0,0,1)) 
dir=c("==", "==", "==", 
      "<=", "<=", "<=") 
rhs=c(275000, 400000, 300000, 
      200000, 600000, 225000) 
res=lp("min", obj, con, dir, rhs, int.vec=1:9, 
       compute.sens=1) 
res$solution


####---- Problem 4 Assignment Problem: ----####
#A construction company has four large bulldozers 
# located at four different garages. The bulldozers 
# are to be moved to four different construction sites.
# The distances in miles between the bulldozers and 
# the construction sites are given below.

# Bulldozer\Site	A	B	C	D
# 1	            90	75	75	80
# 2	            35	85	55	65
# 3	            125 95 90 105
# 4	            45	110 95 115

#How should the bulldozers be moved to the construction sites in order to minimize the total distance traveled?

#Solving it using lp.assign
f.obj <-  c(90,75,75,80,35,85,55,65,125,95,90,105,45,110,95,115)
costMtrx <- matrix(f.obj,nrow=4,byrow=TRUE)
lp.assign (costMtrx, direction="min")$solution

# Solving it using lp
obj <-  c(90,75,75,80,35,85,55,65,125,95,90,105,45,110,95,115)
con=rbind(c(1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0), 
          c(0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0),
          c(0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0),
          c(0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1),
          c(1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0),
          c(0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0),
          c(0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0),
          c(0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1))
dir=c("==", "==", "==","==", 
      "==", "==", "==","==") 
rhs=c(rep(1,8)) 
lp(obj, con, dir, rhs, direction = "min")$solution

####---- Problem 5  ----####
#2 machines are used to produce 2 types of
# products.
#             Prod1	Prod2   Availability
# Machine1	2 hours	1 hour	8 hours
# Machine2	1 hour	2 hours	8 hours
# Uni Profit	300	  200	

#a.	Find number of units of product 1 and product 2 to be made to maximize the daily profit.
library(lpSolve)
obj=c(300,200)
con=rbind(c(2,1), 
          c(1,2), 
          c(0,1),
          c(1,0))
dir=c("<=", "<=", ">=", ">=")
rhs=c(8,8,0,0)

res=lp("max", obj, con, dir, rhs,int.vec = 1:2,compute.sens = 1,)
res
res$solution
# 3 units of product 1 and 2 units of product 2

#b.	If the availability of M1 increases by 1 hour, Find the shadow price. what is the increase in profit?
library(lpSolve)
obj=c(300,200)
con=rbind(c(2,1), 
          c(1,2))
dir=c("<=", "<=", ">=", ">=")
rhs=c(9,8)
res=lp("max", obj, con, dir, rhs,int.vec = 1:2,compute.sens = 1)
res
res$solution

#Shadow price is increase in the objective function
#per unit increase in rhs of constraint.
# Profit is increased by 100. 

#c.	If we increase the capacity of both machines, 
#which of them should get priority? 

obj=c(300,200)
con=rbind(c(2,1), 
          c(1,2))
dir=c("<=", "<=", ">=", ">=")
rhs=c(9,8)
lp("max", obj, con, dir, rhs,compute.sens = 1,int.vec = c(1,1))
rhs=c(8,9)
lp("max", obj, con, dir, rhs,compute.sens = 1,int.vec = c(1,1))

#Profit increased when availability of machine1 increased by 1hour.
#Profit remained unchanged when availability of machine2 is increased by 1 hour.

#d.	Suppose the unit profit for both products increase by 100 and 150 dollars each, will the current solution still hold good?
library(lpSolve)
obj=c(400,350)
con=rbind(c(2,1), 
          c(1,2), 
          c(0,1),
          c(1,0))
dir=c("<=", "<=", ">=", ">=")
rhs=c(8,8,0,0)
res=lp("max", obj, con, dir, rhs,compute.sens = 1,int.vec = c(1,1))
res
res$solution
res$solution


####---- Sensitivity analysis  --####
#Note: sensitivity anlysis in integer soluton values is difficult to interpret***

obj=c(300,200)
con=rbind(c(2,1), 
          c(1,2), 
          c(0,1),
          c(1,0))
dir=c("<=", "<=", ">=", ">=")
rhs=c(8,8,0,0)
res=lp("max", obj, con, dir, rhs,compute.sens = 1)
res$solution

res$sens.coef.from
res$sens.coef.to
#These are the ranges within which the solution holds good, keeping the other constant
#The ranges are exclusive

lp("max", c(251,200), con, dir, rhs,compute.sens = 1)$solution
lp("max", c(551,200), con, dir, rhs,compute.sens = 1)$solution
lp("max", c(999,200), con, dir, rhs,compute.sens = 1)$solution

lp("max", c(300,160), con, dir, rhs,compute.sens = 1)$solution
lp("max", c(300,400), con, dir, rhs,compute.sens = 1)$solution
lp("max", c(300,600), con, dir, rhs,compute.sens = 1)$solution


####---- Duals analysis    --####

obj=c(300,200)
con=rbind(c(2,1), 
          c(1,2), 
          c(0,1),
          c(1,0))
dir=c("<=", "<=", ">=", ">=")
rhs=c(8,8,0,0)

res=lp("max", obj, con, dir, rhs,compute.sens = 1)
res
#Success: the objective function is 1333.333 
res$solution
res$duals

#Interpretation: 
#1 unit increase in rhs of constraint 1,increases objective fn by 133.33333. 
#1 unit increase in the rhs of constraint 2, increases the objective function by 33.33333

#Lets check this
obj=c(300,200)
con=rbind(c(2,1), 
          c(1,2), 
          c(0,1),
          c(1,0))
dir=c("<=", "<=", ">=", ">=")
rhs=c(9,8,0,0)

res=lp("max", obj, con, dir, rhs,compute.sens = 1)
res
#Success: the objective function is 1466.667 
#This is 1333.333  + 133.33333 = 1466.667 

obj=c(300,200)
con=rbind(c(2,1), 
          c(1,2), 
          c(0,1),
          c(1,0))
dir=c("<=", "<=", ">=", ">=")
rhs=c(8,9,0,0)

res=lp("max", obj, con, dir, rhs,compute.sens = 1)
res
#Success: the objective function is 1366.667 
#This is 1333.333  + 33.33333 = 1366.667 

