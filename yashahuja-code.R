#Assessment Task 3

#Q2 Factory Problem

library(lpSolveAPI)

factmodel<-make.lp(9,9) #initializing 9 constraints and 9 decision variables

lp.control(factmodel, sense="maximize") #setting control parameters 

#sense is maximize as we want to maximize the profit

set.objfn(factmodel, c(25,21,25,10,6,10,5,1,5)) #setting objective function

#updating the left hand side of constraints one by one

set.row(factmodel, 1, c(1,1,1), indices = c(1,4,7))
set.row(factmodel, 2, c(1,1,1), indices = c(2,5,8))
set.row(factmodel, 3, c(1,1,1), indices = c(3,6,9))
set.row(factmodel, 4, c(0.45,-0.55,-0.55), indices = c(1,4,7))
set.row(factmodel, 5, c(0.55,-0.45,-0.45), indices = c(2,5,8))
set.row(factmodel, 6, c(0.7,-0.3,-0.3), indices = c(3,6,9))
set.row(factmodel, 7, c(-0.3,0.7,-0.3), indices = c(1,4,7))
set.row(factmodel, 8, c(-0.4,0.6,-0.4), indices = c(2,5,8))
set.row(factmodel, 9, c(-0.5,0.5,-0.5), indices = c(3,6,9))


#updating the right hand side of constraints

set.rhs(factmodel, c(3800,3200,3500,0,0,0,0,0,0))

#updating the symbols

set.constr.type(factmodel, c("<=","<=","<=",">=",">=",">=",">=",">=",">="))

#updating the type of decision variables

set.type(factmodel, c(1:9), "real")

#updating the lower and upper bounds of decision variables

set.bounds(factmodel, lower = rep(0,9), upper = rep(Inf,9))

factmodel #Display the LP model

#since the model is large, using write.lp for printing larger lps

write.lp(factmodel, filename = "fact.lp")  

#it produces a text file which can be analyzed with any text editor

solve(factmodel) # http://lpsolve.sourceforge.net/5.5/solve.htm


optimal_profit<-get.objective(factmodel)
optimal_profit #optimal profit of the factory 

optimal_values<-get.variables(factmodel)
optimal_values #optimal values of the decision variables

constraints<-get.constraints(factmodel)
constraints  #values of the constraints

sum(optimal_values[c(1,4,7)]) #tons of spring product produced
sum(optimal_values[c(2,5,8)]) #tons of autumn product produced
sum(optimal_values[c(3,6,9)]) #tons of winter product produced

sum(optimal_values[c(1,2,3)]) #tons of cotton material used
sum(optimal_values[c(4,5,6)]) #tons of wool material used 
sum(optimal_values[c(7,8,9)]) #tons of silk material used 

#Q3
#Two Player zero zum game

#I have chosen David as Player 1 and Helen as Player 2

#Player 1's game (David)

  davidgame<-make.lp(0,6) #initializing 0 constraints and 6 decision variables
  
  lp.control(davidgame, sense="maximize") #setting control parameters 
  
  #sense is maximize because Helen would want to minimize David's payoff,
  #so she will choose a strategy yj that makes David's payoff equal to
  #min(-5x1-5x2-10x4-10x5, -10x1-10x2-10x3-5x4-5x5, -10x1-10x2-20x3, -5x4-5x5)
  #Then David would choose x1,x2,x3,x4,x5 to make
  #min(-5x1-5x2-10x4-10x5, -10x1-10x2-10x3-5x4-5x5, -10x1-10x2-20x3, -5x4-5x5)
  #as large as possible
  
  set.objfn(davidgame, c(0,0,0,0,0,1)) #x1,x2,x3,x4,x5,v
  
  #adding constraints
  
  add.constraint(davidgame, c(5, 5, 0, 10, 10, 1), "<=", 0)
  
  add.constraint(davidgame, c(10, 10, 10, 5, 5, 1), "<=", 0)
  
  add.constraint(davidgame, c(10, 10, 20, 0, 0, 1), "<=", 0)
  
  add.constraint(davidgame, c(0, 0, 0, 5, 5, 1), "<=", 0)
  
  add.constraint(davidgame, c(1 , 1, 1, 1, 1, 0), "=", 1)
  
  #updating the lower and upper bounds of decision variables
  
  set.bounds(davidgame, lower = c(0, 0, 0, 0, 0, -Inf))
  
  RowNames <- c("Constraint1", "Constraint2", "Constraint3","Constraint4", "Constraint5")
  
  ColNames <- c("x1", "x2", "x3", "x4", "x5", "v")
  
  dimnames(davidgame) <- list(RowNames, ColNames)
  
  davidgame #Display the LP model
  
  solve(davidgame) # http://lpsolve.sourceforge.net/5.5/solve.htm
  
  valueofgame<-get.objective(davidgame)
  valueofgame  #value of the game
  
  optimal_solution<-get.variables(davidgame)
  optimal_solution #optimal strategies for David
  
  constraints1<-get.constraints(davidgame)
  constraints1  #values of the constraints

#Player 2's game (Helen)

helengame<-make.lp(0,8) #initializing 0 constraints and 6 decision variables

lp.control(helengame, sense="minimize") #setting control parameters 

#sense is minimize because David would choose a strategy xi 
#to obtain an expected reward of
#max(-5y1-5y2-10y3-10y4-10y5, -10y3-10y4-20y5, -10y1-10y2-5y3-5y4-5y6-5y7)
#Then Helen would choose y1,y2,y3,y4,y5,y6,y7 to make
#max(-5y1-5y2-10y3-10y4-10y5, -10y3-10y4-20y5, -10y1-10y2-5y3-5y4-5y6-5y7)
#as small as possible


set.objfn(helengame, c(0,0,0,0,0,0,0,1)) #y1,y2,y3,y4,y5,y6,y7,v

#adding constraints

add.constraint(helengame, c(5, 5, 10, 10, 10, 0, 0, 1), ">=", 0)

add.constraint(helengame, c(0, 0, 10, 10, 20, 0, 0, 1), ">=", 0)

add.constraint(helengame, c(10, 10, 5, 5, 0, 5, 5, 1), ">=", 0)

add.constraint(helengame, c(1, 1, 1, 1, 1, 1, 1, 0), "=", 1)

#updating the lower and upper bounds of decision variables

set.bounds(helengame, lower = c(0, 0, 0, 0, 0, 0, 0, -Inf))

RowNames <- c("Constraint1", "Constraint2", "Constraint3","Constraint4")

ColNames <- c("y1", "y2", "y3", "y4", "y5", "y6", "y7", "v")

dimnames(helengame) <- list(RowNames, ColNames)

helengame #Display the LP model

solve(helengame) # http://lpsolve.sourceforge.net/5.5/solve.htm

valueofgame<-get.objective(helengame)
valueofgame  #value of the game

optimal_solution<-get.variables(helengame)
optimal_solution #optimal strategies for Helen

constraints1<-get.constraints(helengame)
constraints1  #values of the constraints






