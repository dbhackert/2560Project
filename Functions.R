install.packages("deSolve")
library(deSolve)
library(ggplot2)
# SIR model 
model_1 <- function(beta, gamma, S0, I0, R0, times) {
  #' Sim of SI (Susceptible and Infectious) infectious disease (ID) transmission model
  #' 
  #' @description Sim of SI with:
  #'    Susceptible (S),Infectious (I) and Recoverd (R).
  #'    
  #'  The model using this function on the Shiny app has default time in days, but units can be modified.
  #' Infected individuals recover and are  removed in this model. 
  #'    
  #' @param S : susceptible individuals : numeric
  #' @param I : infectious individuals : numeric
  #' @param R : recovered individuals : numeric  
  #' @param beta : rate of infection : numeric
  #' @param gamma : rate of infection : numeric
  #' @param tfinal : sim time (days) : numeric
  #' @usage si_model(S, I, beta, tfinal)
  #' @return The function returns the sim results taken from call
  #'   to deSolve app ordinary differential equation solver.
  #' @details A compartmental ID model with three compartments
  #'   simulated as a set of ODEs. This returns the output from odesolver as matrix,
  #'   with one column for each compartment. The content of the first column is time.
  #' @section Warning: 
  #'   This function does not check for errors. illogical parameters return "error"
  #' @export
  
  # calculation of differentiation equation
  
  # calculation of differentiation equation
  equation_1 <- function(time, variables, parameters) {
    with(as.list(c(variables, parameters)), {
      dS <- -beta * I * S
      dI <-  beta * I * S - gamma * I
      dR <-  gamma * I
      return(list(c(dS, dI, dR)))
    })
  }
  
  #values initial for parameters :
  par_val <- c(beta  = beta, gamma = gamma)
  
  # starting values of variables:
  start_values <- c(S = S0, I = I0, R = R0)
  
  # calculating the equation
  df1 <- as.data.frame(ode( start_values, times,  equation_1,  par_val))
  
  # returning the output:
  return(df1)
}
# generating the data 
df1 <- model_1 (beta = 0.004, gamma = 0.5, S0 = 999, I0 = 1, R0 = 0, times = seq(0, 10))
# plotting the data
SIRgraph<-ggplot(df1)+ geom_smooth(aes(time, S),method="auto", se=FALSE, fullrange=FALSE, level=0.95)+
  geom_smooth(aes(time, I),method="auto", se=FALSE, color="red", fullrange=FALSE, level=0.95) +
  geom_smooth(aes(time, R),method="auto", se=FALSE, color="green", fullrange=FALSE, level=0.95)
SIRgraph
# SI
model_2 <- function(beta, S0, I0, times) {
  #' Sim of SI (Susceptible and Infectious) infectious disease (ID) transmission model
  #' 
  #' @description Sim of SI with:
  #'    Susceptible (S) and Infectious (I).
  #'    
  #'  The model using this function on the Shiny app has default time in days, but units can be modified.
  #' Infected individuals do not recover and are not removed in this model. 
  #'    
  #' @param S : susceptible individuals : numeric
  #' @param I : infectious individuals : numeric
  #' @param beta : rate of infection : numeric
  #' @param tfinal : sim time (days) : numeric
  #' @usage si_model(S, I, beta, tfinal)
  #' @return The function returns the sim results taken from call
  #'   to deSolve app ordinary differential equation solver.
  #' @details A compartmental ID model with three compartments
  #'   simulated as a set of ODEs. This returns the output from odesolver as matrix,
  #'   with one column for each compartment. The content of the first column is time.
  #' @section Warning: 
  #'   This function does not check for errors. illogical parameters return "error"
  #' @export
  
  # calculation of differentiation equation
  equation_2 <- function(time, variables, parameters) {
    with(as.list(c(variables, parameters)), {
      dS <- - beta*S*I
      dI <- beta*S*I
      return(list(c(dS, dI)))
    })
  }
  
  #values initial for parameters :
  par_val2 <- c(beta  = beta)
  
  # starting values of variables:
  start_values <- c(S = S0, I = I0)
  
  # calculating the equation
  df2 <- as.data.frame(ode( start_values, times,  equation_2,  par_val2))
  
  # returning the output:
  return(df2)
}
# generating data
df2 <- model_2(beta = 0.004, S0 = 999, I0 = 1, times = seq(0, 10))
# ploting a graph
SIgraph<-ggplot(df2)+ geom_smooth(aes(time, S),method="auto", se=FALSE, fullrange=FALSE, level=0.95)+
  geom_smooth(aes(time, I),method="auto", se=FALSE, color="red", fullrange=FALSE, level=0.95)
SIgraph

