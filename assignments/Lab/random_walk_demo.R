##Random Walk ----
  #work on the code outside of the function, make sure it works, clear the 
    #environment then put the code into a function to test if it actually works

x_start= 0 
y_start= 0
n_steps= 10000

step_data= matrix(0, ncol=2, nrow= n_steps + 1)

step_data[1,1]= x_start
step_data[1,2]=y_start


i=1

for(i in 1:n_steps)
{
  x_current= step_data[i, 1]
  y_current= step_data[i, 2]
  
  x_next= x_current + runif(n=1, min= -1, max=1)
  y_next= y_current + runif(n=1, min=-1, max=1)
  
  step_data[i+1, 1]= x_next
  step_data[i+1, 2]= y_next
}

#Plot random walk 1
plot(step_data[, 1], step_data[, 2], type="l")

#Random Walk Function version 1
r_walk_1= function(x_start, y_start, n_steps)
{
  step_data= matrix(0, ncol=2, nrow= n_steps + 1)
  step_data[1,1]= x_start
  step_data[1,2]=y_start
  
for(i in 1:n_steps)
{
  x_current= step_data[i, 1]
  y_current= step_data[i, 2]
  
  x_next= x_current + runif(n=1, min= -1, max=1)
  y_next= y_current + runif(n=1, min=-1, max=1)
  
  step_data[i+1, 1]= x_next
  step_data[i+1, 2]= y_next
}
  return(step_data)
}

coords_walk_1= r_walk_1(0, 0, 10000)
plot(coords_walk_1, type="l")



#Linear Function ----

slope= 1
intercept= 1
x= 1:5

slope * x + intercept

linear= function(slope, intercept, x)
{
  y= slope * x + intercept
  
  return(y)
}

linear(1, 1, 1:5)



