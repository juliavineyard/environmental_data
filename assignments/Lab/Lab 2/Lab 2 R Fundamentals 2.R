class(4)
class(4.0)
1.0 == TRUE
3.000000000000001 == 3.0
3.0000000000000001 == 3.0
(0 + 1) == TRUE
4/FALSE
FALSE/FALSE
FALSE
TRUE + FALSE
(TRUE + FALSE)
3.0*(TRUE + FALSE)
TRUE - FALSE
FALSE - FALSE
TRUE * 3
TRUE / 5

#Broken apart
int_rnd = sample(100,1)
int_rnd_sentence = paste0("The value of the randomly generated number is:", int_rnd)
int_rnd_sentence

#Nested tasks ^
print(paste0("The value of the randomly generated number is: ", 
             sample(100,1)))

#Loops
for (i in 1:10) {print(i)}

#Intro to custom functions
#The code that executes is written within curly braces {}
print_number = function(n) {print(paste0("The value of the number is ", n))}
print_number(145)

#Argument: an input to a function, a function can have 0 or more arguments
#Arguments can have default values and/or names
#Return value: the value that a function produces
  #Functions do not have to have a return value!

#Arguments
#All of the following are identical
rnorm(10)
rnorm(n=10, sd=1)
rnorm(sd=1, mean=0, n=10)
#The 3 arguments for rnorm() are n, mean, sd
#Both mean and sd have default values of 0 and 1, respectively


#QUESTIONS 1-2
n=12345
vec_1 = sample(12, n, replace = TRUE)
head(vec_1)

vec_2 = vec_1 == 3
vec_1[vec_2]

#Questions 3-5
n=12345
vec_1 = sample(12, n, replace = TRUE)
head(vec_1)
length(vec_1)
sum(vec_1 == 3)

n= 10
vec_1= sample(12, n, replace=TRUE)
paste0("Sum of elements with value 3: ", sum(vec_1 == 3))

#Question 6
for (i in 1:10) 
{i=paste0("This is loop iteration: ", i, ".")
  print(i)
}

#Question 7
n=24
for (i in 1:n) 
{i=paste0("This is loop iteration: ", i, ".")
print(i)
}

#Question 8
##I can't get my loop to only print out 1-17 lines.
#Create a vector with 17 random numbers between 1 and 10
#I think my problem is with vec_1 in the argument -> no, the issue was
  #that you do not need for(i in 1:n)
#At fucking midnight my console started showing 2 columns for odd and even
#number output lines. I don't have any clue what caused this.
n=17
vec_1=sample(1:10, n, replace=TRUE)
vec_1
#Don't need: for (i in 1:n) 
{i=paste0("The element of vec_1 at index ", (1:n), " is: ", vec_1, ".")
print(i)
}

#Question 9
create_and_print_vec= function(n, min= 1, max= 10)
{
  vec_2 = sample(min:max, n, replace = TRUE)
  for(i in 1:n)
    print(paste0("The element of vec_1 at index ", i, " is ", vec_2[i]))
}
create_and_print_vec(24, min= 1, max= 10)
