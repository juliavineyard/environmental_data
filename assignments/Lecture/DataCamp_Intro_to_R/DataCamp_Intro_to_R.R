#Questions 1-6
a = "Julia"
b1 = 45.6
b2 = "45.6"
c1 = 0:3

class(a)
class(b1)
class(b2)
class(c1)

b1 + b2

class(b1)
class(c1)

b1 + c1

#Questions 7-9
v1 = -2:2
print(v1)

v2 = v1 * 3
print(v2)

sum(v2)

#Questions 10-11
vec4 = 1:12
mat_1 = matrix(vec4, nrow = 3, ncol = 4, byrow = TRUE)
print(mat_1)

mat_2 = matrix(vec4, nrow = 3, ncol = 4, byrow = FALSE)
print(mat_2)

#Questions 12-14
my_list_1 = list("two" = 5.2, "one" = "five point two", "three" = 0:5)
print(my_list_1)
my_list_1[[3]]
my_list_1$one

#Questions 15-16
my_vec = rep(1:3, 5)
my_vec

##Don't know how to do the last part of creating my_bool_vec
#Use the logical equality test operator == to create a vector, my_bool_vec of
  #Boolean values from my_vec
my_bool_vec = my_vec
my_bool_vec
my_bool_vec = as.logical(my_vec, [1 == FALSE, 2 == FALSE, 3 == TRUE])
my_bool_vec

my_bool_vec == my_vec
data.frame(my_vec, my_bool_vec)

my_bool_vec = my_vec == 3
my_bool_vec
data.frame(my_vec, my_bool_vec)
my_vec[my_bool_vec]
