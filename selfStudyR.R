list1 <-list(c(2, 5, 3), 21.3, sin)

print(list1)

M = matrix(c('a', 'b', 'c', 'd', 'e', 'f'), nrow = 2, ncol = 3, byrow = TRUE)
print(M)

a <- array(c('a', 'b'), dim = c(3, 3, 2))
print(a)

apple_colors <- c('a', 'a', 'b', 'c', 'c', 'c', 'a')

factor_apple <- factor(apple_colors)

print(factor_apple)
print(nlevels(factor_apple))

BMI <- data.frame(
  gender = c("Male", "Male", "Female"),
  height = c(152, 171.5, 165),
  weight = c(81, 93, 78),
  Age = c(42, 38, 26)
)

print(BMI)

var_x <- "Hello "

#arrow can be either <- nor -> or can assign it by using =
c(2, 3, 4) -> v
t <- c(1, 2, 3)

#can use various of operators
#for multiple logical operator, use &, | not &&, ||
print(v+t)

#create series of number
v <- 2:10
print(v);

#define whether element belong to a vector
v1 <- 8
v2 <- 12
t <- 1:10

print(v1 %in% t)
print(v2 %in% t)

#create a matrix
M = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3, byrow = TRUE)
print(M)
#multiply with it's transpose? can't understand
t = M %*% t(M)
print(t)

#how to use if..else statement
x <- c("a", "b", "c")

if("c" %in% x){
  print("c is found")
} else if("d" %in% x){
  print("d is found")
} else {
  print("nothing found")
}

#using switch statement
#switch(expression, case1, ...)
x <- switch(
  3,
  "a",
  "b",
  "c",
  "d"
)
print(x)

#repeat loop is similar to while with break condition
count <- 2
repeat {
  print(count)
  count <- count + 1
  
  if(count > 5)
    break
}

#just like formal while loop
count <- 2
while(count < 7){
  print(count)
  count = count + 1 #connot use count++
}

#for loop is just to loop in vector
#put series of alphabet when LETTERS[a:b] is used
v <- LETTERS[1:4]
for(i in v){
  if(i == "B")
    next #acts like continue statement in java
  print(i)
}

#how to use function in R
#function_name <- function(arg_1, arg_2, ...) {
# function body
#}
new.function <- function(a, b, c){
  result <- a * b + c
  print(result)
}
new.function(1, 2, 3)
#can call the function by names of arg
new.function(a = 3, b = 2, c = 1)

#calling function with default argument
new.function <- function(a = 1, b = 2) {
  result <- a * b
  print(result)
}
#when default argument is set, can call it without giving arguments
new.function()
#else call with new value
new.function(2, 3)
#p.s) function in R is lazliy evaluated, which means it will be executed 
#until it faces no value argument

#concat string with paste() function
#paste(..., sep = " ", collapse = NULL/"")
#sep = separator
#collapse = eleminate space between element but not space within an element
a <- "1"
b <- '2'
c <- "3 4"

print(paste(a, b, c))
print(paste(a, b, c, sep = "-"))
print(paste(a, b, c, sep = "", collapse = "")) #not sure why use collapse when sep = "" covers everything

#format will make every variable into string
#digits uses to get rid off float after digit assigned
result <- format(11.12345, digits = 3)
#display numbers in scientific notation
result <- format(c(6, 12.3456), scientific = TRUE)
#display minimum number of digits to the right of decimal
result <- format(23.47, nsmall = 5)
#padding with blank for width
result <- format("hello", width = 10)
#padding in center
result <- format("hello", width = 10, justify = "c")
#padding in left
result <- format("hello", width = 10, justify = "l")
print(result)

#.length()
result <- nchar("this shows the length of this sentence")
print(result)

#change to uppercase
result <- toupper("Result")

#change to lowercase
result <- tolower("Result")

#use substring
#substring(x, first, last)
result <- substring("Extract", 5, 7)
print(result)

#vector has 6 datatypes
#char     - "a"
#double   - 12.1
#integer  - 1L
#logical  - true/false
#complex  - 2+3i
#raw      - charToRaw('hello') -> changes to Hex value of each char

#vector in multiple elements(sequence)
v <- 1:10

#it also works in float
v <- 1.1:10.1

#if final element doesnt belong in sequence, it will be discarded
v <- 1.8:10.3
print(v)

#if you want to set the incrementing value, you can use seq(start, fin, by = #toIncrement)
print(seq(1, 5, by = 0.5))

#if at least one of value is character in vector, it changes all of the value into Char
v <- c('hello', 1, TRUE)
print(v)

#position in R language starts with 1(not 0)
#you can call elements by using vector
t <- c("a", "b", "c", "d", "e", "f")

#call multiple values within the vector
u <- t[c(1, 3, 5)]

#can call with negative index
#can't understand with result...
u <- t[c(-2, -1)]

#can call it by using logical(True,False/1,0)
u <- t[c(0, 0, 3, 0, 0, 1)] #this result in "c", "a"
u <- t[c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE)]
print(u)

#vectors can be calculated add/sub/multi/div
vector1 = c(1, 2, 3, 4, 5, 6)
vector2 = c(6, 5, 4, 3, 2, 1)

#addtion of vectors
add.result = vector1 + vector2
print(add.result)

#while in calculation, 
vector1 = c(1, 2, 3, 4, 5, 6)
vector2 = c(1, 2)
#vector2 will be c(1, 2, 1, 2, 1, 2) and calculated
sub.result = vector1 - vector2
print(sub.result)

#with sort() you can sort element by various orders
vector = c(4, 2, 5, 1, 3, 6)
#this will sort incrementally
sort.result <- sort(vector)
print(sort.result)
#this will sort decrementally
reverseSort.result <- sort(vector, decreasing = TRUE)
print(reverseSort.result)
#this will sort int alphabetical order can also decrementally order it.
charVector <- c("c", "b", "a", "d")
sort.result <- sort(charVector)
print(sort.result)

#in Rlanguage, you can make a list(like a Array)
#to create a list
listOfData = list("a", "b", c(1, 2, 3), TRUE, 41.22, 11.11)
print(listOfData)

#create a list that contains vector, matrix, list
listOfData = list(c("jan", "feb", "mar"), matrix(c(1, 2, 3, 4, 5, 6), nrow = 2), list("green", 12.1))

#with a list, you can name each elements in list like..
names(listOfData) <- c("1st Q", "Matrix", "innerList")
print(listOfData)

#can access element in a list using both index or name given
print(listOfData[2])
print(listOfData$Matrix) #1st Q will not work because it will read numeric at the first time

#can ethier add element or update with pointing to specific value
listOfData[3] <- "updated"
listOfData[4] <- "new"
#if you want to remove a data in a list, use NULL
listOfData[4] <- NULL
print(listOfData)

#Merging list unlike vector, char don't affect other elements
list1 = list(1, 2, 3)
list2 = list("a", "b", "c")

merged.list <- c(list1, list2)
print(merged.list)

#can convert list into a vector
list1 = list(1:3)
list2 = list(4:6)

convertedList1 = unlist(list1)
convertedList2 = unlist(list2)

print(convertedList1 + convertedList2)

#syntax in matrix 
#matrix(data - input Vector, nrow - #of rows to create , 
#ncol - #of column to create, byrow - will be arranged by row default is column, 
#dimnames - name to assign in row and column)
rowNames = c("row1", "row2", "row3", "row4")
colNames = c("col1", "col2") #must be strictly assigned
MatrixExample <- matrix(c(5:10), nrow = 4, byrow = TRUE, dimnames = list(rowNames, colNames))
print(MatrixExample)
#can access element by assigning [row, column], [row, ], [, column]
print(MatrixExample[1, 2])

#can compute Matrix
matrix1 <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
matrix2 <- matrix(c(6, 5, 4, 3, 2, 1), nrow = 2)

#add the matrix division, multiplication, subtraction is also possible
result <- matrix1 + matrix2
cat("result of addition\n")
print(result)

#array in R language
vector1 <- c(1, 2, 3) #this will be added to 1 column
vector2 <- c(1, 2, 3, 4, 5, 6)

#take vectors as input to the array
result <- array(c(vector1, vector2), dim = c(3, 3, 2))
print(result)

#ex









