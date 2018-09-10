# 
# 
# closure
# 
# 
rob <- function(status){

    verh <- function(arg){
        if(arg == "s") {
            return(sum(status))
        }
        if(arg == "m") {
            return(mean(status))
        }
    }

}

test0 <- rob(c(1,1,1,111,1))
test0("m")
test0("s")


# 
# 
# S3 objects
# 
# 


# 
# data structure
# 
xypoint <- function(xval, yval) {
  if (!is.numeric(xval) | !is.numeric(yval)) {
        stop("X and Y must be numeric")
    }  
  structure(
    list(x=xval, y=yval),
    class = "xypoint"
    )
}


xlist <- function(xvals) {
  if (!is.numeric(xvals)) {
        stop("X must be numeric")
    }  
  structure(
    list(xs=xvals),
    class = "xlist"
    )
}

# 
# generic function ron
ron <- function(x, ...) {
    UseMethod("ron")
}

# 
# methods for generic function ron
ron.xypoint <- function(x) x$x * x$y
ron.xlist <- function(x) sum(x$xs)

# 
# test
test1 <- xypoint(44,22)
test2 <- xlist(c(2,3,4))

ron(test1)
ron(test2)


# 
# 
# S4 objects
# 
# 

# 
# class defintion
setClass("Person",
  slots = list(name = "character", age = "numeric"))

setClass("Employee",
  slots = list(boss = "Person"),
  contains = "Person") # "inheritance"

# 
# constructor functions
Person <- function(name,age){  
    new("Person", name = name, age = age)
}

Employee <- function(name,age, boss){
    new("Employee", name = name, age = age, boss = boss)
}


# 
# test
alice <-Person(name = "Alice", age = 40)
john <- Employee(name = "John", age = 20, boss = alice)

john@boss@age