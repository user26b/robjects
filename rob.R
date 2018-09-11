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
# http://adv-r.had.co.nz/OO-essentials.html
# 


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




# 
# lca beispiele
# 



# 
# class unit
# 
# 
setClass("Einheit",
         slots = list(name = "character"))

#  constructor
Einheit <- function(name) {
  new("Einheit", name = name)
}

# kg <- Einheit("kg")
# g <- Einheit("g")
# kj <- Einheit("kJ")
# kcal <- Einheit("kcal")
# chf <- Einheit("CHF")



# 
# class functional unit
# 
# 
setClass("Fu",
         slots = list(name = "character", unit = "Einheit"))

#  constructor
Fu <- function(name, unit){  
  new("Fu", name = name, unit = unit)
}

# verdE <- Fu("verdaubareEnergie", kcal)
# protein <- Fu("protein", g)


# 
# class environmental impact
# 
# 
setClass("EnvImp",
         slots = list(name = "character", unit = "Einheit"))

#  constructor
EnvImp <- function(name, unit){  
  new("EnvImp", name = name, unit = unit)
}

# ghg <- EnvImp("GHG", kg)
# terrtoc <- EnvImp("TerrestTox", g)



#
# class mengeEinheit
#
#
#
setClass("MengeEinheiten",
         slots = list(value = "list", unit = "list"))

#  constructor
MengeEinheiten <- function(values, units){
  new("MengeEinheiten", value = values, unit = units)
}



# 
# class product
# 
# 
setClass("Produkt",
         slots = list(name = "character", fuList = "MengeEinheiten", impList = "MengeEinheiten"))

#  constructor
Produkt <- function(name, fuList, impList){  
  new("Produkt", name = name, fuList = fuList, impList = impList)
}



setGeneric("getFuMenge", function(x, y) {
  standardGeneric("getFuMenge")
})

setMethod("getFuMenge", 
          c(x = "Produkt", y = "Fu"),
          function(x, y) {
            x@fuList@value[which(x@fuList@unit[[2]] == y)]
          }
)


# init instances
kg <- Einheit("kg")
g <- Einheit("g")
kj <- Einheit("kJ")
kcal <- Einheit("kcal")
chf <- Einheit("CHF")

verdE <- Fu("verdaubareEnergie", kcal)
protein <- Fu("protein", g)

ghg <- EnvImp("GHG", kg)
terrtoc <- EnvImp("TerrestTox", g)


milch <- Produkt("Milch", MengeEinheiten(list(122, 11), list(verdE, protein)), MengeEinheiten(list(567, 1.3), list(ghg, terrtoc)))

milch@impList

milch@impList@unit[[2]]@name
