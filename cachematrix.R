## The first function, "makeCacheMatrix", creates a special "matrix", 
##which is really a list containing a function to do the following.

##1. set the value of the vector
##2. get the value of the vector
##3. set the value of the mean
##4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL 
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

##The following function calculates the mean of the special "vector" 
##created with the above function. However, it first checks to see if the inverse 
##has already been calculated. If so, it gets the mean from the cache and skips 
##the computation. Otherwise, it calculates the inverse of the "matrix" and sets the inverted matrix
##in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
		inv <- x$getinverse()
		if(!is.null(inv)) {
        	message("getting cached data")
        	return(inv)
    }
    mat <- x$get()
    inv <- solve(mat)
    x$setinverse(inv)
    inv
}

##TEST
##SET DIRECTORY TO WORKING DIRECTORY 
setwd("C:/Users/b.dreeling/Desktop/Coursera_R")

##CALL THE FUNCTION
source("Prog_assign#2.R")

## CREATE MATRIX 'x'
x<-matrix(c(4,2,-1,0), 2, 2)

##SHOW 'x'
x
##     [,1] [,2]
##[1,]    4   -1
##[2,]    2    0

##CTREATE SPECIAL "Matrix" 'x'--> 'm'
m <- makeCacheMatrix(x)

##SHOW INITIAL VALUE
m$getinverse()
NULL

##SHOW (get) 'm'
m$get()
##     [,1] [,2]
##[1,]    4   -1
##[2,]    2    0

##invert 'm', check if it has previously been calculated, if so, retrieve from cache, else invert 
cacheSolve(m)
##     [,1] [,2]
##[1,]    0  0.5
##[2,]   -1  2.0

cacheSolve(m)
##getting cached data  
##     [,1] [,2]
##[1,]    0  0.5
##[2,]   -1  2.0



#Further tests
##>z<-runif(100,0,1)
##>write.csv(z,"C:/Users/b.dreeling/Desktop/Coursera_R/unif.csv")
##>mybigmat<-makeCacheMatrix(matrix(z, 10, 10))
##>mybigmat$getinverse() 
##NULL
##>mybigmat$get()
##....
##>cacheSolve(mybigmat)
##getting cached data
##...
##

