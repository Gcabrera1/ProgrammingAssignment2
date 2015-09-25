## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## This script has two functions:
##
##  => makeCacheMatrix: This function cache the matrix pased in the (x) parameter and the 
##                      inverse of the original matrix (x) using the setinv.
##  => cacheinv: This function uses the chached matrix (x) to calculated the inverse on (x).
##
##  This functions are part of the Cousera course: R Programming (Johns Hopkins)
##
##  September 2015.
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## **************************************************************************************
## The function makeCacheMatrix, takes a matrix in the form of the paramter x.
##
## With the parameter x you can:
##
##  1. Cache the matrix (x).
##  2. Return the chached matrix (x).
##  3. Cache the inverse of a (x) matrix in m.
##  4. Return the inverse of a matrix (x) m.
## *************************************************************************************
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) m <<- inverse
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## *******************************************************************************************
## The function cacheSolve, takes a matrix in the form of the parameter x.
##
## Pre requisite: In order to use this function is necesary that the object you pass to this
##              function can expose the get, set, get inverse, set inverse methods to avoid 
##              the error: $ operator is invalid for atomic vectors. This can achieve calling
##              the makeCacheMatrix, with the desired matrix.
##
## With the parameter x you can:
##
##  1. Get the data of the original matrix (x).
##  2. Calculate the inverse of the matrix (x)
##  3. Cache the inverse of the (x) matrix.
##  4. Get the inverse of the (x) matrix.
## *******************************************************************************************
cacheInv <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
