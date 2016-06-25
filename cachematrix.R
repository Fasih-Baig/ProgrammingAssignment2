## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly.The following two functions are used to cache the inverse of a matrix.

##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated,then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("Getting Cached Data.")
        return(inverse)
    }
    matrix <- x$get()
    inverse <- solve(matrix)
    x$setInverse(inverse)
    inverse ## Return a matrix that is the inverse of 'x'
}
## Sample Run:
## > source("cachematrix.R")
## > z <- cbind(c(1,3),c(3,1))
## > matrix <- makeCacheMatrix(z)
## > cacheSolve(matrix)
##        [,1]   [,2]
## [1,] -0.125  0.375
## [2,]  0.375 -0.125
## > cacheSolve(matrix)
## Getting Cached Data.
##        [,1]   [,2]
## [1,] -0.125  0.375
## [2,]  0.375 -0.125
