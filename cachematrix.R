# Matrix inversion is a CPU intensive operaton. Instead of computing the inverse
# everytime, we can cache the inverse of the matrix and provide it when requested
# repeatedly. The following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    setMatrix <- function(y) {
        mat <<- y
        inv <<- NULL
    }
    getMatrix <- function() {
        mat
    }
    setMatrixInverse <- function(matinv) {
        inv <<- matinv
 
    }
    getMatrixInverse <- function() {
        inv
    }
    list(setMatrix=setMatrix, getMatrix=getMatrix, setMatrixInverse=setMatrixInverse, getMatrixInverse=getMatrixInverse)
}


# The following function returns the inverse of the matrix. 
# This function will first check if inverse is already calcuated using 
# getMatrixInverse() function. If it is, it will # get the result from the cache. 
# Otherwise, it will compute the inverse using the solve() funtion and stores 
# in the cache using setMatrixInverse() function.

cacheSolve <- function(x, ...) {
    inv <- x$getMatrixInverse()
    if(!is.null(inv)) {
        message("Getting cached data.")
        return(inv)
    }
    data <- x$getMatrix()
    inv <- solve(data)
    x$setMatrixInverse(inv)
    inv
}


# Testing:
#> x <- matrix(c(2, 5, 10, 20), 2, 2)
#> m = makeCacheMatrix(x)
#> m$setMatrix(x)
#> m$getMatrix()
#     [,1] [,2]
#[1,]    2   10
#[2,]    5   20
#> 
#> cacheSolve(m)
#     [,1] [,2]
#[1,] -2.0  1.0
#[2,]  0.5 -0.2
#
#> cacheSolve(m)
#Getting cached data.
#     [,1] [,2]
#[1,] -2.0  1.0
#[2,]  0.5 -0.2
