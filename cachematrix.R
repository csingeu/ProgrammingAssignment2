## Matrix inversion is usually a costly computation and there may be some benefits 
## to cache the inverse of a matrix rather than compute it repeatedly. 
## It is also assumed that the matrix supplied is always invertible.
##
## This R script contains a pair of functions that cache the inverse of a matrix.
## 1. makeCacheMatrix
## 2. cacheSolve

## The makeCacheMatrix function creates a special "matrix", which is a list 
## containing functions to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL
        set <- function(y) {
                x <<- y
                inv_x <<- NULL
        }
        get <- function() x
        setinverse<- function(inverse) inv_x <<-inverse
        getinverse <- function() inv_x
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The cacheSolve function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cacheSolve should retrieve the inverse 
## from the cache. Computing the inverse of a square matrix is done with the
## solve function in R.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        inv_x <- x$getinverse()
        
        if (!is.null(inv_x)) {
                message("getting cached inverse matrix")
                return(inv_x)
        } 
        
        inv_x <- solve(x$get())
        x$setinverse(inv_x)
        inv_x
}
