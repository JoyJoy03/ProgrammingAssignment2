## This R script contains two functions, makeCacheMatrix and cacheSolve. The purpose of the first
## function, makeCacheMatrix, is to create a special "matrix" object that can cache its inverse.
## The purpose of the second function, cacheSolve, is to compute the inverse of the special "matrix" 
## returned by the makeCacheMatrix function with a condition that if the inverse has already been
## calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse 
## from the cache instead of recalculating the inverse. 

## Examples of usage are provided at the end of the code.

## For the makeCacheMatrix function, the special "matrix" created from the function is a list 
## containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
## Note that the input argument for the makeCacheMatrix function should be a square matrix. 
## Otherwise, the matrix will not be invertible.

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL 
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
    
}

## The cacheSolve function first checks to see if the inverse of the data has already been calculated.
## If so, it gets the inverse from the cache. Otherwise, it calculates the inverse of the data, and
## sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
    
    ## Get the inverse from the cache if already existed
    inv <- x$getinv()
    if(!is.null(inv)){
        message("The inverse from the cache is returned.")
        return(inv)
    }
    
    ## Otherwise, calculate the inverse of the data
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
    
}

## Example 1:
### > mat1 <- matrix(c(4,3,2,1),nrow=2,ncol=2)
### > mat2 <- makeCacheMatrix(mat1)
### > cacheSolve(mat2)
###      [,1] [,2]
### [1,] -0.5    1
### [2,]  1.5   -2
### > cacheSolve(mat2)
### The inverse from the cache is returned.
###      [,1] [,2]
### [1,] -0.5    1
### [2,]  1.5   -2

## Example 2:
### > mat1 <- matrix(c(1,3,5,2,4,6,7,9,6),nrow=3,ncol=3)
### > mat2 <- makeCacheMatrix(mat1)
###      [,1] [,2] [,3]
### [1,] -3.0  3.0 -1.0
### [2,]  2.7 -2.9  1.2
### [3,] -0.2  0.4 -0.2
### > cacheSolve(mat2)
### The inverse from the cache is returned.
### [,1] [,2] [,3]
### [1,] -3.0  3.0 -1.0
### [2,]  2.7 -2.9  1.2
### [3,] -0.2  0.4 -0.2