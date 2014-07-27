## Put comments here that give an overall description of what your
## functions do
## Like the example, 'makeCacheMatrix' provide a method to set/get
## the matrix and the inverse of the metrix. And 'cacheSolve' 
## calculates the inverse of the matrix, if it can not find it from 
## cache.

## Write a short comment describing this function
## makeCacheMatrix, creates a list of function to 
## 1. set: set the value of the matrix
## 2. get: get the value of the matrix
## 3. setInv: set the value of the inverse to cache
## 4. getInv: get the value of the inverse from cache

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInv <- function(Inv) i <<- Inv
    getInv <- function() i
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## Write a short comment describing this function
## cacheSolve, calculates the inverse of matrix with above function.
## if it finds inverse available from cache, it just read the
## cache and skip the calculation. Otherwise, it calculates
## the inverse of the matrix and store it in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getInv()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInv(i)
    i
}
