## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function, makeCacheMatrix creates a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the Inverse of matrix
## get the value of the Inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        invx <- NULL
        set <- function(y) {
                x <<- y
                invx <<- NULL # set the inverse to null as input has changed
        }
        get <- function() x
        setinverse <- function(invy) invx <<- invy
        getinverse <- function() invx
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function
## This resturns the inverse of the matrix if it is present in the cache.
## If not found in cache, it calculates usin solve() function and returns the value of the inverse and stores it in cache for future use

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invx <- x$getinverse()
        if(!is.null(invx)) {
                message("getting cached data")
                return(invx)
        }
        data <- x$get()
        invx <- solve(data, ...) # calculate the inverse as the cache was Null
        x$setinverse(invx)
        invx        
}