# Matrix inversion is usually a costly computation and there may be some ...
# benefit to caching the inverse of a matrix rather than compute it repeatedly.
# These are a pair of functions that cache the inverse of a matrix.

# This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    
    # Defining a set() to assign new value of matrix. 
    set <- function(y) {                    
            x <<- y                             
            inv <<- NULL                        
    }
    
    # Defining a get() to retrieve the value of the matrix
    get <- function() x                     
        
    setinverse <- function(inverse) inv <<- inverse  
    getinverse <- function() inv                     
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve the 
# inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinverse()
    
    # If the data is already available in the cache, return it
    if(!is.null(inv)) {
        message("We're returning cached data")
        return(inv)
    }
    
    data <- x$get()
    
    # Computing the inverse of a square matrix can be done with the solve ...
    # function in R. For example, if X is a square invertible matrix, then ...
    # solve(X) returns its inverse.
    inv <- solve(data, ...)
    
    x$setinverse(inv)
    inv
}
