## These functions are able to cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache 
## its inverse. Getters and setters for getting and setting matrix
## and ints inverse are included.
## Param x is a matrix from which the object for caching is created

makeCacheMatrix <- function(x = matrix()) {
            ## Return the object for caching
            inv <- NULL
            set <- function(y) {
                        x <<- y
                        inv <<- NULL
            }
            
            get<-function() x
            
            setInversion <- function(solve) inv <<- solve
            
            getInversion <- function() inv
            
            list(set = set, 
                 get = get, 
                 setInversion = setInversion, 
                 getInversion = getInversion)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse has already 
## been calculated, then it retrieve the inverse from the cache.
## Param x is a special caching "matrix"

cacheSolve <- function(x, ...) {
            ## Return a matrix that is the inverse of 'x'
            inv<- x$getInversion()
            if (!is.null(inv)) {
                        message("getting cached data")
                        return(inv)
            }
            
            mat <- x$get()
            inv <- solve(mat,...)
            x$setInversion(inv)
            inv    
}
