## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
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


## Write a short comment describing this function

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
