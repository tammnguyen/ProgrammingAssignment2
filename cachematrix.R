##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     ## Set inverse to null
     inv <- NULL

     ## Set matrix data in the cache
     set <- function(y) {
           x <<- y
           inv <<- NULL
     }

     ## Get matrix data from cache
     get <- function() x

     ## Set the inverse of a given matrix 
     setInverse <- function(inverse) inv <<- inverse

     ## Get the inverse of a given matrix from cache 
     ## (if defined) else returns NULL
     getInverse <- function() inv

     ## List object to store the matrix and its inverse
     list(set=set, get=get,setInverse=setInverse,getInverse=getInverse)
}

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
     ## Check for inverse
     inv <- x$getInverse()
     ## If data is available in cache, then return from cache
     if(!is.null(inv)) {
           message("getting cached data")
           return(inv)
     }
     data <- x$get()
     ## Compute the inverse using solve method
     inv <- solve(data)
     ## Set the inverse of matrix in cache
     x$setInverse(inv)
     ## Return the inverse of matrix
     inv
}
