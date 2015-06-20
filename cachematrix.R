##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     ##Initialize inverse to null
     inv <- NULL

     ## function to set the matrix data in the cache
     set <- function(y) {
           x <<- y
           inv <<- NULL
     }

     ## function to get the matrix data from cache
     get <- function() x

     ## function to set the inverse of a given matrix 
     setInverse <- function(inverse) inv <<- inverse

     ## function to get the inverse of a given matrix from cache 
     ## (if defined) else returns NULL
     getInverse <- function() inv

     ## list object to store the matrix and its inverse
     list(set=set, get=get,setInverse=setInverse,getInverse=getInverse)
}

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
     ## check for inverse
     inv <- x$getInverse()
     ## if data is available in cache then return from cache
     if(!is.null(inv)) {
           message("getting cached data")
           return(inv)
     }
     data <- x$get()
     ## Compute the inverse using solve method
     inv <- solve(data)
     ## set the inverse of matrix in cache
     x$setInverse(inv)
     ## return the inverse of matrix
     inv
}
