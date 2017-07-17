## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        cache_matx <- NULL
        
        ## set the new matrix
        setMatrix <- function(y) {
                x <<- y
                ## set the cached matrix with null
                cache_matx <<- NULL
        }
        
        ## return the matrix  
        getMatrix <- function() {
                x
        }
        
        ## set the cache matrix
        setInverse <- function(inversem) {
                cache_matx <<- inversem
        }
        
        ## get the cache matrix
        getInverse <- function() {
                cache_matx
        }
        
        ## create a list for a matrix
        list(setMatrix = setMatrix,
             getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
   
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## get the cache inverse matrix
        inverse_matx <- x$getInverse()
        
        ## if the cached matrix is existed it returns the inverse matrix and out of the function
        if(!is.null(inverse_matx)) {
                message("getting cached data")
                return(inverse_matx)
        }
        
        ## if the cached inverse matrix is not existed, get the matrix and inverse the matrix
        m_data <- x$getMatrix()
        
        ## function solve(data) to inverse the matrix
        inverse_matx <- solve(m_data, ...)
        x$setInverse(inverse_matx)
        
        ## reuturn the inverse matrix
        inverse_matx
}
