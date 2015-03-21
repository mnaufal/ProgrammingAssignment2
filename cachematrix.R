# makeCacheMatrix is a function that returns a list of functions
# The puspose is to store a martix and a cached value of the inverse of the matrix. 
# It Contains the following functions:
# setMatrix      set value of a matrix
# getMatrix      get value of a matrix
# cacheInverse   get cahced value (inverse of the matrix)
# getInverse     get cahced value (inverse of the matrix)
#

makeCacheMatrix <- function(x = numeric()) {
        
        # set cache to NULL
        cache <- NULL
        
        # store matrix
        setMatrix <- function(newValue) {
                x <<- newValue
                # since the matrix is assigned a new value, flush the cache
                cache <<- NULL
        }

        # get the stored matrix
        getMatrix <- function() {
                x
        }

        # cache the given argument 
        cacheInverse <- function(solve) {
                cache <<- solve
        }

        # get cached value
        getInverse <- function() {
                cache
        }
        
        # return a list - element of the list is a function
        list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


# The following function calculates the inverse of a "special" matrix created with 
# makeCacheMatrix
cacheSolve <- function(y, ...) {
        # get  cached value
        inverse <- y$getInverse()
        # if a cached value exists then return
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        # else get the matrix, caclulate the inverse and store it in cache
        data <- y$getMatrix()
        inverse <- solve(data)
        y$cacheInverse(inverse)
        
        # return inverse
        inverse
}
