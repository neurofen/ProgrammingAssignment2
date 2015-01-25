## Create an cachable matrix object which caches the inversion of a given invertable matrix.
## Using cacheSolve, calculate the inverse of the cacheable matrix. Repeating solve on the
## same cacheable matrix should returned the cached inverse, calculated the first time
## cacheSolve was called.
##
## Usage:
## cached.matrix <- makeCacheMatrix(matrix(1:4,2,2))
## 
## Calling cacheSolve for first time will trigger calculation of matrix inverse
##
## cacheSolve(cached.matrix)
##
##        [,1] [,2]
##   [1,]   -2  1.5
##   [2,]    1 -0.5
##
## Subsequent calls will returned cached inverse
##
## cacheSolve(cached.matrix)
##
## getting cached inverse of matrix
##        [,1] [,2]
##   [1,]   -2  1.5
##   [2,]    1 -0.5

## Given an invertable matrix, return a cacheable
## matrix object that can cached its inverse

makeCacheMatrix <- function(x = matrix()) {
    cache <- NULL
    # set value of matrix
    set <- function(y) {
        x <<- y
        cache <<- NULL
    }
    # get value of matrix
    get <- function() x
    # set value of reverse matrix
    setInverse <- function(inverse) cache <<- inverse
    # get value of reverse matrix
    getInverse <- function() cache
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Return the inverse of an cacheMatrix object. If the inverse has already 
## been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	inverseFunc <- x$getInverse()
    if(!is.null(inverseFunc)) {
        message("getting cached inverse of matrix")
        return(inverseFunc)
    }
    data <- x$get()
    matrixInverse <- solve(data, ...)
    x$setInverse(matrixInverse)
    
    ## Return a matrix that is the inverse of 'x'
    matrixInverse
}
