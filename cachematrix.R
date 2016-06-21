## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## 
##This function creates a special "matrix" object that can cache its inverse.
##   sets the value of the matrix
##   gets the value of the matrix
##   sets the value of the inverse matrix
##   gets the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}


## Calculate the inverse of the special "matrix" created with the above
## function, reusing cached result if it is available


##This function computes the inverse of the special "matrix" 
#returned by makeCacheMatrix above. If the inverse has already
#been calculated (and the matrix has not changed), then the cachesolve
#should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    m <- x$get()
    i <- solve(m, ...)
    x$setinverse(i)
    i
}
