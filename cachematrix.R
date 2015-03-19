## This pair of functions will create a matrix with the properties of being 
## able to cache the solved matrix inversion. 

## This is the matrix constructor function

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) s <<- solve
    getinverse <- function() s
    
    ## Returns a list of the functions avail. in the CacheMatrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This is the inversion solver function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## Check if inverse is cached
    s <- x$getinverse()
    if(!is.null(s)) {
        message("getting cached inverse data")
        return(s)
    }
    
    ## If not, then solve inverse
    data <- x$get()
    s <- solve(data, ...)
    x$setinverse(s)
    s
}
