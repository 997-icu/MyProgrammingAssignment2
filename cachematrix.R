##Matrix inversion is usually a costly computation and there may be 
##some benefit to caching the inverse of a matrix rather than compute it repeatedly 

##Our assignment is to write a pair of functions that cache the inverse of a matrix.

## Create the makeCacheMatrix function to cache the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix_inverse <- function(inverse) m <<- inverse
    getmatrix_inverse <- function() m
    list(set = set, get = get,
         setmatrix_inverse = setmatrix_inverse,
         getmatrix_inverse = getmatrix_inverse)

}



## Create the cacheSolve function to computes the inverse of the special matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
    m <- x$getmatrix_inverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix_inverse(m)
    m
    
        ## Return a matrix that is the inverse of 'x'
}

