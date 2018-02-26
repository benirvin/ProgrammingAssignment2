## Put comments here that give an overall description of what your
## functions do

## this one creates an object defining get & set for the data and for 
## the resulting matrix inverse and containing the original data matrix
## in its environment

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) im <<- inverse
    getinv <- function() im
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## This one takes on object as jsut defined as argument and either
## 1) computes and returns the matrix inverse, or
## 2) retrunrs the cached inverse if it exists

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    im <- x$getinv()
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    data <- x$get()
    im <- solve(data, ...)
    x$setinv(im)
    im
}
