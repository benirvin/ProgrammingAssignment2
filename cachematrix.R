## These functions illustrate use of an object model in R to avoid
## unnecessary repeated calculations.  Given an invertible matrix,
## if the inverse of that matrix is needed more than once in a program
## these functions will save the inverse and return the saved value
## on subsequent calls.  First once must pass the matrix to the 
## makeCacheMatrix function, assigning the result to a new variable.
## This variable will formally be a list of functions to set or get both
## the numeric matrix or its inverse.  The cacheSolve function is then
## used to obtain the inverse and will return the cached result when it 
## is available or will apply the solve() function to find the inverse 
## the first time it is called on a given instance of the object it creates

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
