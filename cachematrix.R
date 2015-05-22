## Funtions to cache the inverse of matrix.
##  Inverse is calculated and stored the first time cacheSolve is called. 
##    Subsequent calls return the stored inverse value
## Usage instruction
##  Call makeCacheMatrix passing in matrix to be inversed 
##  Call cacheSolve passing in the value returned by makeCacheMatrix

## Creates object that will cache inverse of the matrix. Pass resulting object
##      to cacheSolve as parameters 
##  parameter: matrix to be inversed
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
    invisible()
}


## Calculates inverse of a matrix and stores results. Subsequent calls return
##      stored value
##  Parameter: pass in object returned by makeCacheMatrix
##  Return: inverse of passed matrix
cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    ## Returns matrix that is the inverse of 'x'
    s
}
