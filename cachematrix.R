## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## It creates a list object of four functions to 
## manipulate square matrix and its inverse, these four function
## have two environment(where they are defined) variables x, invX
## to store data, these reinitializes inverse to null when x is 
## initialized.
makeCacheMatrix <- function(x = matrix()) {
        invX <- NULL
        set <- function(y){
          x <<- y
          invX <<- NULL
        }
        get <- function() x
        setinv <- function(inv) invX <<- inv
        getinv <- function() invX
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

## this function checks whether we still have cached result of inverse,
## It may be rewritten to null in case x is changed, So it checks for null
## condition for inv and if that fails it return cached value otherwise
## it calculates inverse through solve function and caches it 
## for future use and return that value too.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
