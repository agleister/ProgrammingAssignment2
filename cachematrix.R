## Put comments here that give an overall description of what your
## functions do

## Creates a matrix whose inverse can be cached

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## return the mean of a "makeCacheMatrix"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        init <- x$get
        inv <- solve(x, ...)
        x$setinv(inv)
        inv
}
