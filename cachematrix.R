## Two functions that cache the inverse of a matrix
## By Me

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    minv <- NULL
    set <- function(y){
        x <<- y
        minv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) minv <<- solve
    getinv <- function() minv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes th inverse of the special matrix returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the
## cachesolved should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    minv <- x$getinv()
    if(!is.null(minv)){
        message("getting cache data")
        ## Return a matrix that is the inverse of 'x'
        return(minv)
    }
    data <- x$get()
    minv <- solve(data, ...)
    x$setinv(minv)
    ## Return a matrix that is the inverse of 'x'
    minv
        
}

