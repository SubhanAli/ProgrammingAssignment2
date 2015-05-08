## One function will create a cached version of the matrix
## The other will go ahead and solve for the inverse

## Creating the cached version of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setmatrix <- function(matrix) inv <<- matrix
    getmatrix <- function() inv
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}


## Solving for the inversed of the cached version of the matrix

cacheSolve <- function(x, ...) {
    inv <- x$getmatrix()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setmatrix(inv)
    inv
}
