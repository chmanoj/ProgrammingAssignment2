## The following code creates a list from a given matrix with setter & getter functions for get data & cached inverse
## Inverse is calculated once when cacheSolve is called & stroed in "invMatrix" of the list
## Sample usage
## b <- matrix(1:4,2,2)
## a <- makeCacheMatrix(b)
## cacheSolve(a) ## Inverse calculated here
## cacheSolve(a) ## Inverse returned from cache here

## This function takes a matrix as input & creates a list with matrix & inverse as output
## Inverse in null initially; Data matrix & inverse have setter & getter functions

makeCacheMatrix <- function(inputMatrix = matrix()) {
        invMatrix <- NULL
        set <- function(y) {
                inputMatrix <<- y
                invMatrix <<- NULL
        }
        get <- function() inputMatrix
        setInverse <- function(inv) invMatrix <<- inv
        getInverse <- function() invMatrix
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## This function get the inverse from cache if available & returns it; If not it calculates the inverse & returns it

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
