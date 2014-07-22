## A pair of functions that allow for caching the result of a matrix inverse operation.

## Wrapper class with getters and setters for the matrix and its inverse.
## input: an invertible matrix
## output: a list of getter and setter functions

makeCacheMatrix <- function(x = matrix()) 
{
    inv <- NULL
    set <- function(y) 
    {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(	set = set, 
          get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


## Looks for a cached result. If none is found the inverse matrix is calculated then cached.
## input: a wrapped matrix 
## output: the inverse matrix

cachesolve <- function(x, ...) 
{
    cache <- x$getInverse()
    if(!is.null(cache)) 
    {
        message("getting cached data")
        return(cache)
    }
    data <- x$get()
    i <- solve(data)
    x$setInverse(i)
    i
}