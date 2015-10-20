## Below are two functions that are used to create a special object
## that stores a square invertible matrix and caches its inverse.

## The first function, makeCacheMatrix creates a special "matrix",
## which is actually a list of 4 functions to:

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setminv <- function(minv) m <<- minv
        getminv <- function() m
        list(set = set, get = get,
             setminv = setminv,
             getminv = getminv)
}


## The second function, cacheSolve calculates the inverse of the matrix
## created with the first function. It there is no change in the input data,
## it gets the inverse from the cache, otherwise, it calculates it on the go.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getminv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setminv(m)
        m        
}
