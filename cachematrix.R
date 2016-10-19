## This pair of functions results on caching the inverse of a matrix
## The functions assume that the given matrix is invertible
## which means the matrix is a square matrix

## The first function 'makeCacheMatrix' creates a special "matrix"
## that can cache its inverse.
## At the end is a list containing a function to
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse/solve
## 4.  get the value of the inverse/solve

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
}

## The second function `cacheSolve` computes the inverse of the
## matrix returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}

## tested with:
## matrix <- makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2))
## print(cacheSolve(matrix))
## print(cacheSolve(matrix))
