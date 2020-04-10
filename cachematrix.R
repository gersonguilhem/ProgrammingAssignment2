## The functions below will compute and cache the inverse of a matrix.
## Please, ensure that your matrix is invertible before trying to use these functions.

## makeCacheMatrix receives a matrix as its parameter and creates a list object containing 
## functions to get and cache the values of this matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve will attempt to retrieve the cached inverse matrix from the list object created
## with makeCacheMatrix. If there's no inverse cached, it will compute it
## and then cache its value for future use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}