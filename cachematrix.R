## These functions Cache the inverse of a matrix
## Below are 2 functions that will create a special object
## to store a matrix and cache its inverse.

## The first function creates a special matrix object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function () inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The second function will compute the inverse of the matrix
## created by makeCaceMatrix.  If there is already an inverse
## that has been calcuated, it will print from cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message ("Gathering cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
