## below functions are used to cache the comupte intensive calculation of matrix inversion

## makeCacheMatrix function creates a set of functions to cache the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  set <- function(y) {
    mat <<- y
    inv <<- NULL
  }
  get <- function() mat
  setinv <- function(inv.temp) inv <<- inv.temp
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cache solve will check if inverse is already calculated and if so it will return the cached value without computing
# otherwise it will compute the inverse, cache the inverse and return the value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinv(inv)
  inv
}
