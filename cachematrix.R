
## This function creates a matrix object that can cache its inverse
## This object is a list of 4 functions 
## set() which sets/modifies the original matrix 
## get() which returns original matrix
## setinv() which computes, caches, and returns matrix inverse
## getinv() which returns matrix inverse

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x                      
  setinv <- function(solve) m <<- solve   
  getinv <- function() m                  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


# This function computes, caches and returns the inverse of the special "matrix" returned by makeCacheMatrix. 
# If the inverse has already been calculated (and the matrix has not changed), then the function should retrieve 
# the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
