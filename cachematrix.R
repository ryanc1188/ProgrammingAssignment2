## This is to calculate the inverse of a matrix and store in cache. 
## If it was calculated before, the result will taken from cache.

## This function is to create a list of functions to
##1.  set the value of the matrix
##2.  get the value of the metrix
##3.  set the value of the inverse of matrix
##4.  get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function retrieves result from cache if it was calculated before
## If the matrix is new, inverse of it will be calculated and store in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
