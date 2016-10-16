## Pair of functions "makeCacheMatrix" and "cacheSolve" allow to cache the inverse of a matrix.

## Function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse.
## It consists of 4 functions: set, get, setinv, getinv.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinverse = setinv,
       getinverse = getinv)
}

## Function "cacheSolve" computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix hasn't changed), then the cachesolve retrieves the inverse from the cache.

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
