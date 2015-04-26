## These methods are helper methods for computing inverse of a regular matrix and caaching those results

## makeCacheMatrix accepts a regular matrix and enhances it with caching helper methods
## these methods are getinverse and setinverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve accepts a cacheMatrix and solves for the inverse
##  the method will use the cache solution if one exists, otherwise
##  it will compute the answer and cache the results for future calls
## The caching is helpful for large computationaly expensive matrix inverse operations
## See makeCacheMatrix
## parameter x - an object of type cacheMatrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}