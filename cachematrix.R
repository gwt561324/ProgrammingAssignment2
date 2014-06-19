## makeCacheMatrix takes a matrix and returns an object
## that can cache the inverse when calculated.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse)  i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve returns the inverse of a given "cached" matrix.
## Cached matrices must be created with makeCacheMatrix
## and must be invertible.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message('getting cached data')
    return (i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
