## makeCacheMatrix is a function that creates and saves a special matrix. The intention is to prevent any recalculation

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
}
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve calculates the inverse of the makeCacheMatrix and if the inverse was already calculated and the values had not changed it retrieves the inverse from the cache directly

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m        ## Return a matrix that is the inverse of 'x'
}

