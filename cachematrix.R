## calculating the inverse of a matrix and if it already calculated it will retrieve the value

## caching the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set,get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

a <- matrix(c(5,6,7,8),2,2)
a1 <- makeCacheMatrix(a)
cacheSolve(a1)