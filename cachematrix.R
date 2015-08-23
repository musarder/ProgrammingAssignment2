## Inverse matrix cache object

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinversion <- function(solve) m <<- solve
  getinversion <- function() m
  list(set = set, get = get, setinversion = setinversion, getinversion = getinversion)
}


## calculate inversed matrix with cache support

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinversion()
  if(!is.null(m)) {
    message("getting *cached* inversed matrix! :)")
    return(m)
  }
  message("calculating inversed matrix...")
  data <- x$get()
  m <- solve(data, ...)
  x$setinversion(m)
  m
}
