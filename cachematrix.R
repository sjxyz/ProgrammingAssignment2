## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) m <<- inverse
  get_inverse <- function() m
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
  
  
}



## cachesolve

cacheSolve <- function(x, ...) {
  
  m <- x$get_inverse()
  if(!is.null(m)) {
    return(m)
  }
  data <- x$get()
  m <-solve(data, ...)
  x$set_inverse(m)
  m
}
