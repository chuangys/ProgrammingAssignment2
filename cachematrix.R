## Put comments here that give an overall description of what your
## functions do
## -->
##This function creates a special "matrix" object that can cache its inverse.

## Write a short comment describing this function
## -->


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
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- inv(data, ...)
  x$setinv(m)
  m
  ## Return a matrix that is the inverse of 'x'
}

A <- matrix(c(1,2,3,4),2,2)
solve(A)
cacheSolve(A)
makeCacheMatrix(A)