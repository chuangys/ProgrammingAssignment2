# This function creates a special "matrix" object that can cache its inverse.
# Besides, the function is assumed that the input matrix are supposed invertable.


# The function "makeCacheMatrix" receives in an invertible matrix, returns a list contains:
# set the matrix, get the matrix, set the inverse and get the inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


# The function "cacheSolve" apply the list output from the "makeCacheMatrix", 
# and return a matrix that is the inverse of the matrix.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}

# Following is the try run example
#A <- matrix(c(1,2,3,4),2,2)
#solve(A);
#cacheSolve(makeCacheMatrix(A));

