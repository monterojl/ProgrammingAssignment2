## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function takes a square matrix and generate an special vector
# that caches the inverse matrix resolution.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invmat) inv <<- invmat
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Write a short comment describing this function
# This function takes an special vector that caches a square matrix and its
# inverse. If the inverse is already calculated just returns it. If it doesn't
# exist, it calculates it and caches it in the special vector.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
