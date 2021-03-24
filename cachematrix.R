## With the following functions we will calculate the inverse of a matrix (square and invertible)

##This function creates a special array that can be used when calling the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
  r <- NULL
  set <- function(y){
    x <<- y
    r <<- NULL
  }
  get <- function() x
  setsolve <-function(solve) r <<- solve
  getsolve <-function()  r
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function takes a special matrix created by the makeCacheMatrix function and returns the inverse of that matrix

cacheSolve <- function(x, ...) {
  r <- x$getsolve()
  if(!is.null(r)) {
    message("getting cached data")
    return(r)
  }
  data <- x$get()
  r <- solve(data, ...)
  x$setsolve(r)
  r
}
