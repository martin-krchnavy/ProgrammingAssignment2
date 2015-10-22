## Firstly, it is required to define a square matrix variable (e.g. "x").
## Then it is required to define another variable (e.g. "a") 
## as makeCacheMatrix function with previously defined variable ("x") as an argument.
## Finally, it is possible to use cacheSolve function
## with second variable ("a") as an argument to solve the inverse matrix.

## makeCacheMatrix is a group of functions consisting of 4 functions:
## get prints out the defined matrix;
## set changes the defined matrix and clears the inverse matrix;
## getinv prints out the inverse matrix;
## setinv defines the inverse matrix.

makeCacheMatrix <- function (x = matrix()) {
  inv <- NULL
  set <- function (y = matrix()) {
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


## cacheSolve is a function that firstly pulls the computed inverse matrix:
## if this inverse matrix is defined, it prints it out;
## otherwise it computes the inverse matrix from the defined matrix,
## pairs the result with the matrix and and prints out the result.

cacheSolve <- function (x,  ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  return(inv)
}