## There are two functions in this script - 'makeCacheMatrix' and 'cacheSolve'.
## a) makeCacheMatrix : - a function that can cache the inverse of the supplied matrix
## b) cacheSolve : - a function that first checks if the inverse of the supplied matrix 
##                   has already been computed. If yes, it returns the cached inverse.
##                   If no, it computes the inverse of the matrix and returns it.

## 'makeCacheMatrix' returns a list containing functions which
## can :- a) set the value of the square matrix 
##        b) get the value of the square matrix
##        c) set the value of the inverse matrix
##        d) get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- as.matrix(y, nrow = length(y)/2, ncol = length(y)/2)
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## 'cacheSolve' first checks if the inverse of the supplied matrix has been calculated.
##  If TRUE, it returns the cached inverse matrix
##  If FALSE, it computes the inverse of the supplied matrix and returns the same

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached inverse matrix")
    return(i)
  }
  mtx <- x$get()
  i <- solve(mtx, ...)
  x$setinv(i)
  i
}
