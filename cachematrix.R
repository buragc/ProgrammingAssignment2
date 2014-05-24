# makeCachematrix -> creates a special object that has 4 functions in it along with an encapsulated matrix
# cacheSolve -> expects to work with a makeCacheMatrix generated list. It first checks if there is a cached inverted matrix stored 
#                on the object. If not, it solves it and stores it on the special matrix object for later re-use.

## makeCacheMatrix
# returns a list that contains that encapsulates the matrix along with 4 methods
# set -> sets a new value for the matrix
# get -> returns the encapsulated matrix
# setinverse -> stores the inverse matrix (does not compute it)
# getinverse -> returns the stored inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inv <<- i
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## cacheSolve
# checks if the cacheMatrix already has an inverted matrix stored in it.
# if not, it solves the matrix using the solve method and also stores it in
# cacheMatrix object (x) such that it doesn't have to compute it the next time.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if (!is.null(inverse)) 
  {
    message("Returning the cached inverse")
    return (inverse)
  }
  originalMatrix <- x$get()
  inverse <- solve(originalMatrix)
  x$setinverse(inverse)
  inverse
}
