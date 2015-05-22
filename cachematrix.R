## This function needs to be called when you want to create
## a cached matrix. A cached matrix is a regular matrix except that
## some additional operations and data are made as usable properties
## of the matrix. This property-enhanced matrix is a cached matrix.
## This function in particular stores the inverse of the matrix as
## one of the object's usable properties.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function is called on cached matrix objects to return
## the inverse of the matrix.
## Instead of recalculating the inverse every time this function is
## called - the function uses the cached matrix object to see if
## the object has a non-null inverse value (presumably from a previous
## calculation) and if so returns that value. If not, this function
## calculates the inverse and stores the inverse value with the cached
## matrix object so that the inverse can be returned the next time
## the same cached matrix object is passed in to this function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
