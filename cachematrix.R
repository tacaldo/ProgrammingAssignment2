## makeCacheMatrix and cacheSolve, are a pair of functions that cache the inverse of a matrix.



## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setinverseMatrix <- function(im) inverseMatrix <<- im
  getinverseMatrix <- function() inverseMatrix
  
  list(set = set, get = get,
       setinverseMatrix = setinverseMatrix,
       getinverseMatrix = getinverseMatrix)

}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  im <- x$getinverseMatrix()
  if(!is.null(im) ) { 
    message("getting cached data for inverse matrix...")
    return(im)
  }
  data <- x$get()
  x$set(data)
  im <- solve(data, ...)
  x$setinverseMatrix(im)
  im

}

