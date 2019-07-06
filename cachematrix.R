## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  im <- x$getinverseMatrix()
  if(!is.null(im) ) { 
    message("getting cached data for inverse matrix...")
    return(im)
  }
  data <- x$get()
  x$set(data)
  im <- solve(data, ...) ## TODO: need to confirm this
  x$setinverseMatrix(im)
  im

}

