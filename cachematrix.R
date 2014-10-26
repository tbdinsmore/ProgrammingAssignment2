## makeCacheMatrix - sets up matrix object that can cache its inverse
## cacheSolve - calculates matrix inverse, retrieving from cache if available

## Creates a list containing function to:
## set the values of the matrix
## get the values of the matrix
## set the values of the inverse matrix
## get the values of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get<-function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## Retrieves value of inverse matrix from cache if available
## Otherwise, computes inverse matrix

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
}
