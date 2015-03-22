## Function cachematrix create an inverse of matrix and store it in cache

## Function makeCachMatrix create an object matrix and store it in cache. 
## Create 4 method - set, get, setMatrix, getMatrix and store it the list to be able to access it 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setMatrix <- function(matrix) m <<- matrix
  getMatrix <- function() m
  list(set = set, get = get,
       setMatrix = setMatrix,
       getMatrix = getMatrix)
}

## Function cacheSolve create the inverse of matrix x 
## Function also checks if the x was evaluated before and use it data fron cache

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setMatrix(m)
  m
}
