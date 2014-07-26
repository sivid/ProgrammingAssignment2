## Put comments here that give an overall description of what your
## functions do

## Stores inverse of matrix, gives cached result if input has not been changed, calculates and 
## gives new result if input has been changed.

## Write a short comment describing this function

## makeCacheMatrix(x) accepts square matrix as input and stores relevant info about it, 
## including whether its inverse has been calculated or not, and the inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() 
    x
  setinverse <- function(inverse_matrix)
    m <<- inverse_matrix
  getinverse <- function() 
    m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

## Takes a square matrix as input.
## checks the variable m stored by makeCacheMatrix to see if this current input matrix has
## its inverse calculated or not.  If the input matrix matches with old matrix (indicated by m)
## it will load previously calculated inverse matrix.  If the input matrix does not match with
## old matrix, it will re-calculate inverse, and store it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
