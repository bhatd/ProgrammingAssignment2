## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix object that can cache it's inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y)
  {
    x <<-y
    m<<-null
  }
  get <- function() x
  setinverse <- function(inverse) m<<-solve(inverse)
  getinverse <- function() m
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special matrix created
## by the function makeCacheMatrix(). If the inverse of the matrix has already
## been calculated and it has not changed, then the function should retrieve the
## inverse from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if (!is.null(m))
      {
        message("Getting cached data")
        return(m)
      }
      
      data <- x$get()
      m <- solve(data)
      x$setinverse(m)
      m
}
