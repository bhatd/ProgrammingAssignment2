## Sample run script
## mat <- matrix((3,4,1,2),2,2)  
## p<-makeCacheMatrix(mat)
## cacheSolve(p)  # should NOT print message "Getting cached data"
## cacheSolve(p)  # should print message "Getting cached data"

## This function creates a special matrix object that can cache it's inverse.

makeCacheMatrix <- function(x = matrix()) {

  
  m <- NULL
  
  # This function initializes the matrix and clears out cache
  set <- function(y)
  {
    x <<-y
    m<<-null
  }
  
  # This function gets the matrix
  get <- function() x
  
  # This function calculates the inverse and stores in cache
  setinverse <- function(inverse) m<<-solve(inverse)

  # This function gets the inverse matrix from cache
  getinverse <- function() m
  
  # return values
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special matrix created
## by the function makeCacheMatrix(). If the inverse of the matrix has already
## been calculated and it has not changed, then the function should retrieve the
## inverse from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
      # Try to get the inverse of the matrix from cache
      m <- x$getinverse()
      if (!is.null(m))
      {
        message("Getting cached data")
        return(m)
      }
      
      # if it odes not exist, then calculate inverse and put in cache
      data <- x$get()
      m <- solve(data)
      x$setinverse(m)
      m
}
