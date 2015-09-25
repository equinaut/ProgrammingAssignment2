## Caches both a matrix and its inverse

## Caches a matrix 'x' and, when called, stores its inverse value
makeCacheMatrix <- function(x = matrix()) 
{
  minverse <- NULL
  setmatrix = function(y) 
  {
    x <<- y
    minverse <<- NULL
  }
  setinverse = function(z)
  {
    minverse <<- z
  }
  getmatrix = function() x
  getinverse = function() minverse
  
  list(setmatrix = setmatrix, setinverse = setinverse,
       getmatrix = getmatrix, getinverse = getinverse)
}


## Returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) 
{
  m <- x$getinverse()
  if(!is.null(m))
  {
    message("getting cashed data")
    return(m)
  }
  data <- x$getmatrix()
  minverse <- solve(data)
  x$setinverse(minverse)
  
  minverse
}
