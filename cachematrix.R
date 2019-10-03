## The following functions can be used to store a particular matrix in
## cache and also compute its inverse and cache it as well for performance
## enhancement.

## The makeCacheMatrix function is nothing but a list of functions that
## help us to store, compute and retrieve a matrix and its inverse.

makeCacheMatrix <- function(x = matrix())
{
  matinv <- NULL
  setmat <- function(y) 
  {
    x <<- y
    matinv <<- NULL
  }
  getmat <- function() x
  setmatinv <- function(solve) matinv <<- solve
  getmatinv <- function() matinv  
  list(setmat = setmat, getmat = getmat,
       setmatinv = setmatinv,
       getmatinv = getmatinv)
}


## If the user has computed and cached the inverse of the matrix then the
## function cacheSolve is used to retrieve it. But if the user has not
## computed the inverse earlier, then this function computes, caches and 
## returns the inverse of the matrix.

cacheSolve <- function(x, ...) 
{
  m <- x$getmatinv()
  if(!is.null(m)) 
  {
    message("getting cached data")
    return(m)
  }
  data <- x$getmat()
  m <- solve(data, ...)
  x$setmatinv(m)
  m
}
