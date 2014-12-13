## define function to return list of functions used to modify and return value of input
## matrix as well as to set and return the value to of the cached computed matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  ## initialize cached matrix inverse to null
  s <- NULL
  
  ## define function to modify input matrix and reinitialize cached matrix inverse
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  ## define function to return input matrix
  get <- function() x
  
  ## define function to assign computed inverse matrix to cache variable
  setsolve <- function(solve) s <<- solve
  
  ## define function to return cached matrix inverse
  getsolve <- function() s
  
  ## return list of defined functions
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## define function to return value of calculated matrix inverse, either from a
## previously cached calculation, or in the case of a new input matrix, from a
## fresh computation of the matrix inverse

cacheSolve <- function(x, ...) {
  
  ## retrieve value of cached matrix inverse
  s <- x$getsolve()
  
  ## determine if matrix inverse value had bee previously computed
  ## if so, return cached value of matrix inverse
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  
  ## retreive input matrix
  data <- x$get()
  
  ## compute inverse of input matrix
  s <- solve(data, ...)
  
  ## assign computed value of matrix inverse to cache variable
  x$setsolve(s)
  
  ## print value of matrix inverse
  s
}
