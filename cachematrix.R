## Function makeCacheMatrix generates a matrix environment to set and get the matrix
## and caching it, as well as setting and getting its inverse
## Function cacheSolve proofs if there is already an inverse in cache. If yes, it
## is returned, if not, it is calculated

## makeCacheMatrix: set, get the matrix and set, get its inverse and caches it
makeCacheMatrix <- function(X = matrix()) {
  m <- NULL
  set <- function(x) {
    X <<- x
    m <<- NULL
  }
  get <- function() X
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(X, ...) {
  m <- X$getInverse()
  if(!is.null(m)) {
    message("getting cached inverse matrix")
    return(m)
  }
  data <- X$get()
  m <- solve(data, ...)
  X$setInverse(m)
  m
}
