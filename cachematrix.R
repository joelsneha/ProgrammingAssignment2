## Caching the Inverse of a Matrix
## Stores a matrix and caches its inverse

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
invA <- NULL
  set <- function(y) {
    x <<- y
    invA <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invA <<- inverse
  getInverse <- function() invA
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function creates a special "matrix" object that can cache its inverse makeCacheMatrix above
## If the inverse has already been calculated, then it should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         invA <- x$getInverse()
  if (!is.null(invA)) {
    message("getting cached data")
    return(invA)
  }
  mat <- x$get()
  invA <- solve(mat, ...)
  x$setInverse(invA)
  invA
}
