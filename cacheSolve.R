## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(mtx, ...) {
  inverse <- mtx$getinv()
  if(!is.null(inverse)) {
    message("Getting cached data...")
    return(inverse)
  }
  data <- mtx$get()
  invserse <- solve(data, ...)
  mtx$setinv(inverse)
  return(inverse)
}