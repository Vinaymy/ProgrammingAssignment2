## Computing the inverse of a square matrix is an expensive operation for large matrices.
## It is highly desirable to compute the inverse only once and cache this value and return it directly
## for any requests to recompute the matrix inverse. The following two functions achieve this.

## This function creates a special "matrix" that allows setting and getting of a matrix and 
## setting and getting of the matrix inverse as well, allowing the caching of a computed matrix inverse.  
makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL
  set <- function(y) {
    x <<- y
    x_inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) x_inv <<- inverse
  getinverse <- function() x_inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This method assumes the matrix is invertible. It uses the solve() method to compute the inverse of the 
## matrix defined using the previous function, but only once. It uses the getinverse function of makeCacheMatrix
## to returned the cached value if it is available and thus avoids recomputation.

cacheSolve <- function(x, ...) {
  x_inv <- x$getinverse()
  if(!is.null(x_inv)) {
    message("getting cached data")
    return(x_inv)
  }
  data <- x$get()
  x_inv <- solve(data, ...)
  x$setinverse(x_inv)
  x_inv
}
