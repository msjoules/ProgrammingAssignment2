# The two functions below are used to get a matrix and cache its inverse.
# This prevents having to recompute the inverse which may be time and processing
#   intensive.

# Function 1: List of functions to get & set a passed matrix and to get & set
#  its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) m <<- Inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# Function 2: Used to first determine whether the matrix inverse is in cache by
#   referencing the getInverse() function.
# If so, it simply returns the cached inverse.  
# If not, it will calculate the inverse, cache the inverse by passing it to 
#   the setInverse() function then return the inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message('\nData has been cached. Retrieving cached data...\n')
    return(m)
  }
  data <- x$get()
  message('\nCalculating inverse...\n')
  m <- solve(data, ...)
  x$setInverse(m)
  m
}

# To test the functions, uncomment and run the following lines below:

# a_matrix <- matrix(rnorm(25), 5, 5)
# test_cache <- makeCacheMatrix(a_matrix)
# cacheSolve(test_cache)
# cacheSolve(test_cache)