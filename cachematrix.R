## These functions allow to cache the inverse of a matrix (calculated by solve()), makeCacheMatrix cache the result of a matrix inverstion
# and cacheSolve return the inverse of a matrix. If the matrix was previously inverted than the cached version is returned saving computation time

# the code assume that the matrix will only be changed through using the "set" function
#example to run:
# x <- matrix(rnorm(16),4,4)
# cachedx<-makeCacheMatrix(x)
# cacheSolve(cachedx)
# ....show inverse of X
# cacheSolve(cachedx)
# ....show CACHED inverse of X
# x[1,1] <- x[1,1]/2   #we change x

#Now must set again (because x changed)
#cachedx$set(x)
#cacheSolve(cachedx)
#...NEW inverse calculated



makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
  }


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}


