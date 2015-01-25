## These functions allow to cache the inverse of a matrix (calculated by solve()), makeCacheMatrix cache the result of a matrix inverstion
# and cacheSolve return the inverse of a matrix. If the matrix was previously inverted than the cached version is returned saving computation time


#NOTE ON THE CURRENT  SUBMISSION: 
#the current submission follows the same pattern of the example given in assignement 2 (makeVector and cachemean) and seems to work
#HOWEVER: one know issue with it is that if the matrix change I need to find a way to remove the cached value so that a new one can be calculated . This is work in progress. 


## Write a short comment describing this function

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


