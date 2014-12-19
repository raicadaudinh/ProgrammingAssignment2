##  These are a pair of functions that cache the inverse of a matrix

## The First Function
## This function aims to create a special "matrix" object that can cache inverse

makeCacheMatrix <- function(x = matrix()) {       ## input is a matrix
  
  inverse <- NULL                                 ## our inverse matrix
  
  set <- function(y) {                            ## this step lets us assign 
    x <<- y                                       ## a new value to our object 
    inverse <<- NULL
  }
  
  get <- function() x                             ## return the original value
  
  setInverse <- function(solve) inverse <<- solve ## this function will store
                                                  ## the value of inverse matrix
                                                  ## in global environment by (<<-) super-assignement
  
  getInverse <- function() inverse                ## used by cashSolve to return the inverse matrix
  list(set = set, get = get,                      ## list of method functions
       setInverse = setInverse,
       getInverse = getInverse)
}


## Second Function
## This function will return the inverse matrix and will cache this value
## Next time, we do not need to re-calculate the inverse matrix so we can save memory

cacheSolve <- function(x, ...) {                  ## return the inverse matrix
       
  inverse <- x$getInverse()                       ## we assign the getInverse value to inverse                     
  
  if(!is.null(inverse)) {                         ## this step verify if the inverse matrix is cached
    message("getting cached data")                ## if there is, send this message
    return(inverse)                               ## ther return the inverse matris
  }
  data <- x$get()                                 ## if not, then access the matrix
  inverse <- solve(data, ...)                     ## calculate the inverse
  x$setInverse(inverse)                           ## cache it
  inverse                                         ## return the inverse matrix
}
