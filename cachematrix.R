## This is a simple script for calculting inverse of matrixes. It implements a special
## datatype to store the matrix itself and cache its inversed matrix.
## It also optimizes the calculation of inverses by using the cached information when it's possible

## Implements a special datatype to store a matrix and its inverse.
## Additionally it provides methods for setting and getting the value of the stored matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Get the inversed matrix from a matrix x that was created using the makeCacheMatrix function
cacheSolve <- function(x, ...) {
    inv <- x$getinverse();
    if ( !is.null(inv) ) {
      message("got the inverse from cache :)");
      return(inv)
    }
  
    message("Hold on... I'm just doing some calculations over here");
    
    info <- x$get()
    
    inv <- solve(info)
    x$setinverse(inv)
    
    message("Done! And also cached for future.");
    inv
}
