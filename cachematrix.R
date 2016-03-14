## Inversion of a matrix can be costly, and there may be
## good reasons to cache the inverse of a matrix instead
## of calculating itt repeatedly.  We will cache the
## inverse of a matrix and show it here.

## makeCacheMatrix creates a list containing a function to
## a. set value of matrix
## b. get value of matrix
## c. set value of matrix inverse
## d. get value of matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Inverse of matrix is returned. After checking if inverse
## has already been computed, it receives the result and
## skips over the calculation.  If the inverse has not been
## computed, it will calculate it and put the value in the
## cache using the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
