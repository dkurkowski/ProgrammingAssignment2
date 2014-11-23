# Requirement states: This function creates a special "matrix" object that can 
# cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  NewInv <- NULL
  SetNew <- function(y) {
## Introduce new concept called super assignment
    x <<- y
    NewInv <<- NULL
  }
  get <- function() x
  SetInv <- function(inverse) NewInv <<- inverse
  GetInv <- function() NewInv
  list(SetNew = SetNew, get = get, SetInv = SetInv, GetInv = GetInv)
}

# Requirement states: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then cacheSolve should retrieve the inverse 
# from the cache.

cacheSolve <- function(x, ...) {
  NewInv <- x$GetInv()
##Returns the inverse if it has been calculated along with a user defined message
  if (!is.null(NewInv)) {
    message("Data cached as expected")
    return(NewInv)
  }
##Calculate the inverse. Done for the first run of our testing scenario
  data <- x$get()
  NewInv <- solve(data, ...)
  x$SetInv(NewInv)
  NewInv
}
