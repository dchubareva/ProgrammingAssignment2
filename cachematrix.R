## I created two functions that make inverted matrix without re-counting the same things (with cache option).
## It works in 2 main steps: 
##   - create a special object from your matrix (or redefine with set method)
##     > myMatrix <- makeCacheMatrix(m), where m is your matrix
##   - calculate inverted matrix and use cached results as much as you want
##     > cacheSolve(myMatrix)


## function creates a special object from matrix, which has additional functions to set or get inverted matrix (IM), and to set/get value of your "real" matrix
 
makeCacheMatrix<- function(x = matrix()) {
  invX <- NULL
  set <- function(y) {
    x <<- y
    invX <<- NULL
  }
  get <- function() x
  setinvert <- function(givenMr) invX <<- givenMr
  getinvert <- function() invX
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)
  
}

## function checks whether IM is already calculated or not. If it is calculated, function returns IM and makes text-comment about it. If not, it calculates IM and stores it in makeCacheMatrix-object

cacheSolve <- function(x, ...) {
  inv <- x$getinvert()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinvert(inv)
  inv
}