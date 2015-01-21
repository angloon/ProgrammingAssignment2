## Assignment 2 - Caching inverse of matrix


## Allow to set, get, setinverse, getinverse a matrix
## eg matrix1 <- makeCacheMatrix(c(1,2,3,4)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Get the inverse of a matrix - check cache if the inverse has already been
## calculated and retrieve from cache if found in cache. If not, calculate
## matrix inverse and store copy in cache

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
