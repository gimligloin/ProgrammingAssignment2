## Put comments here that give an overall description of what your
## functions do
  
## makeCacheMatrix generates a list of functions when a matrix is inputed which can then be used to determine inverse from cache if it exists

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i<--inv
  getinv <- function() i
  list(set=set,get=get,setinv=setinv,getinv=getinv)
  
}


## cacheSolve takes input of makeCacheMatrix and returns inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  mat <- x$get()
  i <- solve(mat)
  x$setinv(i)
  i
}

