## The function calculates the inverse of a matrix. 
## The functions cache the inverse of a matrix for repeated use.
## Creates a list of function that set/get the matrix and set/get its inverse
makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setinv <- function(inverse)inv <<- inverse 
  getinv <- function()inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}
## Calculates the inverse of the special "matrix" created with the above function. Returns the inverse from cache if the inverse was calculated before.

cacheSolve <- function(x, ...){
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data!")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  inv
}
