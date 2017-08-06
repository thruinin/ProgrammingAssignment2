## Put comments here that give an overall description of what your
## functions do

## Returns a list of functions that can perform operations to an R object

makeCacheMatrix <- function(x = matrix()) 
  {
  inv = NULL
  
  set = function(y) {
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment. 
    x <<- y
    inv <<- NULL   }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  
  }



## Computes the inverse of matrix given @MakecacheMatrix, 
## but first it checks if result is already stored in variable inv, if so it returns cached value

cacheSolve <- function(x, ...)
  {
  inv = x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)  }
  
  # otherwise, calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  inv
  
  }
