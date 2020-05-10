## The makeCacheMatrix and cacheSolve functions are a pair of functions that work together
## to store the inverse of a matrix, in order to minimize time spent in calculations.

## The makeCacheMatrix function creates an object that contains 4 methods: set, get, 
## setinverse and getinverse. The set() method assigns a matrix to the object. The get() method
## retrieves the matrix from the object. The setinverse() method manually assigns an inverse 
## of the matrix. The getinverse() method retrieves the inverse currently stored in the object.
## Note that this function does not calculate the inverse. It only stores the inverse using  
## the setinverse() method. The cacheSolve function is used to calculate the inverse. This 
## that without the cacheSolve function, the makeCacheMatrix function is not of much use.

makeCacheMatrix <- function(x = numeric()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function takes as input an object of the type makeCacheMatrix (from the 
## above). It first checks whether the object already has an inverse stored in it, using the 
## getinverse() method. If it finds that there is an inverse stored in the object, it  
## returns that stored inverse without spending time calculating the inverse again. On the 
## other hand, if it does not find the inverse stored in the object (NULL), it calculates 
## the inverse using the solve() function. It then stores the inverse it has just calculated 
## in the makeCacheMatrix object, using the setinverse() method.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
