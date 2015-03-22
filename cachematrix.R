## We create two function in order to cache the inverse of a matrix, thus avoiding constantly computing it.

## This function sets and gets the value of the matrix and inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
      x <<- y
      inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function runs when the value of the inverse of the matrix is already cached.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("return cached value.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
        
}

## Example:
## > x = rbind(c(2, -2/6), c(-2/6, 2))
## > m = makeCacheMatrix(x)
## > m$get() 
##       [,1]       [,2]
## [1,]  2.0000000 -0.3333333
## [2,] -0.3333333  2.0000000

## First try without cached value
## > cacheSolve(m)
##      [,1]       [,2]
## [1,] 0.51428571 0.08571429
## [2,] 0.08571429 0.51428571

## Second try return cached value
## > cacheSolve(m)
##      [,1]       [,2]
## [1,] 0.51428571 0.08571429
## [2,] 0.08571429 0.51428571

