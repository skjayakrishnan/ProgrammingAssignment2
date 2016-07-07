## These function set a matrix and return the inverse of the matrix

## Makecachmatrix function creates a matrix provided as argument
## it sets or gets the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set = function (y) {
    x <<- y
    inverse = NULL
  }
  get = function() x
  setinverse = function(inv) inverse <<- inv
  getinverse = function () inverse
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## The function returns the inverse of the matrix. If the inverse is 
## in the catche it is returned.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ginv <- x$getinv()
  if(!is.null(ginv)) return(ginv)
  mat<-x$get()
  ginv<-solve(mat)
  x$setinv(ginv)
  ginv
}

