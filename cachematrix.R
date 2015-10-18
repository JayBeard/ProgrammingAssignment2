## A pair of functions that cache the inverse of a matrix

## Note: assuming all matrices are always invertible 

## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  # cache variable
  m <- NULL
  # return closure with original matrix get accessor and a pair of read and write 
  # cache accessors
  list(get=function()x,set=function(y){ x<<-y; m<<-NULL; }, setinverse=function(inv)m<<-inv,getinverse=function()m)
  }


## computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the retrieve it from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getinverse()
  if(!is.null(m)){
    message("from cache")
    return(m)
  }
  # building solve second argument assuming 'x' is a square matrix 
  m<-solve(x$get(),diag(nrow(x$get())),...)
  x$setinverse(m)
  m
  }
