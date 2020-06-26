## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function will create a custom built matrix,
## which may also hold the value of it's inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
  i<-NULL ## initialize a null inverse matrix
  ## setter function to set value of the matrix
  set<-function(m){
    x<<-m
    i<<-NULL
  }
  get<-function(){x} ## getter function to get value of the matrix
  setInverse<-function(mat){i<<-mat} ## setter function for setting the value of inverse of the matrix
  getInverse<-function(){i} ## getter function to get the inverse of the matrix
  ## list which contains the functions, both inner and outer names have been made same
  ## for convenience
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Write a short comment describing this function
## This function provides cache functionality on applying the solve function over
## a matrix created by above function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i<-x$getInverse() ## get the pre-calculated inverse
  if(is.null(i)){ ## TRUE if the cache is empty
    i<-solve(x$get()) ## calculate inverse by calling solve over x$get
    x$setInverse(i) ## setting the cache with calculated inverse
    i
  }
  else{ ## runs if cache was not empty
    message('Getting cached data...')
    i
  }
}
