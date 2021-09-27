## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## input x is used as a matrix
## calculate inverse matrix
## set the value "s" as null

library(MASS)
makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
          x <<- y
          inv <<- NULL
                      }
  get <- function() {x}                    ## the function used to get the x matrix
  setinv<-function(inverse)inv<<-inverse
  getinv<-function(){
                      inver<-ginv(x)
                      inver%*%x            ## the function used to get the matrix inverse
  }
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}

## this is used to obtain cached data

cacheSolve <- function(x, ...){
  inv <- x$getinv()
  if(!is.null(inv)){
                message("getting cached data")     ## checked the inverse if it is null
                return(inv)                        ## the function to return the inverse value
  }
        mat <- x$get()
        inv <-solve(mat, ...)                     ## solve the inverse value
        x$setinv(inv)
        inv
}
