

## input x is used as a matrix
## calculate inverse matrix
## set the value "s" as null

library(MASS)
makeCacheMatrix <- function(x = matrix()){
  iv <- NULL
  set <- function(y){
          x <<- y
          iv <<- NULL
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
  iv <- x$getinv()
  if(!is.null(iv)){
                message("getting cached data")     ## checked the inverse if it is null
                return(iv)                        ## the function to return the inverse value
  }
        mat <- x$get()
        iv <-solve(mat, ...)                     ## solve the inverse value
        x$setinv(iv)
        iv
}
