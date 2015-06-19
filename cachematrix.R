makeCacheMatrix <- function(x = matrix()) {
  # Make function makeCacheMatrix following the pattern of makeVector
  # x is a matrix.  This function returns a LIST of functions, set matrix, 
  # get matrix, set inverse and get inverse
     mi <- NULL #mi is the matrix inverse-this was mean in example function
     set <- function(y) {
          x <<- y #saves the matrix (makes global) when the function goes away
          mi <<- NULL #  initialize inverse to null for test
     }
     get <- function() x  #lets cacheSolve get the matrix
     seti <- function(inv) mi <<- inv #lets cacheSolve set the inverse
     geti <- function() mi
     list(set=set, get=get, seti=seti, geti=geti)
}

cacheSolve <- function(x, ...) {
  # x is matrix 
  # cacheSolve returns inverse or stored value if already calculated
     mi <- x$geti()  # get stored inverse and check if NULL
     if (!is.null(mi)){  # if not NULL return old value 
          message("getting cached data")
          return(mi)
     }
     mdata <- x$get()   # else, get matrix and solve for inverse
     mi <- solve(mdata, ...)
     x$seti(mi)  #put inverse in cache
     mi
}

#TEST
# a<-matrix(c(1,2,3,4),nrow=2, ncol=2)
# mymat<-makeCacheMatrix(a)
# mymat$get()
# mymat$geti()
# myi<-cacheSolve(mymat)
# myi<-cacheSolve(mymat)
# myi
# solve(myi)
