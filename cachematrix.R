#################################################################################################
##
##  The purpose of this pair of functions is to cache the inverse of a matrix
##
#################################################################################################

##===============================================================================================
## This function creates a special "matrix" object that holds the original and inverse matrices 
## and can be fed to the cacheSolve function to calculate and cache the inverse matrix
##===============================================================================================
makeCacheMatrix <- function( x = matrix() ) {
   xinv <- NULL
   set <- function( y ) {
      x <<- y
      xinv <<- NULL
   }
   get <- function() x
   setinv <- function( z ) xinv <<- z
   getinv <- function() xinv
   list( set = set, get = get, setinv = setinv, getinv = getinv )  
}

##===============================================================================================
## This function computes the inverse of the special "matrix" object returned by makeCacheMatrix 
## function above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.
##===============================================================================================
cacheSolve <- function( x, ... ) {
   m <- x$getinv()
   if( !is.null( m ) ) {
      message( "Getting cached data..." )
      return( m )
   }
   message( "Calculating inverse matrix..." )
   data <- x$get()
   m <- solve( data, ... )
   x$setinv( m )
   m
}
