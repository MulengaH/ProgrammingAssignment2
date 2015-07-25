## Overall Description  -These functions generate variables, which are defined as either 
## functions, nested  functions or as cached variables and utilize these functions to 
##  move data in and out of the cache environment to perform actions on these data.

## Function makeCacheMatrix - This first function, creates a matrix, which is a list 
## containing a function to
## set the matrix
## get the matrix
## set the matrix inverse
## get the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
	matinverse <- NULL      	## initialize local matinverse to null so we can know when there is data in it
	set <- function(y) {     	## create set function to store matrix passed in the call
		x <<- y			## assign initial matrix from command line into cache
            matinverse <<- NULL	## initialize global matinverse to null so we can know when there is data in it
        }
        get <- function() x		## create get function to return matrix passed in the command line
        setinverse <- function(inverse) matinverse <<- inverse  	## set matrix inverse in cache 
        getinverse <- function() matinverse				## get matrix inverse from cache 
        list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}


## Function cacheSolve -This function, receives a matrix variable expected to have
##  been defined as makeCacheMatrix() and returns the inverted form of the matrix.
##  When  called, this function checks for the existence of the inverted matrix in the cache and if cacheSolve
## finds a non null value (inverted matrix) it returns it otherwise it computes it using the solve function

cacheSolve <- function(x, ...) {
      matinverse <- x$getinverse() 	## assign the value we get from the cache environment to matinverse
      if(!is.null(matinverse)) {	## check to see if the value of matinverse is null
          	message("getting cached data")
         	return(matinverse)	## if matinverse is not null, a massage "getting cached data" followed by the value of matinverse is returned
      }
   	data <- x$get() 			## call the function "get" from makeCacheMatrix and get the un-inverted matrix
      matinverse <- solve(data, ...)	## invert the obtained matrix and pass the result to matinverse
      x$setinverse(matinverse)	## call function "setinverse" to set matinverse in the cache environment 
      matinverse				## return or dispaly the value of matinverse	
}
