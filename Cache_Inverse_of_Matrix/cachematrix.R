makeCacheMatrix <- function(x = matrix()) {
    
    ## This function will creates a special matrix, which really a list containing a function to:
    ## set the value of the matrix
    ## get the value of the matrix
    ## set the value of the inverse of the matrix
    ## get the value of the inverse of the matrix
    
    
    inverse <- NULL 	
    setMatrix <- function(y) 
    {
        x <<- y 					
        inverse <<- NULL
    }
    
    getMatrix <- function() x 				
    setInverse <- function(solve) 
        inverse <<- solve  
    
    getInverse <- function() inverse 		
    
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
    
    
    ## This function calculate the inverse of the special "matrix" created with the above function.
    ## However, it first checks to see if the inverse has already been calculated. 
    ## If so, it gets the inverse from the cache and skips the computation. 
    ## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the 'setInverse' function.
    
    
    inverse <- x$getInverse()
    if(!is.null(inverse)) 
    {					
        message("getting cached data")			
        return(inverse)
    }
    data <- x$getMatrix()				
    inverse <- solve(data, ...)			
    x$setInverse(inverse)					
    inverse 						
}