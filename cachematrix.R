## Caching the Inverse of a Matrix

## makeCacheMatrix returns a list containing methods. 
## It contains the following methods:
## setmatrix
## getmatrix
## setinverse
## getinverse

makeCacheMatrix <- function(m = matrix()) { 
   ## Initialize the inverse variable
   i <- NULL
   
   ## Method to set the matrix
   setmatrix <-function(matrix){
        m <<- matrix
        i <<- NULL
   }
   
   ## Method to set the matrix
   getmatrix <- function() {
        ## return the matrix  
        m
   }
    
    ## Method to set the inverse of the matrix
    setinverse <- function(inverse) {
        i <<- inverse
    }
    
    ## Method to get the inverse of the matrix
    getinverse <- function() {
        i
    }
    
    ## Return a list of the methods
    list(setmatrix = setmatrix, getmatrix = getmatrix,
        setinverse = setinverse,
        getinverse = getinverse)

}


## The following function Returns the inverse of matrix which is created with 
# makeCacheMatrix

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        
        ## Just return the inverse if its already set
        if(!is.null(m)){
           message("getting cached data")
           return(m)
        }
        
        ## Get the matrix from our object
        data <- x$getmatrix()
        
        ## Calculate the inverse using matrix multiplication
        m<-solve(data) %% data
        
        ## Set the inverse to the object
        x$setinverse(m)
        
        ## Return the matrix
        m
}
