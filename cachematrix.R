#"makeCacheMatrixcreates" function creates 
#a special "vector", which is really a list containing a function to
#set the value of the Matrix
#get the value of the Matrix
#set the value of the reverse the Matrix
#get the value of the reverse the Matrix

makeCacheMatrix <- function(x = matrix()) {
        # input x will be a matrix
        
        m <- NULL    #  m will be "inverse of matrix" and it's reset to NULL every 
        #    time makeCacheMatrix is called
        set <- function(y) {    # takes an input Matrix
                x <<- y         # saves the input Matrix 
                m <<- NULL      # resets the "inverse of matrix" to NULL
        }
        
        #   These next three functions are defined but not run when makeCacheMatrix is called.
        #   instead, they will be used by cacheSolve() to get values for x or for
        #   m (matrix) and for setting the reverse of matrix. 
        
        get <- function() { x }   # this function returns the value of the original Matrix
        
        setmatrix <- function(matrix)  { m <<- matrix }
        # this is called by cacheSolve() during the first cacheSolve()
        #  access and it will store the value using superassignment
        
        getmatrix <- function() { m } # this will return the cached value to cacheSolve() on
                                      #  subsequent accesses
        
        list(get = get,              #  This is accessed each time makeCacheMatrix() is called,       
             setmatrix = setmatrix,  #   that is, each time we make a new object.  This is a list of 
             getmatrix = getmatrix)  #   the internal functions ('methods') so a calling function
                                     #   knows how to access those methods.  

}


## "cacheSolve" function   computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated (and the
##matrix has not changed), then the cachesolve should retrieve the inverse from 
## the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")  # send this message to the console
                return(m)                       # and return the value 
        }
        data<-x$get()
        m<-solve(data)     # calculate the inverse of the matrix
        x$setmatrix(m)
        m
}
