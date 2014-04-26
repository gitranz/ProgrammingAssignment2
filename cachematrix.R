## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        # set a potentially previous stored inverse matrix in "cached.inv" to
        # NULL The matrix has changed and therefore a new calculation of
        # cacheSolve has to be done.
        cached.inv <- NULL     
        
        # The set function which set/store a new matrix. If a new Matrix is set via
        # this function, a potentially previous stored inverse matrix is set
        # to NULL. All assignements are done with the superassignment operator
        # "<<-". This cause a search through all the parent environments for an
        # existing definition of x and cached.inv.
        set <- function(y) {
                x <<- y
                cached.inv <<- NULL
        }
        
        # An accessor function to get the matrix
        get <- function() x
        
        # set a caluclated inverse matrix "solve" to the cached variable
        # "cached.inv" with the superassignment operator "<<-"
        setsolve <- function(solve) cached.inv <<- solve
        
        # get the cached inverse matrix with this accessor function
        getsolve <- function() cached.inv
        
        # returns the defined four functions in a list.
        list(set = set, 
             get = get, 
             setsolve = setsolve,
             getsolve = getsolve)        
}

##  This function computes the inverse of the special "matrix" returned by
##  makeCacheMatrix above. If the inverse has already been calculated (and the
##  matrix has not changed), then the cachesolve should retrieve the inverse
##  from the cache.

cacheSolve <- function(x, ...) {
        
        # load a potentially previous stored inverse matrix in cached.inv with 
        # the getsolve funktion of makeCacheMatrix
        cached.inv <- x$getsolve()
        
        # if there is a previously calculated inverse matrix (not equal NULL)
        # respectively an existing value in cached.inv (either stored with the
        # set-function of makeCacheMatrix or a new invokement of
        # makeCacheMatrix) the cached inverse matrix is returned with a message
        if(!is.null(cached.inv)) {
                message("getting the cached inverse matrix")
                return(cached.inv)
        }
        
        # No cached value was found ! So we have to calcualte the inverse.
        # Retrieve the matrix with the accessor function get() of
        # makeCacheMatrix and put it in the variable data
        data <- x$get()
        
        # Calculate the inverse matrix of this matrix and store it in calc.inv
        # ! no check if the matrix is not solveable !
        calc.inv <- solve(data)
        
        # set the calculated inverse matrix with the setsolve function
        # and cache it.
        x$setsolve(calc.inv)
        
        # return the calculated inverse matrix.
        calc.inv 
}
