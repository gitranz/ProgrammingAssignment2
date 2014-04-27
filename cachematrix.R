###############################################################################
# Author:       Christian Rothmann
# forked from:  https://github.com/rdpeng/ProgrammingAssignment2 
#
#
# makeCacheMatrix: 
# This function creates a special "matrix" object that can cache its inverse 
# matrix with the use of the superassignment operator "<<-". When invoked,
# "makeCacheMatrix" defines and return a list of four accessor functions: 
# - set()
# - get()
# - setsolve()
# - getsolve()
# which can set and get the matrix and set and get 
# a calculated and cached inverse matrix. The cached value is stored in
# "cached.inv" until a new invokement of "makeCacheMatrix" or the call of the
# set function (store a new matrix).
#
# cacheSolve:
# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated and stored
# in "cached.inv", cacheSolve should retrieve the inverse from this cache
# variable. If the matrix has changed, either with the set-function of
# makeCacheMatrix or a new invokement of makeCacheMatrix, the cacheSolve
# function is calculating the inverse matrix with the solve function and use the
# setsolve() function of makeCacheMatrix to store the value in the cache
# variable "cached.inv".
#
#
# Examples / Usage:
#
# mym1 <- matrix(c(0,1,3,4,2,-1,1,-1,7), 3, 3)  # create my matrix 1
# mym2 <- matrix(c(3,2,1,5,4,2,1,5,2), 3, 3)    # create my matrix 2
#
# mycm <- makeCacheMatrix(mym1) # create special matrix of mym1
#
# cacheSolve(mycm)              # calculate the inverse matrix of mym1
#
# cacheSolve(mycm)              # get the inverse matrix of nym1, this 2nd time
#                               # it will read the result out of the 
#                               # cached variable
#
# mycm$set(mym2)                # set a new matrix with the setter function and
#                               # clear the cached inverse matrix
#
# mycm$getsolve()               # proof: the cached inverse matrix is NULL
#
# cacheSolve(mycm)              # calculate the inverse of the new matrix (nym2)
#                               # and stores it in the cached inverse variable
# 
###############################################################################

## This function creates a special "matrix" object that can cache its
## inverse. When invoked, "makeCacheMatrix" defines and returns four functions
## in a list.

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