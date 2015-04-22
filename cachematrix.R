##Attributions: The logic and structure for these functions
##are heavily based on the makeVector() and cacheMean()
##functions provided for us by RDPeng.


##These two functions allow for the creation of a matrix
##such that the inverse is solved once and stored, with
##all following calls for the inverse returning the stored
##inverse until the matrix itself changes.

##makeCacheMatrix(x=matrix()) returns a list
##of functions that operate on a matrix, allowing
##it and its inverse to be set and cached.

makeCacheMatrix <- function(x= matrix()){
    i<-NULL
    ##sets the matrix (x) to value y and the
    ##stored inverse (i) to NULL
    make <- function(y){
        x <<- y
        i <<- NULL
    }
    ##returns the matrix (x)
    retrieve <- function() x
    ##stores the given inverse (inv) in variable i
    setInv <- function(inv) i <<- inv
    ##returns the stored inverse (i)
    retrieveInv <- function() i
    list(make=make, retrieve=retrieve, setInv=setInv,
        retrieveInv=retrieveInv)
}

##cacheSolve(x,...) retrieves the inverse, then sets
##it if it is still NULL. Returns the inverse.
##Assumes the matrix is square and invertible.
cacheSolve <- function(x, ...){
    inverse <- x$retrieveInv()
    ##sets the inverse w/solve() if inverse is NULL
    if (is.null(inverse)){
        mtx <- x$retrieve()
        inverse <- x$setInv(solve(mtx))
    }
    ##redundant code to make clear I'm returning inverse
    inverse
}
