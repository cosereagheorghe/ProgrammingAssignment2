## Description :
## Create a "cache matrix" ie. matrix that keeps its computed inverse 
## so that further usage of its invers dont require any computation.
##
## The "cache matrix" is just a list with the set/get methods for fields
## we cache; the cached filds are in the parent envirionment where the
## methods are defined ie. the environment of this constructor
##
makeCacheMatrix <- function(x = matrix())
{
    ## assert x is a sqare matrix
    stopifnot(is.matrix(x))
    stopifnot(nrow(x) == ncol(x))
    
    mat <- x;
    inv <- NULL;
    
    set <- function(x) {
        mat <<- x
        inv <<- NULL
        return(mat)
    }
    get <- function() { return(mat)}
    get.inv <- function() { return(inv) }
    set.inv <- function(x) { inv <<- x; return(x) }
    
    return(list(set=set, get=get, set.inv=set.inv, get.inv=get.inv))
}


## Description:
## Return the cached inverse of x;
##
## Obs: if matrix is not invertible the result is a NA-matrix
##
cacheSolve <- function(x, ...)
{
    mat <- x$get();
    inv <- x$get.inv()

    if (is.null(inv)) {
        tryCatch(inv <- solve(mat),
                 error = function(err) {
                         inv <<- matrix(nrow = ncol(mat), ncol = nrow(mat))
                         warning("matrix is not invertible")
                         return(err);
                 })
        x$set.inv(inv)
    }
    
    return(inv)
}
