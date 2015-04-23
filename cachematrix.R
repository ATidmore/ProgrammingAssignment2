## Here I'm creating two functions to calculate the inverse of a matrix and cache 
##      the results so that it can be referrenced later without recalculating the inverse.

##First we will create a special vector (list) which will set the value 
##      of the vector, get the value, set the value of the inverse, then 
##      get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv = NULL
        set = function(y) {
                x <<- y
                m <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Second, this function actually computes the inverse. If it has already been calculated
##      and the matrix hasn't changed, the cached results will be returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv = x$getinv()
        
        if (!is.null(inv)) {
                
                message("getting cached data")
                return(inv)
        }
        
        mat.data = x$get()
        inv = solve(mat.data,...)
        
        x$setinv(inv)
        
        return(inv)
}

