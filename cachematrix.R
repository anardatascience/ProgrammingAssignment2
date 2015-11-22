
## makeCacheMatrix():  This function creates a special "matrix" object that can cache its inverse.

## cacheSolve() : This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache
##Assumption that the matrix supplied is always invertible

makeCacheMatrix <- function(dtset = matrix()) {
        ## @dtset: a square invertible matrix
        ## return: a list containing functions to
        ##              1. set the matrix
        ##              2. get the matrix
        ##              3. set the inverse
        ##              4. get the inverse
        ##         this list is used as the input to cacheSolve()
        
        invrse <- NULL
        set <- function(etset) {
                # use <<- operator which can be used to assign a value to an object in an environment that is 
                # different from the current environment
                dtset <<- etset
                invrse <<- NULL
        }
        get <- function() dtset 
        setinvr <- function(solve) invrse <<- solve
        getinvr <- function() invrse
        list(set=set, get=get, setinvr=setinvr, getinvr=getinvr)
                
        
}


cacheSolve <- function(s) {
        ## s: output of makeCacheMatrix()
        ## return: inverse of the original matrix that is input to makeCacheMatrix()
        
        invrse <- s$getinvr()
        
        # if the inverse has already been calculated
        if (!is.null(invrse)){
                # Exists in the cache. So get it from the cache 
                # and skip the computation of the inverse matrix 
                message("Getting cached data")
                return(invrse)
        }
        
        # Since it does not exist in the cache, calculate the inverse
        # using the solve() function. 
        #Assumption is that the matrix supplied is always invertible
        
        inv.data <- s$get()
        
        invrse <- solve(inv.data)
        
        # sets the value of the inverse in the cache via the setinv function.
        s$setinvr(invrse)
        
        return(invrse)
}
