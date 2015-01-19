
## This function makes a version of a matrix suited for inversion by caching it.
 

makeCacheMatrix <- function(m = matrix()) 
{
        inverse <- NULL 
        
        set <- function(y) 
        {
                m <<- y
                inverse <<- NULL
        }

        get <- function()
        {
                m
        }
        
        setinverse <- function(inv) 
        {
                inverse <<- inv
        }
        

        getinverse <- function() 
        {
                inverse
        }
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)    
}


## This function returns the inverse of a cached matrix.
## If the input matrix is the same as before we do not recompute but return the cached 
## version. If it is not the same it computes the inverse.

cacheSolve <- function(m, ...) {
      
        inv <- m$getinverse()
        if(!is.null(inv)) 
        {
                message("getting cached data")
                return(inv)
        }
        
        data <- m$get()
        inv <- solve(data, ...)
        m$setinverse(inv)
        inv    
}
