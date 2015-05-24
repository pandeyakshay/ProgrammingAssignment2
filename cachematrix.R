## The below two functions cache the inverse of a matrix

## The makeCacheMatrix creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) 
{
        inv = NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, 
             setinv=setinv, 
             getinv=getinv)
}


## The cacheSolve computes the inverse of the matrix that is set
##by makeCacheMatrix. If the inverse was calculated previously
##and the matrix hasnt changes, it will retrieve the inverse from
##the cache rather than calculating it again.

cacheSolve <- function(x, ...) 
{
       inv = x$getinv()

        if (!is.null(inv))
        {
                message("getting cached data")
                return(inv)
        }
        
        mat.data = x$get()
        inv = solve(mat.data, ...)
        x$setinv(inv)
        
        inv
}
