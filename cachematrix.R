## The makeCacheMatrix and the cacheSolve function work in together to allow the inverse of a 2X2  
## matrix to be stored for later use

 ## This Function sets the methods required to store and retrieve a 2X2 matrix and its inverse


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL  
        set <- function(y) {   
                x <<- y    
                m <<- NULL  
        }  
        get <- function() x  
        setInverse <- function(solve) m <<- solve  
        getInverse <- function() m  
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function allows the inverse of a 2 X 2 matrix to be calculated and stored

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setInverse(m)
        m
        
}
