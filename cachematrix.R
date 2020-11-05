## The first function (named makeCacheMatrix) creates a list of functions
## that can be called depending on whether a computation of 
## the inverse of a square matrix has already been solved or not.
## The functions contained in the list does;
##    1. set the value of the matrix
##    2. get the value of the matrix
##    3. set the value of the inverse of the matrix
##    4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
              m <-NULL
              set <- function(y){
                      x<<-y
                      m<<-NULL
              }
              get <- function() x
              setsolve <- function(solve)
              m <<- solve
              getSolve <- function() m
              list(set=set,get=get,setsolve=setsolve
                   ,getsolve=getsolve)

}


## The following function calculates the inverse of a square matrix.
## It first checks to see if the inverse of the given matrix was already solved before.
## If so, it calls the value from the cache using the get function and skips the computation.
## If not, it calculates the inverse and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        m <- x$getSolve()
        if(!is.null(m)){
                messahe("getting cached data")
                return(m)            
        }
        data <- x$get()
        m <- solve(data)
        x$getSolve(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
