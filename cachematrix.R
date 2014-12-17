## These functions create a matrix, calculate the inverse and store the results in memory. 
## When a new matrix is input, the memory is checked for the result having already been calculated
## If the result is found in memory it resturns that result else it calculates the inverse and returns the result.

## The makeCacheMatrix function below creates a matrix, calculates the inverse of the matrix and stores the matrix and it's inverse in memory

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
      x<<-y
      m<<-NULL
    }
    get<-function() x
    setmat<-function(solve) m<<- solve(x)
    getmat<-function() m
    list(set=set, get=get,
         setmat=setmat,
         getmat=getmat)  
}


## The cacheSolve Function below checks the memory to see if the inverse of the matrix has already been calculated
## Returns the result from memory if it has (and the matrix hasn't changed)
## Calculates the inverse if not stored in memory
## Returns the result

cacheSolve <- function(x, ...) {
    m<-x$getmat()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmat(m)
    m
}
