## This program will cahce the inverse of a matrix, then search the cache to determine whether the inverse
## has already been calculated

## The function creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function()x
    setsolve<-function(solve)m<<-solve
    getsolve<-function() m
    list(set=set,get=get,
         setsolve=setsolve,
         getsolve=getsolve)
}


## The function below will search the cache and return the result if the inverse has already been calculated
## If the matrix inverse has not been cached, the function returns the inverse of the matrix

cacheSolve <- function(x, ...) {
    m<-x$getsolve()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data<-x$get()
    m<-solve(data,...)
    x$setsolve(m)
    m
}
        ## Return a matrix that is the inverse of 'x'
