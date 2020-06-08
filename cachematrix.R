## Put comments here that give an overall description of what your
## functions do

 
## This function create a special matrix object to cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    i<- NULL
    set<- function(y){
        x<<-y
        i<<-NULL
    }
    get<- function() x
    setinverse<- function(inverse) i<<-inverse
    getinverse<- function() i
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function takes the special matrix returned by "makeCacheMatrix" and calculate its inverse
## If the inverse has already been existed for the same matrix then it return m=inverse from the cache
cacheSolve <- function(x, ...) {
    i<- x$getinverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data<- x$get()
    i<- solve(data, ...)
    x$setinverse(i)
    i          ## Return a matrix that is the inverse of 'x'
}
