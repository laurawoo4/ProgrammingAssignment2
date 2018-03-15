## Matrix inversion is usually a costly computation and there may be some benefit to caching
##the inverse of a matrix rather than compute it repeatedly.

## function makeCacheMatrix and cacheSolve is to cache the inverse of a matrix 

## makeCachematrix is to create a "matrix", which is really a list containing a function to 
##1.set the value of the matrix
##2.get the value of the matrix
##3.set the value of the inverse of the matrix
##4.get the value of the inverse of the matrix

makeCacheMatrix <- function(x = numeric()) {
        i<-NULL
        set<-function(y){
                x<<-y
                i<<-NULL
        }
        get<-function()x
        setinverse<-function(inverse) i<<-inverse
        getinverse<-function()i
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


## Function cacheSolve calculates the inverse of the matrix,which was returned by 
## makeCacheMatrix above. If the inverse has already been calculated and the matrix has not
## changed, then the cacheSolve function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        i<-x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        ##Computing the inverse of a square matrix, use solve(x)function returns its inverse
        data<-x$get()
        i<-solve(data)
        x$setinverse(i)
        i
        ## Return a matrix that is the inverse of 'x'
}
