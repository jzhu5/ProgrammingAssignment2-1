## Matrix inversion is usually a time-consuming computation and it would be great
## if we could cache the inverse of a matrix rather than compute it repeatedly
## Following is the function would help us to achieve this goal

## This function creats a special "matrix" object that can cache its inverse if applicable

makeCacheMatrix <- function(x = matrix()) {
        
        ## determine whether the matrix is invertible or not
        if (det(x)==0){
                message("It is not an invertible matrix, no inverse exists")
        }
        else {message ("It is an invertible matrix")}
        
        I<- NULL
        set<-function(y){
                x<<-y
                I<<-NULL
                
        }
        get<-function()x
        setinverse<-function(solve) I<<-solve
        getinverse<-function() I
        list(set=set, get=get, 
             setinverse=setinverse,
             getinverse=getinverse)
    

}


## This function computes the inverse of the special "matrix" returned by function above
## error will occur if the matrix is not invertible

cacheSolve <- function(x, ...) {
        ## check if I has already been calculated, if true, return(m)
        I<-x$getinverse()
        if(!is.null(I)){
                message("getting cached data")
                return(I)
        }
        ## if not calculated,solve the matrix and store the data
        data<-x$get()
        I<-solve(data, ...)
        x$setinverse(I)
        I
}
