## The two functions calculate the inverse of a matrix and cache it.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        s<-NULL
        set<-function(y){
                x<<-y
                s<<-NULL
        }
        get<-function()x
        setsolve<-function(solve) s<<-solve
        getsolve<-function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve
        )
        
}


## This function calculates the inverse of the special "matrix" created with the above function. 

cacheSolve <- function(x, ...) {
        s<-x$getsolve()
        if(!is.null(s)){
                message("getting cached data")
                return(s)
        }
        data<-x$get()
        s<-solve(data, ...)
        x$setsolve(s)
        s
        ## Return a matrix that is the inverse of 'x'
}
