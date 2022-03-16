## There are set,get,setsolve, getsolve in this function 
##To calculate inverst for non squared as well as squard natrices, used labrary(MAss)
library(mass)
makeCacheMatrix <- function(x = matrix()) {
        #initializing inverst as null
        s <- NULL        
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x #fucntion to get Matrix x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        
}


##this function is to get tha cache data


cacheSolve <- function(x, ...) { #function to get cach data
        s <- x$getsolve()
        if(!is.null(s)) { #if inverst is Null
                message("getting inversed matrix")
                return(s) #return Iverst Value
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s ##return inverse of x
}
