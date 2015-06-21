## English
## The first function, makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

## Spanish
## Esta funcion crea un objeto especial "matriz" que se puede almacenar en cache su inversa.
## coloca el valor de la matriz
## obtiene el valor de la matriz
## Coloca el valor de la inversa
## obtiene el valor de la inversa

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## English
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

## Spanish
## Esta funcion calcula la inversa de la "matriz" especial devuelto anteriormente por makeCacheMatrix. 
## Si la inversa ya se ha calculado (y la matriz no ha cambiado), 
## entonces el cachesolve debe recuperar el inverso de la memoria cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Regresa una matriz que es inverza de 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data / obteniendo los datos del cache")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...) %*% data
        x$setinverse(i)
        i
}