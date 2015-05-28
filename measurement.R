##
# Measurement class
#
# 1. Can construct it from a vector of measurments
#
# 2. Uncertainties will 
#
# - the inverted matrix will only be updated if the main matrix
#   is "dirtied" : if the underlying matrix has any values changed.

library(gmp)

as.measurement <- function(x,sd=NA,mean=NA,nsigfigs=NA,sep=" ") {

    num_sigfigs <- if ( is.na(nsigfigs) ) { NA } else { nsigfigs }
    
    if ( length(x) >= 1 ) {
        num_sigfigs <- if ( is.na(num_sigfigs) ) { min(sapply(x,sigfigs)) } else { num_sigfigs }
        x_bar <- signif(mean(x),num_sigfigs )
        x_sd <- if ( !is.na(sd) ) { sd } else { signif(sd(x),1) } 
    } else if ( is.na(mean) ) {
        num_sigfigs <- if ( is.na(num_sigfigs) ) { min(sapply(x,sigfigs)) } else { num_sigfigs }
        x_bar <- signif(mean(x),num_sigfigs )
        x_sd <- signif(sd(x),1)
       
    } else {
        x_bar <- mean
        num_sigfigs <- if ( is.na(num_sigfigs) ) { min(sigfigs(x_bar)) } else { num_sigfigs }
        x_bar <- signif(mean,num_sigfigs)

        if ( is.na(sd) ) {
            x_sd <- sd(x_bar)
        } else {
            x_sd <- sd
        }
    }
    set_x <- function (x) { 
        x_bar <- mean(x)
        x_sd <- sd(x)
    }
    get_x <- function() {
        x_bar
    }
    set_uncertainty <- function(sd) {
        x_sd <- sd
    }
    get_uncertainty <- function() {
        x_sd
    }
    get_uncertainty_calc<- function() {
        if ( is.na(x_sd) ) { 0 } else { x_sd } 
    }
    
    get_sigfigs <- function() { num_sigfigs }
    
    uncertainty <- function() {
        if ( is.na(x_sd) ) {
            ""
        } else {
            paste("\302\261", signif(x_sd,1),sep=sep)
        }
    }
    
    inspect <- function() {
        cat(paste(signif(x_bar,num_sigfigs), uncertainty(),"\n"))
    }
    
    tmp <- list(get=get,get_uncertainty_calc=get_uncertainty_calc,get_sigfigs=get_sigfigs,set_x=set_x,get_x=get_x,set_uncertainty=set_uncertainty,get_uncertainty=get_uncertainty,inspect=inspect)
    class(tmp) = 'measurement'
    tmp
}

print.measurement = function(obj) {
    obj$inspect()
}

# Significant figures for multiplication and division should
# be set to the minmium in a sequence of calculations
#
`*.measurement` = function(meas,omeas) {
    as.measurement( (meas$get_x() * omeas$get_x()),
                   sd=(meas$get_x() * omeas$get_x())*(meas$get_uncertainty_calc()/ meas$get_x() + omeas$get_uncertainty_calc()/omeas$get_x()),
                   nsigfigs=min(meas$get_sigfigs(),omeas$get_sigfigs())
                   )
}

`/.measurement` = function(meas,omeas) {
    as.measurement( (meas$get_x() / omeas$get_x()),
                   sd=(meas$get_x() / omeas$get_x())*(meas$get_uncertainty_calc()/ meas$get_x() + omeas$get_uncertainty_calc()/omeas$get_x()),
                   nsigfigs=min(meas$get_sigfigs(),omeas$get_sigfigs())
                   )
}

# Significant figures for addition and subtraction should
# be set to the maximum of a sequence of calculations
`+.measurement` = function(meas,omeas) {
    as.measurement( (meas$get_x() + omeas$get_x()),
                   sd=(meas$get_uncertainty_calc() + omeas$get_uncertainty_calc() ),
                   nsigfigs=max(meas$get_sigfigs(),omeas$get_sigfigs())
                   )
}

`-.measurement` = function(meas,omeas) {
    as.measurement( meas$get_x() - omeas$get_x(),
                   sd=(meas$get_uncertainty_calc() + omeas$get_uncertainty_calc() ),
                   nsigfigs=max(meas$get_sigfigs(),omeas$get_sigfigs())
                   )
}


