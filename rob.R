rob <- function(status){

    verh <- function(arg){
        if(arg == "s") {
            return(sum(status))
        }
        if(arg == "m") {
            return(mean(status))
        }
    }

}