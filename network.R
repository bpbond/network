# network.R
# BBL October 20 2023

# Determine if there's a path from node n1 to node n2
find_path <- function(n1, n2, edge_list, already_visited = NULL) {

    if(n1 == n2) {
        # We have arrived!
        return(n2)
    }

    # Otherwise, see what outbound connections are available...
    outbounds <- subset(edge_list, n1 == edge_list$From &
                            !edge_list$To %in% already_visited)
    # ...and try them one by one
    for(i in outbounds$To) {
        path <- find_path(i, n2, edge_list, c(already_visited, n1))
        if(!is.null(path)) {
            # A way has been found! Add our node to the path and return
            return(c(n1, path))
        }
    }

    # Help, help, no way out!
    if(is.null(already_visited)) message("No path from ", n1, " to ", n2)
    invisible(NULL) # this is the 'no path' signal back to caller
}


edge_list <- read.csv("edges.csv")
