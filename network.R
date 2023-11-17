# network.R
# BBL October 20 2023

successful_paths <- list()

# Determine if there's a path from node n1 to node n2
find_path <- function(n1, n2, edge_list,
                      already_visited = NULL,
                      current_path_so_far = "",
                      current_distance_so_far = 0) {

    if(n1 == n2) {
        successful_paths[[as.character(current_distance_so_far)]] <<-
            paste(current_path_so_far, n2)
        # We have arrived! Return the successful path string
        return(paste(current_path_so_far, n2))
    }

    # Otherwise, add this node to the current path,
    current_path_so_far <- paste(current_path_so_far, n1)
    already_visited <- c(already_visited, n1)
    # see what outbound connections are available,
    outbounds <- subset(edge_list, n1 == edge_list$From &
                            !edge_list$To %in% already_visited)
    # ...and try them one by one
    found_a_path <- FALSE
    for(i in seq_len(nrow(outbounds))) {
        path <- find_path(outbounds$To[i], n2, edge_list,
                          already_visited,
                          current_path_so_far,
                          current_distance_so_far + outbounds$Distance[i])
        if(!is.null(path)) {
            # A way has been found! Add our node to the path
            found_a_path <- TRUE
        }
    }

    if(!found_a_path) {
        invisible(NULL) # this is the 'no path' signal back to caller
    }
}


edge_list <- read.csv("edges.csv")

find_path("A", "Q", edge_list)
