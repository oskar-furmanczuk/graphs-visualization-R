# Function for graph data parsing

readInGraph <- function(path){
  con <- file(path, open='r')
  text <- readLines(con, warn = FALSE)
  outter_list <- list()
  for (i in 1:length(text)){
    line <- text[[i]]
    without_br <-  gsub('(\\{|\\})','', line)
    vec_char <- strsplit(without_br, ", ")
    vec_double <- as.numeric(unlist(vec_char))
    node <-  vec_double[[1]]
    connections <-  vec_double[-1] 
    inner_list <- list(node = node, connections = connections)
    outter_list[[node]] <- inner_list
  }
  return(outter_list)
}

l_graph <- readInGraph("graph_small.dat")

# Function for graph visualization

drawGraph <- function(graph_list, clr_points = "blue", clr_lines = "red", rand_state = 12, nodes_size = 1){
  v_x <- c()
  v_y <- c()
  l <- list()
  set.seed(rand_state)
  for (i in 1:length(graph_list)){
    x <- runif(1, 1, 100)
    y <- runif(1, 1, 100)
    l[[i]] <- c(x,y)
    v_x <- append(v_x, x)
    v_y <- append(v_y, y)
  }
  plot.new()
  pWidth = 100
  pHeight = 100
  plot.window(c(0,pWidth), c(0,pHeight))
  points(v_x, v_y, col=clr_points, pch = 19, cex = nodes_size)
  
  for (i in 1:length(graph_list)){
    for (j in graph_list[[i]]$connections){
      segments(l[[i]][1],l[[i]][2], l[[j]][1], l[[j]][2], col= clr_lines)
    }
  }
}

# EXAMPLES

drawGraph(l_graph)
drawGraph(l_graph, clr_points = "red", clr_lines = "yellow", nodes_size = 0.95)
drawGraph(l_graph, clr_lines = "pink", rand_state = 100, nodes_size = 1.1)
