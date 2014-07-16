# Conway's Game of Life
# http://en.wikipedia.org/wiki/Conway's_Game_of_Life

# cell has 8 neighbors
# alive cell with < 2 neighbors dies
# alive cell with 2 or 3 neighbors lives
# alive cell with > 3 neighbors dies
# dead cell with == 3 neighbors lives

library(audio) # has wait() function

# if manual = T, you press enter to advance each generation; else, advances every 1 second
life = function (start = 0, rows = 5, cols = 5, iters = 25, manual = F){
  world = matrix(data = start, nrow = rows, ncol = cols, byrow = T)  # initialize field
  state = function(world, row, col){
    neighbors = 0 # begin counting neighbors
    for (i in -1:1){ # possible directions to move
      for (j in -1:1){
      if (row+i > rows | row+i < 1){next} # if you've reached the edge
      if (col+j > cols | col+j < 1){next} # "
      if (i==0 & j==0){next} # if it's the current cell
      cur = world[row+i,col+j]
      if (cur == 1){neighbors = neighbors+1}
      }
    }
    if (neighbors < 2){ return(0) } # definitely dead
    if (neighbors == 2){ return(world[row,col]) } # stays alive or stays dead
    if (neighbors == 3){ return(1) } # definitely alive (stays alive or comes alive)
    if (neighbors > 3){ return(0) } # definitely dead
  }
  for (i in 1:iters){  # loop for each iter
    cat("\014") # clear screen
    print(world) # print current world
    cat("generation", i, sep=" ")
    ifelse(manual == T, readline("Press enter for next generation"), wait(1))
    nextIter = world # make a new world to modify, because all rules are applied simultaneously
    for (a in 1:rows){
      for (b in 1:cols){
        nextIter[a,b] = state(world, a, b)
      }
    }
    world = nextIter # set the world to the next state
  }
}

# blinker
life(start = c(rep(0,11), rep(1,3), rep(0,11)), manual=F)

# beacon
life(start = c(rep(0,7), rep(1,2), rep(0,4), 1, rep(0,8), 1, rep(0,4), rep(1,2), rep(0,7)), rows=6, cols=6, manual=F)

# R-pentomino
life(start = c(rep(0,7), 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 1, rep(0,7)))
