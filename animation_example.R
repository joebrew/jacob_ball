##########
# How to make an "animation" in R - for Jake
##########

# Packages
library(maps)

# Plot USA
map('usa', bg = 'black', fill = TRUE, col = 'white')

# Make some zombies in Florida
zombies <- data.frame(n = 1:10,
                      x = rnorm(mean = -82, sd = 0.2, n = 10),
                      y = rnorm(mean = 29.1, sd = 0.1, n = 10))

# Plot the zombies
points(x = zombies$x,
       y = zombies$y,
       pch = '.',
       col = adjustcolor('black', alpha.f = 0.6))

# Now make the zombies spread out
new_zombies <- zombies
bla <- seq(0, 100, by = 0.1)
today <- Sys.Date()
for (i in 1:length(bla)){
    this_time <- new_zombies[sample(1:nrow(new_zombies), 10, replace = FALSE),]
    this_time$x <- this_time$x + rnorm(mean = 0, sd = bla[i], n = 10)
    this_time$y <- this_time$y + rnorm(mean = 0, sd = bla[i], n = 10)
    new_zombies <- rbind(new_zombies, this_time)
  

    map('usa', fill = TRUE, col = 'white', bg = 'black')
  points(x = new_zombies$x,
         y = new_zombies$y,
         pch = 1,
         col = adjustcolor('black', alpha.f = 0.6))
  title(main = paste0('Zombie apocalypse projection: ', format(today + i, format = '%B %d, %Y')),
        col.main = 'white')
  Sys.sleep(0.2)
}


##########
# How to make an "gif" in R - for Jake
##########

# Packages
library(maps)

# Plot USA
map('usa', bg = 'black', fill = TRUE, col = 'white')

# Make some zombies in Florida
zombies <- data.frame(n = 1:10,
                      x = rnorm(mean = -82, sd = 0.2, n = 10),
                      y = rnorm(mean = 29.1, sd = 0.1, n = 10))

# Plot the zombies
points(x = zombies$x,
       y = zombies$y,
       pch = '.',
       col = adjustcolor('black', alpha.f = 0.6))

# Now make the zombies spread out
my_directory <- '~/Desktop/temporary_folder' # Give whatever folder you want here
new_zombies <- zombies
bla <- seq(0, 100, by = 0.1)
today <- Sys.Date()
for (i in 1:length(bla)){
  this_time <- new_zombies[sample(1:nrow(new_zombies), 10, replace = FALSE),]
  this_time$x <- this_time$x + rnorm(mean = 0, sd = bla[i], n = 10)
  this_time$y <- this_time$y + rnorm(mean = 0, sd = bla[i], n = 10)
  new_zombies <- rbind(new_zombies, this_time)
  
  image_name <- i
  while(nchar(image_name) <= 5){
    image_name <- paste0(0, image_name)
  }
  png(paste0(my_directory, '/', image_name, '.png'))
  map('usa', fill = TRUE, col = 'white', bg = 'black')
  points(x = new_zombies$x,
         y = new_zombies$y,
         pch = 1,
         col = adjustcolor('black', alpha.f = 0.6))
  title(main = paste0('Zombie apocalypse projection: ', format(today + i, format = '%B %d, %Y')),
        col.main = 'white')
  dev.off()
}
# You now have an ordered sequence of .pngs in your my_folder
# and you can use some gif software to put them all together
# In linux you can cd into my_folder, and then run the following via command line:
# > convert -delay 10 -loop 0 *.png result.gif
# This will generate a gif called result.gif using all of the pngs in that folder
# In Mac / Windows, you're on your own.

# If you want a video (mp4) instead of a gif, you can use a progra mlike ffmpeg
# Cd into my_folder, and then run the following:
# > ffmpeg -start_number 000001 -r 5 -i %6d.png my_video.mp4
