# https://kateto.net/sunbelt2021#colors-in-r-plots

# pch = point symbol shape
# cex = point size
# col = color
plot(x = 1:10, y = rep(5, 10), pch = 19, cex = 3, col = "dark red")
points(x = 1:10, y = rep(6, 10), pch = 19, cex = 3, col = "557799")
points(x = 1:10, y = rep(4, 10), pch = 19, cex = 3, col = rgb(0.25, 0.5, 0.3))
# or 
# color can be set to 0-255 range like:
# col = rgb(10, 100, 100, maxColorValue=255)

# change opacity with alpha (0-1)
plot(x = 1:5, y = rep(5,5), pch = 19, cex = 12, col = rgb(0.25, 0.5, 0.3, alpha = 0.5), xlim = c(0,6))

# change background color
par(bg = "gray40")
col.tr <- grDevices::adjustcolor("557799", alpha = 0.7)
plot(x = 1:5, y = rep(5,5), pch = 19, cex = 12, col = col.tr, xlim = c(0,6))

# Get all color names with:
colors()
# get all color names that include "blue"
grep("blue", colors(), value = T)

# Can get palettes
pal1 <- heat.colors(5, alpha = 1) # get five colors, opague
pal2 <- rainbow(5, alpha = 0.5) # get five colors, transparent

plot(x = 1:10, y = 1:10, pch = 19, cex = 25, col = pal1) # colors repeat through the five colors chosen above
plot(x= 1:10, y = 1:10, pch = 19, cex = 25, col = pal2) # colors repeat through the five colors chosen above


#create our own gradients using colorRampPalette
palf <- colorRampPalette(c("gray80", "dark red")) # generates extremes of range, later called through a function
plot(x = 10:1, y = 1:10, pch = 19, cex = 25, col = palf(10)) # Call function with 10 stages of color

# Can add transparency by adding alpha parameter
palg <- colorRampPalette(c(rgb(1,1,1, .2), rgb(.8, 0, 0, .7)), alpha = TRUE) #set range from red to pink, with transparence changing from 0.2 to 0.7
plot(x = 10:1, y = 1:10, pch = 19, cex = 25, col = palg(10))

