#Ishaan_Problem2.R
#' plot_crime function
#' @param year_entry Year to be considered
#' @param d Data set
#' @details Creates a google maps plot for top 5 regions with Break-In related crimes
#' @return Google Map plot for the given year showing top 5 regions with Break-In related crimes

plot_crime <- function(year_entry, d)
{
d <- dplyr::filter(d,YEAR==year_entry)

events <- dplyr::select(d, Longitude, Latitude)

max_lat <- 49.29723
min_lat <- 49.20091
max_long <- -123.0236
min_long <- -123.2199
center <- c((max_long + min_long)/2, (max_lat + min_lat)/2)

mt <- ManyTiles(min_lat, max_lat, min_long, max_long, 10, 5, events)

element_1 <- rep(NA, 5)
number_tiles <- get_number_tiles.ManyTiles(mt)
num_events <- rep(NA, number_tiles)

for (i in 1:number_tiles) {
  current_tile <- get_tile.ManyTiles(mt, i)
  current_events <- get_events.Tile(current_tile)
  num_events[i] <- nrow(current_events)
}

new <- sort(num_events, decreasing = TRUE)
highest1 <- new[1]
highest2 <- new[2]
highest3 <- new[3]
highest4 <- new[4]
highest5 <- new[5]

tile_ind_1 <- which(num_events==highest1)
element_1[1] <- tile_ind_1
tile_ind_2 <- which(num_events==highest2)
element_1[2] <- tile_ind_2
tile_ind_3 <- which(num_events==highest3)
element_1[3] <- tile_ind_3
tile_ind_4 <- which(num_events==highest4)
element_1[4] <- tile_ind_4
tile_ind_5 <- which(num_events==highest5)
element_1[5] <- tile_ind_5

tile1 <- get_tile.ManyTiles(mt, element_1[1])
tile2 <- get_tile.ManyTiles(mt, element_1[2])
tile3 <- get_tile.ManyTiles(mt, element_1[3])
tile4 <- get_tile.ManyTiles(mt, element_1[4])
tile5 <- get_tile.ManyTiles(mt, element_1[5])

tile_list <- list(tile1, tile2, tile3, tile4, tile5)
df_list <- as.list(rep(NA, 5))

for (i in 1:length(tile_list)) 
{
  df_list[[i]] <- Tile_to_rectangle_df(tile_list[[i]])
}

df <- do.call(rbind, df_list)
events1 <- get_events.Tile(tile1)
events2 <- get_events.Tile(tile2)
events3 <- get_events.Tile(tile3)
events4 <- get_events.Tile(tile4)
events5 <- get_events.Tile(tile5)
df_events <- rbind(events1, events2, events3, events4, events5)

pt_lat <- rep(NA,length(tile_list))
pt_long <- rep(NA,length(tile_list))
tot_ptevents <- rep(NA,length(tile_list))

for(a in 1:length(tile_list))
{
  pt_long[a] <- tile_list[[a]]$min_long
  pt_lat[a] <- tile_list[[a]]$max_lat
  tot_ptevents[a] <- nrow(tile_list[[a]]$events)
}

count_df <- data.frame(pt_long=pt_long, pt_lat=pt_lat,tot_ptevents=tot_ptevents)

if (!exists("m")) {  # loading the map takes a while, so do it only once
  m <- get_googlemap("vancouver ca", zoom = 12)
}

p <- ggmap(m)
p <- p + geom_rect(mapping=aes(xmin=min_long, xmax=max_long,
                               ymin=min_lat, ymax=max_lat),
                   data=df, fill=NA, size=1, color="red",
                   inherit.aes=FALSE)

p <- p + geom_point(mapping=aes(x=Longitude, y=Latitude),
                   data=df_events, size=0.5, color="black")

p <- p + geom_label(x=-123.1, y=49.33, label=year_entry, size=4, color="black")

p <- p + geom_label(mapping=aes(x=pt_long,y=pt_lat,label=tot_ptevents),
                    data=count_df,size=2)

print(p)
}