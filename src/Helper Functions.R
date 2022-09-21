
# Store icon and pallete variables ----------------------------------------


icon <- makeAwesomeIcon(icon = "glyphicon glyphicon-asterisk",
                        markerColor = "red", iconColor = "yellow",
                        library = "glyphicon") # thumbs-down

icon.glyphicon <- makeAwesomeIcon(icon = "star-empty",
                                  markerColor = "blue",
                                  iconColor = "yellow") # thumbs-down, #star, #star-empty #record #piggy-bank

icon.fa2 <- makeAwesomeIcon(icon = "piggy-bank",
                            markerColor = "white",
                            iconColor = "red") # thumbs-down, #star, #star-empty #record #piggy-bank

icon.fa <- makeAwesomeIcon(icon = "remove", markerColor = "white",
                           library = "fa", iconColor = "red") # , squareMarker =  TRUE)

icon.ion <- makeAwesomeIcon(icon = "home", markerColor = "green", library = "ion")

icon.fa3 <- makeAwesomeIcon(icon = "thumbs-down", markerColor = "red", iconColor = "yellow") # thumbs-down

pal <- colorFactor(c("blue", "red", "green", "yellow", "purple",
                     "brown", "black", "pink", "orange"),
                   domain = unique(lastseven$id)
)

palclusttype <- colorFactor(c("blue", "red", "green"),
                            domain = unique(Feeding_clus_poly_final$predicted_activity)
)

palbots <- colorFactor(topo.colors(45, alpha = 1), NSR$NAME)

paltimeline <- colorFactor(c("blue", "red", "green", "yellow",
                             "purple", "brown", "black", "pink", "orange"),
                           domain = unique(power$id))


icon_bird_down <- makeAwesomeIcon(icon = "star", markerColor = "white", iconColor = "red") # thumbs-down, #star, #star-empty #record #piggy-bank

icon.fa2 <- makeAwesomeIcon(icon = "piggy-bank", markerColor = "white", iconColor = "red") # thumbs-down, #star, #star-empty #record #piggy-bank

icon.ion <- makeAwesomeIcon(icon = "home", markerColor = "green", library = "ion")
