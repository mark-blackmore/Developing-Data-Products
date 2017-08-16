library(leaflet)
my_map <- leaflet() %>%
  addTiles()
  
my_map <- my_map %>%  
  addMarkers(lat = 45.511535, lng = -122.680112,
      popup = "My office")
my_map
