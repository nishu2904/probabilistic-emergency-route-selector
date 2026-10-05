library(igraph)
library(dplyr)

roads <- read.csv("data/road_data.csv")

print(roads)
road_graph <- graph_from_data_frame(
  roads,
  directed = TRUE
)

print(road_graph)
plot(
  road_graph,
  vertex.size = 30,
  vertex.label.color = "black",
  edge.arrow.size = 0.5
)
all_routes <- all_simple_paths(
  road_graph,
  from = "START",
  to = "HOSPITAL"
)

print(all_routes)

route_times <- c()

for (route in all_routes) {
  
  total_time <- 0
  
  for (i in 1:(length(route) - 1)) {
    
    from_node <- V(road_graph)[route[i]]$name
    to_node <- V(road_graph)[route[i + 1]]$name
    
    road <- roads[
      roads$from == from_node & roads$to == to_node,
    ]
    
    total_time <- total_time + road$base_time_min
  }
  
  route_times <- c(route_times, total_time)
}

print(route_times)

traffic_delay <- 2
delay_time <- 3
accident_delay <- 7

print(traffic_delay)
print(delay_time)
print(accident_delay)

road_ab <- roads[
  roads$from == "A" & roads$to == "B",
]

expected_time_ab <- road_ab$base_time_min +
  road_ab$traffic_prob * traffic_delay +
  road_ab$delay_prob * delay_time +
  road_ab$accident_prob * accident_delay

print(expected_time_ab)
roads$expected_time <- roads$base_time_min +
  roads$traffic_prob * traffic_delay +
  roads$delay_prob * delay_time +
  roads$accident_prob * accident_delay

print(
  roads[, c(
    "from",
    "to",
    "base_time_min",
    "traffic_prob",
    "delay_prob",
    "accident_prob",
    "expected_time"
  )]
)
route_expected_times <- c()

for (route in all_routes) {
  
  total_expected_time <- 0
  
  for (i in 1:(length(route) - 1)) {
    
    from_node <- V(road_graph)[route[i]]$name
    to_node <- V(road_graph)[route[i + 1]]$name
    
    road <- roads[
      roads$from == from_node & roads$to == to_node,
    ]
    
    total_expected_time <- total_expected_time +
      road$expected_time
  }
  
  route_expected_times <- c(
    route_expected_times,
    total_expected_time
  )
}

print(route_expected_times)

print(
  roads[, c(
    "from",
    "to",
    "base_time_min",
    "traffic_prob",
    "delay_prob",
    "accident_prob",
    "expected_time"
  )]
)
