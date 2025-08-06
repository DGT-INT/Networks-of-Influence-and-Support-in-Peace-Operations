// Create a Graphology graph
const graph = new graphology.Graph();

graph.addNode("1", {
  label: "Node 1",
  x: 0,
  y: 0,
  size: 10,
  color: "blue"
});

graph.addNode("2", {
  label: "Node 2",
  x: 1,
  y: 1,
  size: 20,
  color: "red"
});

graph.addEdge("1", "2", {
  size: 5,
  color: "purple"
});

// Create and mount the Sigma renderer
const renderer = new sigma.Sigma(graph, document.getElementById("container"));