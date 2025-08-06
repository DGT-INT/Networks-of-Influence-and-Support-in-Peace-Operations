// Step 1: Select the container where Sigma will render the graph
const container = document.getElementById('sigma-container');

// Step 2: Load both nodes and edges JSON files
Promise.all([
  fetch('../data/Processed Data/test_data_nodes.json').then(res => res.json()),
  fetch('../data/Processed Data/test_data_edges.json').then(res => res.json())
])
.then(([nodes, edges]) => {
    console.log("Nodes:", nodes);
    console.log("Edges:", edges);

  // Create graph
const Graph = window.Graph;
const graph = new Graph();


  // Add nodes
  nodes.forEach(node => {
    graph.addNode(node.id, {
        label: node.label,
    });
  });

  // Add edges
  edges.forEach(edge => {
    graph.addEdge(edge.id, edge.source, edge.target, {
        type: edge.type,
    });
  });

  // Initialize Sigma
  const sigmaInstance = new sigma({
    graph: graph,
    container: container,
    settings: {
      defaultNodeColor: '#3388AA',
      defaultEdgeColor: '#ccc',
      defaultEdgeType: 'arrow',
      minArrowSize: 10,
    }
  });

})
