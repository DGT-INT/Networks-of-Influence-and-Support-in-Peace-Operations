      
      // Importing data
      Promise.all([
        fetch("../data/Processed Data/test_data_nodes.json").then(res => res.json()),
        fetch("../data/Processed Data/test_data_edges.json").then(res => res.json())
      ])
      
      .then(([nodes, edges]) => {
        console.log("Nodes:", nodes);
        console.log("Edges:", edges);

      // Use nodes and edges to build your graph
      })
      .catch(error => console.error("Error loading files:", error));

      
      // Create a graphology graph
      const graph = new graphology.Graph();

      // Create nodes
      graph.addNode("1", { label: "Node 1", x: 0, y: 0, size: 10 });
      graph.addNode("2", { label: "Node 2", x: 1, y: 1, size: 20 });
      graph.addNode("3", { label: "Node 3", x: 0, y: 1, size: 10 });
      graph.addNode("4", { label: "Node 4", x: 0.75, y: 0.25, size: 10 });



      // Create edges
      graph.addEdge("1", "2", { size: 5, color: "purple" });
      graph.addEdge("3", "2", { size: 5, color: "brown" });

      


      // Instantiate sigma.js and render the graph
      const sigmaInstance = new Sigma(graph, document.getElementById("container"));
  