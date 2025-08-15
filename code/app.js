document.addEventListener("DOMContentLoaded", () => {
  Promise.all([
    fetch('../data/Processed Data/test_data_nodes.json').then(res => res.json()),
    fetch('../data/Processed Data/test_data_edges.json').then(res => res.json())
  ])
    .then(([nodesRaw, edgesRaw]) => {
      // Convert to Cytoscape format
      const nodes = nodesRaw.map(n => ({ data: { id: n.id, label: n.label } }));
      const edges = edgesRaw.map(e => ({
        data: {
          id: e.id,
          source: e.source,
          target: e.target,
          label: e.label || "", // optional label
          weight: e.n_contracts
        }
      }));

      // Initialize Cytoscape
      const cy = cytoscape({
        container: document.getElementById('cy'),
        elements: {
          nodes: nodes,
          edges: edges
        },
        layout: {
          name: 'fcose',
          quality: 'default',        // good balance between speed and precision
          randomize: true,           // start positions randomized
          animate: true,             // smooth animation to positions
          animationDuration: 1000,   // 1 second animation
          nodeSeparation: 200,       // bigger separation between nodes
          idealEdgeLength: 250,      // longer edges = more spread out
          edgeElasticity: 0.45,      // how stretchy edges are
          gravity: 0.1,               // low gravity to avoid pulling too tight
          gravityRange: 3.8,          // how far gravity spreads its influence
          initialEnergyOnIncremental: 0.8, // helps avoid sudden clumps
          coolingFactor: 0.95,       // slows movement for a stable final layout
          nestingFactor: 0.8,        // keeps compound nodes more compact
        },
        style: [
          {
            selector: 'node',
            style: {
              'label': 'data(label)',
              'background-color': '#0074D9',
              'color': '#fff',
              'text-valign': 'center',
              'text-halign': 'center'
            }
          },
          {
            selector: 'edge',
            style: {
              'label': 'data(label)',
              'width': `data(weight)`,
              'line-color': '#ccc',
              'target-arrow-shape': 'triangle',
              'target-arrow-color': '#ccc',
              'curve-style': 'bezier'
            }
          }
        ]
      });
      cy.style()
  .selector('node')
  .style({
    'label': 'data(label)',
    'font-family': 'Roboto, sans-serif',
    'font-size': '14px',
    'text-valign': 'center',
    'text-halign': 'center',
    'shape': 'ellipse',
    'width': ele => {
  const ctx = document.createElement('canvas').getContext('2d');
  ctx.font = '16px Roboto, sans-serif';
  const textWidth = ctx.measureText(ele.data('label')).width;
  return textWidth + 1; // add padding so text isn’t touching the edge
},
'height': ele => {
  return 30; // enough for one line of text; adjust if font size changes
}
  })
  .update();
    })
    .catch(error => {
      console.error("Error loading JSON:", error);
    });
});

// adding filters

const slider = document.getElementById('contractsSlider');
const sliderValue = document.getElementById('sliderValue');

slider.addEventListener('input', function() {
  const minContracts = parseInt(this.value, 10);
  sliderValue.textContent = minContracts;

  // Show all elements first
  cy.elements().show();

  // Hide edges below threshold
  cy.edges().forEach(edge => {
    if (edge.data('weight') < minContracts) {
      edge.hide();
    }
  });

  // Optionally hide nodes that are now disconnected
  cy.nodes().forEach(node => {
    if (node.connectedEdges(':visible').length === 0) {
      node.hide();
    }
  });
});





