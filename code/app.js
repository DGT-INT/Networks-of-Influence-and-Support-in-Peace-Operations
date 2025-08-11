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
          name: 'cose' // force-directed layout
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


