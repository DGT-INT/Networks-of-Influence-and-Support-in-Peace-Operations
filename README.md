# Networks of Influence and Support in Peace Operations

This project is an interactive **R Shiny application** designed to visually explore the **networks of influence and support** in peace operations. The tool enables users—researchers, policymakers, or practitioners—to investigate key relationships and actors involved in international peacebuilding missions.

## 🕊️ Project Overview

Peace operations are complex systems involving multiple actors—governments, NGOs, donors, military organizations, and local communities. This app aims to:

- Visually map the relationships of **influence** (e.g., advisory roles, decision-making authority) and **support** (e.g., funding, logistics, coordination).
- Allow users to **filter, highlight, and customize** the network to explore specific actors or types of connections.
- Provide insights into **power dynamics**, **information flows**, and **collaborative structures** within peace operations.

## 🎯 Key Features

- **Interactive network visualization** using `visNetwork` or `igraph`
- User controls for:
  - Filtering by actor type (e.g., military, civil society, donor)
  - Customizing edge weight and direction
  - Highlighting central or influential nodes
- **Downloadable reports** and data snapshots
- **Dark/light mode toggle** (optional)

## 🛠️ Built With

- [R](https://www.r-project.org/)
- [Shiny](https://shiny.posit.co/)
- [visNetwork](https://datastorm-open.github.io/visNetwork/) or `igraph`
- `dplyr`, `tidygraph`, `ggraph` (depending on graph structure and layout)
- `shinyWidgets`, `shinythemes` (for UI enhancements)

## 🚧 Project Status

This project is currently in active development.

The foundational network structure and Shiny interface are being built.
Core visualizations are functional, but customization tools and filters are in progress.
Additional features like temporal network views and expanded datasets will be added in future iterations.

## 📂 Project Structure

```plaintext
.
├── app.R                # Main R Shiny app
├── data/                # Network data files (e.g., nodes.csv, edges.csv)
├── www/                 # Custom CSS or JS
├── README.md            # This file
└── ...

