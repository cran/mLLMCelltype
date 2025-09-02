## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  message = FALSE,
  warning = FALSE,
  eval = FALSE
)

## -----------------------------------------------------------------------------
# library(Seurat)
# library(mLLMCelltype)
# library(ggplot2)
# 
# # Add consensus annotations to Seurat object
# seurat_obj$cell_type_consensus <- plyr::mapvalues(
#   x = as.character(Idents(seurat_obj)),
#   from = as.character(0:(length(consensus_results$final_annotations)-1)),
#   to = consensus_results$final_annotations
# )
# 
# # Extract and add consensus metrics
# consensus_metrics <- lapply(names(consensus_results$initial_results$consensus_results), function(cluster_id) {
#   metrics <- consensus_results$initial_results$consensus_results[[cluster_id]]
#   return(list(
#     cluster = cluster_id,
#     consensus_proportion = metrics$consensus_proportion,
#     entropy = metrics$entropy
#   ))
# })
# 
# metrics_df <- do.call(rbind, lapply(consensus_metrics, data.frame))
# 
# # Add consensus proportion
# seurat_obj$consensus_proportion <- as.numeric(plyr::mapvalues(
#   x = as.character(Idents(seurat_obj)),
#   from = metrics_df$cluster,
#   to = metrics_df$consensus_proportion
# ))
# 
# # Add entropy
# seurat_obj$entropy <- as.numeric(plyr::mapvalues(
#   x = as.character(Idents(seurat_obj)),
#   from = metrics_df$cluster,
#   to = metrics_df$entropy
# ))

## -----------------------------------------------------------------------------
# # Basic cell type visualization
# p1 <- DimPlot(seurat_obj,
#               group.by = "cell_type_consensus",
#               label = TRUE,
#               repel = TRUE) +
#   ggtitle("Cell Type Annotations") +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# p1

## -----------------------------------------------------------------------------
# # Visualize consensus proportion
# p2 <- FeaturePlot(seurat_obj,
#                  features = "consensus_proportion",
#                  cols = c("yellow", "green", "blue")) +
#   ggtitle("Consensus Proportion") +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# p2

## -----------------------------------------------------------------------------
# # Visualize Shannon entropy
# p3 <- FeaturePlot(seurat_obj,
#                  features = "entropy",
#                  cols = c("red", "orange", "yellow")) +
#   ggtitle("Shannon Entropy") +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# p3

## -----------------------------------------------------------------------------
# # Combine all three visualizations
# library(patchwork)
# combined_plot <- p1 | p2 | p3
# combined_plot

## -----------------------------------------------------------------------------
# # Install SCpubr if not already installed
# if (!requireNamespace("SCpubr", quietly = TRUE)) {
#   remotes::install_github("enblacar/SCpubr")
# }
# 
# library(SCpubr)
# 
# # Enhanced cell type visualization
# p_enhanced <- SCpubr::do_DimPlot(
#   sample = seurat_obj,
#   group.by = "cell_type_consensus",
#   label = TRUE,
#   repel = TRUE,
#   pt.size = 0.5
# ) +
#   ggtitle("Enhanced Cell Type Annotations")
# 
# p_enhanced

## -----------------------------------------------------------------------------
# # Find marker genes
# library(dplyr)
# pbmc_markers <- FindAllMarkers(seurat_obj, only.pos = TRUE, min.pct = 0.25, logfc.threshold = 0.25)
# 
# # Get top markers
# top_markers <- pbmc_markers %>%
#   group_by(cluster) %>%
#   top_n(n = 3, wt = avg_log2FC) %>%
#   pull(gene) %>%
#   unique()
# 
# # Create dotplot
# Idents(seurat_obj) <- seurat_obj$cell_type_consensus
# 
# DotPlot(seurat_obj, features = top_markers) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   ggtitle("Top Marker Genes by Cell Type")

## -----------------------------------------------------------------------------
# # Save visualizations
# ggsave("cell_type_annotations.png", plot = p1, width = 10, height = 8, dpi = 300)
# ggsave("consensus_proportion.png", plot = p2, width = 10, height = 8, dpi = 300)
# ggsave("entropy.png", plot = p3, width = 10, height = 8, dpi = 300)
# ggsave("combined_results.png", plot = combined_plot, width = 15, height = 5, dpi = 300)

