# mrtree 

R package for MRtree.

MRtree for Multi-resolution Reconciled tree, is a post clustering procedure to build cluster hierarchy based on flat clustering obtained for multi-resolutions. The tool is developed for single-cell RNAseq clustering analysis to identify sub-cell types. It can couple with many popular clustering tools such as Seurat. 

## Kevin Notes:

I threw out all instances of `SymSim` (which was needed for all the functions to simulate data) and `ggimage` (due to some collate error, which was needed for `plot.piechart` option in `plot_tree`).
I also threw out many the plotting functions (importantly, `plot_tree`) all together since they were somehow requiring some bizarre dependency on `aplot` (among other things).

## Citation

```
@article{10.1093/nar/gkab481,
    author = {Peng, Minshi and Wamsley, Brie and Elkins, Andrew G and Geschwind, Daniel H and Wei, Yuting and Roeder, Kathryn},
    title = "{Cell type hierarchy reconstruction via reconciliation of multi-resolution cluster tree}",
    journal = {Nucleic Acids Research},
    volume = {49},
    number = {16},
    pages = {e91-e91},
    year = {2021},
    month = {06},
    abstract = "{A wealth of clustering algorithms are available for single-cell RNA sequencing (scRNA-seq) data to enable the identification of functionally distinct subpopulations that each possess a different pattern of gene expression activity. Implementation of these methods requires a choice of resolution parameter to determine the number of clusters, and critical judgment from the researchers is required to determine the desired resolution. This supervised process takes significant time and effort. Moreover, it can be difficult to compare and characterize the evolution of cell clusters from results obtained at one single resolution. To overcome these challenges, we built Multi-resolution Reconciled Tree (MRtree), a highly flexible tree-construction algorithm that generates a cluster hierarchy from flat clustering results attained for a range of resolutions. Because MRtree can be coupled with most scRNA-seq clustering algorithms, it inherits the robustness and versatility of a flat clustering approach, while maintaining the hierarchical structure of cells. The constructed trees from multiple scRNA-seq datasets effectively reflect the extent of transcriptional distinctions among cell groups and align well with levels of functional specializations among cells. Importantly, application to fetal brain cells identified subtypes of cells determined mainly by maturation states, spatial location and terminal specification.}",
    issn = {0305-1048},
    doi = {10.1093/nar/gkab481},
    url = {https://doi.org/10.1093/nar/gkab481},
    eprint = {https://academic.oup.com/nar/article-pdf/49/16/e91/40358662/gkab481.pdf},
}
```


## Installation
First, make sure to install all the dependent packages indicated in the `DESCRIPTION` file.
`mrtree` package can then be installed through `devtools` in R with the following commands,
```{r}
library("devtools")
devtools::install_github("linnykos/mrtree")
```

`mrtree` provides wrappers for multiple clustering functions (see [vignette 1](https://htmlpreview.github.io/?https://github.com/pengminshi/MRtree/blob/master/vignettes/MRtree-tutorial.html)). The required packages for respective clustering methods are only required if the method is needed. This avoids the overloading dependencies.

## Examples
Please follow the [vignette 1](https://htmlpreview.github.io/?https://github.com/pengminshi/MRtree/blob/master/vignettes/MRtree-tutorial.html) for an example of using this R package with Seurat and SC3 on a simulated dataset, and [vignette 2](https://htmlpreview.github.io/?https://github.com/pengminshi/MRtree/blob/master/vignettes/zeisel_example.html) for the analysis on a mouse brain data.

## Kevin demo

```{r}
library("devtools")install.packages("rlang") # need version >= 1.1.0install.packages("vctrs") # need version >= 0.5.0install.packages("dplyr") # need version >= 1.0.10install.packages("purrr") # need version >= 1.0.1install.packages("ggraph") # need version >= 2.1.0devtools::session_info() #devtools::install_github("linnykos/mrtree")# 3: None# Do you want to install from sources the packages which need compilation? (Yes/no/cancel): no# Package which is only available in source form, and may need compilation of C/C++/Fortran: ‘spatstat.explore’: nolibrary(mrtree)data("clust_example")out <- mrtree(clust_example$clusterings)out$labelmat.mrtree# data frame with given prefix and suffix as inputdf <- as.data.frame(clust_example$clusterings); colnames(df) = paste0('K_', 1:4)df$other <- 1:nrow(clust_example$clusterings)  # add an additional columnout <- mrtree(df, prefix = 'K_', suffix=NULL)cl <- cbind(clust_example$clusterings, clust_example$clusterings)# add some additional noisefor (i in 1:ncol(cl)){    cl[sample(10),i] = sample(1:length(unique(cl[,i])), 10, replace = TRUE)}mrtree(cl, consensus=TRUE) # mrtree with within-resolution consensus clustering

plot_clustree(labelmat = clust_example$clusterings, ref.labels = clust_example$ref.labels)```