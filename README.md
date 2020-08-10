# MMLOS
A Multi-Modal Level of Service (MMLOS) calculator for complete streets projects in *R*.

The package calculates LOS using the current Highway Capacity Manual 6th edition methodology, but also includes proposed revisions to bicycle and pedestrian LOS. The calculation with and without the revisions can be toggled on and off by the user. The pedestrian LOS revisions includes various delay improvements proposed a collection of working papers by Kittelson and Associates [1]. The bicycle LOS revisions are described in the working paper by the authors [2], and include the following proposed revisions to bicycle LOS:

- separated bike lane buffer size in link cross-section factor,
- motorized traffic exposure factor at intersections,
- bicycle delay from right turning vehicle conflict in bicycle lanes at intersections, and
- bicycle delay for left-turning bicycles performing one- and two-stage left turns.

The MMLOS package currently only supports LOS for bicycles and pedestrians, but will be extended to automobiles and transit in future versions.

If you are not familiar with *R*, do not fear, this package is made simple. To run it, you only need to have the open-source *R*-programming language installed on your computer. If you do not have *R*, I recommend starting out with its companion IDE [RStudio](https://rstudio.com/products/rstudio/download/). *R* and RStudio is a *very* high-level statistical programming language intended for data analysis and research. It's designed to be less of a "programming" language and more of an "analysis" language, so it is easy to use and easy to learn. But you don't need to be fluent to run this package, just click and type a few commands.

## Installation
The MMLOS package can be installed in <em>R</em> with the following commands:

	library(devtools) #Install devtools if not already installed!
	install_github("nick-fournier/MMLOS")
## Usage
Load data with:

	dat <- loaddat(dirs)

This contains the directory to formatted CSV files for intersections and links. See the [input data descriptions](data/input_link_template.csv) file and the [link data](data/input_link_template.csv) and [intersection data](data/input_intersection_template.csv) template files for examples. The LOS is then calculated using the function:

	los <- calcMMLOS(dat)

See the not so vigorous [vignette](https://htmlpreview.github.io/?https://github.com/nick-fournier/MMLOS/blob/master/vignette/nsv-vignette.html) for simple usage.

## References
1. Kittelson and Associates Inc., Highway Safety Research Center at the University of North Carolina, and Portland State University. Collection of Working Papers: Pedestrian Crossing Delay and LOS - NCHRP Project 17-87 - Enhancing Pedestrian Volume Estimation and Developing HCM Pedestrian Methodologies for Safe and Sustainable Communities. 2020.

2. Fournier, N., Huang, A., and Skabardonis, A.  Improved Analysis Methodologies and Strategies for Complete Streets (Working paper). 2020.
