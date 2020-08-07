# MMLOS
A Multi-Modal Level of Service (MMLOS) calculator for complete streets projects.

The package calculates LOS using the current Highway Capacity Manual 6th edition methodology, but also includes proposed revisions to bicycle and pedestrian LOS. The calculation with and without the revisions can be toggled on and off by the user. The pedestrian LOS revisions includes various delay improvements proposed a collection of working papers by Kittelson and Associates [1]. The bicycle LOS revisions are described in the working paper by the authors [2], and include the following proposed revisions to bicycle LOS:

- separated bike lane buffer size in link cross-section factor,
- motorized traffic exposure factor at intersections,
- bicycle delay from right turning vehicle conflict in bicycle lanes at intersections, and
- bicycle delay for left-turning bicycles performing one- and two-stage left turns.

The MMLOS package currently only supports LOS for bicycles and pedestrians, but will be extended to automobiles and transit in future versions. 


## Installation
The MMLOS package can be installed in <em>R</em> with the following commands:
		library(devtools) #Install devtools if not already installed!
		install_github("nick-fournier/MMLOS")

## References
1. Kittelson and Associates. Collection of Working Papers: Pedestrian Crossing Delay and LOS - NCHRP Project 17-87 - Enhancing Pedestrian Volume Estimation and Developing HCM Pedestrian Methodologies for Safe and Sustainable Communities. 2020.
2. Fournier, N., Huang, A., and Skabardonis, A.  Improved Analysis Methodologies and Strategies for Complete Streets.