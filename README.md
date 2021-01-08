# MMLOS
A Multi-Modal Level of Service (MMLOS) calculator for complete streets projects in *R*.

The package calculates LOS using the current Highway Capacity Manual 6th edition methodology, but also includes proposed revisions to bicycle and pedestrian LOS. The calculation with and without the revisions can be toggled on and off by the user. The pedestrian LOS revisions includes various delay improvements proposed a collection of working papers by Kittelson and Associates [1]. The bicycle LOS revisions are described in the working papers by the authors [2,3], and include the following proposed revisions to bicycle LOS:

- separated bike lane buffer size in link cross-section factor,
- motorized traffic exposure factor at intersections,
- bicycle delay from right turning vehicle conflict in bicycle lanes at intersections, and
- bicycle delay for left-turning bicycles performing one- and two-stage left turns.

The MMLOS package currently only supports LOS for bicycles and pedestrians, but is currently being extended to automobiles and transit in future versions.

If you are not familiar with *R*, do not fear, this package is made simple. To run it, you only need to have the open-source *R*-programming language installed on your computer. If you do not have *R*, I recommend starting out with its companion IDE [RStudio](https://rstudio.com/products/rstudio/download/). *R* and RStudio is a *very* high-level statistical programming language intended for data analysis and research. It's designed to be less of a "programming" language and more of an "analysis" language, so it is easy to use and easy to learn. But you don't need to be fluent to run this package, just click and type a few commands.

## Installation
The MMLOS package can be installed in <em>R</em> through this GitHub Repo using devtools with the following commands:
  
```{r}
  #If devtools is not already installed, type install.packages('devtools') to install it
  #Load devtools from the library
	library(devtools)
	
	#Then install this package through GitHub using this
	install_github("nick-fournier/MMLOS")
```
	
## Usage
To use this package, there are two steps.

1. Setup your data for links and intersections into a spreadsheet and save it as a CSV file in the correct format. For a description of variable inputs and formatting, see the [input data descriptions](data/input_link_template.csv) file. To start you off, I have also provided two working templates in the [data](data/) directory for Hearst Avenue in Berkeley, CA and Colorado Boulevard in Pasadena, CA. For both locations, there are two example input files, one for [link data](data/input_link_hearstave_template.csv) and the other [intersection data](data/input_intersection_hearstave_template.csv) template files.


2. Next run the `MMLOScalc()` function on the two data files. This function loads the data into the *R* environment and runs the LOS calculations. To import your data, you can either leave the input blank and then navigate with the Graphic User Interface (GUI) to select your input files, or you can also just directly specify the file locations. You may also use my example templates by typing 'berkeley' or 'pasadena'. Here's an example of each:

```{r}
  #GUI
  los_gui <- calcMMLOS()

  #Specific input
  dirs <- c(intersections = "C:/some directory/intersections_data.csv", 
            links = "C:/some directory/links_data.csv")
            
	los_dirs <- calcMMLOS(input_data = dirs)
	
	
	#Using my template data
	los_norcal <- calcMMLOS(input_data = 'berkeley')
	los_socal <- calcMMLOS(input_data = 'pasadena')
```
	

It might give you a warning about NAs in the data. This happens if any cells are empty in the CSV file. This might be OK if, for example, you have a three-way intersection instead of a four-way so you must leave some cells blank.


And finally, you may save your output if you'd like. For those of you *R*ookies out there that aren't familiar with saving CSV files in *R*, you can use the function `MMLOSsave(dat, output_folder)`. The `output_folder` parameter is optional like `input_data`, if it's left blank a GUI will pop up. 


```{r}
  #With GUI
  MMLOSsave(dat = los_norcal)
  
  #Without GUIT
  MMLOSsave(dat = los_socal, output_folder = "C:\\Users\\YourName\\Desktop")
  
```


*NOTE: For the GUIs, I have only tested on Windows. May not work on other operating systems.


For more methodological details, see the bicycle revisions working paper titled [Bicycle Level of Service: Accounting for protected lanes, traffic exposure, and delay](https://github.com/nick-fournier/MMLOS/blob/master/docs/Bicycle%20LOS_7-28-2020_draft.pdf) and for pedestrian revisions see the [collection of working papers](https://github.com/nick-fournier/MMLOS/blob/master/docs/Collection_of_working_papers.pdf) from NCHRP report 17-87.

For an example usage, see the [vignette](https://htmlpreview.github.io/?https://github.com/nick-fournier/MMLOS/blob/master/vignette/nsv-vignette.html) which runs through a few simple calculations and plots the results.

## References
1. Kittelson and Associates Inc., Highway Safety Research Center at the University of North Carolina, and Portland State University. Enhancing Pedestrian Volume Estimation and Developing HCM Pedestrian Methodologies for Safe and Sustainable Communities.*Collection of Working Papers: Pedestrian Crossing Delay and LOS - NCHRP Project 17-87.* 2020.

2. Fournier, N., Huang, A., and Skabardonis, A. Improved Analysis Methodologies and Strategies for Complete Streets. *Working paper.* 2020.

3. Fournier, N., Huang, A., and Skabardonis, A. Bicycle Level of Service: Accounting for protected lanes, traffic exposure, and delay. *Transportation Research Board Annual Meeting.* 2021
