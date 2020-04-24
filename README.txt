README

Name: Patrick Aquino, Adam Imran, Thomas Malejko, Doug Post
NetID : pma50, ai410, tpm72, dyp6, 
Class: ANLY 512

#####  FILES  ##### 
README.txt
####The Data
Traffic_Violations_Clean.csv			The cleaned, merged, and final version of the dataset used for this research.

####R Scripts
ANLY512_FinalProject_InitialCleaning.r		Script that breaks down the full dataset into a three year chunk and conducts preliminary cleaning and statistical analysis
ANLY512_FinalProjectFeatureGeneration.r		A short script that generate the 'Highway', 'MajorRoad', and 'ShortCharge' Features of the final dataset
ANLY512_FinalProject_ExploratoryAnalysis.r	A lengthy script that covers some preliminary exploratory data analysis, along with the principal code for the GBM, Random Forest, Lasso Regression, and Hierarchial Clustering analysis.
ANLY512_FinalProject_Subset.r			A sript that conducts Subset Selection analysis.

####RMD Files (every file has a corresponding .html or .pdf file)
ANLY512_FinalProject_Clustering.rmd		An RMD for the Hierarchial Clusting portion of the Feature 
ANLY512_FinalProject_ExploratoryAnalysis.rmd	An RMD for the main analysis
Generation R Script
ANLY512_FinalProject_SummaryStats.Rmd		An RMD file that highlights some of our intial analysis of the topic in question

####CollectMergeWeather Folder
Contains the script and resulting .csv for accessing the weather data.

####Technical Folder
Contains many of the technical outputs from the GBM, RF, Lasso, and Subset Logistic Regression methods

####SummaryStats Folder
Contains many of the outputs (summary statistics and bar charts) from initial efforts to understand the factors that influence citation rates.

GENERAL NOTES: 
** Do not run the "Initial Import & Downsize (DON'T RUN AGAIN)" Code Chunk...this is needed to subset the original dataframe into the three-year window required for the assignment. It takes about 90 sec to 2 minutes to load the original data frame into R. 
** Do not attempt to run any code chunk associated with GBM or RF...these segments can take upwards for 3 hours to run on a single machine. If required, we can provide the final models (they have been saved) for your use. 
** Description of Variables: https://www.opendatanetwork.com/dataset/data.montgomerycountymd.gov/4mse-ku6q


CONTACT
If you have any questions or concerns about this program, feel free to contact the repo owner at tpm72@georgetown.edu.