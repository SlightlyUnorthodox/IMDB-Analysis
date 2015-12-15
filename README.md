# IMDB-Analysis
Group Project for Fall section of UF CIS4930 Data Mining

# Roadmap Presentation
Viewable [here](http://htmlpreview.github.io/?https://github.com/SlightlyUnorthodox/IMDB-Analysis/blob/master/roadmap.html)

# Notes (Most recent first)

(Dax) Load working dataset with the following code with file *clean10Kdataset.rds* in working directory

```{r}
readRDS("clean10Kdataset.rds")
```

Or build directly from the json file with the following (source functions),

```{r}
data <- jsonToCsv()
data <- preprocessing(data) #Writes clean10Kdataset.rds file
```

(Dax) I've been working out some of the kinks in getting ahold of the data so that we can spend minimal time on data cleaning. Hopefully the comments are enough to explain what's going on, if not feel free to message me whenever. Here's an overview,
  
* Files provided by IMDB (site linked below) are in a weird format ".list.gz"
* The function in *datasetCreation.r* allows you to skip some of the mess
      * Example: movies.data <- downloadRawDataset("movies")
* I have most, but not all of the useful files linked
* The function *exploreData* is really just something I made for checking the skiprow value, use if you feel like it.
* To make sure we're all consistent with what packages we're using make sure to add relevant packages to the *loadPackages* function you're working within the scope of.
* One last thing, when you're trying to get the data into a workable, familiar format, use **read.csv(data,skip=skip_number,sep="\t")** as it'll remove a lot of headache-inducing format issues. But of course, adjust as necessary.

(Dax) I went ahead and drew up the core files that will be needed in this project. Whoever ends up being in the group, I hope everyone involved at least attempts all of the problems described in the project requirements. Also note the report is a .md which will be compiled to article format when the project is completed.

# Relevant Literature

# Methods of Interest

# References & Resources

The Internet Movie Database
[http://www.imdb.com](IMDb)

IMDbPY - Python API and Materials for IMDB searches
[http://www.imdbpy.sourceforge.net](IMDbPY & imdbpy2sql.py)

The Open Movie Database
[http://www.omdbapi.com/](OMDb API)
