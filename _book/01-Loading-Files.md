# Loading datafiles
In this short section, we will start with something seemingly small but fundamental If you cannot load data files, you cannot preprocess, visualize, and analyze their contents. Fortunately, R provides several ways to load (and create) data files. For certain types of data files such as those with .sav extension (from SPSS), you will need to load R packages. As a quick reminder:

```r
  
# The classic way to install a package:
  install.packages("car") # or whatever package you want

  # Alternatively look at your R-studio screen. Look at the left window -> packages -> install
  # This "package window" can also be used to deactivate/activate/remove/update packages

# Let's load some packages (using code lines). The old way is to do:
  library(car)
  library(ggplot2)
  library(dplyr)
    #... too exhausting 

# An alternative is the pacman package
  library(pacman)
  p_load(car,ggplot2,dplyr,effectsize) # bonus: it will auto-install any packages if this was not done previously 

```

## Working directory
Before we open a data file, we need to *tell* R where to find it. In RStudio you can import a data set by point and click (top right window under "Environment"). Of course this may become tedious if you want to load multiple data sets. Alternatively, by R code, we could use the working directory. Personally I would highly recommend to use R projects (see chapter 2) but for the sake of illustration I will provide a quick recap of the working directory.

```r
# Getting/Setting the working directory (henceforth abbreviated as "WD")
# Without R projects and manually:
  getwd() # Which will output the WD of the location where R the script was activated (i.e., where you opened it)

# The WD can also be set. For example to a variable I arbitrarily name "myWD"
  myWD = "C:/Users/Gillian" 
  
# Now suppose I want to load a data file (a .csv extended file) located on my desktop put in a folder called "folder".
  # I could do something like this:
  myWD = "C:/Users/Gillian/desktop/folder"
  library(stringr) # Will use this package to glue strings together, and to replace them (just to demonstrate that this is possible) 
  mydatafile = read.csv(str_glue(mycurrentWD,"/thedatafile.csv"))
  
  # Suppose there is a second dataset in a subfolder
  mydatafile = read.csv(str_glue(mycurrentWD,"/subfolder/thedatafile.csv"))
  
  # Suppose there is a dataset in another folder on the desktop and I refuse to set another WD
  mydatafile = read.csv(str_replace(mycurrentWD,"folder","anotherfolder/thedatafile.csv"))
                      
```

## Loading different types of data files

```r
# Example 1: open a file without telling R the file extension
    Mydata = read.table(file.choose(), header = TRUE, sep=",", dec=".")
      # -> With this code I say: let me manually find a folder on my computer, the first row in that file contains the columnnames, data are separated by a "," and a decimal is noted as a "."   
      # -> the above could work to open a csv file.
    
# Example 2 csv file:
    Mydata = read.csv("datafile.csv", na.string=c("","NA","NaN"))
      # -> Optionally I told R that empty cells, cells with "NA", and cells with "NaN" should be read as NA in your datafile (not administered, empty)
   
# Example 3 spss file (.sav)
    mydata = haven::read_sav("datafile.sav")
    # -> You will need the package "haven"
    # -> Here I used a function of the above package without loading it (only works if you have installed the package)
    # -> Of course, feel free to load packages (library(haven))
  
# Example 4: xlsx file (i.e., your typical excel file)
    Library(readxl)
    Mydata=read_excel("datafile.xlsx")
```

## Loading multiple files (same extension) at once
Suppose we have multiple data files such as after conducting a study. We have one data file per participant or test subject. You want to merge all these individual data files into one big data set. In this case, you could make a folder and put all your data files in there. Then you could open R and tell it to list all files (i.e., put every file below one another) that have the same file extension. The example below was specifically made for csv-files.

```r
      temp_data=list.files("folder_with_files/", pattern="*.csv", full.names=TRUE)
      
      # Now we can "glue" everything together:
      Mydata = do.call(rbind, lapply(temp_data, function(x) read.csv(x, stringsAsFactors = FALSE)))
```
