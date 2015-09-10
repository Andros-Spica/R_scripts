#################################################################
# Analysis of ceramic compositional data - Some alternative protocols
# by Andreas Angourakis (andros.spica@gmail.com)
#################################################################

# General notes on R:
# - How to assign something to a R object:
# almost_AnyText.BeginningWithALetter <- almostAnything
# - In some cases, its also possible to use: almost_AnyText.BeginningWithALetter = almostAnything
# - For R to read your commands you must enter them in the "console" window (after the > at the bottom), or
# Ctrl+Enter while placing the selection in a line of your script, or Ctrl+r to run several lines selected in the script.
# - You can see that entering # in a command line will consider anything at its right to be a "comment",
# i.e. when running your script R will just print it on the console.
# - Any function (not created by your own script) is documented in an on-line database.
# You can access this documentation with: help(functionName) or ?functionName .
# - I highly recommend to use RStudio, since it balances this "cold command environment"
# with a lot of useful utilities, including a good window layout. Additionally, this script
# will be much easier to read in RStudio, since it also colour-code the text.
# - Before doing anything in R, you must always set your working directory (a folder where you will read
# and write files). In the basic user interface you need to set it in "File>Change Dir..." and
# in RStudio you must do it only one time in "Session>Set Working Directory>Choose Directory...",
# given that you save your session when closing.
# - a key aspect of R is the variety of objects or data types. The main are vectors, matrices,
# data frames and lists). I recommend reading http://www.statmethods.net/input/datatypes.html
# - The value of "scripting" is that at any time you can re-do your operations from the beginning,
# or from those points where you stored data in an intermediate object.

# First, load all packages required (to do it at the beginning of the script is completely optional,
# the only condition is that you do it before calling any function of the package).
# If you try to use a function before importing the respective package, an error will be thrown.
# Furthermore,  online documentation on a function (help(functionName) or ?functionName)
# can only be used if the package is loaded.
# Note: the function "require('package name')" checks if the packeage is already loaded and if not, load it.
# An alternative is to use "library('package name'), which just load it directly.

# Packages on compostional data
require(compositions)
require(robCompositions) # I find robCompositions easier to use
require(zCompositions) # the zCompostions is mostly focused in replacing missing values
# other useful packages
require(MASS)
require(ade4)
require(pcaPP)

# Note: if you are using RStudio, notice that you can use lines beggining with "#####" (at least 5 #)
# to build sections in your script and collapse them when you are not using them
# by clicking on the small triangle beside the line number.
# Try doing it with line 51. Can you see the title of the next section?
###########################################################
# Read data
###########################################################

# We assign to a new object "compdata" a "data frame" filled with the content of a csv
# (see
?read.csv
# and read http://www.statmethods.net/input/importingdata.html
# to check all options available)
# In this case, "yourData.csv" is expected to have the row names at the first column
# and the variables names at the first row (the default for read.csv is "header=TRUE").
compdata <- read.csv("FolderInTheWorkingDirectory/yourData.csv",row.names=1)

data(expendituresEU) # expendituresEU is a data frame included in the robCompositions package
compdata <- expendituresEU # here we'll use it for demostration

# A data frame is a matrix which is formated as a set of columns and rows, each with a index and name
# which will be stored by a property called "dimnames":
dimnames(compdata) # a list object with the two vectors with the dimensions (rows and columns) names
names(compdata) # the vector with the column names (equivalent to: dimnames(compdata)[[2]])
rownames(compdata) # the vector with row names (equivalent to: dimnames(compdata)[[1]])
# You can also see the structure of the data frame object:
str(compdata)
# Also read http://www.statmethods.net/input/contents.html
# The columns (variables) of a data frame are vectors that can be filled with any type of data.
# (numeric, character, logical or Boolean and factor).
# A numeric is any type of number (e.g. 1, 3.6, 1E-6, 1/3).
# A character is any type of text (e.g. "A", "1234", "a Character is always within (double or single) quotation marks").
# A logical or Boolean is either TRUE or FALSE.
# A factor is simply a numeric vector mapped to characters (e.g., "Rim"=1, "Boby"=2, "Base"=3)
# You can refer to a single variable by writing:
compdata$variableName # it refers to the column name.
# or
compdata[,variableIndexNumber] # it refers to the column index using the structure: data[rowIndex,columnIndex]
# NOTE: This last option can also be used to refer to rows (passing the rowIndex)
# or to a single cell (passing both rowIndex and columnIndex)

# use summary in a data frame object to obtain the summary statistics
# (min,max,mean,median,quartils) of each variable
summary(compdata)

# In the case of compositional data, you should see that all variables are of type "numeric" (if there are decimals)
# or int (if they are whole numbers).
# If somehow R wrongly interpreted a variable, you can coherce variables into another type by running.
compdata$variableName <- as.numeric(compdata$variableName)
# See http://www.statmethods.net/management/typeconversion.html
# and also WATCH OUT when converting "factor" types
#(http://www.dummies.com/how-to/content/how-to-convert-a-factor-in-r.html).

# Using a little bit of programming (a "for loop"), you can easily convert some selected variables.
numVars <- c(1,2,5,10) # c() function combines a set of elements to form a vector.
for (v in numVars){ # Literally, "for each element in the vector numVars".
  # Here, we refer to the elements as "v", but any valid object name is possible.
  # In this case, "v" is the column index number of the variables we want to coherce.
  # We first coherce it into a character vector to garantee that if "compdata[,v]"
  # is a factor it will be corecctly converted into numeric (see reference above on converting factor types).
  compdata[,v] <- as.numeric(as.character(compdata[,v])) 
}

# You could automatically convert all variables into numeric by transforming the data frame into a matrix.
compdataMatrix <- data.matrix(compdata)
# We should then reconvert the new numeric matrix back into a data frame
compdata <- data.frame(compdataMatrix)

# At this point you should have a data frame with numeric named variables and named entries or rows

####################################################################
# Cleaning data
####################################################################

# you may want to discart some variables. Here are some options:

## using variable indexes
# create a vector with the indexes
indexVariables <- c(3,6,15)
## exclude variables using a vector of index numbers
compdata <- compdata[,-indexVariables] # the "-" operator indicates that you want to exclude these variables
# On the contrary, if you want to select them, and exclude the others, you just run it without the "-"
compdata <- compdata[,indexVariables]

## exclude variables using a vector of variable names
nameVariables <- c("Recreation","Health","Housing")
for (i in nameVariables){ # notice that in this "for loop" we use "i" as element reference (its completely arbitrary!)
  compdata <- compdata[,names(compdata)!=i] # We must position the logical statement where we refer to column index
  # Note: "!=" means "not equal to". WARNING: the opposite statement is "==", not "=".
  # see http://www.statmethods.net/management/operators.html
}

## filter constant variables (or according to some criteria)

out <- vector() # we create a empty vector to hold the indexes of variables to exclude

# Instead of giving the "for loop" a specific vector, we give it a sequence of numbers
# from 1 to the number of columns in our data frame (see ?ncol).

# Note: "firstNumber:secondNumber" returns a ordered vector starting with "firstNumber"
# and adding up this number until it reaches "secondNumber":
1:10
# It also works with decimals, for example try
1.5:10

# This is most useful when setting iterations in loops, as in this case
for (v in 1:ncol(compdata)){ 
  # "if statements" specifies that something should be done only given some conditions,
  # which must be stated within parentesis: if (condition is true) { do this }
  # NOTE: its also possible to specify what should be done if the condition is not true:
  # if (condition is true) { do this } else { do that }
  # "sd()" returns the standard deviation of the data contained in a vector (see ?sd)
  # This function has an additional logical parameter "na.rm" (or "non-numeric remove"),
  # that if set to "TRUE" will ignore missing/non-numeric values.
  if (sd(compdata[,v],na.rm = TRUE)==0){ # Literally, if the variable with index "v" is constant
    out <- c(out,v) # add the variable index to our vector of variables to exclude
  }
}
# Finally, exclude the constant variables
compdata <- compdata[,-out] 

## filter by a character or factor variable (for example, if you are considering site or period)
compdata <- subset(compdata,compdata$SomeCategoricalVariable=="aValueOfTheCategoricalVariable")
# the "subset(data, criterium)" function subsets a data frame using a logical criterium (to be either TRUE or FALSE)

# NOTE: In subset(), its not necessary to refer to a categorical variable in the same data frame.
# Suffies to use any vector with the same lenght as number of rows in the data frame,
# and of course the same order of cases. For example:
df <- data.frame(cbind(1:10,11:20,21:30))
str(df)# number of rows or observations = 10 = nrow(df) # number of columns or variables = 3
aCriterium <- sample(c(TRUE,FALSE), nrow(df), replace=T) # a vector with "nrow(df)" random logic values. See ?sample
df <- subset(df,aCriterium)

# filter known outliers (or any entry that you don´t want in your analyses)
outliers <- c("RowName1","RowName12","RowName29")
for (i in 1:length(outliers)){
  compdata <- subset(compdata, row.names(compdata)!=outliers[i])
}
# you can also do it by row index
outliers <- c(1,12,29)
compdata <- compdata[-outliers,] # notice that we must reference ROW index before the comma

# the robCompositions package has a function that helps identifying outliers,
# based in robust Mahalanobis distance. It implicitly performs an isometric logratio transformation (see below)
outliers <- outCoDa(compdata)
print(outliers) # prints in the console information about the outliers detected
plot(outliers) # plot each individual composition (detected outliers are represented with '+')
str(outliers) # observe how this object is structured

# You may want to single out the outliers by adding labels
outliersNumIndex <- 1:nrow(compdata)
outliersNumIndex <- outliersNumIndex[outliers$outlierIndex]
plot(outliers) # plot each individual composition (detected outliers are represented with '+')
text( # this is a function that allows to place text in the last plot created (if the graphic device is still open)
  x = outliersNumIndex, y = outliers$mahalDist[outliers$outlierIndex], # the position of outliers in the plot
  label=row.names(compdata)[outliersNumIndex], # the row name of outliers as the content of labels
  adj=c(-0.2,0.5), col="red"
  )

# the element "outlierIndex" in the outliers object is a vector with logical values (TRUE/FALSE)
# and can be set as a filter criterium in subset()
compdata.noOutliers <- subset(compdata, !outliers$outlierIndex)
# Notice that we must negate ("!") this vector in order to keep those cases that aren't outliers

####################################################
# Imputation of missing values
####################################################
# we will artificially generate a data set with 10 missing values (chosen at random)
compdata.withMissingValues <- compdata
for (i in 1:10) {
  rowIndex<-runif(1,max=nrow(compdata)) # runif() generates random numbers from a uniform distribution
  colIndex<-runif(1,max=ncol(compdata))
  compdata.withMissingValues[rowIndex,colIndex]<-NaN
}

# With the robCompositions package
compdata.withImputed1 <- impCoda(compdata.withMissingValues)

# with zCompositions
# to use this package missing values must be represented as zeros
compdata.withMissingValues[compdata.withMissingValues==NaN] = 0
compdata.withImputed2 <- cmultRepl(compdata.withMissingValues)

####################################################
# transformations
####################################################

## closure (rows to constant sum, e.g. 100)
# using compositions package
compdata.clo <- clo(compdata, total=100) # here you must recover the variable names
# using robCompositions package
compdata.clo <- constSum(compdata,const=100) # here variable names are preserved



## standarization (normalizing and centering)
compdata.std <- scale(compdata.clo)
str(compdata.std)
class(compdata.std) # class(object) allows to see to which R class (data.frame, matrix, vector, etc) a object belongs
# notice that scale() does not returns a proper data frame
# to use it as a data frame:
compdata.std <- data.frame(scale(compdata.clo))
# see ?scale

## to log-scale
compdata.log <- data.frame(log10(compdata.clo)) # as before, we must convert it to a data frame object

## Additive Logratio transformation
# using robCompositions package
compdata.alr <- addLR(compdata.clo, 1) # using the first variable as the ratio denominator
# Notice that this and the clr logratio transformations in robCompositions return a list, not a data frame:
str(compdata.alr)
# to obtain a comparable data frame we need to get the element "x.alr" as a data frame:
compdata.alr <- data.frame( addLR(compdata.clo, 1)$x.alr )
# regarding logratios, its important to express that the new variables are not proportions of a total, but logratios.
# For this purpose, it's good practice to change their names by adding a specific label
names(compdata.alr)<-paste("ALR-",names(compdata.alr),sep="") # "paste()" allows us to combine text ("character") elements

## Centered Logratio transformation 
# using robCompositions package
compdata.clr <- data.frame( cenLR(compdata.clo)$x.clr )
names(compdata.clr)<-paste("CLR-",names(compdata.clr),sep="")

## Isometric Logratio transformation 
# using robCompositions package
compdata.ilr <- data.frame( isomLR(compdata.clo) )
names(compdata.ilr)<-paste("ILR-",names(compdata.ilr),sep="")

# to save any new data into files use:
write.csv(compdata.clo, file="FolderInTheWorkingDirectory/yourClosedData.csv", row.names = TRUE)
# see ?write.csv for more options

####################################################
# Basic and not-so-basic plotting
####################################################

## Simple scatter plot
# plot a scatter plot with the first column variable in the X-axis and the second in the Y-axis
plot(compdata[,1],compdata[,2], xlab=names(compdata)[1], ylab=names(compdata)[2])

## Pairs or matrices of scatter plots
# a matrix of scatter plots for the first five variables (the variable names are placed in the diagonal)
pairs(compdata[,1:5]) # most correlations are positive...
# but they may be "spureous correlations"
# since more expenditure in one item generally imply more expenditure in the others.

# compare it with the closed data
pairs(compdata.clo[,1:5])
# compare it with the transformed data
pairs(compdata.std[,1:5])
pairs(compdata.log[,1:5])
pairs(compdata.clr[,1:5])
# any of these show, for instance, that
# there is a true positive correlation between Food and Alcohol expenditures,
# now regardless the total amount of expenditures.

## Customising scatter plots by adding extra elements
# Below, we will create a 2x2 layout of plots, one for each format of the data
# and, by calculating a linear regression model for each case, we will draw
# their respective line and display their properties

# First, since we must repeat the same processes for each case, we should declare a function,
# so we can use it whenever we desire.
# To build a function we simply assign to an object the expression
# function( parameters ) { linesOfCode }
lmToPlot <- function(data,dependentVariableIndex,independentVariableIndex) {
  # Create the linear regression model
  model <- lm(data[,dependentVariableIndex]~data[,independentVariableIndex])
  # lm() creates a object of class 'lm" or linear model using a "formula"
  # "formula" in R are generally something like "A ~ B" or "A ~ B + C", where A, B and C  are variables
  # NOTE: the values you may be interest in a linear regression model (such as the R-Squared)
  # are not in the object itself, but in the call "summary(model)"
  
  # lets add to this object a text element breaked into two
  model$display <- "\n" # \n breaks the line (while \t adds a tabulation)
  
  # lets add to the first line the equation y = a -bx, using the intercept and the reg. coefficient
  # NOTE: we will use "as.character(round(value, digits=3))" to obtain a text with a number rounded to 3 decimals
  # We use abs() in the case of b, so we can set the sign (+-) as a text afterwards
  a = as.character(round(summary(model)$coefficients[1,1], digits=3)) # to obtain a or the intercept
  b = as.character(round(abs(summary(model)$coefficients[2,1]), digits=3)) # to obtain b or the reg. coefficient
  # mark their significance (p<0.05)
  signif = if (summary(model)$coefficients[1,4]<0.05) "* " else " " # this is another way of using "if statements"
  a = paste(a,signif,sep="")
  signif = if (summary(model)$coefficients[2,4]<0.05) "* " else " "
  b = paste(b,signif,sep="")
  b.sign = if (summary(model)$coefficients[2,1]<0) " - " else " + "
  
  model$display <- paste(names(data)[dependentVariableIndex], # the name of the dependent variable
                         " = ", a, b.sign, b, # the coefficients
                         names(data)[independentVariableIndex], # the name of the independent variable
                         model$display,sep="") # the line breaker we added before
  
  # lets add to the second and third text lines the value of R-Squared and adjusted R-Squared
  r.sq = as.character(round(summary(model)$r.squared, digits=3))
  adj.r.sq = as.character(round(summary(model)$adj.r.squared, digits=3))
  model$display <- paste( model$display, # the text added until now, with the line breaker at the end
                         "R-Squared = ", r.sq, # the R-Squared
                         "\nadjusted R-Squared = ", adj.r.sq, sep="") # the adjusted R-Squared
  
  # finally, we return the model object
  return(model) 
}
# notice that you can collapse the lines within {}, for instance the code inside a function,
# by clicking in the triangle

## lets set some parameters to use in all four plots
# this is the scale in which the display will be printed out
cexmodel = .8
# these are the proportion (between 0 and 1) of the range of variation in each axis
# marking the position of the display (text) of each linear regression model
# For instance, xPos = 0.5 correspond to the middle of the horizontal axis
xPos = .5
yPos = .75

# to create composite layout of plots:
layout(matrix(c(1,2,3,4),2,2,byrow=T))
# "layout()" accepts a matrix that requires
#  a set of elements ("c(1,2,3,4)")
#  the number of rows and columns ("2,2")
# and how we want elements fo fill the matrix ("byrow=TRUE")
# the first plot (raw data)
plot(compdata[,1],compdata[,2], xlab=names(compdata)[1], ylab=names(compdata)[2], main="raw data")
m<-lmToPlot(compdata,2,1)
abline(m, col="red")
text(x=((max(compdata[,1])-min(compdata[,1])) * xPos)+min(compdata[,1]),
     y=((max(compdata[,2])-min(compdata[,2])) * yPos)+min(compdata[,2]),
     label=m$display, cex=cexmodel)
# NOTE: a "graphical device" is called by plot() or similar functions.
# If there's no device open, functions such abline() or text(), that draw additional elements, will not work.
# the second plot (standardized data)
plot(compdata.std[,1],compdata.std[,2], xlab=names(compdata.std)[1], ylab=names(compdata.std)[2], main="standardized\ndata")
m<-lmToPlot(compdata.std,2,1)
abline(m, col="red")
text(x=((max(compdata.std[,1])-min(compdata.std[,1])) * xPos)+min(compdata.std[,1]),
     y=((max(compdata.std[,2])-min(compdata.std[,2])) * yPos)+min(compdata.std[,2]),
     label=m$display, cex=cexmodel)
# the third plot (log-transformed data)
plot(compdata.log[,1],compdata.log[,2], xlab=names(compdata.log)[1], ylab=names(compdata.log)[2], main="log-scaled\ndata")
m<-lmToPlot(compdata.log,2,1)
abline(m, col="red")
text(x=((max(compdata.log[,1])-min(compdata.log[,1])) * xPos)+min(compdata.log[,1]),
     y=((max(compdata.log[,2])-min(compdata.log[,2])) * yPos)+min(compdata.log[,2]),
     label=m$display, cex=cexmodel)
# the fourth plot (centered logratio transformed data)
plot(compdata.clr[,1],compdata.clr[,2], xlab=names(compdata.clr)[1], ylab=names(compdata.clr)[2], main="clr-transformed\ndata")
m<-lmToPlot(compdata.clr,2,1)
abline(m, col="red")
text(x=((max(compdata.clr[,1])-min(compdata.clr[,1])) * xPos)+min(compdata.clr[,1]),
     y=((max(compdata.clr[,2])-min(compdata.clr[,2])) * yPos)+min(compdata.clr[,2]),
     label=m$display, cex=cexmodel)

# For more options on basic scatter plots, see http://www.statmethods.net/graphs/scatterplot.html

####################################################
# Ternary diagrams (using robCompositions)
####################################################

ternaryDiag(compdata[,c(1,2,3)])
ternaryDiag(compdata.clo[,c(1,2,3)])
# the ternary diagram show that cases vary mostly
# in the ratio between food and clothing expenditures

# NOTE: ternary diagram are to be used only with raw or closed data,
# not with transformed data (standardized, log-transformed, logratio-transformed).
# The data must have a positive sum.

?ternaryDiag # for more options (grouping, drawing lines, etc)

####################################################
# Statistical tests (using robCompositions)
####################################################

# Anderson-Darling Normality Tests
adtest(rnorm(100)) # see that a normal distribution will return a non significative p-value (>0.05)
# which means that the null hypothesis (that this data was drawn from a normal distribution)
# cannot be rejected
# Note: rnorm() generates random numbers for a normal distribution. see ?rnorm
adtest(compdata) # While our expenditures data is clearly not normally distributed
# This is relevant, given that some statistical methods and tests assumes normality

####################################################
# Principal Components Analysis
####################################################

pca <- pcaCoDa(compdata.clo, method = "robust")
# see in ?pcaCoDa:
# this function will perform a isometric logratio transformation,
# calculate the principal components, and then back transform the result
# so its possible to display the original variables as arrows

plot(pca)
# calling plot() with a pcaCoDa object will call another function, plot.pcaCoDA()
# in the robCompositions package. Its a biplot...

# the default function to do a PCA is princomp().
# Using this function,
# compare with the classical pca done directly with clr-transformation:
pca.clr <- princomp(compdata.clr)
biplot(pca.clr)
# or the robust version
robpca.clr <- suppressWarnings(princomp(compdata.clr, covmat=covMcd(compdata.clr, cor=FALSE), cor=FALSE))
biplot(robpca.clr)
# or the robust pca displayed without back-transforming to clr space
robpca.ilr <- suppressWarnings(princomp(compdata.ilr, covmat=covMcd(compdata.ilr, cor=FALSE), cor=FALSE))
biplot(robpca.ilr)

# Its better to plot them all together
layout(matrix(c(1,2,3,4),2,2,byrow=T))
plot(pca, main="Robust PCA - ILR->CLR")
biplot(robpca.ilr, main="Robust PCA - ILR")
biplot(pca.clr, main="PCA - CLR")
biplot(robpca.clr, main="Robust PCA - CLR")

# my opinion is that such biplots are often quite ugly and difficult to customize
# notice for example that some labels will overlap or even disappear beyond the plot borders,
# or simply that there is too much going on near the origin (i.e. point 0,0)

# lets check the elements of our pca, so we can use them to draw our own plots
str(pca)
# scores: the coordinate for each case (27) in each principal component (11)
# loadings: the coordinate for each variable (12) in each principal component (11)
# eigenvalues: this is the eigenvector with the eigenvalue used for each principal component (11)
# method: the pca method we used
# princomOutputClr: is the pca done with the clr-transformation
