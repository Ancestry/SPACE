# SPACE is a tool for dynamically interacting with PCA plots


# Plotting with SPACE
# 1) Run PCA with your favorite PCA or approximate PCA tool

# 2) Prepare a File for SPACE.
#    The file format is tab delimited text. PC data should
#    be in columns with headers PC1, PC2, PC3 etc.
#    Any additional columns are considered labels.
#    Any number of PC columns and label columns will work
#    Columns can be in any order
#    IDs aren't required by SPACE, but will be displayed

# Example:
id	PC1	PC2	PC3	PC4	status	continent
1	.027	0.33	-0.12	0.98	case	Europe
2	.011	0.31	-0.98	0.71	control	Europe
3	.093	0.23	-0.33	0.13	case	Australia
4	.021	0.45	-0.10	0.56	control	Asia

# Running SPACE locally (Windows)
# 1) Put ui.R and server.R into a directory, e.g. C:\Users\nberkowitz\R_projects\SPACE

# 2) Open an R shell
> install.packages("shiny") # only needs to be run once
> library(shiny)

> setwd("C:\Users\nberkowitz\R_projects\") # navigate to the parent of your new directory

> runApp("SPACE")

# A browser window should open with SPACE running.


# Running SPACE on a server

#1) Install R and shiny server
# Detailed instructions here: http://docs.rstudio.com/shiny-server/

# Install R (if it's not already installed)
> sudo apt-get install r-base

# Install shiny. Alternately install it from an R prompt as shown above.
> sudo su - -c "R -e \"install.packages('shiny')\""

# install gdebi-core and shiny-server
> sudo apt-get install gdebi-core
> sudo gdebi shiny-server-1.5.4.deb

#2) Start the shiny server
> sudo systemctl start shiny-server

#3) Set up the SPACE files
# create a new directory in /srv/shiny-server e.g.
> mkdir /srv/shiny-server/shiny_pca

# put ui.R and server.R in the new directory

#4) Navigate to to your server in a browser. By default shiny server runs on port 3838
http://127.0.0.1:3838/SPACE/

#5) Done! If you want to edit the code, just save and refresh to see your changes

