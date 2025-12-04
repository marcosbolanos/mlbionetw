FROM rocker/rstudio:devel

RUN Rscript -e "install.packages(c('miic', 'igraph'), repos='https://cloud.r-project.org')"

