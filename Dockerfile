FROM rocker/rstudio:devel

RUN Rscript -e "install.packages(c('mmic', 'igraph'), repos='https://cloud.r-project.org')"

