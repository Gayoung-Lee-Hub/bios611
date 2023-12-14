FROM rocker/rstudio

RUN Rscript -e "install.packages(c('tidyverse', 'ggplot2'))"
RUN Rscript -e "install.packages('RColorBrewer')"
RUN Rscript -e "install.packages('ggridges')"
RUN Rscript -e "install.packages('cowplot')"
RUN Rscript -e "install.packages('png')"
RUN Rscript -e "install.packages('grid')"
RUN Rscript -e "install.packages('gridExtra')"
