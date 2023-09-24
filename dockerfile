FROM rocker/rstudio
RUN apt update && apt install -y man-db manpages coreutils && rm -rf /var/lib/apt/lists/*
RUN yes | unminimize
