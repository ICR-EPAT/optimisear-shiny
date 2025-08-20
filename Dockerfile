##################################################################
# the icrsc/ is the icr's docker account, replace with your own docker user id if you iwsh to push it to docker
# docker build -t icrsc/optimisear-shiny .

# docker push icrsc/optimisear-shiny

# To run the docker image you need to pass in the path to the data, so assimungin the data is at my/path run the docker image like
# docker run -p 3838:3838 icrsc/optimisear-shiny
##################################################################

# 1. Base image
FROM continuumio/miniconda3

# 2. Set the working directory
WORKDIR /app

# 3. Add and create the conda environment
ADD local.yml .
RUN conda env create -f local.yml

# 5. Copy relevant app files
COPY app.R .

# 6. Use the correct environment name (match your .yml file!)
# Assuming your environment name is "env-opt"
SHELL ["conda", "run", "--no-capture-output", "-n", "env-opt", "/bin/bash", "-c"]

RUN apt-get clean && apt-get update --allow-insecure-repositories --allow-unauthenticated \
    && apt-get install -y --no-install-recommends \
    build-essential \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libpq-dev \
    libgdal-dev \
    apt-file && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* /var/cache/apt/archives/*

# 8. Install additional R packages from CRAN and GitHub
# RUN Rscript -e "install.packages('RPostgreSQL', repos='https://cloud.r-project.org')"
# RUN Rscript -e "remotes::install_github('nvelden/NGLVieweR')"

# 9. Expose and run the application
EXPOSE 3838
CMD ["conda", "run", "--no-capture-output", "-n", "env-opt", "Rscript", "-e", "shiny::runApp('/app', port = 3838, host = '0.0.0.0')"]


RUN apt-get clean && apt-get update --allow-insecure-repositories --allow-unauthenticated \
    && apt-get install -y --no-install-recommends 