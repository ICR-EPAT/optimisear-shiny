##################################################################
# the icrsc/ is the icr's docker account, replace with your own docker user id if you iwsh to push it to docker
# docker build -t icrsc/demo-shiny .

# docker push icrsc/demo-shiny

# To run the docker image you need to pass in the path to the data, so assimungin the data is at my/path run the docker image like
# docker run --rm --name eg-r -p 3838:3838 -v ./data:/app/data icrsc/demo-shiny
##################################################################

# 1. Base image
FROM rocker/r-ver:4.3.2

# 2. System updates and installations
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \ 
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    apt-file \
    && rm -rf /var/lib/apt/lists/*

# 3. Set the working directory
WORKDIR /app

# 4. Copy relevant app files
# Copy relevant app files
RUN mkdir www
COPY www ./www
COPY app.R .

# 5. Install R dependencies
RUN Rscript -e "install.packages('rspm') ; rspm::enable() ; install.packages(c('devtools', 'shinydashboard', 'DT', 'shinythemes', 'stringr', 'htmlTable', 'ggplot2'))" 

# 6. Run the application
EXPOSE 3838
CMD Rscript -e "shiny::runApp('/app', port = 3838, host = '0.0.0.0')"
# Should then be visible on http://localhost:3838/