# Template Shiny-App for the Shiny-Proxy Service
Demonstration of the bare bones of a shiny app with a docker build file and example shiny-proxy configuration.

## Create the development environment
I use mamba, but you can use conda or micromamba. You can also install directly to base R if you want, I prefer not to as I move environments around a lot.  

**Create the environment**  
`mamba create --name shiny-env -c bioconda r-base=4.3 r-devtools r-shinydashboard r-DT r-bslib r-ggplot2`  

**Activate the environment**  
`mamba activate shiny-env`

**Run the app from the cmd line**
`Rscript -e "shiny::runApp('.')"`

## Additional resources

Document on writing shiny apps: https://mastering-shiny.org/basic-app.html  
