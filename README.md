# Summer Project - OPTIMISE-AR Shiny App
This is a template for the OPTIMISE-AR shiny app which you will be working on this summer. It demonstrates the bare bones of a shiny app with a docker build file and example shiny-proxy configuration.

## Create the development environment
To create an environment, we recommend using mamba.

**Create the environment**  
`mamba create --name shiny-env -c bioconda r-base=4.3 r-devtools r-shinydashboard r-DT r-bslib r-ggplot2`  

**Activate the environment**  
`mamba activate shiny-env`

**Run the app from the cmd line**  
`Rscript -e "shiny::runApp('.')"`

## Additional resources
Document on writing shiny apps: https://mastering-shiny.org/basic-app.html  

---

The template was created by the ICR RSE team.

Contacts:
* Stacy Shcherbakova - stacy.shcherbakova@icr.ac.uk
* Rachel Alcraft - rachel.alcraft@icr.ac.uk