# Summer Project - OPTIMISE-AR Shiny App
This is a template for the OPTIMISE-AR shiny app which you will be working on this summer. It demonstrates the bare bones of a shiny app with a docker build file and example shiny-proxy configuration.

## Create the development environment
To create an environment, we recommend using mamba. If you don't have mamba installed on your computer, you can also use conda.

**Create the environment**  
```bash
mamba create --name optimisear-shiny r-base=4.3.0 r-devtools r-shinydashboard r-DT r-bslib r-ggplot2
```  

**Activate the environment**  
```bash
mamba activate optimisear-shiny
```

**Run the app from the cmd line**  
```bash
Rscript -e "shiny::runApp('.')"
```

**Access the app**  
Type in `http://127.0.0.1:4318` in your preferred browser to access the website locally.

## Additional resources
Document on writing shiny apps: https://mastering-shiny.org/basic-app.html  

Examples of shiny apps:
- SYnthetic Lethal Vulnerabilities Exhibiting Reciprocation
    - Website: https://software.icr.ac.uk/app/sylver
    - Repo: https://github.com/instituteofcancerresearch/SYLVER


## About the template
The template was created by the ICR RSE team.

Contacts:
* Stacy Shcherbakova - stacy.shcherbakova@icr.ac.uk
* Rachel Alcraft - rachel.alcraft@icr.ac.uk