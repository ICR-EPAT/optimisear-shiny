# Summer Project - OPTIMISE-AR Shiny App
This is a template for the OPTIMISE-AR shiny app which you will be working on this summer. It demonstrates the bare bones of a shiny app with a docker build file and example shiny-proxy configuration.

## Create the development environment
To create an environment, we recommend using mamba. If you don't have mamba installed on your computer, you can also use conda.

**Create the environment**  
```bash
conda create -n optimisear-shiny -c conda-forge \
  r-base=4.3.2 \
  r-shiny \
  r-ggplot2 \
  r-shinydashboard \
  r-dt \
  r-magrittr \
  r-dplyr \
  r-ggsurvfit \
  r-ggh4x \
  r-tidyr \
  r-gt \
  r-svglite
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
Type `http://127.0.0.1:XXXX` (where XXXX is the port number provided when you start the app) into your preferred browser to access the website locally.

## Additional resources
1) Alma Cook Book: https://almacookbook.github.io/   
Alma Cook Book provides a comprehensive guide on how to set up environment, how to set up a container and more.

2) RSE Team "Git for Beginners" course: https://github.com/ICR-RSE-Group/2025-ICR-GIT-beginners 

3) Document on writing shiny apps: https://mastering-shiny.org/basic-app.html  

4) Examples of shiny apps:
- SYnthetic Lethal Vulnerabilities Exhibiting Reciprocation
    - Website: https://software.icr.ac.uk/app/sylver
    - Source code: https://github.com/instituteofcancerresearch/SYLVER

- MCran
    - Website: https://software.icr.ac.uk/app/mrcan  
    - Source code: https://github.com/instituteofcancerresearch/MRCan 

- 8q24 
    - Website: https://software.icr.ac.uk/app/icr8q24 
    - Source code: https://github.com/oncogenetics/8q24  

- Reversions Database:
    - Website: https://software.icr.ac.uk/app/reversions-web  
    - Source code: https://github.com/instituteofcancerresearch/reversions-web?tab=readme-ov-file  

- Locus Explorer:
    - Website: https://software.icr.ac.uk/app/locus-explorer 
    - Source code: https://github.com/oncogenetics/LocusExplorer?tab=readme-ov-file 

## About the template
The template was created by the ICR RSE team.

Contacts:
* Stacy Shcherbakova - stacy.shcherbakova@icr.ac.uk
* Rachel Alcraft - rachel.alcraft@icr.ac.uk
