# OPTIMISE-AR Shiny App
The OPTIMISE-AR  (incOrporating PaTIent-reported outcoMes In doSE-finding trials–Analysis Recommendations) shiny app is a companion software to the OPTIMISE-AR guidance [1]. It provides an easy to use platform to generate graphics to summarise Patient Reported Outcome (PRO) data for use in dose-finding oncology trial (DFOT) publications. This toolkit is designed to support trialists in line with three key PRO endpoint types:

**Time-to-event PRO endpoints -**
For DFOTs, a PRO objective could look to assess tolerability across doses by identifying whether patients at higher doses experience earlier undesirable PRO events e.g., symptom worsening or dose-limiting events compared to those at lower doses. Such objectives are typically described using time-to-event endpoints.

**PRO endpoints across timepoints -**
For DFOTs, a PRO objective could look to assess tolerability across doses by analysing the longitudinal tolerability profile of a treatment. Such endpoints may assess whether patients at higher doses experience worse tolerability than patients at lower doses at any timepoint within the trial.

**PRO endpoints between timepoints -**
For DFOTs, a PRO objective could look to assess tolerability by analysing tolerability to treatment at two clinically meaningful timepoints across doses. For example, such endpoints may assess whether patients at higher doses experience worse tolerability at the final assessment timepoint than patients at lower doses when compared to baseline.


[1] Alger, Emily, et al. "A practical toolkit with recommendations for analysing and visualising patient-reported outcomes in early phase dose-finding oncology trials (OPTIMISE-AR)." The Lancet Oncology 27.4 (2026): e218-e230.

## To create the development environment (for contributors)
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
  r-svglite \
  r-bsicons \
  r-gridExtra \
  r-survminer \
  r-rmarkdown \
  pandoc
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



## About the app
This app is based on a template created by the ICR RSE team.

Contacts:
*For issues with the app, please contact George Knight - george.knight2@icr.ac.uk
*For issues with hosting, please contact Stacy Shcherbakova - stacy.shcherbakova@icr.ac.uk