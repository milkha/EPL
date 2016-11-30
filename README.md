# Hierarchical Bayesian Modeling of the English Premier League

For the description of the model and results see "epl.pdf". 

All the code for generating this report is in the R markdown file "epl.rmd".

The Stan model is in "stan_model.stan".

The DATA (teams' previous and current performances) are in the DATA folder.

The Stan fits (for each week) are saved in a folder with the name FITS (will be created if it does not exist).

The code to read/munge data and fit the Stan model is also present in the file "epl.R"
