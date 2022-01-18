# Sociosemantic as mutualist network

This Github repository contains code and materials related to the article "Socio-semantic networks as mutualistic networks".

All the scripts to replicate the paper can be found under `src`. The project requires both python and R to replicate. After having clone the repositoty, one must run the following lines in a R console and on the command line, respectively:

```r
renv::restore()
```
```sh
conda env create --file socsemics-enron.yml
```

`src/1_1-preprocessing.py` and `src/1_2-preprocessing` collectively preprocess the raw data into a clean social network and corpus. The fact that two separate files in two different languages are needed to clean up the dataset has to do with the sensitive dependency on the initial conditions of the project. This may change in the future to do it all in one language.

The scripts `src/plot_fig*` reproduce the figures found in the article. 

Finally, `src/4-mutualism.R` is a script to systemically compare our matrix to true mutualist networks found on the [web of life database](http://www.web-of-life.es/). The results are discussed in the supplementary materials.


## Data

 - Download `enron-mysqldump2.RData` from [Arne Hendrik Ruhe website](http://www.ahschulz.de/enron-email-data/) and put it in `raw-data`.
 - Call `make all` to preprocess the raw-data.
