# Causal inference, social networks, and chain graphs
Elizabeth L.Ogburn, Ilya Shpitser, and Youjin Lee

## Data 
Raw data is available at Washington University Law School's Supreme Court Database (http://scdb.wustl.edu/data.php), summarized version is at ``Data/AllCase.csv`` and we used the subset of the data (``Data/longdata.csv``) corresponding to the Second Rehnquist Court (1994- 2004).

## Reproducibility and Replicability
- ``Code/DAGmodel.R``: Generate the simulated data following DAG and test (conditional) independence between non-adjacent pairs.
- ``Code/ReadSupreme.R``: Extract the subset of the data from the raw data.
- ``Code/SupremeModel.R``: Learn a chain component using ``bnlearn`` and estimate the parameters associated with main effect, treatment effect of each issue area, and two-way interaction effects. 
- ``Code/SupremeResult.R``: Provide a probability table presenting the causal probability of collective outcomes.
- ``Code/SupremeSimModel.R``: Generate counterfactual covariates, treatments, and outcomes using a chain graph model. Parameters to generate the data depend on the real data.
- ``Code/SupremeSimResult.R``: Based on the estimates, drive probabilities associated with potential outcomes using a g-formula.

## Simulated data
Exemplary data are available at ``Data/``.

## Reference
Scutari, M., & Ness, R. (2012). bnlearn: Bayesian network structure learning, parameter learning and inference. *R package version*, 3.