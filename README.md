# Enhanced_surveillance_questionnaire
Test-negative-case-control COVID-19 vaccine effectiveness study using questionnaire data to investigate presence of potential biases and alternative causal pathways

### Author: Sophie Graham
### Date created: 14/06/2023
### Software used: R version 4.2.2

This study used nationwide vaccination and COVID-19 PCR testing data from one of the first test-negative-case-control studies COVID-19 vaccine effectivness studies conducted in the UK. This data was linked to a questionnaire that had been sent to a sub-sample of the original population to further investigate the potential precense of biases and alternative causal pathways that could have been present in the original study. The original study population included individuals => 70 years of age with a COVID-19 PCR test who had a COVID-19 test in the community with self-reported symptoms and a symptom onset date between 8th December 2020 and 21st February 2021. The questionnaire was sent to all individuals with a PCR test that occured in February 2021. The current study included data from those that responded to this questionnaire. 

* A preprint of the manuscript is available [here](https://assets.researchsquare.com/files/rs-2409555/v1/55eb23e9979706568611d214.pdf?c=1673302876)

                               ### Summary of analysis code
                               (1) **Surveillance Questionnaire R script_github.R**
                                 * Reads in the data
                               * Sets variable classes
                               * Provides a description of patients that are vaccinated versus non-vaccinated and cases versus negative controls
                               * Provides a description for each potential bias
                               * Updates the model accounting for each potential bias separately
                               * Updates the full model accounting for all potential biases at once



