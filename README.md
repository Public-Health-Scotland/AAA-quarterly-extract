# AAA-quarterly-extract


This set of scripts is run quarterly (1st March, 1st June, 1st September, and 1st December) to create a check on the progress of the AAA program. The June extract runs through an additional audit on the records flagged for quality assurance. The March and September processed files are used to create the spring and autumn KPIs, and the September data is used in the annual AAA statistics publication.



There is one processing script, a checking script, a vascular outputs script, a script for the annual QA audit, and an archival script. The archival script (x_recreate_202206.R) was used to quickly create an RDS file to check against files created in R (when first translated) and can be used if previous years need to be recreated.
