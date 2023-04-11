# ChildhoodCancerDataInitiative-Tempate_UpdateR
This script will take a CCDI metadata manifest file and transfer the contents to a different version of the CCDI metadata manifest.

To run this script, please obtain a [CCDI Submission Template](https://github.com/CBIIT/ccdi-model/tree/main/metadata-manifest).

To run the script on a CCDI template, run the following command in a terminal where R is installed for help.

```
Rscript --vanilla CCDI-Template_UpdateR.R -h
```

```
Usage: CCDI-Template_UpdateR.R [options]

CCDI-Template_UpdateR v1.0.0

Options:
	-f CHARACTER, --file=CHARACTER
		dataset file, CCDI_submission_metadata_template.xlsx

	-t CHARACTER, --template=CHARACTER
		dataset template file, CCDI_submission_metadata_template.xlsx (likey a newer version)

	-h, --help
		Show this help message and exit
```
