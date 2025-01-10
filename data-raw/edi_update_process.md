JPE Battle Adutl EDI Upload Workflow
================

This guide provides step-by-step instructions to update the Battle Creek
Adult EDI package with new redd data.

## Navigating to the Repository

All data processing and EDI uploads are managed in the same
repository.  
1. Navigate to the [project
repository](https://github.com/FlowWest/jpe-battle-adult-edi) and clone
it to your local machine.

## Adding Data

2.  Assuming data was entered following the structure established on
    [Data Entry Template for Battle Creek Redd Survey
    Data](https://github.com/SRJPE/jpe-battle-adult-edi/blob/dec-2024-updates/data-raw/data_input_instructions.md),
    save the xlsx document on the
    [data-raw](https://github.com/SRJPE/jpe-battle-adult-edi/tree/main/data-raw)
    folder using `redd_yyyy_data.xlsx` naming convention (e.g
    redd_2024_data.xlsx).

## Processing Data

3.  Update the new data file name from line 31 of
    `clean_data_for_updates.R` (located
    [here](https://github.com/SRJPE/jpe-battle-adult-edi/blob/dec-2024-updates/data-raw/clean_data_for_updates.R))
    to match name set on step 2.

4.  Run the code

- This script cleans and consolidates data to ensure consistency and
  accuracy across different years.
- The cleaned data and metadata are saved as CSV files in the project
  repository’s `data` folder:
  - `battle_redd.csv`
  - `battle_redd_summary.csv`

### Summary of CSVs Generated for EDI Upload

- `battle_redd.csv`
- `battle_redd_summary.csv`
- `battle_upstream_passage_raw.csv` (this will not change)
- `battle_upstream_passage_estimates.csv` (this will not change)

## EDI Update/Upload

- Before starting this process, make sure you have an EDI account setup
- Create or edit your .Renviron file to incluse your EDI username and
  password. To do this, enter the following code:
  `usethis::edit_r_environ()`. This will open your .Renviron file. Add a
  line for`edi_user_id = [enter your user name]`, and
  `edi_password = [enter your password]`

The data upload to EDI is handled in the `make_metadata_xml.R` script.
The necessary modifications include:

5.  Change the EDI package number at line 36
    (`edi_number <- "edi.0000.1"`) to the new version number. For
    example, if the current version is `edi.0000.1`, change it to
    `edi.0000.2`.
6.  After successfully running the script, manually evaluate the package
    by logging in to the [EDI
    website](https://portal.edirepository.org/nis/login.jsp) portal, and
    navigating to the Tools tab and click on Evaluate/Upload Data
    Packages.
7.  Add .xml file under EML Metadata File, select “manually upload data”
    under Data Upload Options. Click “Evaluate” button.
8.  Attach corresponding csv file and click “Evaluate”. Check for any
    errors, warning messages are generally okay.
9.  After evaluating the package without any errors, return back to
    `make_metadata_xml` script and update line 75 to the version number
    that will be updated. On this example, package version 0000.1 will
    be updated. For the next update, this code will have the next most
    recent package version number (i.e 0000.2, 0000.3, 0000.4, etc):
    `EMLaide::update_edi_package(Sys.getenv("edi_user_id"), Sys.getenv("edi_password"), "edi.0000.1", paste0(edi_number, ".xml"))`
10. Uncomment this code line, and run it (note: running this code will
    automatically upload the EDI package. Packages can not be
    overwritten, so if any changes are needed, both (1) new edi number
    on line 36 and (2) update_edi_package on line 75 will have to be
    updated, and script has to be ran again)

## EDI Upload Check

To verify the new package upload, navigate to the [EDI repository
portal](https://portal.edirepository.org/nis/home.jsp) and search for
the updated package.