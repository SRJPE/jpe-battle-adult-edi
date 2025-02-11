Data Entry Template for Redd Survey Data
================

**Author**: Badhia Yunes Katz  
**Date**: January 2025

The goal of this document is to provide guidance on data entry for Redd
data entry on Battle and Clear Creek. By having a more unified data
entry, less cleaning will be needed, and therefore EDI updates will run
smoothly.

## General Guidelines

1.  **Date Format**: Use the format `MM/DD/YYYY` for all dates.
2.  **Consistency in Categories**:
    - Use standardized categories for substrate sizes based on the
      Wentworth Scale.
    - Ensure consistent naming conventions for `REACH`, `SPECIES`, and
      other categorical fields.
3.  **Mandatory Fields**: Enter values for all required fields. Use `NA`
    if a value is not applicable or missing.

------------------------------------------------------------------------

## Field Definitions and Allowed Values

| Field Name                   | Data Type   | Description                                 | Allowed Values/Format                                              |
|------------------------------|-------------|---------------------------------------------|--------------------------------------------------------------------|
| `date`                       | Date        | Date of the observation.                    | `MM/DD/YYYY`                                                       |
| `date_visit_2`               | Date        | Date of the second redd visit.              | `MM/DD/YYYY`                                                       |
| `date_visit_3`               | Date        | Date of the third redd visit.               | `MM/DD/YYYY`                                                       |
| `date_visit_4`               | Date        | Date of the fourth redd visit.              | `MM/DD/YYYY`                                                       |
| `date_visit_5`               | Date        | Date of the fifth redd visit.               | `MM/DD/YYYY`                                                       |
| `age`                        | Numeric     | Age of the redd at the time of observation. | Integer values                                                     |
| `age_visit_2`                | Numeric     | Age observed during second redd visit.      | Integer values                                                     |
| `age_visit_3`                | Numeric     | Age observed during third redd visit.       | Integer values                                                     |
| `age_visit_4`                | Numeric     | Age observed during fourth redd visit.      | Integer values                                                     |
| `age_visit_5`                | Numeric     | Age observed during fifth redd visit.       | Integer values                                                     |
| `x_3`                        | Numeric     | Longitude of the observation.               | Decimal format (e.g., -122.12345)                                  |
| `y_4`                        | Numeric     | Latitude of the observation.                | Decimal format (e.g., 47.12345)                                    |
| `river_mile`                 | Numeric     | River mile of the observation.              | Positive decimal values                                            |
| `pre_redd_substrate_in`      | Categorical | Substrate size before the redd.             | “\<0.25”, “0.25-0.5”, “0.5-1”, “1-2”, “2-4”, “4-8”, “8-16”, “\>16” |
| `side_substrate_in`          | Categorical | Substrate size at the redd.                 | Same as `pre_redd_substrate_in`                                    |
| `tailspill_substrate_in`     | Categorical | Substrate size in the tailspill.            | Same as `pre_redd_substrate_in`                                    |
| `pre_redd_depth_in`          | Numeric     | Depth before the redd (in inches).          | Positive numeric values                                            |
| `tailspill_depth_in`         | Numeric     | Depth of the tailspill (in inches).         | Positive numeric values                                            |
| `length_in`                  | Numeric     | Length of the redd (in inches).             | Positive numeric values                                            |
| `width_in`                   | Numeric     | Width of the redd (in inches).              | Positive numeric values                                            |
| `flowmeter_start`            | Numeric     | Start reading of the flow meter.            | Positive numeric values                                            |
| `flowmeter_end`              | Numeric     | End reading of the flow meter.              | Positive numeric values                                            |
| `flowmeter_time_s`           | Numeric     | Time for flow meter reading (seconds).      | Positive numeric values                                            |
| `flowmeter_80_percent_start` | Numeric     | 80% start reading of flow meter.            | Positive numeric values                                            |
| `flowmeter_80_percent_end`   | Numeric     | 80% end reading of flow meter.              | Positive numeric values                                            |
| `flowmeter_80_percent_time`  | Numeric     | Time for 80% flow meter reading (seconds).  | Positive numeric values                                            |
| `species`                    | Categorical | Species observed.                           | “Chinook”, “O.mykiss”                                              |
| `fish_on_redd`               | Boolean     | Observed fish guarding the redd.            | `TRUE`, `FALSE`                                                    |
| `redd`                       | String      | Original identifier for the redd.           | Unique string per redd                                             |
| `redd_loc`                   | Categorical | Location of the redd.                       | Predefined location codes                                          |
| `redd_measured`              | Boolean     | Was the redd measured?                      | `TRUE`, `FALSE`                                                    |
| `why_not_measured`           | String      | Reason why redd was not measured.           | Free text (limit to 100 characters)                                |
| `date_measured`              | Date        | Date when the redd was measured.            | `MM/DD/YYYY`                                                       |
| `flow_fps`                   | Numeric     | Water velocity (fps).                       | Positive numeric values                                            |
| `survey`                     | Categorical | Survey identifier.                          | Predefined survey codes                                            |
| `fork`                       | Numeric     | Fork length (in inches).                    | Positive numeric values                                            |
| `pre_redd_depth`             | Numeric     | Depth before the redd (in inches).          | Positive numeric values                                            |
| `reach_sub_unit`             | Categorical | Sub-unit of the survey reach.               | Predefined sub-unit codes                                          |
| `redd_tail_depth`            | Numeric     | Depth at the redd tail (in inches).         | Positive numeric values                                            |
| `flow_meter`                 | Categorical | Type of flow meter used.                    | Predefined flow meter types                                        |
| `survey_method`              | Categorical | Survey method used.                         | Predefined survey method codes                                     |

## Substrate Classification System

- For the `PRE_SUB`, `SIDE_SUB`, and `TAIL_SUB` fields, **always use the
  standardized size ranges** listed below:

| Standardized Size Range | Substrate Class          |
|-------------------------|--------------------------|
| `<0.25`                 | Fine sand                |
| `0.25-0.5`              | Medium sand              |
| `0.5-1`                 | Coarse sand              |
| `1-2`                   | Very coarse sand         |
| `2-4`                   | Very fine gravel         |
| `4-8`                   | Fine gravel              |
| `8-16`                  | Medium gravel            |
| `>16`                   | Coarse gravel to boulder |

------------------------------------------------------------------------

## Automated Validations

1.  **Substrate Size Consistency**:
    - Ensure all `PRE_SUB`, `SIDE_SUB`, and `TAIL_SUB` values match one
      of the standardized size ranges listed above.
    - Automatically map sizes to their corresponding substrate classes.
2.  **Reach Standardization**:
    - Convert numeric reaches (`1`, `2`, etc.) to the format `R1`, `R2`,
      etc., during processing.
3.  **Missing Data Checks**:
    - Ensure no required fields are left blank. Flag entries with
      missing mandatory values.
4.  **Date Validation**:
    - Verify that `DATE` and `DATE_MEA` are in `MM/DD/YYYY` format.
