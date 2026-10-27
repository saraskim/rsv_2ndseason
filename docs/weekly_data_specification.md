# Weekly records dataset

## Structure and follow-up

Include one row per infant per fully observed week, beginning at birth and continuing through the second-season administrative end date or the first enrollment gap, whichever occurs first. Include the interval between seasons. Stop at the last fully observed week; omit a final partially observed week and do not permit reentry after an enrollment gap.

Weeks are infant-relative: week 0 contains ages 0–6 days, week 1 contains ages 7–13 days, and so forth. Retain rows after monoclonal receipt and after any outcome. Week numbering and cumulative counts do not reset between seasons. The unique row identifier is `(NEWBORN_ID, week)`.

## Identification, follow-up, and baseline columns

| Column | Description |
|---|---|
| `NEWBORN_ID` | Existing infant identifier. |
| `birth_date` | Birth date, derived from `NEWBORN_DOB_ESTIMATE` as in the existing analysis. |
| `week` | Integer week since birth, starting at 0. |
| `week_date` | Start of the week: `birth_date + 7 * week`, retaining the existing variable name. |
| `week_end_date` | End of the week: `week_date + 6`. Include a row only when this date is within continuous enrollment and on or before the administrative end date. |
| `overall_enr_end` | Last covered date before the first enrollment gap. Use the existing field if it represents continuous enrollment from birth; otherwise derive this date from enrollment spans. |
| `last_study_date` | Administrative follow-up end date in the second season, to be supplied. Replaces the original birth-plus-182-days definition. |
| `last_observed_week` | Last fully observed week included for the infant. |
| `followup_end_reason` | `enrollment_end` or `administrative_end`, according to which limits follow-up; use `both` if the dates coincide. |
| Existing baseline covariates | Retain the baseline covariates from the [existing preparation script](../code/1_prep_data.R), unchanged across an infant's weekly records. |
| `pref_binary`, `pref_binary_proper`, `pref_lt14`, `preftobirth`, `pref_month` | Retain the existing maternal vaccination fields and definitions from the preparation script. These remain constant across infant weeks. |

Exact administration and encounter dates need not appear in the weekly table. Use them upstream to assign records to infant-relative weeks. For dates on or after birth, `week = floor((date - birth_date) / 7)`.

## Monoclonal receipt columns

Use all monoclonal administrations during included follow-up, independently of whether an outcome has already occurred.

| Column | Type | Description |
|---|---|---|
| `mab_event` | 0/1 | 1 in a week containing monoclonal receipt; 0 otherwise. Returns to 0 in weeks without receipt. |
| `mab_n_prior` | Integer | Number of prior weeks containing monoclonal receipt, strictly before the current week. Starts at 0. |
| `mab_n_through` | Integer | Cumulative number of receipt-positive weeks through the current week: `mab_n_prior + mab_event`. |
| `mab_first_week` | Integer or NA | First included week with monoclonal receipt, repeated on every row for the infant. NA if no receipt occurs during included follow-up. |

A later administration produces another `mab_event = 1` and increments the cumulative count. For example, receipt in weeks 0 and 40 gives a prior count of 1 at week 40 and a cumulative count through week 40 of 2. Counts here represent receipt-positive weeks.

`mab_first_week` replaces the outcome-specific timing fields such as `mab_week_rsv_ma_fixed` and `mab_week_ari_ma_fixed`. Unlike those fields, it is not changed by the timing of any outcome. The existing outcome-specific `mab_*_tv` fields indicate receipt from a selected point onward; `mab_event` instead indicates receipt in the current week only.

## Outcome columns

Retain the existing endpoint definitions, extending ascertainment through the new follow-up period. Use these endpoint stems:

| Stem | Outcome | Existing infant-level indicator |
|---|---|---|
| `rsv_ma` | Medically attended RSV | `rsv_ma` |
| `rsv_hosp` | RSV hospitalization | `rsv_hosp` |
| `ari_ma` | Medically attended ARI | `any_ma` |
| `ari_hosp` | ARI hospitalization | `any_hosp` |
| `bronc_ma` | Medically attended bronchiolitis | `bronc_ma` |
| `bronc_hosp` | Bronchiolitis hospitalization | `bronc_hosp` |
| `rsv_ma_test` | Medically attended RSV meeting the testing criterion | `rsv_ma_with_test` |
| `rsv_hosp_test` | RSV hospitalization meeting the testing criterion | `rsv_hosp_with_test` |

For each stem `e`, include the following four columns. For example, the ARI medically attended columns are `ari_ma_event`, `ari_ma_n_prior`, `ari_ma_n_through`, and `ari_ma_first_week`.

| Column template | Type | Description |
|---|---|---|
| `{e}_event` | 0/1 | 1 if at least one qualifying encounter occurs in the current week; 0 otherwise. |
| `{e}_n_prior` | Integer | Number of positive weeks for this endpoint strictly before the current week. Starts at 0. |
| `{e}_n_through` | Integer | Number of positive weeks through the current week: `{e}_n_prior + {e}_event`. |
| `{e}_first_week` | Integer or NA | First included week with this endpoint, repeated on every row for the infant. NA if the endpoint never occurs during included follow-up. |

Collapse multiple qualifying encounters for the same endpoint within a week to one positive week. Positive weeks separated by event-free weeks, or occurring consecutively, each contribute 1 to the count. Apply this rule separately to each endpoint; an encounter may qualify for more than one endpoint. For hospitalizations, assign the encounter using its original event-date definition rather than counting every week spanned by a continuous stay. Apply the testing criterion to the qualifying encounter when constructing repeated tested outcomes.

The existing fields such as `rsv_ma_tv` remain 1 after the first event. They are therefore different from the new weekly event indicators, which can return to 0. Existing first dates such as `first_rsv_ma_date` correspond to the new first-occurrence weeks, but first dates alone cannot describe subsequent events.

## Timing and use of histories

Prior counts exclude the current week. In particular, `ari_ma_n_prior` is the cumulative history available for the current week's monoclonal receipt model. Current-week ARI enters that history starting in the following week.

First-occurrence weeks are summaries of the infant's included follow-up. They can be used to derive first-event or ever-event indicators. Prior outcome counts equal to 0 identify weeks still at risk for a first event. Weekly event indicators and cumulative counts retain the information needed for recurrent-event analyses.

## Example weekly records

An infant receives monoclonals in weeks 0 and 5 and has medically attended ARI in weeks 2 and 4. The tables below show selected columns from the same seven weekly records.

| `week` | `mab_event` | `mab_n_prior` | `mab_n_through` | `mab_first_week` |
|---|---:|---:|---:|---:|
| 0 | 1 | 0 | 1 | 0 |
| 1 | 0 | 1 | 1 | 0 |
| 2 | 0 | 1 | 1 | 0 |
| 3 | 0 | 1 | 1 | 0 |
| 4 | 0 | 1 | 1 | 0 |
| 5 | 1 | 1 | 2 | 0 |
| 6 | 0 | 2 | 2 | 0 |

| `week` | `ari_ma_event` | `ari_ma_n_prior` | `ari_ma_n_through` | `ari_ma_first_week` |
|---|---:|---:|---:|---:|
| 0 | 0 | 0 | 0 | 2 |
| 1 | 0 | 0 | 0 | 2 |
| 2 | 1 | 0 | 1 | 2 |
| 3 | 0 | 1 | 1 | 2 |
| 4 | 1 | 1 | 2 | 2 |
| 5 | 0 | 2 | 2 | 2 |
| 6 | 0 | 2 | 2 | 2 |

If ARI never occurred, all three ARI indicator/count columns would be 0 and `ari_ma_first_week` would be NA. The same four-column structure applies to every other endpoint.
