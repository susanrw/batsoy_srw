---

---

# batsoy

Data from "Biotic and abiotic factors shaping bat activity in Maryland soybean fields" by L.D. Maynard, W.M Ford, J.D. Parker, and S.R. Whitehead

Code written by L.D. Maynard & S.R. Whitehead

Published in TBD 202X

## Abstract

Bats are important pest control agents in agriculture. Yet, the underlying fine-scale biotic and abiotic mechanisms that drive their foraging behaviors and responses to insect outbreaks are unclear. Herbivore-induced plant volatiles (HIPVs) can attract both invertebrate and avian natural enemies that use the chemical plant cues to locate insect prey. This raises the question of whether HIPVs may also be a biotic factor influencing insectivorous bat activity. Additionally, abiotic factors, such as weather conditions, can affect bat activity in agricultural settings, but little is known about how bats respond to shifting environmental conditions on short time scales in this landscape context. Using a model crop system, soybean (*Glycine max*), our study asked three questions: 1) Which bat species are active in eastern Maryland soybean fields?; 2) Is insectivorous bat activity affected by naturally occurring soybean HIPVs and/or synthetic soybean HIPVs (indole or farnesene); and 3) How is insectivorous bat activity affected by hourly weather conditions? In soybean fields in eastern Maryland, we created paired treatment plots: HIPV plots (damaged plants or synthetic dispensers) and control plots (undamaged plants or empty dispensers). We measured bat activity using ultrasonic recorders, summarizing hourly and nightly activity, and detected ten total species. The most abundant species group was comprised of big brown (*Eptesicus fuscus*) and silver-haired (*Lasionycteris noctivagans*) bats. Our results do not support our expectation that bats in eastern Maryland use soybean HIPVs to locate insect prey, as bat activity did not significantly differ between control and HIPV plots in any of the three experiments. However, we did find that bat activity increased with increasing average hourly temperature and wind speed. A variety of factors and conditions can affect the interactions among plants, herbivores, and natural enemies. This initial study of bats and HIPVs, as well as the fine-scale examination of weather conditions on bat activity, may serve as a guide for future bat--plant research that can support the development of new strategies for sustainable pest management.

# Data files and script

This repository contains 1 R script, 4 standalone data files and 2 data file folders:

### Script 1) Maynard_etal_batsoy_code.R

This script contains all code for statistical analyses and figures.

### File 1) Data_Plant_Summary.csv

This is raw output from Kaleidoscope Pro (idsummary.csv) for the naturally occurring soybean HIPV trials. \
\
Variables include:

jdate: Julian date\
date: date (YYYMMDD)\
EPTFUS: number of big brown bat call files \
LASBOR: number of eastern red bat call files \
LASCIN: number of hoary bat call files \
LASNOC: number of silver-haired bat call files \
LASSEM: number of Seminole bat call files \
MYOLUC: number of little brown bat call files \
NYCHUM: number of evening bat call files \
PERSUB: number of tricolored bat call files \
NOID: number of unidentified bat call files \
side: side of site (E=east, W=west) \
treatment: HIPV treatment (D=damaged plants, U=undamaged plants) \
trial: trial number (1-5)

### File 2) Data_Indole_Summary.csv

This is raw output from Kaleidoscope Pro (idsummary.csv) for the synthetic soybean HIPV trials (indole). \
\
Variables include:

jdate: Julian date\
date: date (YYYMMDD)\
EPTFUS: number of big brown bat call files \
LASBOR: number of eastern red bat call files \
LASCIN: number of hoary bat call files \
LASNOC: number of silver-haired bat call files \
LASSEM: number of Seminole bat call files \
MYOLUC: number of little brown bat call files \
NYCHUM: number of evening bat call files \
PERSUB: number of tricolored bat call files \
NOID: number of unidentified bat call files \
site: individual identifier for site\
treatment: HIPV treatment (Dispenser, Control) \
trial: trial number (1-15)

### File 3) Data_Farnesene_Summary.csv

This is raw output from Kaleidoscope Pro (idsummary.csv) for the synthetic soybean HIPV trials (farnesene). \
\
Variables include:

jdate: Julian date\
date: date (YYYMMDD)\
EPTFUS: number of big brown bat call files \
LASBOR: number of eastern red bat call files \
LASCIN: number of hoary bat call files \
LASNOC: number of silver-haired bat call files \
LASSEM: number of Seminole bat call files \
MYOLUC: number of little brown bat call files \
NYCHUM: number of evening bat call files \
PERSUB: number of tricolored bat call files \
NOID: number of unidentified bat call files \
site: individual identifier for site\
treatment: HIPV treatment (Dispenser, Control) \
trial: trial number (1-8)

###  File 4) Data_other_bat_activity.csv

This is raw output from Kaleidoscope Pro (idsummary.csv) for additional days of bat activity data that were not part of HIPV trials.

Variables include:

jdate: Julian date\
date: date (YYYMMDD)\
EPTFUS: number of big brown bat call files \
LASBOR: number of eastern red bat call files \
LASCIN: number of hoary bat call files \
LASNOC: number of silver-haired bat call files \
LASSEM: number of Seminole bat call files \
MYOLUC: number of little brown bat call files \
NYCHUM: number of evening bat call files \
PERSUB: number of tricolored bat call files \
NOID: number of unidentified bat call files \
site: individual identifier for site

### Folder 1) SERC_Weather

This folder contains four data files with weather data from SERC meteorological tower Jun-Sep 2021. Data include minute-level data of meteorological data.

Variables include:

date: Date (YYYY-MM-DD)\
hour: Hour (0-23)\
TIMESTAMP: Time (HH:MM:SS)\
RECORD: Unique identifier for measurement\
Wind_direction_min_degrees: Minimum wind direction (degrees)\
Wind_direction_avg_degrees: Mean wind direction (degrees)\
Wind_direction_max_degrees: Maximum wind direction (degrees)\
Wind_speed_min_m.s: Minimum wind speed (m/s)\
Wind_speed_avg_m.s: Mean wind speed (m/s)\
Wind_speed_max_m.s: Maximum wind speed (m/s)\
Air_Temperature_C: Air temperature (degrees C)\
Internal_Temperature_C: Internal temperature (degrees C)\
Relative_Humidity_pct: % relative humidity\
Air_Pressure_pascal: Air pressure (Pascal)\
Rain_Accumulation_mm: Rain accumulation (mm)\
Rain_Duration_s: Rain duration (s)\
Rain_Intensity_mm.h: Rain intensity (mm/h)\
Hail_Accumulation_hits.cm2: Hail accumulation (hits/cm\^2)\
Hail_Duration_s: Hail duration (s)\
Hail_Intensity_hits.cm2: Hail intensity (hits/cm\^2)\
Heating_Temperature_C: Heating temperature (degrees C)\
Heating_Voltage_V: Heating voltage (V)\
Supply_Voltage_V: Supply voltage (V)\
Reference_Voltage_V: Reference voltage (V)\
delta.air: Change in air pressure (Pascal)

### Folder 2) call_files

This raw output from Kaleidoscope Pro (id.csv), including 49 call files. The id.csv output has a row for each detected signal, and the following columns:\

INDIR: Absolute path to input directory.\
OUTDIR: Absolute path to output directory.\
FOLDER: Directory path to the input file relative to the input root.\
IN FILE: Input file name.\
CHANNEL: Channel number from the input file (0=left, 1=right).\
OFFSET: Offset in seconds into the input file where output begins.\
DURATION: Duration in seconds of the output file.\
OUT FILE FS: Name of the output file (full-spectrum).\
OUT FILE ZC: Name of output file (zero-crossing).\
DATE: Date in form YYYY-MM-DD of the recording.\
TIME: Time in form of hh:mm:ss of the recording.\
HOUR: Hour of the recording (0-23) for convenient pivot tables by hour.\
DATE-12: Date 12 hours prior to date of recording (e.g. for night vs. day) in the form YYYY-MM- DD.\
TIME-12: Time 12 hours prior to time of recording (e.g. for night vs. day) in the form hh:mm:ss.\
HOUR-12: Hour 12 hours prior to time of recording (e.g. for night vs. day).\
AUTO ID: Automatic classification result.\
PULSES: Number of pulses detected in the file that were identified to species.\
MATCHING: Number of pulses matching the auto classification result.\
MATCH RATIO: The ratio of MATCHING over PULSES.\
MARGIN: Classification margin\
ALTERNATE 1: First alternate.\
ALTERNATE 2: Second alternate.\
N: Total number of pulses detected. This is used to derive average values for the following 12
parameters:\
Fc: Average characteristic frequency (kHz) - the body of the call is the portion of the call consisting of
the flattest slope where the characteristic frequency is typically the frequency at the latest part of the call
body.\
Sc: Average characteristic slope (Octaves per Second) - this is the slope of the body of the call. Positive
values correspond to decreasing frequency while negative values correspond to increasing frequency.\
Dur: Average duration (ms) - this is the duration of the call.\
Fmax Average maximum frequency (kHz) - the maximum frequency detected in the call.\
Fmin: Average minimum frequency (kHz) - the minimum frequency detected in the call.\
Fmean: Average mean frequency (kHz) - the time-weighted mean frequency of the call.\
TBC: Average time between calls (ms) - if N above is greater than one, this is the average period of the
calls from the start of one call to the start of the next.\
Fk: Average frequency of the knee (kHz) - the frequency at the beginning of the call body.\
Tk: Average time to the knee (ms) - the time from the beginning of the call to the beginning of the call
body.\
S1: Average initial slope (octaves per second) - the initial slope of the call.\
Tc: Average time to the characteristic (ms) - the time from the beginning of the call to the end of the
call body.\
Qual: Average call quality (%) - a measure of the smoothness of the call where smaller values indicate a
smoother call.\
FILES: The number 1, indicating one file, as a convenience for pivot tables by file count.\
MANUAL ID: Manual identification (this field populated during review with Viewer).\
USERID: Nickname or email address of Managed Cloud Account who has run this batch process.\
REVIEW ORIGID: If manual ID is present, this is the UUID of the organization corresponding to the manual ID.\
REVIEW USERID: If manual ID present, this is the nickname or email address of the user who created the manual ID\
INFILEMD5: Unique identification used internally by Kaleidoscope Pro corresponding to the input file.\
OUTFILEMD5FS: Unique identification used internally by Kaleidoscope Pro corresponding to a full-spectrum output
file.\
OUTFILEMD5ZC Unique identification used internally by Kaleidoscope Pro corresponding to a zero-crossing output
file.

## Figures 

This repository includes eight figures produced by the code in Maynard_etal_batsoy_code.R

4 figures are present in the main text of the paper: Figures 1-4

4 figures are present in the supplemental text: Appendix S2: Figures S1-S4
