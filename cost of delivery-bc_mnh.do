clear all 
cd "E:\ResearchProject\Baker Bhai\Cost of Delivery"

set maxvar 30000

*import 2022 data 
use "E:\ResearchProject\Baker Bhai\Cost of Delivery\BD_2022_DHS_Stata\BDIR81DT\BDIR81FL", replace 
*tables 9.12-13
*keep necessary variable 

keep v007 v005 v012 v013 v021 v022 v024 v025 v106 v130 v136 v158 v159 v190 v218 v445 v701 v704 v705 v714 v730 m14_1 m15_1 m15_2 m15_3 m15_4 m15_5 m15_6 m17_1 m17_2 m17_3 m17_4 m17_5 m17_6 s436a_1  bord_01 

* create sample weight variable
gen wgt=v005/1000000
label variable wgt "sample weight"
* check with and without weight 
*tab v025 year
*tab v025 year [iweight=wgt]
svyset[pw=wgt],psu(v021) strata(v022)

*******************************************************************************
*******************************************************************************
*				inclusion exclusion 
********************************************************************************
*******************************************************************************

tab m15_1,m
drop if m15_1==.

*(xxx observations deleted)

*******************************************************************************

*outcome variable

tab s436a_1,m
gen s436a_1a=s436a_1
recode s436a_1a .a=.
recode s436a_1a 999995=.
recode s436a_1a 999998=.
drop if s436a_1a==.
tab s436a_1a,m

gen cod_bdt=s436a_1a
gen cod_usd=cod_bdt/84.81

hist cod_bdt
hist cod_usd


kdensity cod_bdt, normal
kdensity cod_usd, normal

gen cod_usd_log=log10(cod_usd) 
gen cod_usd_ln=ln(cod_usd)
tabstat cod_usd, stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)


********************************************************************************
*#Predictor variables are:
*Place of Delivery
tab m15_1
recode m15_1 10/11=.
recode m15_1 42/96=.
recode m15_1 (20/28=0 "Public Facility-based deliveries") (30/41=1 "Private/NGO Facility-based deliveries"), gen(delivery_place)

tab delivery_place,m
label var delivery_place "Place of delivery"
label val delivery_place delivery_place
tab delivery_place

svy: tab delivery_place, count format(%9.0f)
svy: tab delivery_place, format(%9.4f)

tabstat cod_usd, by (delivery_place) stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd delivery_place

tabstat cod_usd if delivery_place==0, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if delivery_place==1, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd delivery_place##delivery_types


anova cod_usd delivery_place if delivery_types==0
anova cod_usd delivery_place if delivery_types==1
anova cod_usd delivery_place


*Type of Delivery
tab m17_1
recode m17_1 (0=0 "Normal") (1=1 "C-section"), gen(delivery_types)

tab delivery_types,m
label var delivery_types "Types of delivery"
label val delivery_types delivery_types
tab delivery_types

svy: tab delivery_types, count format(%9.0f)
svy: tab delivery_types, format(%9.4f)

tabstat cod_usd, by (delivery_types) stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd delivery_types

tabstat cod_usd if delivery_types==0, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if delivery_types==1, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd delivery_place##delivery_types


anova cod_usd delivery_types if delivery_place==0
anova cod_usd delivery_types if delivery_place==1
anova cod_usd delivery_types

*age

tabstat v012, by (delivery_types) stat(n mean sd p50 p25 p75) format(%9.2f)
tabstat v012, by (delivery_place) stat(n mean sd p50 p25 p75) format(%9.2f)
ttest v012, by(delivery_types)
ttest v012, by(delivery_place)

tab v013
gen age_cat=v013
recode age_cat 4/7=4
tab age_cat 
label define age_cat 1 "15-19" 2 "20-24" 3 "25-29" 4 "30-49"
label val age_cat age_cat
tab age_cat,m

svy: tab age_cat, count format(%9.0f)
svy: tab age_cat, format(%9.4f)


tabstat cod_usd if age_cat==1, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if age_cat==2, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if age_cat==3, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if age_cat==4, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd age_cat##delivery_types

anova cod_usd age_cat if delivery_types==0
anova cod_usd age_cat if delivery_types==1
anova cod_usd age_cat


tabstat cod_usd if age_cat==1, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if age_cat==2, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if age_cat==3, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if age_cat==4, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd  age_cat##delivery_place

anova cod_usd age_cat if delivery_place==0
anova cod_usd age_cat if delivery_place==1
anova cod_usd age_cat

* women edducation 
tab v106,m

svy: tab v106, count format(%9.0f)
svy: tab v106, format(%9.4f)

tabstat cod_usd if v106==0, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v106==1, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v106==2, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v106==3, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd v106##delivery_types

anova cod_usd v106 if delivery_types==0
anova cod_usd v106 if delivery_types==1
anova cod_usd v106

tabstat cod_usd if v106==0, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v106==1, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v106==2, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v106==3, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd v106##delivery_place

anova cod_usd v106 if delivery_place==0
anova cod_usd v106 if delivery_place==1
anova cod_usd v106

*Women Currently working
tab v714
svy: tab v714, count format(%9.0f)
svy: tab v714, format(%9.4f)

tabstat cod_usd if v714==0, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v714==1, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd v714##delivery_types

anova cod_usd v714 if delivery_types==0
anova cod_usd v714 if delivery_types==1
anova cod_usd v714

tabstat cod_usd if v714==0, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v714==1, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd v714##delivery_place

anova cod_usd v714 if delivery_place==0
anova cod_usd v714 if delivery_place==1
anova cod_usd v714

*BMI v445
gen bmi=v445/100
gen bmi4a=.
replace bmi4a=0 if bmi<=18.49
replace bmi4a=1 if bmi>=18.50 & bmi<=22.99
replace bmi4a=2 if bmi>=23.00 & bmi<=27.49
replace bmi4a=3 if bmi>=27.50
tab bmi4a, missing 
label variable bmi4a "asian standard bmi four category"
label define bmi4alabel 0 "Underweight" 1 "Normalweight" 2 "Overweight" 3 "Obese" 
label value bmi4a bmi4alabel
tab bmi4a,m

svy: tab bmi4a, count format(%9.0f)
svy: tab bmi4a, format(%9.4f)

tabstat cod_usd if bmi4a==0, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if bmi4a==1, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if bmi4a==2, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if bmi4a==3, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd bmi4a##delivery_types

anova cod_usd bmi4a if delivery_types==0
anova cod_usd bmi4a if delivery_types==1
anova cod_usd bmi4a

tabstat cod_usd if bmi4a==0, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if bmi4a==1, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if bmi4a==2, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if bmi4a==3, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd bmi4a##delivery_place

anova cod_usd bmi4a if delivery_place==0
anova cod_usd bmi4a if delivery_place==1
anova cod_usd bmi4a

*ANC (NW).
tab m14_1
recode m14_1 98=.
recode m14_1 (0=0 "No ANC") (1/3=1 "1-3") (4/20=2 ">=4"), gen(anc)
label define anc 0 "No ANC" 1 "1-3" 2 ">=4", replace

svy: tab anc, count format(%9.0f)
svy: tab anc, format(%9.4f)

tabstat cod_usd if anc==0, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if anc==1, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if anc==2, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd anc##delivery_types

anova cod_usd anc if delivery_types==0
anova cod_usd anc if delivery_types==1
anova cod_usd anc

tabstat cod_usd if anc==0, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if anc==1, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if anc==2, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd anc##delivery_place

anova cod_usd anc if delivery_place==0
anova cod_usd anc if delivery_place==1
anova cod_usd anc


*Husband age
tab v730
gen hage_cat=v730
recode hage_cat 15/29=0
recode hage_cat 30/44=1
recode hage_cat 45/95=2
tab hage_cat 
label define hage_cat 0 "15-29" 1 "30-44" 2 "45-95"
label val hage_cat hage_cat
tab hage_cat,m

svy: tab hage_cat, count format(%9.0f)
svy: tab hage_cat, format(%9.4f)

tabstat cod_usd if hage_cat==0, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if hage_cat==1, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if hage_cat==2, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd hage_cat##delivery_types

anova cod_usd hage_cat if delivery_types==0
anova cod_usd hage_cat if delivery_types==1
anova cod_usd hage_cat

tabstat cod_usd if hage_cat==0, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if hage_cat==1, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if hage_cat==2, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd hage_cat##delivery_place

anova cod_usd hage_cat if delivery_place==0
anova cod_usd hage_cat if delivery_place==1
anova cod_usd hage_cat

*husband education 
tab v701,m 
replace v701=. if v701==8

svy: tab v701, count format(%9.0f)
svy: tab v701, format(%9.4f)

tabstat cod_usd if v701==0, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v701==1, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v701==2, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v701==3, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd v701##delivery_types

anova cod_usd v701 if delivery_types==0
anova cod_usd v701 if delivery_types==1
anova cod_usd v701

tabstat cod_usd if v701==0, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v701==1, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v701==2, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v701==3, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd v701##delivery_place

anova cod_usd  v701 if delivery_place==0
anova cod_usd  v701 if delivery_place==1
anova cod_usd  v701

*husband occupation 
*v705 (Farmer, day labourer, factory worker, driver, service holder, business, other)
tab v704,m
recode v704 99998=.
recode v704 98=.
recode v704 0=.
recode v704 (12=0 "Farmer")(13/15=1 "Agricultural,fishing & poultry Worker")(21=2 "day labor") (23=3 "factory worker") (41=4 "service holder") (51/52 16=5 "Business") (31=6 "Carpenter, Masson, Bus/taxi driver, Construction supervisor, Seamstresses/Tailor, Policeman, Armed services, Dai, Community health worker, FWA, Similar services") (61/62 11 22 96=7 "others"), gen(husband_occu)

label define husband_occu 0 "Farmer" 1 "Agricultural,fishing & poultry Worker" 2 "day labor" 3 "factory worker" 4 "service holder" 65"Business" 6 "skilled worker" 7 "others", replace
tab v704 husband_occu,m
tab husband_occu,m

svy: tab husband_occu, count format(%9.0f)
svy: tab husband_occu, format(%9.4f)

tabstat cod_usd if husband_occu==0, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==1, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==2, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==3, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==4, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==5, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==6, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==7, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd husband_occu##delivery_types

anova cod_usd husband_occu if delivery_types==0
anova cod_usd husband_occu if delivery_types==1
anova cod_usd husband_occu

tabstat cod_usd if husband_occu==0, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==1, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==2, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==3, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==4, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==5, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==6, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==7, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd husband_occu##delivery_place

anova cod_usd  husband_occu if delivery_place==0
anova cod_usd  husband_occu if delivery_place==1
anova cod_usd  husband_occu

*household size  (<4, 4-5, >5)
tab v136
gen household_member=v136
recode household_member 1/4=0
recode household_member 5/30=1
label define household_member 0 "<=4" 1 ">4" 
label val household_member household_member
tab household_member,m

svy: tab household_member, count format(%9.0f)
svy: tab household_member, format(%9.4f)

tabstat cod_usd if household_member==0, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if household_member==1, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd household_member##delivery_types

anova cod_usd household_member if delivery_types==0
anova cod_usd household_member if delivery_types==1
anova cod_usd household_member

tabstat cod_usd if household_member==0, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if household_member==1, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd household_member##delivery_place

anova cod_usd household_member if delivery_place==0
anova cod_usd household_member if delivery_place==1
anova cod_usd household_member

* wealth index 
tab v190

recode v190 (1/2=0 "Poor") (3=1 "Middle") (4/5=2 "Rich"), gen(WealthIndex)

label define WealthIndex 0 "Poor" 1 "Middle" 2 "Rich", replace

svy: tab WealthIndex, count format(%9.0f)
svy: tab WealthIndex, format(%9.4f)

tabstat cod_usd if WealthIndex==0, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if WealthIndex==1, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if WealthIndex==2, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd WealthIndex##delivery_types

anova cod_usd WealthIndex if delivery_types==0
anova cod_usd WealthIndex if delivery_types==1
anova cod_usd WealthIndex

tabstat cod_usd if WealthIndex==0, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if WealthIndex==1, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if WealthIndex==2, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd WealthIndex##delivery_place

anova cod_usd WealthIndex if delivery_place==0
anova cod_usd WealthIndex if delivery_place==1
anova cod_usd WealthIndex

*birth order 
tab bord_01,m

gen birthord_cat=.
replace birthord_cat=0 if bord_01<=1
replace birthord_cat=1 if bord_01>=2 & bord_01<=3
replace birthord_cat= 2  if bord_01>=4 & bord_01 != .
label define birthord_catlabel 0 "1" 1 "2-3" 2 "4+"
label value birthord_cat birthord_catlabel
tab birthord_cat,m

svy: tab birthord_cat, count format(%9.0f)
svy: tab birthord_cat, format(%9.4f)

tabstat cod_usd if birthord_cat==0, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if birthord_cat==1, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if birthord_cat==2, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd birthord_cat##delivery_types

anova cod_usd birthord_cat if delivery_types==0
anova cod_usd birthord_cat if delivery_types==1
anova cod_usd birthord_cat

tabstat cod_usd if birthord_cat==0, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if birthord_cat==1, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if birthord_cat==2, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd birthord_cat##delivery_place

anova cod_usd birthord_cat if delivery_place==0
anova cod_usd birthord_cat if delivery_place==1
anova cod_usd birthord_cat


/*
*watching tv and radio 
tab v159,m
gen tvexp=v159
recode tvexp 0=0
recode tvexp 1/2=1
tab v159 tvexp,m
tab tvexp,m

tab v158,m
gen radioexp=v158
recode radioexp 0=0
recode radioexp 1/2=1

tab radioexp

*Create massmedia exposure variable (yes and no)
gen massmedia_exposure=. 
replace massmedia_exposure=0 if tvexp==0 & radioexp==0 
replace massmedia_exposure=1 if tvexp==1 & radioexp==1
tab massmedia_exposure,m


label define massmedia_exposure 0 "No" 1 "Yes", replace
label value massmedia_exposure massmedia_exposure
tab massmedia_exposure,m

svy: tab massmedia_exposure, count format(%9.0f)
svy: tab massmedia_exposure, format(%9.4f)


tabstat cod_usd if massmedia_exposure==0, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if massmedia_exposure==1, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd massmedia_exposure##delivery_types

anova cod_usd massmedia_exposure if delivery_types==0
anova cod_usd massmedia_exposure if delivery_types==1
anova cod_usd massmedia_exposure

tabstat cod_usd if massmedia_exposure==0, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if massmedia_exposure==1, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd  massmedia_exposure##delivery_place

anova cod_usd massmedia_exposure if delivery_place==0
anova cod_usd massmedia_exposure if delivery_place==1
anova cod_usd massmedia_exposure

*/

*residence and diviion 
tab1 v024 v025

svy: tab v024, count format(%9.0f)
svy: tab v024, format(%9.4f)

tabstat cod_usd if v024==1, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==2, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==3, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==4, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==5, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==6, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==7, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==8, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd v024##delivery_types

anova cod_usd v024 if delivery_types==0
anova cod_usd v024 if delivery_types==1
anova cod_usd v024

tabstat cod_usd if v024==1, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==2, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==3, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==4, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==5, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==6, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==7, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==8, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd v024##delivery_place

anova cod_usd v024 if delivery_place==0
anova cod_usd v024 if delivery_place==1
anova cod_usd v024

svy: tab v025, count format(%9.0f)
svy: tab v025, format(%9.4f)

tabstat cod_usd if v025==1, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v025==2, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd v025##delivery_types

anova cod_usd  v025 if delivery_types==0
anova cod_usd  v025 if delivery_types==1
anova cod_usd  v025

tabstat cod_usd if v025==1, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v025==2, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd v025##delivery_place

anova cod_usd v025 if delivery_place==0
anova cod_usd v025 if delivery_place==1
anova cod_usd v025

*religion ( muslim and others)
tab v130
gen religon_cat=v130
recode religon_cat 1=1
recode religon_cat 2/4=2
label define religon_catlabel 1 "Islam" 2 "Others" 
label value religon_cat religon_catlabel
tab religon_cat v130,m

svy: tab religon_cat, count format(%9.0f)
svy: tab religon_cat, format(%9.4f)

tabstat cod_usd if religon_cat==1, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if religon_cat==2, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd religon_cat##delivery_types

anova cod_usd religon_cat if delivery_types==0
anova cod_usd religon_cat if delivery_types==1
anova cod_usd religon_cat

tabstat cod_usd if religon_cat==1, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if religon_cat==2, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd religon_cat##delivery_place

anova cod_usd religon_cat if delivery_place==0
anova cod_usd religon_cat if delivery_place==1
anova cod_usd religon_cat





histogram cod_usd, frequency by(delivery_types, total)
histogram cod_usd, frequency by(delivery_place, total)

histogram cod_usd_log, frequency by(delivery_types, total)
histogram cod_usd_log, frequency by(delivery_place, total)

histogram cod_usd_ln, frequency by(delivery_types, total)
histogram cod_usd_ln, frequency by(delivery_types, total)

*normality test 
swilk cod_bdt cod_usd cod_usd_log cod_usd_ln
sfrancia cod_bdt cod_usd cod_usd_log cod_usd_ln

kdensity cod_bdt
kdensity cod_usd_log,normal
kdensity cod_usd_log
kdensity cod_usd_ln


* create sample weight variable
gen wgt=v005/1000000
label variable wgt "sample weight"
* check with and without weight 
*tab v025 year
*tab v025 year [iweight=wgt]
svyset[pw=wgt],psu(v021) strata(v022)

*all predictor variables 
set cformat %9.2f
svy: regress cod_usd_log i.delivery_place i.age_cat i.v106 i.v714 i.bmi4a i.anc i.hage_cat i.v701 i.husband_occu i.household_member i.WealthIndex i.birthord_cat i.v024 i.v025 i.religon_cat if delivery_types == 0

regress cod_usd_log i.delivery_place i.age_cat i.v106 i.v714 i.bmi4a i.anc i.hage_cat i.v701 i.husband_occu i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure i.v024 i.v025 i.religon_cat if delivery_types == 0
vif
estat ic

set cformat %9.2f
svy: regress cod_usd_log i.delivery_place i.age_cat i.v106 i.v714 i.bmi4a i.anc i.hage_cat i.v701 i.husband_occu i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure i.v024 i.v025 i.religon_cat if delivery_types == 1

regress cod_usd_log i.delivery_place i.age_cat i.v106 i.v714 i.bmi4a i.anc i.hage_cat i.v701 i.husband_occu i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure i.v024 i.v025 i.religon_cat if delivery_types == 1
vif
estat ic




set cformat %9.2f
svy: regress cod_usd_log i.delivery_types i.age_cat i.v106 i.v714 i.bmi4a i.anc i.hage_cat i.v701 i.husband_occu i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure i.v024 i.v025 i.religon_cat if delivery_place == 0

regress cod_usd_log i.delivery_types i.age_cat i.v106 i.v714 i.bmi4a i.anc i.hage_cat i.v701 i.husband_occu i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure i.v024 i.v025 i.religon_cat if delivery_place == 0
vif
estat ic


set cformat %9.2f
svy: regress cod_usd_log i.delivery_types i.age_cat i.v106 i.v714 i.bmi4a i.anc i.hage_cat i.v701 i.husband_occu i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure i.v024 i.v025 i.religon_cat if delivery_place == 1

regress cod_usd_log i.delivery_types i.age_cat i.v106 i.v714 i.bmi4a i.anc i.hage_cat i.v701 i.husband_occu i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure i.v024 i.v025 i.religon_cat if delivery_place == 1
vif
estat ic


set cformat %9.2f
svy: regress cod_usd_log i.delivery_types i.v714 i.hage_cat i.v701  i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure  i.v025 i.religon_cat if delivery_place == 2

regress cod_usd_log i.delivery_types i.v714 i.hage_cat i.v701  i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure  i.v025 i.religon_cat if delivery_place == 2
vif
estat ic


*Total
set cformat %9.2f
svy: regress cod_usd_log i.delivery_place i.delivery_types  i.age_cat i.v106 i.v714 i.bmi4a i.anc i.hage_cat i.v701 i.husband_occu i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure i.v024 i.v025 i.religon_cat

regress cod_usd_log i.delivery_place i.delivery_types  i.age_cat i.v106 i.v714 i.bmi4a i.anc i.hage_cat i.v701 i.husband_occu i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure i.v024 i.v025 i.religon_cat
vif
estat ic













clear all 
cd "E:\ResearchProject\Baker Bhai\Cost of Delivery"

set maxvar 30000

*import 2014 data 
use "E:\ResearchProject\Baker Bhai\Cost of Delivery\BD_2014_DHS_Stata\bdir70dt\BDIR70FL", replace 
*tables 9.12-13
*keep necessary variable 

keep v007 v005 v012 v013 v021 v022 v024 v025 v106 v130 v136 v158 v159 v190 v218 v445 v701 v704 v705 v714 v730 m14_1 m15_1 m15_2 m15_3 m15_4 m15_5 m15_6 m17_1 m17_2 m17_3 m17_4 m17_5 m17_6 s435iv  bord_01 

* create sample weight variable
gen wgt=v005/1000000
label variable wgt "sample weight"
* check with and without weight 
*tab v025 year
*tab v025 year [iweight=wgt]
svyset[pw=wgt],psu(v021) strata(v022)

*******************************************************************************
*******************************************************************************
*				inclusion exclusion 
********************************************************************************
*******************************************************************************

tab m15_1,m
drop if m15_1==.
drop if m15_1==10
drop if m15_1==11
drop if m15_1==96

*(xxx observations deleted)

*******************************************************************************

*outcome variable

tab s435iv ,m
gen s435iva=s435iv 
recode s435iva .a=.
recode s435iva 999995=.
recode s435iva 999998=.
recode s435iva 999999.000=.
drop if s435iva==.
tab s435iva,m

gen cod_bdt=s435iva
gen cod_usd=cod_bdt/79.93

hist cod_bdt
hist cod_usd


kdensity cod_bdt, normal
kdensity cod_usd, normal

gen cod_usd_log=log10(cod_usd) 
gen cod_usd_ln=ln(cod_usd)
tabstat cod_usd, stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)

********************************************************************************
*#Predictor variables are:
*Place of Delivery
tab m15_1
recode m15_1 10/11=.
recode m15_1 96=.
recode m15_1 (20/27=0 "Public Facility-based deliveries") (30/46=1 "Private/NGO Facility-based deliveries"), gen(delivery_place)

tab delivery_place,m
label var delivery_place "Place of delivery"
label val delivery_place delivery_place
tab delivery_place

svy: tab delivery_place, count format(%9.0f)
svy: tab delivery_place, format(%9.4f)

tabstat cod_usd, by (delivery_place) stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd delivery_place

tabstat cod_usd if delivery_place==0, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if delivery_place==1, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd delivery_place##delivery_types


anova cod_usd delivery_place if delivery_types==0
anova cod_usd delivery_place if delivery_types==1
anova cod_usd delivery_place


*Type of Delivery
tab m17_1
recode m17_1 (0=0 "Normal") (1=1 "C-section"), gen(delivery_types)

tab delivery_types,m
label var delivery_types "Types of delivery"
label val delivery_types delivery_types
tab delivery_types

svy: tab delivery_types, count format(%9.0f)
svy: tab delivery_types, format(%9.4f)

tabstat cod_usd, by (delivery_types) stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd delivery_types

tabstat cod_usd if delivery_types==0, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if delivery_types==1, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd delivery_place##delivery_types


anova cod_usd delivery_types if delivery_place==0
anova cod_usd delivery_types if delivery_place==1
anova cod_usd delivery_types

*age

tabstat v012, by (delivery_types) stat(n mean sd p50 p25 p75) format(%9.2f)
tabstat v012, by (delivery_place) stat(n mean sd p50 p25 p75) format(%9.2f)
ttest v012, by(delivery_types)
ttest v012, by(delivery_place)

tab v013
gen age_cat=v013
recode age_cat 4/7=4
tab age_cat 
label define age_cat 1 "15-19" 2 "20-24" 3 "25-29" 4 "30-49"
label val age_cat age_cat
tab age_cat,m

svy: tab age_cat, count format(%9.0f)
svy: tab age_cat, format(%9.4f)


tabstat cod_usd if age_cat==1, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if age_cat==2, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if age_cat==3, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if age_cat==4, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd age_cat##delivery_types

anova cod_usd age_cat if delivery_types==0
anova cod_usd age_cat if delivery_types==1
anova cod_usd age_cat


tabstat cod_usd if age_cat==1, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if age_cat==2, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if age_cat==3, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if age_cat==4, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd  age_cat##delivery_place

anova cod_usd age_cat if delivery_place==0
anova cod_usd age_cat if delivery_place==1
anova cod_usd age_cat

* women edducation 
tab v106,m

svy: tab v106, count format(%9.0f)
svy: tab v106, format(%9.4f)

tabstat cod_usd if v106==0, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v106==1, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v106==2, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v106==3, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd v106##delivery_types

anova cod_usd v106 if delivery_types==0
anova cod_usd v106 if delivery_types==1
anova cod_usd v106

tabstat cod_usd if v106==0, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v106==1, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v106==2, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v106==3, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd v106##delivery_place

anova cod_usd v106 if delivery_place==0
anova cod_usd v106 if delivery_place==1
anova cod_usd v106

*Women Currently working
tab v714
svy: tab v714, count format(%9.0f)
svy: tab v714, format(%9.4f)

tabstat cod_usd if v714==0, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v714==1, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd v714##delivery_types

anova cod_usd v714 if delivery_types==0
anova cod_usd v714 if delivery_types==1
anova cod_usd v714

tabstat cod_usd if v714==0, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v714==1, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd v714##delivery_place

anova cod_usd v714 if delivery_place==0
anova cod_usd v714 if delivery_place==1
anova cod_usd v714

*BMI v445
gen bmi=v445/100
gen bmi4a=.
replace bmi4a=0 if bmi<=18.49
replace bmi4a=1 if bmi>=18.50 & bmi<=22.99
replace bmi4a=2 if bmi>=23.00 & bmi<=27.49
replace bmi4a=3 if bmi>=27.50
tab bmi4a, missing 
label variable bmi4a "asian standard bmi four category"
label define bmi4alabel 0 "Underweight" 1 "Normalweight" 2 "Overweight" 3 "Obese" 
label value bmi4a bmi4alabel
tab bmi4a,m

svy: tab bmi4a, count format(%9.0f)
svy: tab bmi4a, format(%9.4f)

tabstat cod_usd if bmi4a==0, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if bmi4a==1, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if bmi4a==2, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if bmi4a==3, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd bmi4a##delivery_types

anova cod_usd bmi4a if delivery_types==0
anova cod_usd bmi4a if delivery_types==1
anova cod_usd bmi4a

tabstat cod_usd if bmi4a==0, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if bmi4a==1, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if bmi4a==2, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if bmi4a==3, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd bmi4a##delivery_place

anova cod_usd bmi4a if delivery_place==0
anova cod_usd bmi4a if delivery_place==1
anova cod_usd bmi4a

*ANC (NW).
tab m14_1
recode m14_1 98=.
recode m14_1 (0=0 "No ANC") (1/3=1 "1-3") (4/20=2 ">=4"), gen(anc)
label define anc 0 "No ANC" 1 "1-3" 2 ">=4", replace

svy: tab anc, count format(%9.0f)
svy: tab anc, format(%9.4f)

tabstat cod_usd if anc==0, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if anc==1, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if anc==2, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd anc##delivery_types

anova cod_usd anc if delivery_types==0
anova cod_usd anc if delivery_types==1
anova cod_usd anc

tabstat cod_usd if anc==0, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if anc==1, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if anc==2, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd anc##delivery_place

anova cod_usd anc if delivery_place==0
anova cod_usd anc if delivery_place==1
anova cod_usd anc


*Husband age
tab v730
gen hage_cat=v730
recode hage_cat 15/29=0
recode hage_cat 30/44=1
recode hage_cat 45/95=2
tab hage_cat 
label define hage_cat 0 "15-29" 1 "30-44" 2 "45-95"
label val hage_cat hage_cat
tab hage_cat,m

svy: tab hage_cat, count format(%9.0f)
svy: tab hage_cat, format(%9.4f)

tabstat cod_usd if hage_cat==0, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if hage_cat==1, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if hage_cat==2, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd hage_cat##delivery_types

anova cod_usd hage_cat if delivery_types==0
anova cod_usd hage_cat if delivery_types==1
anova cod_usd hage_cat

tabstat cod_usd if hage_cat==0, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if hage_cat==1, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if hage_cat==2, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd hage_cat##delivery_place

anova cod_usd hage_cat if delivery_place==0
anova cod_usd hage_cat if delivery_place==1
anova cod_usd hage_cat

*husband education 
tab v701,m 
replace v701=. if v701==8

svy: tab v701, count format(%9.0f)
svy: tab v701, format(%9.4f)

tabstat cod_usd if v701==0, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v701==1, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v701==2, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v701==3, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd v701##delivery_types

anova cod_usd v701 if delivery_types==0
anova cod_usd v701 if delivery_types==1
anova cod_usd v701

tabstat cod_usd if v701==0, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v701==1, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v701==2, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v701==3, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd v701##delivery_place

anova cod_usd  v701 if delivery_place==0
anova cod_usd  v701 if delivery_place==1
anova cod_usd  v701

*husband occupation 
*v705 (Farmer, day labourer, factory worker, driver, service holder, business, other)
tab v704,m
recode v704 99998=.
recode v704 98=.
recode v704 0=.
recode v704 (12=0 "Farmer")(13/15=1 "Agricultural,fishing & poultry Worker")(21=2 "day labor") (23=3 "factory worker") (41=4 "service holder") (51/52 16=5 "Business") (31=6 "Carpenter, Masson, Bus/taxi driver, Construction supervisor, Seamstresses/Tailor, Policeman, Armed services, Dai, Community health worker, FWA, Similar services") (61/62 11 22 96=7 "others"), gen(husband_occu)

label define husband_occu 0 "Farmer" 1 "Agricultural,fishing & poultry Worker" 2 "day labor" 3 "factory worker" 4 "service holder" 65"Business" 6 "skilled worker" 7 "others", replace
tab v704 husband_occu,m
tab husband_occu,m

svy: tab husband_occu, count format(%9.0f)
svy: tab husband_occu, format(%9.4f)

tabstat cod_usd if husband_occu==0, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==1, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==2, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==3, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==4, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==5, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==6, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==7, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd husband_occu##delivery_types

anova cod_usd husband_occu if delivery_types==0
anova cod_usd husband_occu if delivery_types==1
anova cod_usd husband_occu

tabstat cod_usd if husband_occu==0, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==1, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==2, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==3, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==4, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==5, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==6, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==7, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd husband_occu##delivery_place

anova cod_usd  husband_occu if delivery_place==0
anova cod_usd  husband_occu if delivery_place==1
anova cod_usd  husband_occu

*household size  (<4, 4-5, >5)
tab v136
gen household_member=v136
recode household_member 1/4=0
recode household_member 5/30=1
label define household_member 0 "<=4" 1 ">4" 
label val household_member household_member
tab household_member,m

svy: tab household_member, count format(%9.0f)
svy: tab household_member, format(%9.4f)

tabstat cod_usd if household_member==0, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if household_member==1, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd household_member##delivery_types

anova cod_usd household_member if delivery_types==0
anova cod_usd household_member if delivery_types==1
anova cod_usd household_member

tabstat cod_usd if household_member==0, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if household_member==1, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd household_member##delivery_place

anova cod_usd household_member if delivery_place==0
anova cod_usd household_member if delivery_place==1
anova cod_usd household_member

* wealth index 
tab v190

recode v190 (1/2=0 "Poor") (3=1 "Middle") (4/5=2 "Rich"), gen(WealthIndex)

label define WealthIndex 0 "Poor" 1 "Middle" 2 "Rich", replace

svy: tab WealthIndex, count format(%9.0f)
svy: tab WealthIndex, format(%9.4f)

tabstat cod_usd if WealthIndex==0, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if WealthIndex==1, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if WealthIndex==2, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd WealthIndex##delivery_types

anova cod_usd WealthIndex if delivery_types==0
anova cod_usd WealthIndex if delivery_types==1
anova cod_usd WealthIndex

tabstat cod_usd if WealthIndex==0, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if WealthIndex==1, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if WealthIndex==2, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd WealthIndex##delivery_place

anova cod_usd WealthIndex if delivery_place==0
anova cod_usd WealthIndex if delivery_place==1
anova cod_usd WealthIndex

*birth order 
tab bord_01,m

gen birthord_cat=.
replace birthord_cat=0 if bord_01<=1
replace birthord_cat=1 if bord_01>=2 & bord_01<=3
replace birthord_cat= 2  if bord_01>=4 & bord_01 != .
label define birthord_catlabel 0 "1" 1 "2-3" 2 "4+"
label value birthord_cat birthord_catlabel
tab birthord_cat,m

svy: tab birthord_cat, count format(%9.0f)
svy: tab birthord_cat, format(%9.4f)

tabstat cod_usd if birthord_cat==0, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if birthord_cat==1, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if birthord_cat==2, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd birthord_cat##delivery_types

anova cod_usd birthord_cat if delivery_types==0
anova cod_usd birthord_cat if delivery_types==1
anova cod_usd birthord_cat

tabstat cod_usd if birthord_cat==0, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if birthord_cat==1, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if birthord_cat==2, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd birthord_cat##delivery_place

anova cod_usd birthord_cat if delivery_place==0
anova cod_usd birthord_cat if delivery_place==1
anova cod_usd birthord_cat

/*
*watching tv and radio 
tab v159,m
gen tvexp=v159
recode tvexp 0=0
recode tvexp 1/2=1
tab v159 tvexp,m
tab tvexp,m

tab v158,m
gen radioexp=v158
recode radioexp 0=0
recode radioexp 1/2=1

tab radioexp

*Create massmedia exposure variable (yes and no)
gen massmedia_exposure=. 
replace massmedia_exposure=0 if tvexp==0 & radioexp==0 
replace massmedia_exposure=1 if tvexp==1 & radioexp==1
tab massmedia_exposure,m


label define massmedia_exposure 0 "No" 1 "Yes", replace
label value massmedia_exposure massmedia_exposure
tab massmedia_exposure,m

svy: tab massmedia_exposure, count format(%9.0f)
svy: tab massmedia_exposure, format(%9.4f)


tabstat cod_usd if massmedia_exposure==0, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if massmedia_exposure==1, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd massmedia_exposure##delivery_types

anova cod_usd massmedia_exposure if delivery_types==0
anova cod_usd massmedia_exposure if delivery_types==1
anova cod_usd massmedia_exposure

tabstat cod_usd if massmedia_exposure==0, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if massmedia_exposure==1, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd  massmedia_exposure##delivery_place

anova cod_usd massmedia_exposure if delivery_place==0
anova cod_usd massmedia_exposure if delivery_place==1
anova cod_usd massmedia_exposure

*/

*residence and diviion 
tab1 v024 v025

svy: tab v024, count format(%9.0f)
svy: tab v024, format(%9.4f)

tabstat cod_usd if v024==1, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==2, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==3, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==4, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==5, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==6, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==7, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd v024##delivery_types

anova cod_usd v024 if delivery_types==0
anova cod_usd v024 if delivery_types==1
anova cod_usd v024

tabstat cod_usd if v024==1, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==2, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==3, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==4, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==5, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==6, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==7, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd v024##delivery_place

anova cod_usd v024 if delivery_place==0
anova cod_usd v024 if delivery_place==1
anova cod_usd v024

svy: tab v025, count format(%9.0f)
svy: tab v025, format(%9.4f)

tabstat cod_usd if v025==1, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v025==2, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd v025##delivery_types

anova cod_usd  v025 if delivery_types==0
anova cod_usd  v025 if delivery_types==1
anova cod_usd  v025

tabstat cod_usd if v025==1, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v025==2, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd v025##delivery_place

anova cod_usd v025 if delivery_place==0
anova cod_usd v025 if delivery_place==1
anova cod_usd v025

*religion ( muslim and others)
tab v130
gen religon_cat=v130
recode religon_cat 1=1
recode religon_cat 2/4=2
label define religon_catlabel 1 "Islam" 2 "Others" 
label value religon_cat religon_catlabel
tab religon_cat v130,m

svy: tab religon_cat, count format(%9.0f)
svy: tab religon_cat, format(%9.4f)

tabstat cod_usd if religon_cat==1, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if religon_cat==2, by (delivery_types)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd religon_cat##delivery_types

anova cod_usd religon_cat if delivery_types==0
anova cod_usd religon_cat if delivery_types==1
anova cod_usd religon_cat

tabstat cod_usd if religon_cat==1, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if religon_cat==2, by (delivery_place)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd religon_cat##delivery_place

anova cod_usd religon_cat if delivery_place==0
anova cod_usd religon_cat if delivery_place==1
anova cod_usd religon_cat





histogram cod_usd, frequency by(delivery_types, total)
histogram cod_usd, frequency by(delivery_place, total)

histogram cod_usd_log, frequency by(delivery_types, total)
histogram cod_usd_log, frequency by(delivery_place, total)

histogram cod_usd_ln, frequency by(delivery_types, total)
histogram cod_usd_ln, frequency by(delivery_types, total)

*normality test 
swilk cod_bdt cod_usd cod_usd_log cod_usd_ln
sfrancia cod_bdt cod_usd cod_usd_log cod_usd_ln

kdensity cod_bdt
kdensity cod_usd_log,normal
kdensity cod_usd_log
kdensity cod_usd_ln


* create sample weight variable
gen wgt=v005/1000000
label variable wgt "sample weight"
* check with and without weight 
*tab v025 year
*tab v025 year [iweight=wgt]
svyset[pw=wgt],psu(v021) strata(v022)

*all predictor variables 
set cformat %9.2f
svy: regress cod_usd_log i.delivery_place i.age_cat i.v106 i.v714 i.bmi4a i.anc i.hage_cat i.v701 i.husband_occu i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure i.v024 i.v025 i.religon_cat if delivery_types == 0

regress cod_usd_log i.delivery_place i.age_cat i.v106 i.v714 i.bmi4a i.anc i.hage_cat i.v701 i.husband_occu i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure i.v024 i.v025 i.religon_cat if delivery_types == 0
vif
estat ic

set cformat %9.2f
svy: regress cod_usd_log i.delivery_place i.age_cat i.v106 i.v714 i.bmi4a i.anc i.hage_cat i.v701 i.husband_occu i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure i.v024 i.v025 i.religon_cat if delivery_types == 1

regress cod_usd_log i.delivery_place i.age_cat i.v106 i.v714 i.bmi4a i.anc i.hage_cat i.v701 i.husband_occu i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure i.v024 i.v025 i.religon_cat if delivery_types == 1
vif
estat ic




set cformat %9.2f
svy: regress cod_usd_log i.delivery_types i.age_cat i.v106 i.v714 i.bmi4a i.anc i.hage_cat i.v701 i.husband_occu i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure i.v024 i.v025 i.religon_cat if delivery_place == 0

regress cod_usd_log i.delivery_types i.age_cat i.v106 i.v714 i.bmi4a i.anc i.hage_cat i.v701 i.husband_occu i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure i.v024 i.v025 i.religon_cat if delivery_place == 0
vif
estat ic


set cformat %9.2f
svy: regress cod_usd_log i.delivery_types i.age_cat i.v106 i.v714 i.bmi4a i.anc i.hage_cat i.v701 i.husband_occu i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure i.v024 i.v025 i.religon_cat if delivery_place == 1

regress cod_usd_log i.delivery_types i.age_cat i.v106 i.v714 i.bmi4a i.anc i.hage_cat i.v701 i.husband_occu i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure i.v024 i.v025 i.religon_cat if delivery_place == 1
vif
estat ic


*Total
set cformat %9.2f
svy: regress cod_usd_log i.delivery_place i.delivery_types  i.age_cat i.v106 i.v714 i.bmi4a i.anc i.hage_cat i.v701 i.husband_occu i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure i.v024 i.v025 i.religon_cat

regress cod_usd_log i.delivery_place i.delivery_types  i.age_cat i.v106 i.v714 i.bmi4a i.anc i.hage_cat i.v701 i.husband_occu i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure i.v024 i.v025 i.religon_cat
vif
estat ic































********************************************************************************
********************************************************************************
*WEIGHT, STRATA, CLUSTER VARIABLE FOR THE APPENDED DATA
********************************************************************************
* create sample weight variable
gen wgt=v005/1000000
label variable wgt "sample weight"
* check with and without weight 
*tab v025 year
*tab v025 year [iweight=wgt]
svyset[pw=wgt],psu(v021) strata(v022)

/* set survey clustering effects 
*/

* generate new psuid and stratumid variable for svy command by DHS expert
egen psuid=group(year v021)
egen stratumid=group(year v022)

* set survey clustering effects 
svyset[pw=wgt],psu(psuid) strata(stratumid) // by DHS generaic rules 
svyset[pw=v005],strata(stratumid) singleunit(centered) //suggested by dhs expert
save "ir-bdhs", replace

********************************************************************************
*EDA 
********************************************************************************


*histogram cod_bdt, by(delivery, total)
use "ir-bdhs", clear

tabstat cod_usd, by (delivery) stat(mean p50 sd min max n) 
tabstat cod_usd, by (delivery) stat(mean p50 sd min max n), if year==2014
tabstat cod_usd, by (delivery) stat(mean p50 sd min max n), if year==2018


histogram cod_usd, by(delivery, total)
histogram cod_usd, frequency by(year)

histogram cod_usd, frequency by(delivery, total) , if year==2014
histogram cod_usd, frequency by(delivery, total) , if year==2018

histogram cod_usd, frequency , if year==2018 & delivery==0
histogram cod_usd, frequency , if year==2018 & delivery==1
histogram cod_usd, frequency , if year==2018 & delivery==2

histogram cod_usd, frequency , if year==2014 & delivery==0
histogram cod_usd, frequency , if year==2014 & delivery==1
histogram cod_usd, frequency , if year==2014 & delivery==2

tabstat cod_bdt, by (year) stat(mean sd min max n) nototal

sort delivery_place 
tabstat cod_bdt, by (delivery_place) stat(mean sd min max n) 
tabstat cod_bdt, by (delivery) stat(mean sd min max n) 


*normality test 
swilk cod_bdt cod_usd cod_usd_log
sfrancia cod_bdt cod_usd cod_usd_log

histogram cod_usd_log, bin(30)

kdensity cod_bdt
kdensity cod_usd_log,normal
kdensity cod_usd_log
kdensity cod_usd_ln


sort year
qnorm cod_usd_ln, over(year)

graph box cod_usd_log
graph box cod_usd_log, if cod_usd_log>0

save "ir-bdhs", replace


********************************************************************************
* ANALYSIS
********************************************************************************

use "ir-bdhs" , clear

*outcome characterisics 
tabstat cod_usd, by (delivery) stat(n mean sd p50 p25 p75), if year==2014
tabstat cod_usd, by (delivery) stat(n mean sd p50 p25 p75), if year==2018


*glm model 
use "ir-bdhs" , clear
keep if year==2014
svy: glm cod_usd i.age_cat i.v024 i.v025 i.v106 i.v701 i.husband_occu i.v190 i.household_member i.bmi4a i.nlc i.massmedia_exposure i.anc_cat i.birthord_cat,family(gaussian) link(identity)



keep if year==2018

svy: regress cod_usd_log v013
svy: regress cod_usd_log delivery age_cat v024 v025 v106 v701 husband_occu v190 household_member bmi4a nlc massmedia_exposure anc_cat birthord_cat


sort delivery
by delivery: regress cod_usd_log delivery age_cat v024 v025 v106 v701 husband_occu v190 household_member bmi4a nlc massmedia_exposure anc_cat birthord_cat
svy: regress cod_usd_log i.delivery i.age_cat v024 v025 v106 v701 husband_occu v190 household_member bmi4a nlc massmedia_exposure anc_cat birthord_cat


tab1 delivery_place delivery age_cat v024 v025 v106 v701 husband_occu v190 household_member bmi4a nlc massmedia_exposure anc_cat birthord_cat year,m 


tab1 delivery_place delivery age_cat age_cat2 v024 v025 v106 v701 husband_occu v190 household_member bmi4a nlc massmedia_exposure anc_cat birthord_cat year,m 













*import 2017-18 data 
use "E:\ResearchProject\Baker Bhai\Cost of Delivery\BD_2017-18_DHS_12012021_346_170459\BDIR7RDT\BDIR7RFL", replace
*keep necessary variable 
keep v007 v005 v012 v013 v021 v022 v024 v025 v106 v130 v136 v158 v159 v190 v218 v445 v701 v704 v705 v714 m14_1 s433l_1 m15_1 m17_1 bord_01

* recode 7 division to 6 division
* codebook v024
clonevar v024a=v024
replace v024a=1 if v024==1
replace v024a=2 if v024==2
replace v024a=3 if v024==3
replace v024a=4 if v024==4
replace v024a=3 if v024==5
replace v024a=6 if v024==6
replace v024a=7 if v024==7
replace v024a=8 if v024==8
tab1 v024 v024a 
gen year=2018

*******************************************************************************
*******************************************************************************
*				inclusion exclusion 
********************************************************************************
*******************************************************************************

tab m15_1 v007,m
drop if m15_1==.

*(xxx observations deleted)

*******************************************************************************

*outcome variable

tab s436a_1,m
gen s436a_1a=s436a_1
recode s436a_1a .a=.
recode s436a_1a 999995=.
recode s436a_1a 999998=.
tab s436a_1a,m

gen cod_bdt=s436a_1a
gen cod_usd=cod_bdt/84.81

hist cod_bdt
hist cod_usd


kdensity cod_bdt, normal
kdensity cod_usd, normal

gen cod_usd_log=log10(cod_usd) //log transformed 
gen cod_usd_ln=ln(cod_usd) //ln transformed 


********************************************************************************
*#Predictor variables are:
*mode of delivery 
tab m15_1
recode m15_1 (10/11=0 "Home/Non-facility deliverie")(20/26=1 "Public Facility-based deliveries") (27/36=2 "Private Facility-based deliveries") (40/41=3 "NGO") (42/96=4 "Others"), gen(delivery_place)

tab delivery_place,m
label var delivery_place "Place of delivery"
label val delivery_place delivery_place
tab delivery_place m17_1,m
tab delivery_place ,m

tab delivery_place year ,m

*create delivery variable 
gen delivery=. 
/*home delivery */ 
replace delivery=0 if m17_1==0 & delivery_place==0 
/*institutional normal */
replace delivery=1 if m17_1==0 & delivery_place!=0 
/*c-section */
replace delivery=2 if m17_1==1 & delivery_place!=0 
drop if delivery==.
tab delivery,m 

label define delivery 0 "Home delivery" 1 "Institutional normal" 2 "Institutional c-section "
label val delivery delivery
tab delivery,m

tab delivery year,m

tabstat cod_usd, by (delivery) stat(n mean sd p50 p25 p75)

*age
tab v013
gen age_cat=v013
recode age_cat 4/7=4
tab age_cat 
label define age_cat 1 "15-19" 2 "20-24" 3 "25-29" 4 "30-49"
label val age_cat age_cat
tab age_cat,m



tab v013
gen age_cat2=v013
recode age_cat2 5/7=5
tab age_cat2
label define age_cat2 1 "15-19" 2 "20-24" 3 "25-29" 4 "30-34" 5 "35-49"
label val age_cat2 age_cat2
tab age_cat2,m

* women edducation 
tab v106,m
*husband education 
tab v701,m 
replace v701=. if v701==8

*husband occupation 
*v705 (Farmer, day labourer, factory worker, driver, service holder, business, other)
tab v704,m
recode v704 99998=.
recode v704 98=.
recode v704 0=.
recode v704 (12=1 "Farmer")(13/15=2 "Agricultural,fishing & poultry Worker")(21=3 "day labor") (23=4 "factory worker") (41=5 "service holder") (51/52 16=6 "Business") (31=7 "Carpenter, Masson, Bus/taxi driver, Construction supervisor, Seamstresses/Tailor, Policeman, Armed services, Dai, Community health worker, FWA, Similar services") (61/62 11 22 96=8 "others"), gen(husband_occu)

label define husband_occu 1 "Farmer" 2 "Agricultural,fishing & poultry Worker" 3 "day labor" 4 "factory worker" 5 "service holder" 6 "Business" 7 "skilled worker" 8 "others", replace
tab v704 husband_occu,m
tab husband_occu v007,m

* wealth index 
tab v190
*household size  (<4, 4-5, >5)
tab v136
gen household_member=v136
recode household_member 1/3=1
recode household_member 4/5=2
recode household_member 5/30=3
label define household_member 1 "<4" 2 "4-5" 3 ">5" 
label val household_member household_member
tab household_member,m

*BMI v445
gen bmi=v445/100
gen bmi4a=.
replace bmi4a=0 if bmi<=18.49
replace bmi4a=1 if bmi>=18.50 & bmi<=22.99
replace bmi4a=2 if bmi>=23.00 & bmi<=27.49
replace bmi4a=3 if bmi>=27.50
tab bmi4a, missing 
label variable bmi4a "asian standard bmi four category"
label define bmi4alabel 0 "Underweight" 1 "Normalweight" 2 "Overweight" 3 "Obese" 
label value bmi4a bmi4alabel
tab bmi4a,m


*number of living children 
tab v218

*create number of living children categorical variable
*sort v218

gen nlc=.
replace nlc=0 if v218==0
replace nlc=1 if v218==1 | v218==2
replace nlc=2 if v218==3 | v218==4
replace nlc=3 if v218>=5
label variable nlc "number of living childern category"
label define nlclabel 0 "0" 1 "1~2" 2 "3~4" 3 ">5"
label value nlc nlclabel 
*tab nlc, missing
tab nlc, missing
tab nlc v007, m column
tab nlc year,m

*residence and diviion 
tab1 v024a v025
* working stats of women 
tab v714 
*religion ( muslim and others)
tab v130
gen religon_cat=v130
recode religon_cat 1=1
recode religon_cat 2/4=2
label define religon_catlabel 1 "Islam" 2 "Others" 
label value religon_cat religon_catlabel
tab religon_cat v130,m

*watching tv and radio 
tab v159,m
gen tvexp=v159
recode tvexp 0=0
recode tvexp 1/2=1
tab v159 tvexp,m
tab tvexp,m

tab v158,m
gen radioexp=v158
recode radioexp 0=0
recode radioexp 1/2=1

tab radioexp

*Create massmedia exposure variable (yes and no)
gen massmedia_exposure=. 
replace massmedia_exposure=0 if tvexp==0 & radioexp==0 
replace massmedia_exposure=1 if tvexp==1 & radioexp==1
tab massmedia_exposure,m

*number of ANC visits 
tab m14_1,m
*(no, 1-3, >=4)

gen anc_cat=.
replace anc_cat=1 if m14_1<1
replace anc_cat=2 if m14_1>=1 & m14_1<=3
replace anc_cat=3  if m14_1>=4 & m14_1 != .
tab anc_cat,m

label define anc_catl 1 "No" 2 "1-3" 3 ">=4"
label value anc_cat anc_catl
tab anc_cat,m

*birth order 
tab bord_01,m

gen birthord_cat=.
replace birthord_cat=1 if bord_01<=1
replace birthord_cat=2 if bord_01>=2 & bord_01<=3
replace birthord_cat= 3  if bord_01>=4 & bord_01 != .
label define birthord_catlabel 1 "1" 2 "2-3" 3 "4+"
label value birthord_cat birthord_catlabel
tab birthord_cat,m

*all predictor variables 
sum cod_usd cod_usd_log

tab1 delivery_place delivery age_cat age_cat2 v024 v025 v106 v701 husband_occu v190 household_member bmi4a nlc massmedia_exposure anc_cat birthord_cat year,m 



********************************************************************************
********************************************************************************
*WEIGHT, STRATA, CLUSTER VARIABLE FOR THE APPENDED DATA
********************************************************************************
* create sample weight variable
gen wgt=v005/1000000
label variable wgt "sample weight"
* check with and without weight 
*tab v025 year
*tab v025 year [iweight=wgt]
svyset[pw=wgt],psu(v021) strata(v022)

/* set survey clustering effects 
*/

* generate new psuid and stratumid variable for svy command by DHS expert
egen psuid=group(year v021)
egen stratumid=group(year v022)

* set survey clustering effects 
svyset[pw=wgt],psu(psuid) strata(stratumid) // by DHS generaic rules 
svyset[pw=v005],strata(stratumid) singleunit(centered) //suggested by dhs expert
save "ir-bdhs", replace

********************************************************************************
*EDA 
********************************************************************************


*histogram cod_bdt, by(delivery, total)
use "ir-bdhs", clear

tabstat cod_usd, by (delivery) stat(mean p50 sd min max n) 
tabstat cod_usd, by (delivery) stat(mean p50 sd min max n), if year==2014
tabstat cod_usd, by (delivery) stat(mean p50 sd min max n), if year==2018


histogram cod_usd, by(delivery, total)
histogram cod_usd, frequency by(year)

histogram cod_usd, frequency by(delivery, total) , if year==2014
histogram cod_usd, frequency by(delivery, total) , if year==2018

histogram cod_usd, frequency , if year==2018 & delivery==0
histogram cod_usd, frequency , if year==2018 & delivery==1
histogram cod_usd, frequency , if year==2018 & delivery==2

histogram cod_usd, frequency , if year==2014 & delivery==0
histogram cod_usd, frequency , if year==2014 & delivery==1
histogram cod_usd, frequency , if year==2014 & delivery==2

tabstat cod_bdt, by (year) stat(mean sd min max n) nototal

sort delivery_place 
tabstat cod_bdt, by (delivery_place) stat(mean sd min max n) 
tabstat cod_bdt, by (delivery) stat(mean sd min max n) 


*normality test 
swilk cod_bdt cod_usd cod_usd_log
sfrancia cod_bdt cod_usd cod_usd_log

histogram cod_usd_log, bin(30)

kdensity cod_bdt
kdensity cod_usd_log,normal
kdensity cod_usd_log
kdensity cod_usd_ln


sort year
qnorm cod_usd_ln, over(year)

graph box cod_usd_log
graph box cod_usd_log, if cod_usd_log>0

save "ir-bdhs", replace


********************************************************************************
* ANALYSIS
********************************************************************************

use "ir-bdhs" , clear

*outcome characterisics 
tabstat cod_usd, by (delivery) stat(n mean sd p50 p25 p75), if year==2014
tabstat cod_usd, by (delivery) stat(n mean sd p50 p25 p75), if year==2018


*glm model 
use "ir-bdhs" , clear
keep if year==2014
svy: glm cod_usd i.age_cat i.v024 i.v025 i.v106 i.v701 i.husband_occu i.v190 i.household_member i.bmi4a i.nlc i.massmedia_exposure i.anc_cat i.birthord_cat,family(gaussian) link(identity)



keep if year==2018

svy: regress cod_usd_log v013
svy: regress cod_usd_log delivery age_cat v024 v025 v106 v701 husband_occu v190 household_member bmi4a nlc massmedia_exposure anc_cat birthord_cat


sort delivery
by delivery: regress cod_usd_log delivery age_cat v024 v025 v106 v701 husband_occu v190 household_member bmi4a nlc massmedia_exposure anc_cat birthord_cat
svy: regress cod_usd_log i.delivery i.age_cat v024 v025 v106 v701 husband_occu v190 household_member bmi4a nlc massmedia_exposure anc_cat birthord_cat


tab1 delivery_place delivery age_cat v024 v025 v106 v701 husband_occu v190 household_member bmi4a nlc massmedia_exposure anc_cat birthord_cat year,m 


tab1 delivery_place delivery age_cat age_cat2 v024 v025 v106 v701 husband_occu v190 household_member bmi4a nlc massmedia_exposure anc_cat birthord_cat year,m 




















*import 2014 data 
use "E:\ResearchProject\Baker Bhai\Cost of Delivery\BD_2014_DHS_Stata\bdir70dt\BDIR70FL", replace
keep v007 v005 v012 v013 v021 v022 v024 v025 v106 v130 v136 v158 v159 v190 v218 v445 v701 v704 v705 v714 m14_1 s435iv m15_1 m17_1 bord_01
rename s435iv s433l_1
gen year=2014
save "ir-bdhs14", replace


* append all data files 


use "ir-bdhs22" , clear
append using "ir-bdhs17"
append using "ir-bdhs14"
save "ir-bdhs", replace

tab v007 year 

*******************************************************************************
*******************************************************************************
*				inclusion exclusion 
********************************************************************************
*******************************************************************************

tab m15_1 v007,m
drop if m15_1==.

*(xxx observations deleted)

*******************************************************************************

*outcome variable

sum s433l_1
sort s433l_1

gen cod_bdt=s433l_1
recode cod_bdt 999998=.
gen cod_usd=cod_bdt/78.5

hist cod_bdt
hist cod_usd


kdensity cod_bdt, normal
kdensity cod_usd, normal

gen cod_usd_log=log10(cod_usd) //log transformed 
gen cod_usd_ln=ln(cod_usd) //ln transformed 


********************************************************************************
*#Predictor variables are:
*mode of delivery 
tab m15_1
recode m15_1 (11=0 "Home/Non-facility deliverie")(21/28=1 "Public Facility-based deliveries") ///
(31/36=2 "Private Facility-based deliveries") (41=3 "NGO") (42/96=4 "Others"), gen(delivery_place)

tab delivery_place,m
label var delivery_place "Place of delivery"
label val delivery_place delivery_place
tab delivery_place m17_1,m
tab delivery_place ,m

tab delivery_place year ,m

*create delivery variable 
gen delivery=. 
replace delivery=0 if m17_1==0 & delivery_place==0 //home delivery 
replace delivery=1 if m17_1==0 & delivery_place!=0 //institutional normal 
replace delivery=2 if m17_1==1 & delivery_place!=0 //c-section 
drop if delivery==.
tab delivery,m 

label define delivery 0 "Home delivery" 1 "Institutional normal" 2 "Institutional c-section "
label val delivery delivery
tab delivery,m

tab delivery year,m


*age
tab v013
gen age_cat=v013
recode age_cat 4/7=4
tab age_cat 
label define age_cat 1 "15-19" 2 "20-24" 3 "25-29" 4 "30-49"
label val age_cat age_cat
tab age_cat,m


tab v013
gen age_cat2=v013
recode age_cat2 5/7=5
tab age_cat2
label define age_cat2 1 "15-19" 2 "20-24" 3 "25-29" 4 "30-34" 5 "35-49"
label val age_cat2 age_cat2
tab age_cat2,m

* women edducation 
tab v106,m
*husband education 
tab v701,m 
replace v701=. if v701==66

*husband occupation 
*v705 (Farmer, day labourer, factory worker, driver, service holder, business, other)
tab v704,m
recode v704 99998=.
recode v704 98=.
recode v704 0=.
recode v704 (12=1 "Farmer")(13/15=2 "Agricultural,fishing & poultry Worker")(21=3 "day labor") ///
(23=4 "factory worker") (41=5 "service holder") (51/52 16=6 "Business") (31=7 "Carpenter, Masson, Bus/taxi driver, Construction supervisor, Seamstresses/Tailor, Policeman, Armed services, Dai, Community health worker, FWA, Similar services") (61/62 11 22 96=8 "others"), gen(husband_occu)

label define husband_occu 1 "Farmer" 2 "Agricultural,fishing & poultry Worker" 3 "day labor" 4 "factory worker" 5 "service holder" 6 "Business" 7 "skilled worker" 8 "others", replace
tab v704 husband_occu,m
tab husband_occu v007,m

* wealth index 
tab v190
*household size  (<4, 4-5, >5)
tab v136
gen household_member=v136
recode household_member 1/3=1
recode household_member 4/5=2
recode household_member 5/30=3
label define household_member 1 "<4" 2 "4-5" 3 ">5" 
label val household_member household_member
tab household_member,m

*BMI v445
gen bmi=v445/100
gen bmi4a=.
replace bmi4a=0 if bmi<=18.49
replace bmi4a=1 if bmi>=18.50 & bmi<=22.99
replace bmi4a=2 if bmi>=23.00 & bmi<=27.49
replace bmi4a=3 if bmi>=27.50
tab bmi4a, missing 
label variable bmi4a "asian standard bmi four category"
label define bmi4alabel 0 "Underweight" 1 "Normalweight" 2 "Overweight" 3 "Obese" 
label value bmi4a bmi4alabel
tab bmi4a,m


*number of living children 
tab v218

*create number of living children categorical variable
*sort v218

gen nlc=.
replace nlc=0 if v218==0
replace nlc=1 if v218==1 | v218==2
replace nlc=2 if v218==3 | v218==4
replace nlc=3 if v218>=5
label variable nlc "number of living childern category"
label define nlclabel 0 "0" 1 "1~2" 2 "3~4" 3 ">5"
label value nlc nlclabel 
*tab nlc, missing
tab nlc, missing
tab nlc v007, m column
tab nlc year,m

*residence and diviion 
tab1 v024 v025
* working stats of women 
tab v714 
*religion ( muslim and others)
tab v130
gen religon_cat=v130
recode religon_cat 1=1
recode religon_cat 2/4=2
label define religon_catlabel 1 "Islam" 2 "Others" 
label value religon_cat religon_catlabel
tab religon_cat v130,m

*watching tv and radio 
tab v159,m
gen tvexp=v159
recode tvexp 0=0
recode tvexp 1/2=1
tab v159 tvexp,m
tab tvexp,m

tab v158,m
gen radioexp=v158
recode radioexp 0=0
recode radioexp 1/2=1

tab radioexp

*Create massmedia exposure variable (yes and no)
gen massmedia_exposure=. 
replace massmedia_exposure=0 if tvexp==0 & radioexp==0 
replace massmedia_exposure=1 if tvexp==1 & radioexp==1
tab massmedia_exposure,m

*number of ANC visits 
tab m14_1,m
*(no, 1-3, >=4)

gen anc_cat=.
replace anc_cat=1 if m14_1<1
replace anc_cat=2 if m14_1>=1 & m14_1<=3
replace anc_cat=3  if m14_1>=4 & m14_1 != .
tab anc_cat,m

label define anc_catl 1 "No" 2 "1-3" 3 ">=4"
label value anc_cat anc_catl
tab anc_cat,m

*birth order 
tab bord_01,m

gen birthord_cat=.
replace birthord_cat=1 if bord_01<=1
replace birthord_cat=2 if bord_01>=2 & bord_01<=3
replace birthord_cat= 3  if bord_01>=4 & bord_01 != .
label define birthord_catlabel 1 "1" 2 "2-3" 3 "4+"
label value birthord_cat birthord_catlabel
tab birthord_cat,m

*all predictor variables 
sum cod_usd cod_usd_log

tab1 delivery_place delivery age_cat age_cat2 v024 v025 v106 v701 husband_occu v190 household_member bmi4a nlc massmedia_exposure anc_cat birthord_cat year,m 

save "ir-bdhs", replace

********************************************************************************
********************************************************************************
*WEIGHT, STRATA, CLUSTER VARIABLE FOR THE APPENDED DATA
********************************************************************************
* create sample weight variable
gen wgt=v005/1000000
label variable wgt "sample weight"
* check with and without weight 
*tab v025 year
*tab v025 year [iweight=wgt]
svyset[pw=wgt],psu(v021) strata(v022)

/* set survey clustering effects 
*/

* generate new psuid and stratumid variable for svy command by DHS expert
egen psuid=group(year v021)
egen stratumid=group(year v022)

* set survey clustering effects 
svyset[pw=wgt],psu(psuid) strata(stratumid) // by DHS generaic rules 
svyset[pw=v005],strata(stratumid) singleunit(centered) //suggested by dhs expert
save "ir-bdhs", replace

********************************************************************************
*EDA 
********************************************************************************


*histogram cod_bdt, by(delivery, total)
use "ir-bdhs", clear

tabstat cod_usd, by (delivery) stat(mean p50 sd min max n) 
tabstat cod_usd, by (delivery) stat(mean p50 sd min max n), if year==2014
tabstat cod_usd, by (delivery) stat(mean p50 sd min max n), if year==2018


histogram cod_usd, by(delivery, total)
histogram cod_usd, frequency by(year)

histogram cod_usd, frequency by(delivery, total) , if year==2014
histogram cod_usd, frequency by(delivery, total) , if year==2018

histogram cod_usd, frequency , if year==2018 & delivery==0
histogram cod_usd, frequency , if year==2018 & delivery==1
histogram cod_usd, frequency , if year==2018 & delivery==2

histogram cod_usd, frequency , if year==2014 & delivery==0
histogram cod_usd, frequency , if year==2014 & delivery==1
histogram cod_usd, frequency , if year==2014 & delivery==2

tabstat cod_bdt, by (year) stat(mean sd min max n) nototal

sort delivery_place 
tabstat cod_bdt, by (delivery_place) stat(mean sd min max n) 
tabstat cod_bdt, by (delivery) stat(mean sd min max n) 


*normality test 
swilk cod_bdt cod_usd cod_usd_log
sfrancia cod_bdt cod_usd cod_usd_log

histogram cod_usd_log, bin(30)

kdensity cod_bdt
kdensity cod_usd_log,normal
kdensity cod_usd_log
kdensity cod_usd_ln


sort year
qnorm cod_usd_ln, over(year)

graph box cod_usd_log
graph box cod_usd_log, if cod_usd_log>0

save "ir-bdhs", replace


********************************************************************************
* ANALYSIS
********************************************************************************

use "ir-bdhs" , clear

*outcome characterisics 
tabstat cod_usd, by (delivery) stat(n mean sd p50 p25 p75), if year==2014
tabstat cod_usd, by (delivery) stat(n mean sd p50 p25 p75), if year==2018


*glm model 
use "ir-bdhs" , clear
keep if year==2014
svy: glm cod_usd i.age_cat i.v024 i.v025 i.v106 i.v701 i.husband_occu i.v190 i.household_member i.bmi4a i.nlc i.massmedia_exposure i.anc_cat i.birthord_cat,family(gaussian) link(identity)



keep if year==2018

svy: regress cod_usd_log v013
svy: regress cod_usd_log delivery age_cat v024 v025 v106 v701 husband_occu v190 household_member bmi4a nlc massmedia_exposure anc_cat birthord_cat


sort delivery
by delivery: regress cod_usd_log delivery age_cat v024 v025 v106 v701 husband_occu v190 household_member bmi4a nlc massmedia_exposure anc_cat birthord_cat
svy: regress cod_usd_log i.delivery i.age_cat v024 v025 v106 v701 husband_occu v190 household_member bmi4a nlc massmedia_exposure anc_cat birthord_cat


tab1 delivery_place delivery age_cat v024 v025 v106 v701 husband_occu v190 household_member bmi4a nlc massmedia_exposure anc_cat birthord_cat year,m 


tab1 delivery_place delivery age_cat age_cat2 v024 v025 v106 v701 husband_occu v190 household_member bmi4a nlc massmedia_exposure anc_cat birthord_cat year,m 
