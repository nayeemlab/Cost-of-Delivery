*******************************************************************************
*				             2014                                             *
*******************************************************************************

clear all 
cd "E:\ResearchProject\Baker Bhai\Cost of Delivery"

set maxvar 30000

*import 2014 data 
use "E:\ResearchProject\Baker Bhai\Cost of Delivery\BD_2014_DHS_Stata\bdir70dt\BDIR70FL", replace 
*tables 9.12-13
*keep necessary variable 

keep v007 v005 v012 v013 v021 v022 v024 v025 v106 v130 v136 v158 v159 v190 v218 v445 v701 v704 v705 v714 v730 m14_1 m15_1 m15_2 m15_3 m15_4 m15_5 m15_6 m17_1 m17_2 m17_3 m17_4 m17_5 m17_6 s435iv  bord_01 v158 v159

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

tab s435iv,m
gen s436a_1a=s435iv
recode s436a_1a 999998=.
drop if s436a_1a==.
tab s436a_1a,m

gen cod_bdt=s436a_1a
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
recode m15_1 (10/11=0 "Home/Non-facility deliverie")(20/27=1 "Public Facility-based deliveries") (30/31=2 "Private Facility-based deliveries") (41/46=3 "NGO") (96=4 "Others"), gen(delivery_place)

tab delivery_place,m
label var delivery_place "Place of delivery"
label val delivery_place delivery_place
tab delivery_place m17_1,m
tab delivery_place ,m

*create delivery variable 
gen delivery=. 
replace delivery=0 if m17_1==0 & delivery_place==0 
replace delivery=1 if m17_1==0 & delivery_place!=0  
replace delivery=2 if m17_1==1 & delivery_place!=0  
drop if delivery==.
tab delivery,m 

label define delivery 0 "Home delivery" 1 "Institutional normal" 2 "Institutional c-section "
label val delivery delivery
tab delivery,m

svy: tab delivery, count format(%9.0f)
svy: tab delivery, format(%9.4f)


*age
tabstat v012, stat(n mean sd p50 p25 p75) format(%9.2f)
tabstat v012, by (delivery) stat(n mean sd p50 p25 p75) format(%9.2f)
anova cod_usd v012##delivery

tab v013
gen age_cat=v013
recode age_cat 4/7=4
tab age_cat 
label define age_cat 1 "15-19" 2 "20-24" 3 "25-29" 4 "30-49"
label val age_cat age_cat
tab age_cat,m

svy: tab age_cat, count format(%9.0f)
svy: tab age_cat, format(%9.4f)


tabstat cod_usd if age_cat==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if age_cat==2, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if age_cat==3, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if age_cat==4, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd age_cat##delivery

anova cod_usd age_cat if delivery==0
anova cod_usd age_cat if delivery==1
anova cod_usd age_cat if delivery==2
anova cod_usd age_cat

* women edducation 
tab v106,m

svy: tab v106, count format(%9.0f)
svy: tab v106, format(%9.4f)

tabstat cod_usd if v106==0, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v106==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v106==2, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v106==3, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd v106##delivery

anova cod_usd v106 if delivery==0
anova cod_usd v106 if delivery==1
anova cod_usd v106 if delivery==2
anova cod_usd v106

*Women Currently working
tab v714
svy: tab v714, count format(%9.0f)
svy: tab v714, format(%9.4f)

tabstat cod_usd if v714==0, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v714==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd v714##delivery

anova cod_usd v714 if delivery==0
anova cod_usd v714 if delivery==1
anova cod_usd v714 if delivery==2
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

tabstat cod_usd if bmi4a==0, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if bmi4a==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if bmi4a==2, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if bmi4a==3, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd bmi4a##delivery

anova cod_usd bmi4a if delivery==0
anova cod_usd bmi4a if delivery==1
anova cod_usd bmi4a if delivery==2
anova cod_usd bmi4a


*ANC (NW).
tab m14_1
recode m14_1 98=.
recode m14_1 (0=0 "No ANC") (1/3=1 "1-3") (4/20=2 ">=4"), gen(anc)
label define anc 0 "No ANC" 1 "1-3" 2 ">=4", replace

svy: tab anc, count format(%9.0f)
svy: tab anc, format(%9.4f)

tabstat cod_usd if anc==0, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if anc==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if anc==2, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd anc##delivery

anova cod_usd anc if delivery==0
anova cod_usd anc if delivery==1
anova cod_usd anc if delivery==2
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

tabstat cod_usd if hage_cat==0, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if hage_cat==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if hage_cat==2, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd hage_cat##delivery

anova cod_usd hage_cat if delivery==0
anova cod_usd hage_cat if delivery==1
anova cod_usd hage_cat if delivery==2
anova cod_usd hage_cat

*husband education 
tab v701,m 
replace v701=. if v701==8

svy: tab v701, count format(%9.0f)
svy: tab v701, format(%9.4f)

tabstat cod_usd if v701==0, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v701==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v701==2, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v701==3, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd v701##delivery

anova cod_usd v701 if delivery==0
anova cod_usd v701 if delivery==1
anova cod_usd v701 if delivery==2
anova cod_usd v701


*husband occupation 
*v705 (Farmer, day labourer, factory worker, driver, service holder, business, other)
tab v704,m
recode v704 99998=.
recode v704 98=.
recode v704 (12=1 "Farmer")(13/15=2 "Agricultural,fishing & poultry Worker")(21=3 "day labor") (23=4 "factory worker") (41=5 "service holder") (51/52 16=6 "Business") (31=7 "Carpenter, Masson, Bus/taxi driver, Construction supervisor, Seamstresses/Tailor, Policeman, Armed services, Dai, Community health worker, FWA, Similar services") (61/62 11 22 96=8 "others"), gen(husband_occu)

label define husband_occu 1 "Farmer" 2 "Agricultural,fishing & poultry Worker" 3 "day labor" 4 "factory worker" 5 "service holder" 6 "Business" 7 "skilled worker" 8 "others", replace
tab v704 husband_occu,m
tab husband_occu v007,m

svy: tab husband_occu, count format(%9.0f)
svy: tab husband_occu, format(%9.4f)

tabstat cod_usd if husband_occu==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==2, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==3, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==4, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==5, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==6, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==7, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==8, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd husband_occu##delivery

anova cod_usd husband_occu if delivery==0
anova cod_usd husband_occu if delivery==1
anova cod_usd husband_occu if delivery==2
anova cod_usd husband_occu

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

tabstat cod_usd if household_member==0, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if household_member==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd household_member##delivery

anova cod_usd household_member if delivery==0
anova cod_usd household_member if delivery==1
anova cod_usd household_member if delivery==2
anova cod_usd household_member


* wealth index 
tab v190

recode v190 (1/2=0 "Poor") (3=1 "Middle") (4/5=2 "Rich"), gen(WealthIndex)

label define WealthIndex 0 "Poor" 1 "Middle" 2 "Rich", replace

svy: tab WealthIndex, count format(%9.0f)
svy: tab WealthIndex, format(%9.4f)

tabstat cod_usd if WealthIndex==0, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if WealthIndex==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if WealthIndex==2, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd WealthIndex##delivery

anova cod_usd WealthIndex if delivery==0
anova cod_usd WealthIndex if delivery==1
anova cod_usd WealthIndex if delivery==2
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

tabstat cod_usd if birthord_cat==0, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if birthord_cat==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if birthord_cat==2, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd birthord_cat##delivery

anova cod_usd birthord_cat if delivery==0
anova cod_usd birthord_cat if delivery==1
anova cod_usd birthord_cat if delivery==2
anova cod_usd birthord_cat


*watching tv, radio, and internet 
*media

*watching tv, radio, and internet 
*media
tab v159,m
gen tvexp=v159
recode tvexp 0=0
recode tvexp 1/3=1
tab v159 tvexp,m
tab tvexp,m

tab v158,m
gen radioexp=v158
recode radioexp 0=0
recode radioexp 1/3=1
tab radioexp

gen massmedia_exposure = tv + radio
svy: tab massmedia_exposure
recode massmedia_exposure 0=0
recode massmedia_exposure 1/3=1
label define massmedia_exposure 0 "No" 1 "Yes"
label values massmedia_exposure massmedia_exposure
tab massmedia_exposure,m

svy: tab massmedia_exposure, count format(%9.0f)
svy: tab massmedia_exposure, format(%9.4f)


tabstat cod_usd if massmedia_exposure==0, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if massmedia_exposure==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd massmedia_exposure##delivery

anova cod_usd massmedia_exposure if delivery==0
anova cod_usd massmedia_exposure if delivery==1
anova cod_usd massmedia_exposure if delivery==2
anova cod_usd massmedia_exposure


*residence and diviion 
tab1 v024 v025

recode v024 (5=6 "rajshahi") (6=7 "rangpur") (7=8 "sylhet"), gen(Division)
drop v024
recode Division (1=1 "barisal") (2=2 "chittagong") (3=3 "dhaka") (4=4 "khulna") (5=5 "mymensingh") (6=6 "rajshahi") (7=7 "rangpur") (8=8 "sylhet"), gen(v024)

svy: tab v024, count format(%9.0f)
svy: tab v024, format(%9.4f)

tabstat cod_usd if v024==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==2, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==3, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==4, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==5, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==6, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==7, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd v024##delivery

anova cod_usd v024 if delivery==0
anova cod_usd v024 if delivery==1
anova cod_usd v024 if delivery==2
anova cod_usd v024

svy: tab v025, count format(%9.0f)
svy: tab v025, format(%9.4f)

tabstat cod_usd if v025==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v025==2, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd v025##delivery

anova cod_usd  v025 if delivery==0
anova cod_usd  v025 if delivery==1
anova cod_usd  v025 if delivery==2
anova cod_usd  v025

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

tabstat cod_usd if religon_cat==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if religon_cat==2, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd religon_cat##delivery

anova cod_usd religon_cat if delivery==0
anova cod_usd religon_cat if delivery==1
anova cod_usd religon_cat if delivery==2
anova cod_usd religon_cat

tabstat cod_usd, by (delivery) stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd delivery


histogram cod_usd, frequency by(delivery, total)

histogram cod_usd_log, frequency by(delivery, total)

histogram cod_usd_ln, frequency by(delivery, total)

*normality test 
swilk cod_bdt cod_usd cod_usd_log cod_usd_ln
sfrancia cod_bdt cod_usd cod_usd_log cod_usd_ln

kdensity cod_bdt
kdensity cod_usd_log,normal
kdensity cod_usd_log
kdensity cod_usd_ln



*all predictor variables 
set cformat %9.2f
svy: regress cod_usd_log i.age_cat i.v106 i.v714 i.bmi4a i.anc i.hage_cat i.v701 i.husband_occu i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure i.v024 i.v025 i.religon_cat if delivery == 0

regress cod_usd_log i.age_cat i.v106 i.v714 i.bmi4a i.anc i.hage_cat i.v701 i.husband_occu i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure i.v024 i.v025 i.religon_cat if delivery == 0
vif
estat ic


set cformat %9.2f
svy: regress cod_usd_log i.age_cat i.v714 i.bmi4a i.hage_cat i.v701 i.husband_occu i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure i.v024 i.v025 i.religon_cat if delivery == 1

regress cod_usd_log i.age_cat  i.v714 i.bmi4a i.hage_cat i.v701 i.husband_occu i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure i.v024 i.v025 i.religon_cat if delivery == 1
vif
estat ic

set cformat %9.2f
svy: regress cod_usd_log i.age_cat i.v106 i.v714 i.bmi4a i.anc i.hage_cat i.v701 i.husband_occu i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure i.v024 i.v025 i.religon_cat if delivery == 2

regress cod_usd_log i.age_cat i.v106 i.v714 i.bmi4a i.anc i.hage_cat i.v701 i.husband_occu i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure i.v024 i.v025 i.religon_cat if delivery == 2
vif
estat ic


*Total
set cformat %9.2f
svy: regress cod_usd_log i.delivery  i.age_cat i.v106 i.v714 i.bmi4a i.anc i.hage_cat i.v701 i.husband_occu i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure i.v024 i.v025 i.religon_cat 

regress cod_usd_log i.delivery i.age_cat i.v106 i.v714 i.bmi4a i.anc i.hage_cat i.v701 i.husband_occu i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure i.v024 i.v025 i.religon_cat 
vif
estat ic

save "E:\ResearchProject\Baker Bhai\Cost of Delivery\2014_CD_Data.dta"






*******************************************************************************
*				             2017-18                                          *
*******************************************************************************

clear all 
cd "E:\ResearchProject\Baker Bhai\Cost of Delivery"

set maxvar 30000

*import 2017 data 
use "E:\ResearchProject\Baker Bhai\Cost of Delivery\BD_2017-18_DHS_12012021_346_170459\BDIR7RDT\BDIR7RFL", replace 
*tables 9.12-13
*keep necessary variable 

keep v007 v005 v012 v013 v021 v022 v024 v025 v106 v130 v136 v158 v159 v190 v218 v445 v701 v704 v705 v714 v730 m14_1 m15_1 m15_2 m15_3 m15_4 m15_5 m15_6 m17_1 m17_2 m17_3 m17_4 m17_5 m17_6 s433l_1 bord_01 v158 v159

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

tab s433l_1,m
gen s436a_1a=s433l_1
recode s436a_1a 999998=.
drop if s436a_1a==.
tab s436a_1a,m

gen cod_bdt=s436a_1a
gen cod_usd=cod_bdt/77.67

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
recode m15_1 (10/11=0 "Home/Non-facility deliverie")(20/28=1 "Public Facility-based deliveries") (30/36=2 "Private Facility-based deliveries") (40/41=3 "NGO") (42/96=4 "Others"), gen(delivery_place)

tab delivery_place,m
label var delivery_place "Place of delivery"
label val delivery_place delivery_place
tab delivery_place m17_1,m
tab delivery_place ,m

*create delivery variable 
gen delivery=. 
replace delivery=0 if m17_1==0 & delivery_place==0 
replace delivery=1 if m17_1==0 & delivery_place!=0  
replace delivery=2 if m17_1==1 & delivery_place!=0  
drop if delivery==.
tab delivery,m 

label define delivery 0 "Home delivery" 1 "Institutional normal" 2 "Institutional c-section "
label val delivery delivery
tab delivery,m

svy: tab delivery, count format(%9.0f)
svy: tab delivery, format(%9.4f)


*age

tabstat v012, by (delivery) stat(n mean sd p50 p25 p75) format(%9.2f)
tabstat v012, stat(n mean sd p50 p25 p75) format(%9.2f)
anova cod_usd v012##delivery

tab v013
gen age_cat=v013
recode age_cat 4/7=4
tab age_cat 
label define age_cat 1 "15-19" 2 "20-24" 3 "25-29" 4 "30-49"
label val age_cat age_cat
tab age_cat,m

svy: tab age_cat, count format(%9.0f)
svy: tab age_cat, format(%9.4f)


tabstat cod_usd if age_cat==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if age_cat==2, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if age_cat==3, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if age_cat==4, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd age_cat##delivery

anova cod_usd age_cat if delivery==0
anova cod_usd age_cat if delivery==1
anova cod_usd age_cat if delivery==2
anova cod_usd age_cat

* women edducation 
tab v106,m

svy: tab v106, count format(%9.0f)
svy: tab v106, format(%9.4f)

tabstat cod_usd if v106==0, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v106==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v106==2, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v106==3, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd v106##delivery

anova cod_usd v106 if delivery==0
anova cod_usd v106 if delivery==1
anova cod_usd v106 if delivery==2
anova cod_usd v106

*Women Currently working
tab v714
svy: tab v714, count format(%9.0f)
svy: tab v714, format(%9.4f)

tabstat cod_usd if v714==0, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v714==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd v714##delivery

anova cod_usd v714 if delivery==0
anova cod_usd v714 if delivery==1
anova cod_usd v714 if delivery==2
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

tabstat cod_usd if bmi4a==0, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if bmi4a==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if bmi4a==2, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if bmi4a==3, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd bmi4a##delivery

anova cod_usd bmi4a if delivery==0
anova cod_usd bmi4a if delivery==1
anova cod_usd bmi4a if delivery==2
anova cod_usd bmi4a


*ANC (NW).
tab m14_1
recode m14_1 98=.
recode m14_1 (0=0 "No ANC") (1/3=1 "1-3") (4/20=2 ">=4"), gen(anc)
label define anc 0 "No ANC" 1 "1-3" 2 ">=4", replace

svy: tab anc, count format(%9.0f)
svy: tab anc, format(%9.4f)

tabstat cod_usd if anc==0, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if anc==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if anc==2, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd anc##delivery

anova cod_usd anc if delivery==0
anova cod_usd anc if delivery==1
anova cod_usd anc if delivery==2
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

tabstat cod_usd if hage_cat==0, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if hage_cat==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if hage_cat==2, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd hage_cat##delivery

anova cod_usd hage_cat if delivery==0
anova cod_usd hage_cat if delivery==1
anova cod_usd hage_cat if delivery==2
anova cod_usd hage_cat

*husband education 
tab v701,m 
replace v701=. if v701==8

svy: tab v701, count format(%9.0f)
svy: tab v701, format(%9.4f)

tabstat cod_usd if v701==0, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v701==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v701==2, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v701==3, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd v701##delivery

anova cod_usd v701 if delivery==0
anova cod_usd v701 if delivery==1
anova cod_usd v701 if delivery==2
anova cod_usd v701


*husband occupation 
*v705 (Farmer, day labourer, factory worker, driver, service holder, business, other)
tab v704,m
recode v704 99998=.
recode v704 98=.
recode v704 (0=0 "Not working") (12=1 "Farmer")(13/15=2 "Agricultural,fishing & poultry Worker")(21=3 "day labor") (23=4 "factory worker") (41=5 "service holder") (51/52 16=6 "Business") (31=7 "Carpenter, Masson, Bus/taxi driver, Construction supervisor, Seamstresses/Tailor, Policeman, Armed services, Dai, Community health worker, FWA, Similar services") (61/62 11 22 96=8 "others"), gen(husband_occu)

label define husband_occu 0 "Not working" 1 "Farmer" 2 "Agricultural,fishing & poultry Worker" 3 "day labor" 4 "factory worker" 5 "service holder" 6 "Business" 7 "skilled worker" 8 "others", replace
tab v704 husband_occu,m
tab husband_occu v007,m

svy: tab husband_occu, count format(%9.0f)
svy: tab husband_occu, format(%9.4f)

tabstat cod_usd if husband_occu==0, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==2, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==3, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==4, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==5, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==6, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==7, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==8, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd husband_occu##delivery

anova cod_usd husband_occu if delivery==0
anova cod_usd husband_occu if delivery==1
anova cod_usd husband_occu if delivery==2
anova cod_usd husband_occu

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

tabstat cod_usd if household_member==0, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if household_member==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd household_member##delivery

anova cod_usd household_member if delivery==0
anova cod_usd household_member if delivery==1
anova cod_usd household_member if delivery==2
anova cod_usd household_member


* wealth index 
tab v190

recode v190 (1/2=0 "Poor") (3=1 "Middle") (4/5=2 "Rich"), gen(WealthIndex)

label define WealthIndex 0 "Poor" 1 "Middle" 2 "Rich", replace

svy: tab WealthIndex, count format(%9.0f)
svy: tab WealthIndex, format(%9.4f)

tabstat cod_usd if WealthIndex==0, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if WealthIndex==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if WealthIndex==2, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd WealthIndex##delivery

anova cod_usd WealthIndex if delivery==0
anova cod_usd WealthIndex if delivery==1
anova cod_usd WealthIndex if delivery==2
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

tabstat cod_usd if birthord_cat==0, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if birthord_cat==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if birthord_cat==2, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd birthord_cat##delivery

anova cod_usd birthord_cat if delivery==0
anova cod_usd birthord_cat if delivery==1
anova cod_usd birthord_cat if delivery==2
anova cod_usd birthord_cat


*watching tv, radio, and internet 
*media

*watching tv, radio, and internet 
*media
tab v159,m
gen tvexp=v159
recode tvexp 0=0
recode tvexp 1/3=1
tab v159 tvexp,m
tab tvexp,m

tab v158,m
gen radioexp=v158
recode radioexp 0=0
recode radioexp 1/3=1
tab radioexp

gen massmedia_exposure = tv + radio
svy: tab massmedia_exposure
recode massmedia_exposure 0=0
recode massmedia_exposure 1/3=1
label define massmedia_exposure 0 "No" 1 "Yes"
label values massmedia_exposure massmedia_exposure
tab massmedia_exposure,m

svy: tab massmedia_exposure, count format(%9.0f)
svy: tab massmedia_exposure, format(%9.4f)


tabstat cod_usd if massmedia_exposure==0, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if massmedia_exposure==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd massmedia_exposure##delivery

anova cod_usd massmedia_exposure if delivery==0
anova cod_usd massmedia_exposure if delivery==1
anova cod_usd massmedia_exposure if delivery==2
anova cod_usd massmedia_exposure


*residence and diviion 
tab1 v024 v025

svy: tab v024, count format(%9.0f)
svy: tab v024, format(%9.4f)

tabstat cod_usd if v024==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==2, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==3, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==4, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==5, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==6, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==7, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==8, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd v024##delivery

anova cod_usd v024 if delivery==0
anova cod_usd v024 if delivery==1
anova cod_usd v024 if delivery==2
anova cod_usd v024

svy: tab v025, count format(%9.0f)
svy: tab v025, format(%9.4f)

tabstat cod_usd if v025==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v025==2, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd v025##delivery

anova cod_usd  v025 if delivery==0
anova cod_usd  v025 if delivery==1
anova cod_usd  v025 if delivery==2
anova cod_usd  v025

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

tabstat cod_usd if religon_cat==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if religon_cat==2, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd religon_cat##delivery

tabstat cod_usd, by (delivery) stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd delivery


anova cod_usd religon_cat if delivery==0
anova cod_usd religon_cat if delivery==1
anova cod_usd religon_cat if delivery==2
anova cod_usd religon_cat

histogram cod_usd, frequency by(delivery, total)

histogram cod_usd_log, frequency by(delivery, total)

histogram cod_usd_ln, frequency by(delivery, total)

*normality test 
swilk cod_bdt cod_usd cod_usd_log cod_usd_ln
sfrancia cod_bdt cod_usd cod_usd_log cod_usd_ln

kdensity cod_bdt
kdensity cod_usd_log,normal
kdensity cod_usd_log
kdensity cod_usd_ln



*all predictor variables 
set cformat %9.2f
svy: regress cod_usd_log i.age_cat i.v106 i.v714 i.bmi4a i.anc i.hage_cat i.v701 i.husband_occu i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure i.v024 i.v025 i.religon_cat if delivery == 0

regress cod_usd_log i.age_cat i.v106 i.v714 i.bmi4a i.anc i.hage_cat i.v701 i.husband_occu i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure i.v024 i.v025 i.religon_cat if delivery == 0
vif
estat ic


set cformat %9.2f
svy: regress cod_usd_log i.age_cat i.v106 i.v714 i.bmi4a i.anc i.hage_cat i.v701 i.husband_occu i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure i.v024 i.v025 i.religon_cat if delivery == 1

regress cod_usd_log i.age_cat i.v106 i.v714 i.bmi4a i.anc i.hage_cat i.v701 i.husband_occu i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure i.v024 i.v025 i.religon_cat if delivery == 1
vif
estat ic

set cformat %9.2f
svy: regress cod_usd_log i.age_cat i.v106 i.v714 i.bmi4a i.anc i.hage_cat i.v701 i.husband_occu i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure i.v024 i.v025 i.religon_cat if delivery == 2

regress cod_usd_log i.age_cat i.v106 i.v714 i.bmi4a i.anc i.hage_cat i.v701 i.husband_occu i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure i.v024 i.v025 i.religon_cat if delivery == 2
vif
estat ic


*Total
set cformat %9.2f
svy: regress cod_usd_log i.delivery  i.age_cat i.v106 i.v714 i.bmi4a i.anc i.hage_cat i.v701 i.husband_occu i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure i.v024 i.v025 i.religon_cat 

regress cod_usd_log i.delivery i.age_cat i.v106 i.v714 i.bmi4a i.anc i.hage_cat i.v701 i.husband_occu i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure i.v024 i.v025 i.religon_cat 
vif
estat ic


save "E:\ResearchProject\Baker Bhai\Cost of Delivery\2017_CD_Data.dta"




*******************************************************************************
*				             2022                                             *
*******************************************************************************

clear all 
cd "E:\ResearchProject\Baker Bhai\Cost of Delivery"

set maxvar 30000

*import 2022 data 
use "E:\ResearchProject\Baker Bhai\Cost of Delivery\BD_2022_DHS_Stata\BDIR81DT\BDIR81FL", replace 
*tables 9.12-13
*keep necessary variable 

keep v007 v005 v012 v013 v021 v022 v024 v025 v106 v130 v136 v158 v159 v190 v218 v445 v701 v704 v705 v714 v730 m14_1 m15_1 m15_2 m15_3 m15_4 m15_5 m15_6 m17_1 m17_2 m17_3 m17_4 m17_5 m17_6 s436a_1  bord_01 v158 v159 v171a

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
recode m15_1 (10/11=0 "Home/Non-facility deliverie")(20/28=1 "Public Facility-based deliveries") (31/36=2 "Private Facility-based deliveries") (41=3 "NGO") (42/96=4 "Others"), gen(delivery_place)

tab delivery_place,m
label var delivery_place "Place of delivery"
label val delivery_place delivery_place
tab delivery_place m17_1,m
tab delivery_place ,m

*create delivery variable 
gen delivery=. 
replace delivery=0 if m17_1==0 & delivery_place==0 
replace delivery=1 if m17_1==0 & delivery_place!=0  
replace delivery=2 if m17_1==1 & delivery_place!=0  
drop if delivery==.
tab delivery,m 

label define delivery 0 "Home delivery" 1 "Institutional normal" 2 "Institutional c-section "
label val delivery delivery
tab delivery,m

svy: tab delivery, count format(%9.0f)
svy: tab delivery, format(%9.4f)


*age

tabstat v012, by (delivery) stat(n mean sd p50 p25 p75) format(%9.2f)
ttest v012, by(delivery)

tab v013
gen age_cat=v013
recode age_cat 4/7=4
tab age_cat 
label define age_cat 1 "15-19" 2 "20-24" 3 "25-29" 4 "30-49"
label val age_cat age_cat
tab age_cat,m

svy: tab age_cat, count format(%9.0f)
svy: tab age_cat, format(%9.4f)


tabstat cod_usd if age_cat==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if age_cat==2, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if age_cat==3, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if age_cat==4, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd age_cat##delivery

anova cod_usd age_cat if delivery==1
anova cod_usd age_cat if delivery==2
anova cod_usd age_cat

* women edducation 
tab v106,m

svy: tab v106, count format(%9.0f)
svy: tab v106, format(%9.4f)

tabstat cod_usd if v106==0, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v106==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v106==2, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v106==3, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd v106##delivery

anova cod_usd v106 if delivery==1
anova cod_usd v106 if delivery==2
anova cod_usd v106

*Women Currently working
tab v714
svy: tab v714, count format(%9.0f)
svy: tab v714, format(%9.4f)

tabstat cod_usd if v714==0, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v714==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd v714##delivery

anova cod_usd v714 if delivery==1
anova cod_usd v714 if delivery==2
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

tabstat cod_usd if bmi4a==0, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if bmi4a==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if bmi4a==2, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if bmi4a==3, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd bmi4a##delivery

anova cod_usd bmi4a if delivery==1
anova cod_usd bmi4a if delivery==2
anova cod_usd bmi4a


*ANC (NW).
tab m14_1
recode m14_1 98=.
recode m14_1 (0=0 "No ANC") (1/3=1 "1-3") (4/20=2 ">=4"), gen(anc)
label define anc 0 "No ANC" 1 "1-3" 2 ">=4", replace

svy: tab anc, count format(%9.0f)
svy: tab anc, format(%9.4f)

tabstat cod_usd if anc==0, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if anc==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if anc==2, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd anc##delivery

anova cod_usd anc if delivery==1
anova cod_usd anc if delivery==2
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

tabstat cod_usd if hage_cat==0, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if hage_cat==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if hage_cat==2, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd hage_cat##delivery

anova cod_usd hage_cat if delivery==1
anova cod_usd hage_cat if delivery==2
anova cod_usd hage_cat

*husband education 
tab v701,m 
replace v701=. if v701==8

svy: tab v701, count format(%9.0f)
svy: tab v701, format(%9.4f)

tabstat cod_usd if v701==0, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v701==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v701==2, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v701==3, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd v701##delivery

anova cod_usd v701 if delivery==1
anova cod_usd v701 if delivery==2
anova cod_usd v701


*husband occupation 
*v705 (Farmer, day labourer, factory worker, driver, service holder, business, other)
tab v704,m
recode v704 99998=.
recode v704 98=.
recode v704 (0=0 "Not working") (12=1 "Farmer")(13/15=2 "Agricultural,fishing & poultry Worker")(21=3 "day labor") (23=4 "factory worker") (41=5 "service holder") (51/52 16=6 "Business") (31=7 "Carpenter, Masson, Bus/taxi driver, Construction supervisor, Seamstresses/Tailor, Policeman, Armed services, Dai, Community health worker, FWA, Similar services") (61/62 11 22 96=8 "others"), gen(husband_occu)

label define husband_occu 0 "Not working" 1 "Farmer" 2 "Agricultural,fishing & poultry Worker" 3 "day labor" 4 "factory worker" 5 "service holder" 6 "Business" 7 "skilled worker" 8 "others", replace
tab v704 husband_occu,m
tab husband_occu,m

svy: tab husband_occu, count format(%9.0f)
svy: tab husband_occu, format(%9.4f)

tabstat cod_usd if husband_occu==0, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==2, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==3, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==4, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==5, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==6, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==7, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==8, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd husband_occu##delivery

anova cod_usd husband_occu if delivery==1
anova cod_usd husband_occu if delivery==2
anova cod_usd husband_occu

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

tabstat cod_usd if household_member==0, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if household_member==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd household_member##delivery

anova cod_usd household_member if delivery==1
anova cod_usd household_member if delivery==2
anova cod_usd household_member


* wealth index 
tab v190

recode v190 (1/2=0 "Poor") (3=1 "Middle") (4/5=2 "Rich"), gen(WealthIndex)

label define WealthIndex 0 "Poor" 1 "Middle" 2 "Rich", replace

svy: tab WealthIndex, count format(%9.0f)
svy: tab WealthIndex, format(%9.4f)

tabstat cod_usd if WealthIndex==0, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if WealthIndex==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if WealthIndex==2, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd WealthIndex##delivery

anova cod_usd WealthIndex if delivery==1
anova cod_usd WealthIndex if delivery==2
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

tabstat cod_usd if birthord_cat==0, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if birthord_cat==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if birthord_cat==2, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd birthord_cat##delivery

anova cod_usd birthord_cat if delivery==1
anova cod_usd birthord_cat if delivery==2
anova cod_usd birthord_cat


*watching tv, radio, and internet 
*media
tab v159,m
gen tvexp=v159
recode tvexp 0=0
recode tvexp 1/3=1
tab v159 tvexp,m
tab tvexp,m

tab v158,m
gen radioexp=v158
recode radioexp 0=0
recode radioexp 1/3=1

tab radioexp

*internet
gen inte=v171a
recode inte 0=0
recode inte 1/3=1

gen massmedia_exposure = tv + radio + inte
svy: tab massmedia_exposure
recode massmedia_exposure 0=0
recode massmedia_exposure 1/3=1
label define massmedia_exposure 0 "No" 1 "Yes"
label values massmedia_exposure massmedia_exposure
tab massmedia_exposure,m

svy: tab massmedia_exposure, count format(%9.0f)
svy: tab massmedia_exposure, format(%9.4f)


tabstat cod_usd if massmedia_exposure==0, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if massmedia_exposure==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd massmedia_exposure##delivery

anova cod_usd massmedia_exposure if delivery==1
anova cod_usd massmedia_exposure if delivery==2
anova cod_usd massmedia_exposure


*residence and diviion 
tab1 v024 v025

svy: tab v024, count format(%9.0f)
svy: tab v024, format(%9.4f)

tabstat cod_usd if v024==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==2, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==3, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==4, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==5, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==6, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==7, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==8, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd v024##delivery

anova cod_usd v024 if delivery==1
anova cod_usd v024 if delivery==2
anova cod_usd v024

svy: tab v025, count format(%9.0f)
svy: tab v025, format(%9.4f)

tabstat cod_usd if v025==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v025==2, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd v025##delivery

anova cod_usd  v025 if delivery==1
anova cod_usd  v025 if delivery==2
anova cod_usd  v025

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

tabstat cod_usd if religon_cat==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if religon_cat==2, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd religon_cat##delivery

anova cod_usd religon_cat if delivery==1
anova cod_usd religon_cat if delivery==2
anova cod_usd religon_cat

tabstat cod_usd, by (delivery) stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd delivery

histogram cod_usd, frequency by(delivery, total)

histogram cod_usd_log, frequency by(delivery, total)

histogram cod_usd_ln, frequency by(delivery, total)

*normality test 
swilk cod_bdt cod_usd cod_usd_log cod_usd_ln
sfrancia cod_bdt cod_usd cod_usd_log cod_usd_ln

kdensity cod_bdt
kdensity cod_usd_log,normal
kdensity cod_usd_log
kdensity cod_usd_ln



*all predictor variables 
set cformat %9.2f
svy: regress cod_usd_log i.age_cat i.v106 i.v714 i.bmi4a i.anc i.hage_cat i.v701 i.husband_occu i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure i.v024 i.v025 i.religon_cat if delivery == 1

regress cod_usd_log i.age_cat i.v106 i.v714 i.bmi4a i.anc i.hage_cat i.v701 i.husband_occu i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure i.v024 i.v025 i.religon_cat if delivery == 1
vif
estat ic

set cformat %9.2f
svy: regress cod_usd_log i.age_cat i.v106 i.v714 i.bmi4a i.anc i.hage_cat i.v701 i.husband_occu i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure i.v024 i.v025 i.religon_cat if delivery == 2

regress cod_usd_log i.age_cat i.v106 i.v714 i.bmi4a i.anc i.hage_cat i.v701 i.husband_occu i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure i.v024 i.v025 i.religon_cat if delivery == 2
vif
estat ic


*Total
set cformat %9.2f
svy: regress cod_usd_log i.delivery  i.age_cat i.v106 i.v714 i.bmi4a i.anc i.hage_cat i.v701 i.husband_occu i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure i.v024 i.v025 i.religon_cat 

regress cod_usd_log i.delivery i.age_cat i.v106 i.v714 i.bmi4a i.anc i.hage_cat i.v701 i.husband_occu i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure i.v024 i.v025 i.religon_cat 
vif
estat ic

save "E:\ResearchProject\Baker Bhai\Cost of Delivery\2022_CD_Data.dta"








*******************************************************************************
*				           Pooled Data                                        *
*******************************************************************************


clear
*directory
cd "E:\ResearchProject\Baker Bhai\Cost of Delivery"

append using 2014_CD_Data 2017_CD_Data 2022_CD_Data

gen wgt_app=wgt
svyset v021 [pweight=wgt_app], singleunit(scaled) strata(v022)


*delivery variable 
svy: tab delivery, count format(%9.0f)
svy: tab delivery, format(%9.4f)



*age
tabstat v012, by (delivery) stat(n mean sd p50 p25 p75) format(%9.2f)
tabstat v012, stat(n mean sd p50 p25 p75) format(%9.2f)
anova cod_usd v012##delivery
svy: tab age_cat, count format(%9.0f)
svy: tab age_cat, format(%9.4f)
tabstat cod_usd if age_cat==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if age_cat==2, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if age_cat==3, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if age_cat==4, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd age_cat##delivery
anova cod_usd age_cat if delivery==0
anova cod_usd age_cat if delivery==1
anova cod_usd age_cat if delivery==2



* women edducation
svy: tab v106, count format(%9.0f)
svy: tab v106, format(%9.4f)
tabstat cod_usd if v106==0, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v106==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v106==2, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v106==3, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd v106##delivery
anova cod_usd v106 if delivery==0
anova cod_usd v106 if delivery==1
anova cod_usd v106 if delivery==2
anova cod_usd v106




*Women Currently working
svy: tab v714, count format(%9.0f)
svy: tab v714, format(%9.4f)
tabstat cod_usd if v714==0, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v714==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd v714##delivery
anova cod_usd v714 if delivery==0
anova cod_usd v714 if delivery==1
anova cod_usd v714 if delivery==2
anova cod_usd v714




*BMI v445
svy: tab bmi4a, count format(%9.0f)
svy: tab bmi4a, format(%9.4f)
tabstat cod_usd if bmi4a==0, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if bmi4a==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if bmi4a==2, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if bmi4a==3, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd bmi4a##delivery
anova cod_usd bmi4a if delivery==0
anova cod_usd bmi4a if delivery==1
anova cod_usd bmi4a if delivery==2
anova cod_usd bmi4a





*ANC (NW).
svy: tab anc, count format(%9.0f)
svy: tab anc, format(%9.4f)
tabstat cod_usd if anc==0, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if anc==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if anc==2, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd anc##delivery
anova cod_usd anc if delivery==0
anova cod_usd anc if delivery==1
anova cod_usd anc if delivery==2
anova cod_usd anc




*Husband age
svy: tab hage_cat, count format(%9.0f)
svy: tab hage_cat, format(%9.4f)
tabstat cod_usd if hage_cat==0, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if hage_cat==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if hage_cat==2, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd hage_cat##delivery
anova cod_usd hage_cat if delivery==0
anova cod_usd hage_cat if delivery==1
anova cod_usd hage_cat if delivery==2
anova cod_usd hage_cat



*husband education 
svy: tab v701, count format(%9.0f)
svy: tab v701, format(%9.4f)
tabstat cod_usd if v701==0, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v701==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v701==2, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v701==3, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd v701##delivery
anova cod_usd v701 if delivery==0
anova cod_usd v701 if delivery==1
anova cod_usd v701 if delivery==2
anova cod_usd v701




*husband occupation 
svy: tab husband_occu, count format(%9.0f)
svy: tab husband_occu, format(%9.4f)
tabstat cod_usd if husband_occu==0, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==2, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==3, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==4, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==5, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==6, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==7, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if husband_occu==8, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd husband_occu##delivery
anova cod_usd husband_occu if delivery==0
anova cod_usd husband_occu if delivery==1
anova cod_usd husband_occu if delivery==2
anova cod_usd husband_occu




*household size  (<4, 4-5, >5)
svy: tab household_member, count format(%9.0f)
svy: tab household_member, format(%9.4f)
tabstat cod_usd if household_member==0, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if household_member==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd household_member##delivery
anova cod_usd household_member if delivery==0
anova cod_usd household_member if delivery==1
anova cod_usd household_member if delivery==2
anova cod_usd household_member



* wealth index
svy: tab WealthIndex, count format(%9.0f)
svy: tab WealthIndex, format(%9.4f)
tabstat cod_usd if WealthIndex==0, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if WealthIndex==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if WealthIndex==2, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd WealthIndex##delivery
anova cod_usd WealthIndex if delivery==0
anova cod_usd WealthIndex if delivery==1
anova cod_usd WealthIndex if delivery==2
anova cod_usd WealthIndex



*birth order 
svy: tab birthord_cat, count format(%9.0f)
svy: tab birthord_cat, format(%9.4f)
tabstat cod_usd if birthord_cat==0, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if birthord_cat==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if birthord_cat==2, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd birthord_cat##delivery
anova cod_usd birthord_cat if delivery==0
anova cod_usd birthord_cat if delivery==1
anova cod_usd birthord_cat if delivery==2
anova cod_usd birthord_cat



*watching tv, radio, and internet 
svy: tab massmedia_exposure, count format(%9.0f)
svy: tab massmedia_exposure, format(%9.4f)
tabstat cod_usd if massmedia_exposure==0, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if massmedia_exposure==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd massmedia_exposure##delivery
anova cod_usd massmedia_exposure if delivery==0
anova cod_usd massmedia_exposure if delivery==1
anova cod_usd massmedia_exposure if delivery==2
anova cod_usd massmedia_exposure




*residence and diviion 
svy: tab v024, count format(%9.0f)
svy: tab v024, format(%9.4f)
tabstat cod_usd if v024==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==2, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==3, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==4, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==5, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==6, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==7, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v024==7, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd v024##delivery
anova cod_usd v024 if delivery==0
anova cod_usd v024 if delivery==1
anova cod_usd v024 if delivery==2
anova cod_usd v024




svy: tab v025, count format(%9.0f)
svy: tab v025, format(%9.4f)
tabstat cod_usd if v025==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if v025==2, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd v025##delivery
anova cod_usd  v025 if delivery==0
anova cod_usd  v025 if delivery==1
anova cod_usd  v025 if delivery==2
anova cod_usd  v025




*religion ( muslim and others)
svy: tab religon_cat, count format(%9.0f)
svy: tab religon_cat, format(%9.4f)

tabstat cod_usd if religon_cat==1, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
tabstat cod_usd if religon_cat==2, by (delivery)  stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd religon_cat##delivery

anova cod_usd religon_cat if delivery==0
anova cod_usd religon_cat if delivery==1
anova cod_usd religon_cat if delivery==2
anova cod_usd religon_cat


*Total delivery
tabstat cod_usd, by (delivery) stat(n mean sd median iqr  p50 p25 p75) format(%9.2f)
anova cod_usd delivery




*all predictor variables 
set cformat %9.2f
svy: regress cod_usd_log i.age_cat i.v106 i.v714 i.bmi4a i.anc i.hage_cat i.v701 i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure i.v024 i.v025 i.religon_cat if delivery == 0

regress cod_usd_log i.age_cat i.v106 i.v714 i.bmi4a i.anc i.hage_cat i.v701 i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure i.v024 i.v025 i.religon_cat if delivery == 0
vif
estat ic


set cformat %9.2f
svy: regress cod_usd_log i.age_cat i.v106 i.v714 i.bmi4a i.anc i.hage_cat i.v701 i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure i.v024 i.v025 i.religon_cat if delivery == 1

regress cod_usd_log i.age_cat i.v106 i.v714 i.bmi4a i.anc i.hage_cat i.v701 i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure i.v024 i.v025 i.religon_cat if delivery == 1
vif
estat ic

set cformat %9.2f
svy: regress cod_usd_log i.age_cat i.v106 i.v714 i.bmi4a i.anc i.hage_cat i.v701 i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure i.v024 i.v025 i.religon_cat if delivery == 2

regress cod_usd_log i.age_cat i.v106 i.v714 i.bmi4a i.anc i.hage_cat i.v701 i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure i.v024 i.v025 i.religon_cat if delivery == 2
vif
estat ic


*Total
set cformat %9.2f
svy: regress cod_usd_log i.delivery  i.age_cat i.v106 i.v714 i.bmi4a i.anc i.hage_cat i.v701 i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure i.v024 i.v025 i.religon_cat 

regress cod_usd_log i.delivery i.age_cat i.v106 i.v714 i.bmi4a i.anc i.hage_cat i.v701 i.household_member i.WealthIndex i.birthord_cat i.massmedia_exposure i.v024 i.v025 i.religon_cat 
vif
estat ic







