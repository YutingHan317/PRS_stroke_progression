* 对应Fig 2， Sup tab 6
cd "/public/home/hanyuting/2022-04_CMM_G"
import delimited "1.Data/rec_stroke_baseline.csv", case(preserve) clear

keep if surv_28d ==1 
keep if no_relatedness == 1

* 计算病后28天的年龄
egen age_strata2=cut(age_surv_28d), at(30 40(5)80 100) label
	tab age_strata2
	
clonevar outcome = sec_anystroke
	replace outcome = 2 if CHD==1&age_CHD<age_sec_anystroke
	replace outcome = 3 if death==1&sec_anystroke==0&outcome!=2
gen age_1th = min(age_sec_anystroke, age_CHD, age_death)

replace age_surv_28d= age_surv_28d-0.0001 if age_surv_28d == age_sec_anystroke

stset age_1th, origin(age_surv_28d) failure(outcome==1)
stcrreg i.PGS002725_metaISTROKE_EAScat is_female PC1-PC5 i.age_strata i.region_code, compete(outcome==2/3)
stcurve,at1(PGS002725_metaISTROKE_EAScat==1) at2(PGS002725_metaISTROKE_EAScat==3) cif outfile("1.Data\rec_stroke.dta",replace)
preserve
use "1.Data\rec_stroke.dta",clear
gen category = "rec_stroke"
export delimited "3.Result\CKB\Trans_plot\CKB_rec_stroke.csv", replace
restore


stset age_1th, origin(age_surv_28d) failure(outcome==2)
stcrreg i.PGS002725_metaISTROKE_EAScat is_female PC1-PC5 i.age_strata i.region_code, compete(outcome==1 3)
stcurve,at1(PGS002725_metaISTROKE_EAScat==1) at2(PGS002725_metaISTROKE_EAScat==3) cif outfile("1.Data\CHD.dta",replace)
preserve
use "1.Data\CHD.dta",clear
gen category = "chd"
export delimited "3.Result\CKB\Trans_plot\CKB_CHD.csv", replace
restore

stset age_death, origin(age_surv_28d) failure(death==1)
* estimate and lrtest for between-group difference
stcox i.PGS002725_metaISTROKE_EAScat is_female PC1-PC5 i.age_strata i.region_code
estimate store model1
stcox is_female PC1-PC5 i.age_strata i.region_code
estimate store model2
lrtest model1 model2

stcox i.PGS002725_metaISTROKE_EAScat is_female PC1-PC5 i.age_strata i.region_code
stcurve,at1(PGS002725_metaISTROKE_EAScat==1) at2(PGS002725_metaISTROKE_EAScat==3) failure outfile("1.Data\death.dta",replace)
preserve
use "1.Data\death.dta",clear
gen category = "death"
export delimited "3.Result\CKB\Trans_plot\CKB_death.csv", replace
restore
