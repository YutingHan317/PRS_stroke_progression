cd "/public/home/hanyuting/2022-04_CMM_G"
import delimited "1.UKB/ukb_rec_stroke_baseline.csv", case(preserve) clear

keep if surv_28d ==1 
keep if no_relatedness == 1

# 调整为生存为28天后的年龄
egen age_strata2=cut(age_surv_28d), at(50(5)90) label
	tab age_strata2

	clonevar outcome = sec_anystroke
	replace outcome = 2 if (CHD==1&sec_anystroke==1&age_CHD<age_sec_anystroke) | (CHD==1&sec_anystroke==0)
	replace outcome = 3 if death==1&sec_anystroke==0&outcome!=2
gen age_1th = min(age_sec_anystroke, age_CHD, age_death)

replace age_surv_28d= age_surv_28d-0.0001 if age_surv_28d == age_1th

stset age_1th, origin(age_surv_28d) failure(outcome==1)
stcrreg i.PGS002724_metaISTROKE_EURcat i.is_female PC1-PC10 i.age_strata2, compete(outcome==2/3)
stcurve, at1(PGS002724_metaISTROKE_EURcat==1) at2(PGS002724_metaISTROKE_EURcat==3) cif outfile("1.UKB\rec_stroke.dta",replace)
preserve
use "1.UKB\rec_stroke.dta",clear
gen category = "rec_stroke"
export delimited "2.Results\UKB\UKB_rec_stroke.csv", replace
restore


stset age_1th, origin(age_surv_28d) failure(outcome==2)
stcrreg i.PGS002724_metaISTROKE_EURcat i.is_female PC1-PC10 i.age_strata2, compete(outcome==1 3)
stcurve,at1(PGS002724_metaISTROKE_EURcat==1) at2(PGS002724_metaISTROKE_EURcat==3) cif outfile("1.UKB\CHD.dta",replace)
preserve
use "1.UKB\CHD.dta",clear
gen category = "chd"
export delimited "2.Results\UKB\UKB_CHD.csv", replace
restore


stset age_death, origin(age_surv_28d) failure(death==1)
* estimate and lrtest for between-group difference
stcox i.PGS002724_metaISTROKE_EURcat i.is_female PC1-PC10 i.age_strata2
estimate store model1
stcox i.is_female PC1-PC10 i.age_strata2
estimate store model2
lrtest model1 model2

* graph
stcurve,at1(PGS002724_metaISTROKE_EURcat==1) at2(PGS002724_metaISTROKE_EURcat==3) failure outfile("1.UKB\death.dta",replace)
preserve
use "1.UKB\death.dta",clear
gen category = "death"
export delimited "2.Results\UKB\UKB_death.csv", replace
restore
