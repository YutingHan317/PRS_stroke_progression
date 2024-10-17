**************************************************************************************************************************
*************************************         1.Import and Merge       ***************************************************
**************************************************************************************************************************
cd "/public/home/hanyuting/2022-04_CMM_G"

*-----------------------------------------------------------------
*** Endpoints/这个版本数据纳入了PVD icase的数据，所以不能直接用combined data
import delimited "/public/home/hanyuting/00-原始数据/2023-05-19_CMM_G/endpoints.csv", encoding(utf8) clear

* Format all date variable
des *date*,full varlist 
local varlist = r(varlist)
foreach var of local varlist{

	local var_1 = substr("`var'",1,26)
    gen `var_1' = substr(`var',1,10)
	gen `var_1'1 = date(`var_1',"YMD",2050)
	drop `var' `var_1'
	rename `var_1'1 `var'
	format `var' %td
    di "`var'"
	
}

***  incident cases
* ischemic heart disease
gen CHD = (ep_ckb0003_da_ep == 1 | ep_ckb0003_du_ep == 1 | ep_ckb0003_dis_ep == 1 | ep_ckb0003_hiip_ep == 1)
gen CHD_date = min(ep_ckb0003_da_datedeveloped,ep_ckb0003_du_datedeveloped,ep_ckb0003_dis_datedeveloped,ep_ckb0003_hiip_datedeveloped)

*diabetes
gen E10 = (ep_ckb0047_da_ep == 1 | ep_ckb0047_du_ep == 1 | ep_ckb0047_dis_ep == 1 | ep_ckb0047_hiip_ep == 1)
gen E10_date = min(ep_ckb0047_da_datedeveloped,ep_ckb0047_du_datedeveloped,ep_ckb0047_dis_datedeveloped,ep_ckb0047_hiip_datedeveloped)
gen E11 = (ep_ckb0048_da_ep == 1 | ep_ckb0048_du_ep == 1 | ep_ckb0048_dis_ep == 1 | ep_ckb0048_hiip_ep == 1)
gen E11_date = min(ep_ckb0048_da_datedeveloped,ep_ckb0048_du_datedeveloped,ep_ckb0048_dis_datedeveloped,ep_ckb0048_hiip_datedeveloped)
gen c_ep0301 = (ep_ckb0301_da_ep == 1 | ep_ckb0301_du_ep == 1 | ep_ckb0301_dis_ep == 1 | ep_ckb0301_hiip_ep == 1)
gen c_ep0301_date = min(ep_ckb0301_da_datedeveloped,ep_ckb0301_du_datedeveloped,ep_ckb0301_dis_datedeveloped,ep_ckb0301_hiip_datedeveloped)
gen c_ep0302 = (ep_ckb0302_da_ep == 1 | ep_ckb0302_du_ep == 1 | ep_ckb0302_dis_ep == 1 | ep_ckb0302_hiip_ep == 1)
gen c_ep0302_date = min(ep_ckb0302_da_datedeveloped,ep_ckb0302_du_datedeveloped,ep_ckb0302_dis_datedeveloped,ep_ckb0302_hiip_datedeveloped)

global eplist CHD E10 E11 c_ep0301 c_ep0302
global eplist_date CHD_date E10_date E11_date c_ep0301_date c_ep0302_date
keep csid ${eplist} ${eplist_date}
misstable sum csid ${eplist} ${eplist_date}

save "1.Data/endpoints.dta",replace

*** Baseline
import delimited "/public/home/hanyuting/00-原始数据/2023-05-19_CMM_G/data_baseline_questionnaires.csv", encoding(utf8) clear

*** 查看数据结构
des,fullnames

*** 处理死亡变量
gen death = (censoring_reason == "Dead                          ")
gen death_date = censoring_date

*** Merge
* 数据申请时如果选择GWAS linkage就没办法勾选很多协变量，因此我从以前GWAS数据库中通过一些变量匹配到ccvid，从而可以跟GWAS数据merge
* merge数据库生成代码如下
// import delimited "/public/home/hanyuting/2022-04_Gastric_Cancer/Others/sample.linkage.csv", clear case(preserve)
// save "1.Data/linkage.dta",replace

// import delimited "/public/home/hanyuting/00-原始数据/2022-07-19_GC/data_baseline_questionnaires.csv", clear case(preserve)
// keep csid study_date dob_anon region_code is_female
// save "1.Data/cov_gc.dta",replace

// import delimited "/public/home/hanyuting/00-原始数据/2022-07-19_GC/data_gwas_genetics.csv", clear case(preserve)
// keep csid ccvid is_in_gwas_population_subset gwas_array_type
// merge 1:1 csid using "1.Data/cov_gc.dta", nogen
// drop csid
// save "1.Data/complementary_gc.dta",replace
merge 1:1 study_date dob_anon region_code is_female using "1.Data/complementary_gc.dta",nogen keepusing(ccvid is_in_gwas_population_subset gwas_array_type)
merge 1:1 csid using "1.Data/endpoints.dta",nogen

*** transformation of data variables
local varlist dob_anon study_date censoring_date death_date
foreach var of local varlist{

	gen `var'1 = substr(`var',1,10)
	gen `var'2 = date(`var'1,"YMD",2050)
	drop `var' `var'1
	rename `var'2 `var'
	format `var' %td
	
}

***Generate T2DM using lv's code
gen T2D = 1 if E11 == 1
replace T2D = 1 if E11 == 0 & c_ep0301 == 1 & (E10 == 0 & c_ep0302 == 0)
replace T2D = 0 if E11 == 0 & c_ep0301 == 1 & (E10 == 1 | c_ep0302 == 1)
replace T2D = 0 if E11 == 0 & c_ep0301 == 0

gen T2D_date = E11_date if E11 == 1 & c_ep0301 == 0
replace T2D_date = E11_date if E11 == 1 & c_ep0301 == 1 & (E10 == 0 & c_ep0302 == 0) & (E11_date <= c_ep0301_date)
replace T2D_date = c_ep0301_date if E11 == 1 & c_ep0301 == 1 & (E10 == 0 & c_ep0302 == 0) & (E11_date > c_ep0301_date)
replace T2D_date = E11_date if E11 == 1 & c_ep0301 == 1 & (E10 == 1 | c_ep0302 == 1)
replace T2D_date = c_ep0301_date if E11 == 0 & c_ep0301 == 1 & (E10 == 0 & c_ep0302 == 0)
replace T2D_date = death_date if T2D == 0

misstable summarize $eplist_status $eplist_status T2D T2D_date

save "1.Data/merged_database.dta",replace

**************************************************************************************************************************
************************************          2. Baseline diseases        ************************************************
**************************************************************************************************************************
*** Import dataset
use "1.Data/merged_database.dta", clear

*** Labeling
label variable has_diabetes "Basline diabetes"
label variable chd_diag "Basline CHD"
label variable stroke_or_tia_diag "Baseline stroke"
* label variable has_copd "Baseline COPD"
* label variable cancer_diag "Baseline cancer"
* label variable rheum_heart_dis_diag "Baseline rheumatic heart disease"
* label variable kidney_dis_diag "Baseline chronic kidney disease"


*** hypertension
gen has_hypertension = 1 if hypertension_diag == 1 | sbp_mean >= 140 | dbp_mean >= 90 | used_blood_pressure_drugs == 1
	replace has_hypertension = 0 if hypertension_diag == 0 & sbp_mean < 140 & dbp_mean < 90 & used_blood_pressure_drugs == 0
label variable has_hypertension "Baseline hypertension"

local medicinelist taking_aspirin taking_ace_i taking_beta_blocker taking_statins taking_diuretics taking_ca_antagonist taking_chlor_metaformin taking_insulin
foreach med of local medicinelist{

	clonevar `med'_2groups = `med'
		replace `med'_2groups = 0 if mi(`med'_2groups)
		local varlable :  variable label `med'
		label variable `med'_2groups "`varlable'"
		label values `med'_2groups taking_medicine

}

*** 合并降糖药和胰岛素
gen glucose_lowering_2groups = 1 if taking_chlor_metaformin_2groups == 1 | taking_insulin_2groups == 1
	replace glucose_lowering_2groups = 0 if mi(glucose_lowering_2groups)
	label variable glucose_lowering_2groups "Lower blood glucose medicine"


**************************************************************************************************************************
************************************          3. Incident diseases        ************************************************
**************************************************************************************************************************
*-------------------------------------------------------------------------------------------------------------------------
/* Transform to age */
local eplist CHD T2D death
loca varlist dob_anon
foreach ep of local eplist{

	gen double age_`ep' = (`ep'_date - dob_anon)/365.25
	local varlist `varlist' age_`ep' 
}
sum `varlist'

**************************************************************************************************************************
************************************             4. Covariates            ************************************************
**************************************************************************************************************************
/* Sociodemographic variables */
***residence
label variable region_is_urban residence
label define residence 0 "Rural" 1 "Urban"
label values region_is_urban residence

***education
label variable highest_education "highest education"
label define highest_education 0 "no formal school" 1 "primary school" 2 "middle school" 3 "high school" 4 "technical school" 5 "university"
label values highest_education highest_education

***age at baseline
egen age_strata=cut(age_at_study_date), at(30(5)80) label
	tab age_strata
egen age_2groups = cut(age_at_study_date), at(0 60 100) icodes
	label variable age_2groups "age two groups at baseline"
	label define age_2groups 0 "<50 y" 1 ">=60 y" 
	label values age_2groups age_2groups	


/* Family history */
***stroke family history
gen stroke_fh = 1 if mother_stroke == 1 | father_stroke == 1 | (siblings_stroke >= 1 & siblings_stroke != .)
	replace stroke_fh = 0 if (mother_stroke == 0 & father_stroke == 0 & siblings_stroke == 0)|(mother_stroke == 0 & father_stroke == 0 & siblings == 0)
	replace stroke_fh = 2 if stroke_fh == .
label variable stroke_fh "Family history: stroke"
label define stroke_fh 2 "Don't know" 1 "Yes" 0 "No"
label values stroke_fh stroke_fh
recode stroke_fh (0 2=0 "No") (1=1 "Yes"), gen(stroke_fh_2groups)
	label variable stroke_fh_2groups "Stroke family history 2groups"

/* 高血脂 */
gen has_hyperlipidemia = (taking_statins == 1)
	replace has_hyperlipidemia = 0 if mi(has_hyperlipidemia)

**********************************************************************************************************
****************************                5 Lifestyles            **************************************
**********************************************************************************************************
/* traditional variables */
***smoking
* 2023-10-11 四分组定义
gen smoking_4groups = 1 if smoking_category == 1 | smoking_category == 2     
	replace smoking_4groups = 2 if smoking_category == 3 & (smoking_stopped_reason > 0 & smoking_stopped_reason <= 4)
	replace smoking_4groups = 3 if smoking_category == 3 & smoking_stopped_reason == 0
	replace smoking_4groups = 4 if smoking_category == 4 

***alcohol
* 2023-10-11 饮酒五分组
gen alcohol_5groups = 1 if alcohol_category == 1 | alcohol_category == 3 | alcohol_category == 4
	replace alcohol_5groups = 2 if alcohol_category == 2 | alcohol_category == 5
	replace alcohol_5groups = 3 if alcohol_category == 6 & (alc_weekly == 0 | alc_weekly == 1)
	replace alcohol_5groups = 4 if alcohol_category == 6 & alc_weekly == 2 & total_alc_typ_day_g < 15.00
	replace alcohol_5groups = 5 if alcohol_category == 6 & alc_weekly == 2 & total_alc_typ_day_g >= 15.00

***diet

* 新分析中分析了饮食与6种结局之间的关联，决定纳入红肉、蛋类、新鲜蔬菜、新鲜水果  05-09
gen diet_component1 = 1 if diet_freq_fresh_veg == 0
	replace diet_component1 = 0 if mi(diet_component1)
	label variable diet_component1 "Daily eating vegetable"
gen diet_component2 = 1 if diet_freq_fresh_fruit == 0
	replace diet_component2 = 0 if mi(diet_component2)
	label variable diet_component2 "Daily eating fruit"
gen diet_component3 = 1 if diet_freq_meat > 0 & diet_freq_meat < 3
	replace diet_component3 = 0 if mi(diet_component3)
	label variable diet_component3 "weekly not daily meat"
gen diet_component4 = 1 if diet_freq_eggs == 0 
	replace diet_component4 = 0 if mi(diet_component4)
	label variable diet_component4 "daily eating egg"
egen diet_5groups = rowtotal(diet_component1 diet_component2 diet_component3 diet_component4)
tab diet_5groups
gen healthy_diet = 1 if diet_5groups == 4
	replace healthy_diet = 0 if mi(healthy_diet)
label variable healthy_diet "Healthy diet"

***obesity
** BMI
recode bmi_calc (min/18.499=1 "Underweight <18.5") (18.5/23.999=2 "Normal <24.0") (24.0/27.9=3 "Overweight <28.0") (28.0/60.0=4 "Obesity >=28.0"), gen(bmi_4groups)
* 将超重纳入正常
recode bmi_4groups (2/3=1 "normal&overweight") (1 4=0 "Underweight&obesity"), gen(healthy_bmi)
* obesity
recode bmi_calc (min/27.999=1 "not obesity") (28.0/60.0=4 "Obesity >=28.0"), gen(obesity)

***waist
gen waist_cm = waist_mm/10
gen WC_3groups = 1 if is_female == 0 & waist_mm < 850
	replace WC_3groups = 1 if is_female == 1 & waist_mm < 800
	replace WC_3groups = 2 if is_female == 0 & waist_mm >= 850
	replace WC_3groups = 2 if is_female == 1 & waist_mm >= 800
	replace WC_3groups = 3 if is_female == 0 & waist_mm >= 900
	replace WC_3groups = 3 if is_female == 1 & waist_mm >= 850
	label variable WC_3groups "Central obesity based on WC: men-85/90; women-80/85"

*** 下面这个是最终使用的版本！！！！
gen obesity_5groups = 1 if bmi_4groups == 1
	replace obesity_5groups = 2 if (bmi_4groups == 2 | bmi_4groups == 3) & WC_3groups < 3
	replace obesity_5groups = 3 if (bmi_4groups == 2 | bmi_4groups == 3) & WC_3groups == 3
	replace obesity_5groups = 4 if bmi_4groups == 4 & WC_3groups < 3
	replace obesity_5groups = 5 if bmi_4groups == 4 & WC_3groups == 3
	label variable obesity_5groups "Obesity based on BMI & WC"
label define obesity_5groups 1 "Underweight" 2 "Normal/overweight with normal WC" 3 "Normal/overweight with central obesity" 4 "General obesity with normal WC" 5 "General & central obesity"
label values obesity_5groups obesity_5groups
* 有两个人缺失，其中一个测了基因，这两个人填补为低风险组
replace obesity_5groups = 2 if mi(obesity_5groups)

**********************************************************************************************************
****************************            6 Generate database         **************************************
**********************************************************************************************************
use "1.Data/basic database.dta", clear
keep if !mi(ccvid)
merge 1:1 ccvid using "1.Data/linkage.dta",nogen keep(match)
save "1.Data/full_data.dta",replace

