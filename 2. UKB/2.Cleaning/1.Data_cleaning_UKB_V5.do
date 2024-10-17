
* 设置工作路径
cd /public/home/hanyuting/2022-04_CMM_G/1.UKB

************************************************************************************************************************************************
*************************************                                                  *********************************************************
*************************************         1. 先处理Algorithm-defined outcomes       *********************************************************
*************************************                                                  *********************************************************
************************************************************************************************************************************************
/* 选择Algorithm-defined outcomes as the indicator of the first stroke */
do "/public/home/hanyuting/UKB/Main_dataset/raw_other_ep.do"
/* save "/public/home/hanyuting/UKB/Main_dataset/raw_other_ep.dta",replace */

* 重命名保留的变量
rename n_eid eid
rename ts_131360_0_0 ADO_I60_date
rename ts_131362_0_0 ADO_I61_date
rename ts_131366_0_0 ADO_I63_date
rename ts_131368_0_0 ADO_I64_date

* 保留对应的列
keep eid ADO* ts_40000_0_0 n_131361_0_0 n_131363_0_0 n_131367_0_0 n_131369_0_0 

* 死亡
gen death = !mi(ts_40000_0_0)
gen death_date = ts_40000_0_0
format death_date %td
drop ts_40000_0_0
* 生成结局变量
* 1980
gen ADO_I60 = !mi(ADO_I60_date)
* 2285
gen ADO_I61 = !mi(ADO_I61_date)
* 9681
gen ADO_I63 = !mi(ADO_I63_date)
* 9872
gen ADO_I64 = !mi(ADO_I64_date)
tab1 ADO_I60 ADO_I61 ADO_I63 ADO_I64
*egen no_stroke = rowtotal(ADO_I60 ADO_I61 ADO_I63 ADO_I64) 

* 生成是否发生I60\I61\I63\I64的变量
gen first_anystroke = (ADO_I60==1|ADO_I61==1|ADO_I63==1|ADO_I64==1)
egen first_anystroke_date = rowmin(ADO_I60_date ADO_I61_date ADO_I63_date ADO_I64_date) 
format first_anystroke_date %td

* UKB同样存在不同亚型发病日期相同的情况
* 按照SAH>ICHD>IS>Uspecified定义亚型
gen first_stroke_type = 4 if ADO_I64 == 1 & ADO_I64_date == first_anystroke_date 
	replace first_stroke_type = 3 if ADO_I63 == 1 & ADO_I63_date == first_anystroke_date 
	replace first_stroke_type = 2 if ADO_I61 == 1 & ADO_I61_date == first_anystroke_date 
	replace first_stroke_type = 1 if ADO_I60 == 1 & ADO_I60_date == first_anystroke_date 
	label define subtype 1 "I60" 2 "I61" 3 "I63" 4 "I64"
	label values first_stroke_type subtype

* 生成亚型患病与否
gen first_I60 = 1 if first_stroke_type == 1
gen first_I61 = 1 if first_stroke_type == 2
gen first_I63 = 1 if first_stroke_type == 3
gen first_I64 = 1 if first_stroke_type == 4
tab1 first_I6* first_stroke_type


* 生成考虑了subtypes的疾病日期
gen first_I60_date = first_anystroke_date if first_stroke_type == 1
gen first_I61_date = first_anystroke_date if first_stroke_type == 2
gen first_I63_date = first_anystroke_date if first_stroke_type == 3
gen first_I64_date = first_anystroke_date if first_stroke_type == 4
format first_I*_date %td
cd /public/home/hanyuting/2022-04_CMM_G/1.UKB
drop ADO*
save "first_stroke.dta",replace

************************************************************************************************************************************************
*************************************                                                  *********************************************************
*************************************            2. 清理hospital inpatient data        *********************************************************
*************************************                                                  *********************************************************
************************************************************************************************************************************************
* 这部份主要使用UKB的record-level data
/* 先处理日期信息 */
* 入院信息使用了主要诊断和次要诊断
cd /public/home/hanyuting/2022-04_CMM_G/1.UKB
import delimited using "/home/hanyuting/UKB/Health_record/hesin_2023.txt",clear
keep eid ins_index dsource epistart epiend epiorder spell_index spell_seq spelbgin spelend admidate disdate
sort eid ins_index
* 生成相应的日期变量，参考UKB官方说明，定义为epistart > admidate > epiend > disdate
gen date = date(epistart,"DMY",2050)
	replace date = date(admidate,"DMY",2050) if mi(date)
	replace date = date(epiend,"DMY",2050) if mi(date)
	replace date = date(disdate,"DMY",2050) if mi(date)
* 有42条记录没有入院的纪录
keep eid ins_index dsource date epiorder spell_index spell_seq spelbgin spelend 
format date %td
*misstable sum
save "hesin_date.dta", replace

/* 再处理疾病诊断信息 */
import delimited using "/home/hanyuting/UKB/Health_record/hesin_diag_2023.txt",clear
*des
*codebook
*每条记录要么由ICD-10编码，要么由ICD-9编码
gen ICD9_raw =  substr(diag_icd9,1,3)
gen ICD10_raw =  substr(diag_icd10,1,3)
* 全部清理为疾病类型
gen SAH = 1 if ICD9_raw == "430" | ICD10_raw == "I60" 
gen ICH = 1 if ICD9_raw == "431" | ICD10_raw == "I61" 
gen  IS = 1 if ICD9_raw == "434" | ICD10_raw == "I63" 
gen Uns = 1 if ICD9_raw == "436" | ICD10_raw == "I64" 
gen stroke = 1 if SAH == 1 | ICH == 1 | IS == 1 | Uns == 1

* 仅保留需要的变量
keep eid ins_index arr_index level SAH ICH IS Uns stroke
* 仅保留卒中//33,811次卒中的事件（包括三种诊断方式）
keep if stroke == 1 
* 2023/07/19 主要诊断和次要诊断都保留


drop arr_index
cd /public/home/hanyuting/2022-04_CMM_G/1.UKB
save "hesin_stroke_diag.dta", replace


/* 跟日期数据融合，数据库中仅有发生卒中的研究对象 */
use "hesin_stroke_diag.dta", clear
*2023/07/18 若不剔除次要诊断的话，这里匹配需要多对一的匹配方式
merge m:1 eid ins_index using "hesin_date.dta", keep(match) nogen
sort eid date
gen epid = "I60" if SAH == 1
	replace epid = "I61" if ICH == 1
	replace epid = "I63" if IS  == 1
	replace epid = "I64" if Uns == 1
* 剔除一次spell中诊断相同的episodes  11025 were excluded
duplicates drop eid epid spell_index, force
* 标记一下数据来源
gen source = "HESIN"
keep eid epid date source
cd /public/home/hanyuting/2022-04_CMM_G/1.UKB
save "hesin.dta",replace

************************************************************************************************************************************************
*************************************                                                  *********************************************************
*************************************                 3. 清理death registry             *********************************************************
*************************************                                                  *********************************************************
************************************************************************************************************************************************
* 读取包含death registry的数据
* 同时使用了主要死因和次要死因，跟CKB保持一致
do "/public/home/hanyuting/UKB/Main_dataset/raw_other_ep.do"
rename n_eid eid
* 保留所需变量，两个date of death日期相同
keep eid s_40001_0_0-s_40002_1_9 ts_40000_0_0 ts_40000_1_0
* 仅保留死亡纪录
keep if !mi(ts_40000_0_0)

local varlist s_40001_0_0 s_40001_1_0 s_40002_0_1 s_40002_0_2 s_40002_0_3 s_40002_0_4 s_40002_0_5 s_40002_0_6 s_40002_0_7 s_40002_0_8 s_40002_0_9 s_40002_0_10 s_40002_0_11 s_40002_0_12 s_40002_0_13 s_40002_0_14 s_40002_1_1 s_40002_1_2 s_40002_1_3 s_40002_1_4 s_40002_1_5 s_40002_1_6 s_40002_1_7 s_40002_1_8 s_40002_1_9 
foreach var of local varlist{
	replace `var' = substr(`var',1,3)
}

gen SAH = 1 if s_40001_0_0 == "I60"
gen ICH = 1 if s_40001_0_0 == "I61"
gen IS  = 1 if s_40001_0_0 == "I63"
gen Uns = 1 if s_40001_0_0 == "I64"
local varlist1 s_40001_1_0 s_40002_0_1 s_40002_0_2 s_40002_0_3 s_40002_0_4 s_40002_0_5 s_40002_0_6 s_40002_0_7 s_40002_0_8 s_40002_0_9 s_40002_0_10 s_40002_0_11 s_40002_0_12 s_40002_0_13 s_40002_0_14 s_40002_1_1 s_40002_1_2 s_40002_1_3 s_40002_1_4 s_40002_1_5 s_40002_1_6 s_40002_1_7 s_40002_1_8 s_40002_1_9
foreach var of local varlist1{
	replace SAH=1 if `var'=="I60"
	replace ICH=1 if `var'=="I61"
	replace IS =1 if `var'=="I63"
	replace Uns=1 if `var'=="I64"
} 
tab1 SAH ICH IS Uns
gen epid = "I64" if Uns == 1
	replace epid = "I63" if IS == 1 
	replace epid = "I61" if ICH == 1
	replace epid = "I60" if SAH == 1
clonevar date = ts_40000_0_0
format date %td
gen source = "Death"
keep eid epid date source
keep if !mi(epid)
* misstable sum // 前述的42个缺失值不存在于stroke相关病例中
cd /public/home/hanyuting/2022-04_CMM_G/1.UKB
save "death.dta",replace


************************************************************************************************************************************************
*************************************                                                  *********************************************************
*************************************               4. 合并三个数据库做最后的清理        *********************************************************
*************************************                                                  *********************************************************
************************************************************************************************************************************************
cd /public/home/hanyuting/2022-04_CMM_G/1.UKB
use "hesin.dta",clear
append using "death.dta"
sort epid date
* 剔除不同来源重复报告的病例
duplicates report eid date epid
duplicates drop eid date epid,force
* 与ADO数据库合并 
merge m:1 eid using "first_stroke.dta",keep(match) nogen
* 仅保留ADO first event 之后的数据;不考虑28d的间隔
keep if date > first_anystroke_date
keep eid date epid

* 生成首发卒中后发病最早的日期
bysort eid:egen sec_anystroke_date = min(date)
format sec_anystroke_date %td

* 标记第二次stroke(不分亚型)
gen sec_anystroke = 1 if date == sec_anystroke_date
    replace sec_anystroke = 0 if date > sec_anystroke_date


* 仅保留第二次卒中，并进行转置
keep if sec_anystroke == 1
reshape wide date, i(eid) j(epid) string

* 复发卒中亚型
gen sec_stroke_type = 4 if !mi(dateI64)
    replace sec_stroke_type = 3 if !mi(dateI63)
    replace sec_stroke_type = 2 if !mi(dateI61)
    replace sec_stroke_type = 1 if !mi(dateI60)
tab sec_stroke_type
label define subtype 1 "I60" 2 "I61" 3 "I63" 4 "I64"
label values sec_stroke_type subtype


gen sec_I60 = 1 if sec_stroke_type == 1
gen sec_I61 = 1 if sec_stroke_type == 2
gen sec_I63 = 1 if sec_stroke_type == 3
gen sec_I64 = 1 if sec_stroke_type == 4

tab1 sec_I6* sec_stroke_type

drop dateI60-dateI64
* 生成考虑了subtypes的疾病日期
gen sec_I60_date = sec_anystroke_date if sec_stroke_type == 1
gen sec_I61_date = sec_anystroke_date if sec_stroke_type == 2
gen sec_I63_date = sec_anystroke_date if sec_stroke_type == 3
gen sec_I64_date = sec_anystroke_date if sec_stroke_type == 4
format sec_I*_date %td

cd /public/home/hanyuting/2022-04_CMM_G/1.UKB
save "sec_stroke.dta",replace

***** 得到最终的数据库
cd /public/home/hanyuting/2022-04_CMM_G/1.UKB
use "first_stroke.dta",clear
merge 1:1 eid using "sec_stroke.dta",keep(master match) nogen
tab first_stroke_type sec_stroke_type,row
save "rec_stroke.dta",replace

************************************************************************************************************************************************
*************************************                                                  *********************************************************
*************************************                     5. 清理结局数据               *********************************************************
*************************************                                                  *********************************************************
************************************************************************************************************************************************

do "/public/home/hanyuting/UKB/Main_dataset/raw_other_ep.do"
rename n_eid eid
cd /public/home/hanyuting/2022-04_CMM_G/1.UKB
merge 1:1 eid using "rec_stroke.dta",nogen

****** Death
*** Age of death 40007
* codebook n_40007_0_0 n_40007_1_0
* count if mi(n_40007_0_0) & !mi(n_40007_1_0) // 0
rename n_40007_0_0 age_death 

****** CAD
* I20:131297;DATE:131296;     36883人
gen I20 = (n_131297_0_0!=.)
gen I20_date = ts_131296_0_0 
* I21:131299;DATE:131298 
gen I21 = (n_131299_0_0!=.)
gen I21_date = ts_131298_0_0 
* I22:131301;DATE:131300 
gen I22 = (n_131301_0_0!=.)
gen I22_date = ts_131300_0_0 
* I23:131303;DATE:131302 
gen I23 = (n_131303_0_0!=.)
gen I23_date = ts_131302_0_0 
* I24:131305;DATE:131304 
gen I24 = (n_131305_0_0!=.)
gen I24_date = ts_131304_0_0 
* I25:131307;DATE:131306
gen I25 = (n_131307_0_0!=.)
gen I25_date = ts_131306_0_0 

* CHD
gen CHD = (I20 ==1 | I21 ==1 |I22 ==1 |I23 ==1 |I24 ==1 |I25 ==1)
egen CHD_date = rowmin(I20_date I21_date I22_date I23_date I24_date I25_date)

****** 房颤
gen AF = (n_131351_0_0!=.)
gen AF_date = ts_131350_0_0 

****** 还得二型糖尿病
*** E10
gen E10 = (n_130707_0_0!=.)
gen E10_date = ts_130706_0_0 

*** E11
gen E11 = (n_130709_0_0!=.)
gen E11_date = ts_130708_0_0 

*** E12
gen E12 = (n_130711_0_0!=.)
gen E12_date = ts_130710_0_0 

*** E13
gen E13 = (n_130713_0_0!=.)
gen E13_date = ts_130712_0_0 

*** E14
gen E14 = (n_130715_0_0!=.)
gen E14_date = ts_130714_0_0 

** Diabetes
gen DM = 1 if (E10==1 | E11==1 | E12==1 | E13==1 | E14==1)
egen DM_date = rowmin(E10_date E11_date E12_date E13_date E14_date)

** Hypertension
gen HT = 1 if  n_131287_0_0!=. | n_131289_0_0!=. | n_131291_0_0!=. | n_131293_0_0!=. | n_131295_0_0!=. 
egen HT_date = rowmin(ts_131286_0_0 ts_131288_0_0 ts_131290_0_0 ts_131292_0_0 ts_131294_0_0)

****** 删失原因和日期
* England&Wales&Scotland的死亡登记系统都是到2022年12月31日
* 但inpatient data的censoring date三个不一致，England&Wales&Scotland那个地区的删失日期分别为10/31/2022、7/31/2021、1/28/2018
rename n_190_0_0 censoring_reason
rename ts_191_0_0 censoring_date
* codebook censoring* // 有501,111人date缺失
tab censoring_reason


****** 根据调查点的位置来确定某人来自于哪个地区 n_54_0_0 这个变量无任何缺失
recode n_54_0_0 (11012 11021 11011 11008 11024 11020 11018 11010 11016 11001 11017 11009 11013 11002 11007 11014 10003 11006=1 "England") (11003 11022 11023=2 "Wales") (11005 11004=3 "Scotland"),gen(assess_region)
tab assess_region,mi
* England |    445,766       88.73       88.73
*   Wales |     20,805        4.14       92.87
*Scotland |     35,838        7.13      100.00
replace censoring_date = date("2022/10/31","YMD") if mi(censoring_date) & assess_region == 1
replace censoring_date = date("2021/07/31","YMD") if mi(censoring_date) & assess_region == 2
replace censoring_date = date("2018/01/28","YMD") if mi(censoring_date) & assess_region == 3

****** 对上述所有结束重新赋值，从而得出最终的数据库
* 先看下现有结局结构
local eplist HT DM AF CHD death first_anystroke first_I60 first_I61 first_I63 first_I64 sec_anystroke sec_I60 sec_I61 sec_I63 sec_I64
tab1 `eplist',mi
foreach ep of local eplist{
	replace `ep' = 0 if `ep'==.
	replace `ep'_date = censoring_date if `ep' == 0
	* 这里考虑有些人发病在截止日期之后，所以将这些病人的发病状态赋值为0.发病日期赋值为删失日期
	replace `ep'_date = censoring_date if `ep' == 1 & `ep'_date > censoring_date
	replace `ep' = 0 if `ep' == 1 & `ep'_date > censoring_date
	local ep_date_list `ep_date_list' `ep'_date
}
misstable sum `eplist' `ep_date_list'
keep eid assess_region censoring_date censoring_reason first_stroke_type sec_stroke_type `eplist' `ep_date_list'

save "/public/home/hanyuting/2022-04_CMM_G/1.UKB/endpoints.dta",replace

************************************************************************************************************************************************
*************************************                                                  *********************************************************
*************************************                     6. 清理基线数据               *********************************************************
*************************************                                                  *********************************************************
************************************************************************************************************************************************

*do "/public/home/hanyuting/UKB/Main_dataset/raw_baseline.do"
*save "/public/home/hanyuting/2022-04_CMM_G/1.UKB/raw_baseline.dta",replace
use "/public/home/hanyuting/2022-04_CMM_G/1.UKB/raw_baseline.dta",clear
rename n_eid eid

*===============================================================================================================================================
/* covariates */
****** 入组时间
rename ts_53_0_0 study_date

****** 年龄
rename n_21022_0_0 age_at_study_date

sum age_at_study_date
egen age_strata=cut(age_at_study_date), at(30 45(5)80) label
	tab age_strata
egen age_3groups = cut(age_at_study_date), at(0 50 60 100) icodes
	label variable age_3groups "age three groups at baseline"
	label define age_3groups 0 "<50 y" 1 "50~59 y" 2 ">=60 yr"
	label values age_3groups age_3groups

****** 性别
gen is_female = (n_31_0_0==0)

****** 社会剥夺指数
gen townsend_depri_index = n_26410_0_0
	replace townsend_depri_index = n_26427_0_0 if mi(townsend_depri_index)
	replace townsend_depri_index = n_26426_0_0 if mi(townsend_depri_index)
sum townsend_depri_index,detail
* 第一次生成缺失值指标：但这个变量最后没有用到
gen incomplete_case = 1 if mi(townsend_depri_index)
	replace townsend_depri_index = r(p50) if mi(townsend_depri_index)
egen depri_2g = cut(townsend_depri_index), group(2)

****** Family history
gen stroke_fh_2groups = (n_20107_0_0==2|n_20107_0_1==2|n_20107_0_2==2|n_20107_0_3==2|n_20107_0_4==2|n_20107_0_5==2|n_20107_0_6==2|n_20107_0_7==2|n_20107_0_8==2|n_20107_0_9==2|n_20110_0_0==2|n_20110_0_1==2|n_20110_0_2==2|n_20110_0_3==2|n_20110_0_4==2|n_20110_0_5==2|n_20110_0_6==2|n_20110_0_7==2|n_20110_0_8==2|n_20110_0_9==2|n_20110_0_10==2|n_20111_0_0==2|n_20111_0_1==2|n_20111_0_2==2|n_20111_0_3==2|n_20111_0_4==2|n_20111_0_5==2|n_20111_0_6==2|n_20111_0_7==2|n_20111_0_8==2|n_20111_0_9==2|n_20111_0_10==2|n_20111_0_11==2)

*===============================================================================================================================================
/* Lifestyle */
****** 生活方式变量定义
*** 吸烟 n_20116_0_0  这个变量是由1239和1249 两个变量生成的

recode n_20116_0_0 (-3 . = 9 "Mi/No answer") (0=0 "Never") (1=1 "Previous") (2=2 "Current"), gen(smoking_status)

* 定义因病戒烟变量 n_6157_0_0 n_6157_0_1 n_6157_0_2 n_6157_0_3
gen illness_quit_smoke = 1 if n_6157_0_0 == 1 | n_6157_0_1==1 | n_6157_0_2 == 1 | n_6157_0_3==1

* 考虑了因病戒烟的吸烟分组
clonevar smoking_status1=smoking_status
replace smoking_status1 = 2 if illness_quit_smoke == 1

* 定义吸烟类型
rename n_3446_0_0 smoking_type

* 定义吸烟量: 太多缺失值了，考虑后续不用这个变量
* 对于当前吸卷烟（1239 为手工制作和工业生产的卷烟两种）的人，使用当前吸烟量
* 对于当前吸雪茄和烟斗的，问他既往是否吸卷烟，如果既往吸卷烟，那就用既往的吸烟量来定义
* 对于因病戒烟的人，在smoking_category中应该属于previous，给他附上既往的每日吸烟量
misstable sum n_3456_0_0 n_6183_0_0 n_2887_0_0
gen cig_equiv_day = n_3456_0_0 if smoking_status == 2 & (smoking_type == 1 | smoking_type == 2) 
replace cig_equiv_day = n_6183_0_0 if smoking_status == 2 & smoking_type == 3
replace cig_equiv_day = n_2887_0_0 if smoking_status == 1

* 2023-10-11 定义吸烟四分组用于变量调整
gen smoking_4groups = 1 if smoking_status == 0
	replace smoking_4groups = 2 if smoking_status == 1 & illness_quit_smoke != 1 
	replace smoking_4groups = 3 if smoking_status == 1 & illness_quit_smoke == 1 
	replace smoking_4groups = 4 if smoking_status == 2
	* 2940人缺失，赋值为不吸烟组
	replace incomplete_case = 1 if mi(smoking_4groups)
	replace smoking_4groups = 1 if mi(smoking_4groups)

*** 饮酒
* 每周饮酒者调查其每周饮酒量
* 饮酒但非每周饮酒者调查其每月饮酒量
* Alcohol status：n_20117_0_0 有899个缺失值
recode n_20117_0_0 (. -3=9 "Do not know/Prefer not to answer") (0=0 "Never") (1=1 "Previous") (2=2 "Current"), gen(drinking_status)
// tab drinking_status,mi

* 饮酒频率：n_1588_0_0 有899个缺失值
recode n_1558_0_0 (1=1 "Daily or almost daily") (2=2 "Three or four times a week") (3=3 "Once or twice a week") (4=4 "One to three times a month") (5=5 "Special occasions only") (6=6 "Never") (-3 .=9 "Prefer not to answer"), gen(drinking_freq)

* 因为身体原因戒酒
gen illness_quit_drink = 1 if n_3859_0_0 == 1

* 个人饮酒量
*-1 不知道；-3 不想回答
*每日饮酒量，转换为units, 1 units 
*一个人要么报告每周饮酒量，要么报告每月饮酒量
*count if alcohol_week_red_wine_0 == . & alcohol_month_red_wine_0 ==.
*转化系数来自“Estimating alcohol consumption from survey data: updated method of converting volumes to units”
*Link：“https://escoe-website.s3.amazonaws.com/wp-content/uploads/2019/11/21120355/GSS-Methodology-Series-No.-37-Estimating-alcohol-consumption-from-survey-data-updated-method-of-converting-volumes-to-units.pdf”
gen week_red_wine_units = n_1568_0_0*2
	replace week_red_wine_units = . if week_red_wine_units == -2 | week_red_wine_units == -6
gen week_champagne_units = n_1578_0_0*2
	replace week_champagne_units = . if week_champagne_units == -2 | week_champagne_units == -6
gen week_beer_units = n_1588_0_0*2
	replace week_beer_units = . if week_beer_units == -2 | week_beer_units == -6
gen week_spirits_units = n_1598_0_0*1
	replace week_spirits_units = . if week_spirits_units == -1 | week_spirits_units == -3
gen week_fortified_wine_units = n_1608_0_0*1
	replace week_fortified_wine_units = . if week_fortified_wine_units == -1 | week_fortified_wine_units == -3

*egen+rowtotal函数 treat missing as 0
egen week_alcohol_units = rowtotal(week_red_wine_units week_champagne_units week_beer_units week_spirits_units week_fortified_wine_units)
	replace week_alcohol_units = . if mi(week_red_wine_units)&mi(week_champagne_units)&mi(week_beer_units)&mi(week_spirits_units)&mi(week_fortified_wine_units)

*定义每周饮酒且每周饮酒量大于等于14units;戒酒者被归为风险组;1658人缺失
gen risky_alcohol = 1 if (drinking_freq==1|drinking_freq==2|drinking_freq == 3)&week_alcohol_units>14 & !mi(week_alcohol_units)
	replace risky_alcohol = 0 if drinking_freq==4|drinking_freq==5|drinking_freq==6|((drinking_freq==1|drinking_freq==2|drinking_freq==3)&week_alcohol_units<=14)
	replace risky_alcohol = 1 if drinking_status == 1
	* 1658人缺失，填补为低风险组
	replace incomplete_case = 1 if mi(risky_alcohol)
	replace risky_alcohol = 0 if mi(risky_alcohol)

* 2023-10-11 定义四分组 （1）从不、偶尔和非每周；（2）戒酒；（3）每周不过量；（4） 每周但过量
gen drinking_4groups = 1 if drinking_freq == 4 | drinking_freq == 5 | drinking_freq == 6
	replace drinking_4groups = 2 if drinking_status == 1
	replace drinking_4groups = 3 if (drinking_status == 2 | drinking_status==9)&(drinking_freq==1|drinking_freq==2|drinking_freq==3) & week_alcohol_units<=14
	replace drinking_4groups = 4 if (drinking_status == 2 | drinking_status==9)&(drinking_freq==1|drinking_freq==2|drinking_freq==3) & week_alcohol_units>14 & !mi(week_alcohol_units)
	* 1658缺失 赋值为4
	replace drinking_4groups = 4 if mi(drinking_4groups)

*** 膳食
*https://cdn.jamanetwork.com/ama/content_public/journal/jama/938141/joi190074supp1_prod.pdf
*Association of Lifestyle and Genetic Risk With Incidence of Dementia
*Following the definition of a JAMA article
*At least 4 of the following 7 food groups:
*1. Fruits: ≥ 3 servings/day
*2. Vegetables: ≥ 3 servings/day 
*3. Fish: ≥2 servings/week
*4. Processed meats: ≤ 1 serving/week
*5. Unprocessed red meats: ≤ 1.5 servings/week
*6. Whole grains: ≥ 3servings/day
*7. Refined grains: ≤1.5servings/day
* 定义时结合靳光付老师发表在Nutrients上的文章
* Vegetables including cooked and raw
clonevar cooked_vege = n_1289_0_0
	replace cooked_vege = 0.5 if n_1289_0_0 == -10
	replace cooked_vege = . if n_1289_0_0 == -1 | n_1289_0_0 == -3
	* 1 serving = 3 tablespoon
	replace cooked_vege = cooked_vege/3 if !mi(cooked_vege)
clonevar raw_vege = n_1299_0_0
	replace raw_vege = 0.5 if n_1289_0_0 == -10
	replace raw_vege = . if n_1289_0_0 == -1 | n_1289_0_0 == -3
	* 1 serving = 3 tablespoon
	replace raw_vege = raw_vege/3 if !mi(raw_vege)

* units:7593缺失
egen vegetable = rowtotal(cooked_vege raw_vege)
	replace vegetable = . if mi(cooked_vege)&mi(raw_vege)

* Fruits: including fresh and cooked vegetable
clonevar fresh_fruit = n_1309_0_0
	replace fresh_fruit = 0.5 if n_1309_0_0 == -10
	replace fresh_fruit = . if n_1309_0_0 == -1 | n_1309_0_0 == -3
clonevar dried_fruit = n_1319_0_0
	replace dried_fruit = 0.5 if n_1319_0_0 == -10
	replace dried_fruit = . if n_1319_0_0 == -1 | n_1319_0_0 == -3
	* 1 servings = 2 pieces
	replace dried_fruit = dried_fruit/2 if !mi(dried_fruit)
* 2141 missing
egen fruit = rowtotal(fresh_fruit dried_fruit)
	replace fruit = . if mi(fresh_fruit)&mi(dried_fruit)

* Fish
recode n_1329_0_0 (-1 -3 . = .) (0=0) (1=0.5) (2=1) (3=3) (4=5.5) (5=7),gen(oily_fish)
recode n_1339_0_0 (-1 -3 . = .) (0=0) (1=0.5) (2=1) (3=3) (4=5.5) (5=7),gen(non_oily_fish)
* 2055missing
egen fish = rowtotal(oily_fish non_oily_fish)
	replace fish = . if mi(oily_fish)&mi(non_oily_fish)

* Processed meats,2231 missing
recode n_1349_0_0 (-1 -3 . = .) (0=0) (1=0.5) (2=1) (3=3) (4=5.5) (5=7),gen(processed_meat)

* Unprocessed red meats
recode n_1369_0_0 (-1 -3 . = .) (0=0) (1=0.5) (2=1) (3=3) (4=5.5) (5=7),gen(unprocessed_beef)
recode n_1379_0_0 (-1 -3 . = .) (0=0) (1=0.5) (2=1) (3=3) (4=5.5) (5=7),gen(unprocessed_lamb)
recode n_1389_0_0 (-1 -3 . = .) (0=0) (1=0.5) (2=1) (3=3) (4=5.5) (5=7),gen(unprocessed_pork)
*1855missing
egen unprocessed_meat = rowtotal(unprocessed_beef unprocessed_lamb unprocessed_pork)
	replace unprocessed_meat =. if mi(unprocessed_beef)&mi(unprocessed_lamb)&mi(unprocessed_pork)

* Whole grains: including bread and cereal
gen whole_grain_bread_type = (n_1448_0_0 == 3)
	replace whole_grain_bread_type = . if n_1448_0_0 == -3 | n_1448_0_0 == -1 
replace n_1438_0_0 = 0.5 if n_1438_0_0 == -10
replace n_1438_0_0 = . if n_1438_0_0 == -1 | n_1438_0_0 == -3
gen double whole_grain_bread = whole_grain_bread_type*n_1438_0_0

gen whole_grain_cereal_type = (n_1468_0_0 == 1 | n_1468_0_0 == 3 | n_1468_0_0 == 4)
	replace whole_grain_cereal_type = . if n_1468_0_0 == -3 | n_1468_0_0 == -1 
replace n_1458_0_0 = 0.5 if n_1458_0_0 == -10
replace n_1458_0_0 = . if n_1458_0_0 == -1 | n_1458_0_0 == -3
gen double whole_grain_cereal = whole_grain_cereal_type*n_1458_0_0
* 2096人为missing
egen double whole_grain =rowtotal(whole_grain_bread whole_grain_cereal)
	replace whole_grain = . if mi(whole_grain_bread)&mi(whole_grain_cereal) 

* refined grains
gen refined_grain_bread_type = (n_1448_0_0==1 | n_1448_0_0==2 | n_1448_0_0==4)
	replace refined_grain_bread_type = . if n_1448_0_0 == -3 | n_1448_0_0 == -1
gen double refined_grain_bread = refined_grain_bread_type*n_1438_0_0

gen refined_grain_cereal_type = (n_1468_0_0 == 2 | n_1468_0_0 == 5)
	replace refined_grain_cereal_type = . if n_1468_0_0 == -3 | n_1468_0_0 == -1
gen double refined_grain_cereal = refined_grain_cereal_type*n_1458_0_0

* 2096 missing
egen double refined_grain = rowtotal(refined_grain_bread refined_grain_cereal)
	replace refined_grain = . if mi(refined_grain_bread)&mi(refined_grain_cereal)

* healthy diet score
replace incomplete_case = 1 if mi(score_fruit)&mi(score_vegetable)&mi(score_fish)&mi(score_processed_meat)&mi(score_unprocessed_meat)&mi(score_whole_grain)&mi(score_refined_grain)
gen score_fruit = 1 if fruit >=3 & fruit!=.
	replace score_fruit =0 if fruit <3
	replace score_fruit =0 if mi(score_fruit)
gen score_vegetable = 1 if vegetable >=3 & vegetable!=.
	replace score_vegetable =0 if vegetable <3
	replace score_vegetable =0 if mi(score_vegetable)
gen score_fish = 1 if fish >=3 & fish!=.
	replace score_fish =0 if fish <3
	replace score_fish =0 if mi(score_fish)
gen score_processed_meat = 1 if processed_meat <=1
	replace score_processed_meat =0 if processed_meat >1 & processed_meat !=.
	replace score_processed_meat =1 if mi(score_processed_meat)
gen score_unprocessed_meat = 1 if unprocessed_meat <=1.5
	replace score_unprocessed_meat =0 if unprocessed_meat >1.5 & unprocessed_meat !=.
	replace score_unprocessed_meat =0 if mi(score_unprocessed_meat)
gen score_whole_grain = 1 if whole_grain >=3 & whole_grain!=.
	replace score_whole_grain =0 if whole_grain <3
	replace score_whole_grain =1 if mi(score_whole_grain)
gen score_refined_grain = 1 if refined_grain <=1.5
	replace score_refined_grain =0 if refined_grain >1.5 & refined_grain !=.
	replace score_refined_grain =0 if mi(score_refined_grain)

egen diet_score = rowtotal(score_fruit score_vegetable score_fish score_processed_meat score_unprocessed_meat score_whole_grain score_refined_grain)
recode diet_score (0/2=1) (3/4=2) (5/7=3),gen(diet_3groups)
* 2023-10-11 跟CKB匹配，设置五分组
recode diet_score (0/1=1) (2=2) (3=3) (4/5=4) (6/7=5),gen(diet_5groups)


*** BMI:3107 missing
rename n_21001_0_0 bmi_calc
codebook bmi_calc
recode bmi_calc (min/18.49999=1 "Underweight <18.5") (18.5/24.99999=2 "Normal") (25/29.99999=3 "Overweight") (30.0/80=4 "Obesity >=28.0"), gen(bmi_4groups)

*** Waist circumference:2163 missing 
rename n_48_0_0 waist_cm
codebook waist_cm

* according to the WHO's criteria 
gen obesity_5groups = 1 if bmi_4groups == 1
	replace obesity_5groups = 2 if (bmi_4groups == 2 | bmi_4groups == 3) & ((is_female == 1 & waist_cm < 80) | (is_female == 0 & waist_cm < 94))
	replace obesity_5groups = 3 if (bmi_4groups == 2 | bmi_4groups == 3) & ((is_female == 1 & waist_cm >= 80 & waist_cm !=.) | (is_female == 0 & waist_cm >= 94 & waist_cm !=.))
	replace obesity_5groups = 4 if bmi_4groups == 4 & ((is_female == 1 & waist_cm < 80) | (is_female == 0 & waist_cm < 94))
	replace obesity_5groups = 5 if bmi_4groups == 4 & ((is_female == 1 & waist_cm >= 80 & waist_cm !=.) | (is_female == 0 & waist_cm >= 94 & waist_cm !=.))
	replace incomplete_case = 1 if mi(obesity_5groups)
	replace obesity_5groups = 2 if mi(obesity_5groups)
gen risky_obesity = 0 if (bmi_4groups == 2 | bmi_4groups == 3) & ((is_female == 1 & waist_cm < 80) | (is_female == 0 & waist_cm < 94))
	replace risky_obesity = 1 if bmi_4groups == 1 | bmi_4groups == 4 | ((bmi_4groups == 2 | bmi_4groups == 3) & ((is_female == 1 & waist_cm >= 80 & waist_cm !=.) | (is_female == 0 & waist_cm >= 94 & waist_cm !=.)))
	replace risky_obesity = 1 if mi(risky_obesity)
* tab obesity_5groups,mi // 3213 missing
* tab risky_obesity,mi // 3163 missing

*===============================================================================================================================================
/* Biomarkers */
*** 收缩压
gen double sbp_mean = (n_4080_0_0 + n_4080_0_1)/2 if !mi(n_4080_0_0)&!mi(n_4080_0_1)
	replace sbp_mean = n_4080_0_0 if !mi(n_4080_0_0)&mi(n_4080_0_1)
	replace sbp_mean = n_4080_0_1 if mi(n_4080_0_0)&!mi(n_4080_0_1)
* 舒张压
gen double dbp_mean = (n_4079_0_0 + n_4079_0_1)/2 if !mi(n_4079_0_0)&!mi(n_4079_0_1)
	replace dbp_mean = n_4079_0_0 if !mi(n_4079_0_0)&mi(n_4079_0_1)
	replace dbp_mean = n_4079_0_1 if mi(n_4079_0_0)&!mi(n_4079_0_1)

* 降压药
gen used_blood_pressure_drugs =  (n_6153_0_0 == 2 | n_6153_0_1 == 2 | n_6153_0_2 == 2 | n_6153_0_3 == 2 |  n_6177_0_0 == 2 | n_6177_0_1 == 2 | n_6177_0_2 == 2)
gen double sbp_mean2 = sbp_mean + 15 if n_6153_0_0 == 2 | n_6153_0_1 == 2 | n_6153_0_2 == 2 | n_6153_0_3 == 2 |  n_6177_0_0 == 2 | n_6177_0_1 == 2 | n_6177_0_2 == 2
	replace sbp_mean2 = sbp_mean if mi(sbp_mean2)
	replace incomplete_case = 1 if mi(sbp_mean2)

***  Total cholesterol 30690_0_0; 32909missing
clonevar TC = n_30690_0_0
***  Low-density lipoprotein cholesterol 30780-0.0
clonevar LDL_C = n_30780_0_0
***  High-density lipoprotein cholesterol 30760-0.0
clonevar HDL_C = n_30760_0_0
***  Triglycerides 30870-0.0
clonevar TG = n_30870_0_0
*** Glucose: according to the ADA criteria, based on random glucose level ≥11.1 mmol/L or glycated hemoglobin (HbA1c) level ≥48 mmol/mol (6.5%) :Diabetes Care 2022;45(2):319–329
* glucose 30740 mmol/L
clonevar RG = n_30740_0_0
* HbA1c 30750 mmol/mol
clonevar HbA1c = n_30750_0_0

*===============================================================================================================================================
/* Genetics */
*** 无亲缘关系的人群
rename n_22020_0_0 no_relatedness
tab no_relatedness

*** PCs:22009
forvalues i = 1/20{

	rename n_22009_0_`i' PC`i'
}

misstable sum PC*


*===============================================================================================================================================
/* 仅保留有用的变量 */
// keep eid study_date age_at_study_date is_female townsend_depri_index smoking_status drinking_status drinking_freq bmi_calc bmi_4groups waist_cm obesity_5groups no_relatedness
keep eid age_at_study_date study_date age_strata is_female townsend_depri_index smoking_status smoking_status1 smoking_4groups drinking_status week_alcohol_units drinking_freq drinking_4groups score_fruit score_vegetable diet_score diet_3groups diet_5groups bmi_calc bmi_4groups waist_cm obesity_5groups risky_* no_relatedness PC* sbp_mean2 sbp_mean dbp_mean TC LDL_C HDL_C TG RG HbA1c used_blood_pressure_drugs age_3groups depri_2g stroke_fh_2groups n_20003_0* incomplete_case


*===============================================================================================================================================
/* 跟结局数据库合并 */
merge 1:1 eid using "/public/home/hanyuting/2022-04_CMM_G/1.UKB/endpoints.dta",nogen

* Prevalent diseases
* 48mmol/mol = 6.5%
gen has_diabetes = 1 if (RG >= 11.1&RG!=.)|(HbA1c>=48&HbA1c!=.)|(DM==1&DM_date<= study_date)
	replace has_diabetes = 0 if mi(has_diabetes)
gen has_AF = 1 if (AF==1&AF_date<=study_date)
	replace has_AF = 0 if mi(has_AF)
* Prevalent hypertension
gen has_hypertension = 1 if (sbp_mean >= 140 & sbp_mean != .) | (dbp_mean >= 90 & dbp_mean != .) | (used_blood_pressure_drugs == 1) | (HT==1 & HT_date <= study_date)
    replace has_hypertension = 0 if mi(has_hypertension)
	
*===============================================================================================================================================
/* 生成一些年龄变量 */

local eplist DM CHD first_anystroke first_I60 first_I61 first_I63 first_I64 sec_anystroke sec_I60 sec_I61 sec_I63 sec_I64 death
foreach ep of local eplist{

	gen double age_`ep' = age_at_study_date + (`ep'_date-study_date)/365.25

}

export delimited "/public/home/hanyuting/2022-04_CMM_G/1.UKB/Cleaned_data.csv", nolabel datafmt replace 



