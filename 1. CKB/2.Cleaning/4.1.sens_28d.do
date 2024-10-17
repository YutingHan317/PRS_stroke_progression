* 这个文件跟“2.Data_cleaning_recurrent_stroke_V2”好像就差一行代码，第98行
**************************************************************************************************************************
*************************************         1.Import and Merge       ***************************************************
**************************************************************************************************************************
cd "/public/home/hanyuting/2022-04_CMM_G"
import delimited "/public/home/hanyuting/00-原始数据/2023-05-19_CMM_G/event_endpoints.csv", encoding(utf8) clear

*** 把新版数据的不需要的来源给删掉
drop if type == "icase" | type == "oa" | type == "pvd"

*** 处理日期变量
gen date1 = substr(datedeveloped,1,10)
gen date = date(date1,"YMD",2050)
	format date %td
drop date1 
*** 处理疾病编码，仅保留前三位
gen ICD = substr(diagnosis,1,3)

*** 有些变量感觉没啥用，删掉
drop hospital_code tier ad_date dis_date is_outpatient agency_name type type2 datedeveloped source_desc 
save "1.Data/event_endpoints_raw.dta",replace

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
use "1.Data/event_endpoints_raw.dta",clear

*** 仅保留发生过至少一次脑卒中的研究对象，包括I60、I61、I63、I64
keep if epid == "CKB0007" | epid == "CKB0008" | epid == "CKB0009" | epid == "CKB0010"
* 查看有多少人患有至少一次stroke，66,494人跟endpoints数据库对的上
codebook csid

*** 某一天一个人可能从多个渠道被诊断为同一种类型，如：同一天被Dis和Hi同时记录为IS
duplicates report csid date ICD
duplicates drop csid date ICD,force

*** 首次卒中
* 生成首次stroke（不分亚型）的日期
bysort csid: egen first_anystroke_date = min(date)
format first_anystroke_date %td

* 标记第一次stroke（不分亚型）
gen first_anystroke = 1 if date == first_anystroke_date
	replace first_anystroke = 0 if date > first_anystroke_date

* 计算首次发生卒中时出现几种亚型
bysort csid: egen no_first_sub = sum(first_anystroke)
tab no_first_sub


*** 先处理首次卒中
preserve
keep if first_anystroke == 1
keep csid date ICD first_anystroke_date first_anystroke no_first_sub
* 长转宽，方便处理首次疾病类型
reshape wide date, i(csid) j(ICD) string
* 首次卒中类型
gen first_stroke_type = 4 if !mi(dateI64)
    replace first_stroke_type = 3 if !mi(dateI63)
    replace first_stroke_type = 2 if !mi(dateI61)
    replace first_stroke_type = 1 if !mi(dateI60)
label define subtype 1 "I60" 2 "I61" 3 "I63" 4 "I64"
label values first_stroke_type subtype

gen first_I60 = 1 if first_stroke_type == 1
gen first_I61 = 1 if first_stroke_type == 2
gen first_I63 = 1 if first_stroke_type == 3
gen first_I64 = 1 if first_stroke_type == 4

tab1 first_I6* first_stroke_type


drop dateI6*
* 生成考虑了subtypes的疾病日期
gen first_I60_date = first_anystroke_date if first_stroke_type == 1
gen first_I61_date = first_anystroke_date if first_stroke_type == 2
gen first_I63_date = first_anystroke_date if first_stroke_type == 3
gen first_I64_date = first_anystroke_date if first_stroke_type == 4
format first_I*_date %td
save "1.Data/first_stroke.dta",replace
restore

*** 再处理第二次卒中   n=64,826
* 仅保留间隔在28天及以上的研究对象
keep if date >= first_anystroke_date + 28

drop if first_anystroke==1 
keep csid date ICD

* 生成第二次stroke（不分亚型）的日期
bysort csid: egen sec_anystroke_date = min(date)
format sec_anystroke_date %td

* 标记第二次stroke（不分亚型）
gen sec_anystroke = 1 if date == sec_anystroke_date
    replace sec_anystroke = 0 if date > sec_anystroke_date

* 标记第二次stroke那天同时纪录了几种stroke
bysort csid: egen no_sec_sub = sum(sec_anystroke)
tab no_sec_sub

* 仅保留第二次卒中，并进行转置
keep if sec_anystroke == 1
reshape wide date, i(csid) j(ICD) string

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

save "1.Data/sec_stroke_28d.dta",replace

********************************************************************* 与随访数据库合并
use "1.Data/full_data.dta",clear
merge 1:1 csid using "1.Data/first_stroke.dta",nogen keep(master match)
merge 1:1 csid using "1.Data/sec_stroke_28d.dta",nogen keep(master match)
* 检查一下生成的结局变量是否有误
tab1 first_I60-first_I64 first_stroke_type sec_I60-sec_I64 sec_stroke_type

* 目前数据库中仅记录了发生某结局的信息，将对应结局变量为缺失的研究对象的结局赋值为0，日期赋值为删失日期
local eplist first_anystroke first_I60 first_I61 first_I63 first_I64 sec_anystroke sec_I60 sec_I61 sec_I63 sec_I64 
foreach ep of local eplist{

    di "Processing `ep'"
    replace `ep'_date = censoring_date if mi(`ep')
    replace `ep' = 0 if mi(`ep')
    * Transform to age
    gen double age_`ep' = (`ep'_date - dob_anon)/365.25
}

export delimited "1.Data/cleaned_recur_stroke_28d.csv", nolabel datafmt replace 
