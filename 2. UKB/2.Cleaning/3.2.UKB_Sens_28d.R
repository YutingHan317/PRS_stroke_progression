
################################################################################################################################################
# 设置工作路径
setwd("/public/home/hanyuting/2022-04_CMM_G/1.UKB")

# 加载r包
rm(list=ls())
options(width=170)
library(mstate)
library(data.table)
library(dplyr)
library(magrittr)
library(reshape2)


################################################################################################################################################
####################################                                                        ####################################################
####################################                   1. Basic cleaning                    ####################################################
####################################                                                        ####################################################
################################################################################################################################################
#===================================================================================================
# Loading phenotype and covariates
pheno <- fread("Cleaned_data_28d.csv",h=T)   
nrow(pheno)  # 502409人
df <- pheno

#===================================================================================================
# Merge All_PRS for UKB
PGSlist <- c("PGS001793_prscsSTROKE_EUR","PGS002724_metaISTROKE_EUR")
for (i in 1:length(PGSlist)){
    message(PGSlist[i]," is processing")
    PRS <- fread(paste0("/public/home/hanyuting/2.PRS/",PGSlist[i],".sscore"),h=T)
    PRS <- data.frame(PRS)
    PRS[,2] <- scale(PRS[,2])
    PRS[,3] <- as.factor(cut(PRS[,2],breaks=quantile(PRS[,2],probs = c(0,0.25,0.75,1)),labels=c(1,2,3),include.lowest=T))
    colnames(PRS) <- c("eid",PGSlist[i],paste0(PGSlist[i],".cat"))
    df <- merge(df,PRS,by="eid")
    nrow(df)
}
summary(df[,..PGSlist])

#===================================================================================================
# 处理不同阶段日期相同的情况
# 总体逻辑是优先级：first stroke（SAH>ICH>IS>Uns) > second_stroke > chd > death;为了在后面生成长数据阶段不出现error，死亡日期不能变

# 考虑以下四种情况:TIPs,在用attach的时候，避免用attach后更改后的数据作为后面的条件
# 0. 首发卒中、CHD和死亡日期相同，以死亡日期为准，依次减0.5天
if0 <- ifelse(df$first_anystroke==1&df$CHD==1&df$death==1&df$age_first_anystroke==df$age_CHD&df$age_first_anystroke==df$age_death,TRUE,FALSE)
df$age_CHD[if0] <- df$age_death[if0] - 0.5/365.25 
df$age_first_anystroke[if0] <- df$age_CHD[if0] - 0.5/365.25 
df$age_first_I60[if0&df$first_I60==1] <- df$age_CHD[if0&df$first_I60==1] - 0.5/365.25 
df$age_first_I61[if0&df$first_I61==1] <- df$age_CHD[if0&df$first_I61==1] - 0.5/365.25 
df$age_first_I63[if0&df$first_I63==1] <- df$age_CHD[if0&df$first_I63==1] - 0.5/365.25 
df$age_first_I64[if0&df$first_I64==1] <- df$age_CHD[if0&df$first_I64==1] - 0.5/365.25 

# 1. 首发卒中和死亡日期相同
if1 <- ifelse(df$first_anystroke==1&df$death==1&df$age_first_anystroke==df$age_death,TRUE,FALSE)
df$age_first_anystroke[if1] <- df$age_death[if1] - 0.5/365.25 
df$age_first_I60[if1&df$first_I60==1] <- df$age_death[if1&df$first_I60==1] - 0.5/365.25 
df$age_first_I61[if1&df$first_I61==1] <- df$age_death[if1&df$first_I61==1] - 0.5/365.25 
df$age_first_I63[if1&df$first_I63==1] <- df$age_death[if1&df$first_I63==1] - 0.5/365.25 
df$age_first_I64[if1&df$first_I64==1] <- df$age_death[if1&df$first_I64==1] - 0.5/365.25 

# 2. 首发卒中和CHD日期相同，但跟死亡日期不同; 427人存在这种情况
if2 <- ifelse(df$first_anystroke==1&df$CHD==1&df$age_first_anystroke==df$age_CHD&df$age_CHD!=df$age_death,TRUE,FALSE)
df$age_first_anystroke[if2] <- df$age_CHD[if2] - 0.5/365.25 
df$age_first_I60[if2&df$first_I60==1] <- df$age_CHD[if2&df$first_I60==1] - 0.5/365.25 
df$age_first_I61[if2&df$first_I61==1] <- df$age_CHD[if2&df$first_I61==1] - 0.5/365.25 
df$age_first_I63[if2&df$first_I63==1] <- df$age_CHD[if2&df$first_I63==1] - 0.5/365.25 
df$age_first_I64[if2&df$first_I64==1] <- df$age_CHD[if2&df$first_I64==1] - 0.5/365.25 

# 3. CHD和死亡日期相同，且均高于首发卒中日期
if3 <- ifelse(df$first_anystroke==1&df$CHD==1&df$death==1&df$age_first_anystroke<df$age_CHD&df$age_CHD==df$age_death,TRUE,FALSE)
df$age_CHD[if3] <- df$age_CHD[if3] - 0.5/365.25 

# 4. 复发卒中和死亡日期相同，且均高于首发卒中日期
if4 <- ifelse(df$sec_anystroke==1&df$death==1&df$age_sec_anystroke==df$age_death,TRUE,FALSE)
df$age_sec_anystroke[if4] <- df$age_sec_anystroke[if4] - 0.5/365.25
df$age_sec_I60[if4&df$sec_I60==1] <- df$age_sec_I60[if4&df$sec_I60==1] - 0.5/365.25
df$age_sec_I61[if4&df$sec_I61==1] <- df$age_sec_I61[if4&df$sec_I61==1] - 0.5/365.25
df$age_sec_I63[if4&df$sec_I63==1] <- df$age_sec_I63[if4&df$sec_I63==1] - 0.5/365.25
df$age_sec_I64[if4&df$sec_I64==1] <- df$age_sec_I64[if4&df$sec_I64==1] - 0.5/365.25

# 4.1 有些人在随访末没有死亡但复发卒中或者CHD发生在随访末
if4.0 <- ifelse(df$first_anystroke==1&df$age_first_anystroke==df$age_death&df$death_date=="28jan2018"&df$assess_region==3,TRUE,FALSE)
if4.1 <- ifelse(df$sec_anystroke==1&df$age_sec_anystroke==df$age_death&df$death_date=="28jan2018"&df$assess_region==3,TRUE,FALSE)
if4.2 <- ifelse(df$CHD==1&df$age_CHD==df$age_death&df$death_date=="28jan2018"&df$assess_region==3,TRUE,FALSE)
if4.3 <- ifelse(df$sec_anystroke==1&df$age_sec_anystroke==df$age_death&df$death_date=="31oct2022"&df$assess_region==1,TRUE,FALSE)
df$age_death[if4.0|if4.1|if4.2|if4.3] <- df$age_death[if4.0|if4.1|if4.2|if4.3] + 0.5/365.25


# 此时仍存在研究对象CHD和复发卒中发病日期相同的情况，不过后续转为长数据时会默认按照前述的优先级变换

#===================================================================================================
# 处理28天是否仍未出现结局或删失这种情况
df$age_surv_28d <- df$age_first_anystroke+28/365.25
df$age_surv_28d_I60 <- df$age_first_anystroke+28/365.25
df$age_surv_28d_I61 <- df$age_first_anystroke+28/365.25
df$age_surv_28d_I63 <- df$age_first_anystroke+28/365.25
df$age_surv_28d_I64 <- df$age_first_anystroke+28/365.25

# 未患卒中
df$surv_28d[df$first_anystroke==0] <- 0  
# 卒中后28天内未发生复发、卒中、死亡和因随访期结束而发生删失
if5 <- ifelse(df$first_anystroke==1&(df$age_death-df$age_first_anystroke>28/365.25)&(df$age_CHD-df$age_first_anystroke>28/365.25)&(df$age_sec_anystroke-df$age_first_anystroke>28/365.25),TRUE,FALSE)
df$surv_28d[if5] <- 1

# 卒中后28天内发生以下情况之一：复发、卒中、死亡和因随访期结束而发生删失
if6 <- ifelse(df$first_anystroke==1&((df$age_death-df$age_first_anystroke<=28/365.25)|(df$age_CHD-df$age_first_anystroke<=28/365.25)|(df$age_sec_anystroke-df$age_first_anystroke<=28/365.25)),TRUE,FALSE)
df$surv_28d[if6] <- 0
# 查看三类人分布
table(df$surv_28d)

# 处理下不同亚型的首发疾病
df$surv_28d_I60 <- ifelse(if5&df$first_I60==1,1,0)
df$surv_28d_I61 <- ifelse(if5&df$first_I61==1,1,0)
df$surv_28d_I63 <- ifelse(if5&df$first_I63==1,1,0)
df$surv_28d_I64 <- ifelse(if5&df$first_I64==1,1,0)

#===================================================================================================
# 在merge PRS的那一步，没有遗传数据就已经被剔除了
# nrow(df)  # 487162
# UKB由于病例是可以往前追溯的，因此这部分暂时不剔除，看看结果;但有三个人入组时间缺失，先剔除了
mltstate.origin.any <- df %>% filter(!is.na(age_at_study_date)) 
mltstate.origin.sub <- df %>% filter(!is.na(age_at_study_date))
# nrow(mltstate.origin.any) # 487162
mltstate.origin.any %<>% filter(age_first_anystroke>age_at_study_date&age_CHD>age_at_study_date&age_death>age_at_study_date)
mltstate.origin.sub %<>% filter(age_first_anystroke>age_at_study_date&age_CHD>age_at_study_date&age_death>age_at_study_date)
# nrow(mltstate.origin.any) # 454812
# mltstate.origin.any %<>% filter(no_relatedness==1) 
# mltstate.origin.sub %<>% filter(no_relatedness==1)
# nrow(mltstate.origin.any) # 380348

PGSlist <- c("PGS001793_prscsSTROKE_EUR","PGS002724_metaISTROKE_EUR")
genetics <- c(PGSlist,paste0(PGSlist,".cat"),"no_relatedness")
PCs <- paste0("PC",1:10)
states <- c("first_I60","first_I61","first_I63","first_I64","sec_I60","sec_I61","sec_I63","sec_I64","first_anystroke","sec_anystroke","death")
dates <- c("age_at_study_date","age_first_I60","age_first_I61","age_first_I63","age_first_I64","age_sec_I60","age_sec_I61","age_sec_I63","age_sec_I64",
   "age_first_anystroke","age_sec_anystroke","age_death","study_date","first_anystroke_date","sec_anystroke_date","CHD_date","death_date")
lifestyles <- c("smoking_4groups","drinking_4groups","bmi_4groups","obesity_5groups","diet_5groups")
covariates <- c("assess_region","age_at_study_date","age_strata", "age_3groups","is_female","no_relatedness","townsend_depri_index","depri_2g","sbp_mean","sbp_mean2","TC","LDL_C","HDL_C","TG","RG","HbA1c","has_diabetes","has_AF","used_blood_pressure_drugs","stroke_fh_2groups")
varlist <- c(genetics,PCs,states,dates,lifestyles,covariates)
# summary(df[,..varlist])

################################################################################################################################################
####################################                                                        ####################################################
####################################                   2. Incident model                    ####################################################
####################################                                                        ####################################################
################################################################################################################################################

################################################################################################################################################
####################################                   2.1 From any                         ####################################################
################################################################################################################################################

# # =====================================================
# 先不考虑复杂的，把所有基线前发病的人都剔除
mltstate.origin.any %<>% mutate(enrollment=1)

# 首先分析任何卒中I60~64,及其向复发卒中、死亡和CHD的预后,额外把28d的结局拆分出来
tmat3 <- transMat(x = list(c(2), c(3), c(4,5,6), c(6),c(6), c()), names = c("healthy","firststroke","28d-surv","Rec_stroke","CHD","Death"))

# 转为Multistate数据库:如果同时死亡和发生CHD，优先级：CHD高于死亡，因此目前暂不单独处理CHD和death日期相同的情况了
mltstate.long.any3 <- msprep(data = mltstate.origin.any, trans = tmat3,
                        time = c(NA,"age_first_anystroke","age_surv_28d","age_sec_anystroke","age_CHD","age_death"),
                        status = c(NA,"first_anystroke","surv_28d","sec_anystroke","CHD","death"),
                        keep = c("eid",varlist),
                        start = list(state = mltstate.origin.any$enrollment,time = mltstate.origin.any$age_at_study_date))

# 查看是否有time=0的转移
sum(mltstate.long.any3$time==0)

# 查看转移矩阵
events(mltstate.long.any3[mltstate.long.any3$no_relatedness==1,])
# fwrite(mltstate.long.any3,paste0("ukb_any3.csv"),sep=",",col.names=T)
# $Frequencies
#              to
# from          healthy firststroke 28d-surv Rec_stroke    CHD  Death no event total entering
#   healthy           0        7800        0          0      0      0   372548         380348
#   firststroke       0           0     5767          0      0      0     2033           7800
#   28d-surv          0           0        0        947    416    518     3886           5767
#   Rec_stroke        0           0        0          0      0    389      558            947
#   CHD               0           0        0          0      0    137      279            416
#   Death             0           0        0          0      0      0     1044           1044

# Transition-specific covariates
mltstate.full.any3 <- expand.covs(mltstate.long.any3, c(genetics,PCs,lifestyles,covariates), longnames = FALSE)  # 均为哑变量
# head(mltstate.full.any3)
# names(mltstate.full.any3)

# Drop observation of which time = 0 
message(sum(mltstate.full.any3$time==0)," records was droped")
mltstate.final.any3 <- subset(mltstate.full.any3,time != 0)

fwrite(mltstate.final.any3,paste0("ukb_rec_stroke_any3_28d.csv"),sep=",",col.names=T)
rm(mltstate.final.any3)
message(rep("!",50),'\n',paste0("ukb_rec_stroke_any3_28d.csv"," is generated"),'\n',rep("!",50))


################################################################################################################################################
####################################                   2.2 From subtypes                    ####################################################
################################################################################################################################################
mltstate.origin.sub %<>% filter(age_first_anystroke>age_at_study_date&age_CHD>age_at_study_date&age_death>age_at_study_date) %>% 
   mutate(enrollment=1)

# 定义转移矩阵
tmat6 <- transMat(x = list(c(2,3,4,5),c(6),c(7),c(8),c(9),c(10,11,12),c(10,11,12),c(10,11,12),c(10,11,12), c(12),c(12), c()), names = c("healthy","SAH","ICH","IS","Uns","surv_28d_I60","surv_28d_I61","surv_28d_I63","surv_28d_I64","Rec_stroke","CHD","Death"))

# 选择要保留的数据

# 转为Multistate数据库
mltstate.long.sub3 <- msprep(data = mltstate.origin.sub, trans = tmat6,
                        time = c(NA,"age_first_I60","age_first_I61","age_first_I63","age_first_I64","age_surv_28d_I60","age_surv_28d_I61","age_surv_28d_I63","age_surv_28d_I64","age_sec_anystroke","age_CHD","age_death"),
                        status = c(NA,"first_I60","first_I61","first_I63","first_I64","surv_28d_I60","surv_28d_I61","surv_28d_I63","surv_28d_I64","sec_anystroke","CHD","death"),
                        keep = c("eid",varlist),
                        start = list(state = mltstate.origin.sub$enrollment,time = mltstate.origin.sub$age_at_study_date))

# 查看是否有time=0的转移
sum(mltstate.long.sub3$time==0)

# 查看转移矩阵
events(mltstate.long.sub3[mltstate.long.sub3$no_relatedness==1,])
#               to
# from           healthy    SAH    ICH     IS    Uns surv_28d_I60 surv_28d_I61 surv_28d_I63 surv_28d_I64 Rec_stroke    CHD  Death no event total entering
#   healthy            0    733   1072   4915   1080            0            0            0            0          0      0      0   372548         380348
#   SAH                0      0      0      0      0          489            0            0            0          0      0      0      244            733
#   ICH                0      0      0      0      0            0          650            0            0          0      0      0      422           1072
#   IS                 0      0      0      0      0            0            0         3836            0          0      0      0     1079           4915
#   Uns                0      0      0      0      0            0            0            0          792          0      0      0      288           1080
#   surv_28d_I60       0      0      0      0      0            0            0            0            0        102     18     33      336            489
#   surv_28d_I61       0      0      0      0      0            0            0            0            0        166     26     93      365            650
#   surv_28d_I63       0      0      0      0      0            0            0            0            0        553    308    319     2656           3836
#   surv_28d_I64       0      0      0      0      0            0            0            0            0        126     64     73      529            792
#   Rec_stroke         0      0      0      0      0            0            0            0            0          0      0    389      558            947
#   CHD                0      0      0      0      0            0            0            0            0          0      0    137      279            416
#   Death              0      0      0      0      0            0            0            0            0          0      0      0     1044           1044

# Transition-specific covariates
mltstate.full.sub3 <- expand.covs(mltstate.long.sub3, c(genetics,PCs,lifestyles,covariates), longnames = FALSE)  # 均为哑变量

# Drop observation of which time = 0 
message(sum(mltstate.full.sub3$time==0)," records was droped")
mltstate.final.sub3 <- subset(mltstate.full.sub3,time != 0)

fwrite(mltstate.final.sub3,paste0("ukb_rec_stroke_sub3_28d.csv"),sep=",",col.names=T)
rm(mltstate.final.sub3)
message(rep("!",50),'\n',paste0("ukb_rec_stroke_sub3_28d.csv"," is generated"),'\n',rep("!",50))
