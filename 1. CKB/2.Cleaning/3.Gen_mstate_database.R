################################################################################################################################################
# 设置工作路径
setwd("/public/home/hanyuting/2022-04_CMM_G")

# 加载r包
rm(list=ls())
options(width=150)
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
pheno <- fread("1.Data/cleaned_recur_stroke.csv",h=T)   
nrow(pheno)  # 100,639人
df <- pheno

#===================================================================================================
# Merge PCs
PCs <- fread("/public/data/GWAS/2018-11-22-Impute_QCed/ckb_pca_v1.1/national_pca.eigenvec.v1.1.txt")
df <- merge(df,PCs,by=c("FID","IID"))

#===================================================================================================
# Merge All_PRS
PGSlist <- c("PGS002259_metaSTROKE_Lu_EAS","PGS002725_metaISTROKE_EAS","CKB_90104535")
for (i in 1:length(PGSlist)){
    message(PGSlist[i]," is processing")
    PRS <- fread(paste0("/public/home/hanyuting/2.PRS/",PGSlist[i],".sscore"),h=T)
    PRS <- data.frame(PRS)
    PRS[,5] <- scale(PRS[,5])
    PRS[,6] <- as.factor(cut(PRS[,5],breaks=quantile(PRS[,5],probs = c(0,0.25,0.75,1)),labels=c(1,2,3),include.lowest=T))
    colnames(PRS)[c(1,2,5,6)] <- c("FID","IID",PGSlist[i],paste0(PGSlist[i],".cat"))
    df <- merge(df,PRS[,c(1,2,5,6)],by=c("FID","IID"))
    nrow(df)
}
summary(df[,..PGSlist])

#===================================================================================================
# 导入无亲缘关系的IID，然后仅保留其中没有亲缘关系个体
# 导出一个数据库用来计算baseline characteristics
id_norel <- fread("/public/home/hanyuting/01-GWAS_data/Relatedness/id_norel.txt",h=T)
df[df$IID %in% id_norel$IID,"no_relatedness"] <- 1
fwrite(df[no_relatedness==1],"1.Data/CKB_table1.csv",sep=",",col.names=T)

#===================================================================================================
# 处理不同阶段日期相同的情况
# 总体逻辑是优先级：first stroke（SAH>ICH>IS>Uns) > second_stroke > chd > death;为了在后面生成长数据阶段不出现error，死亡日期不能变

# 考虑以下几种情况:
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

# 2. 首发卒中和CHD日期相同，但跟死亡日期不同(如果研究对象的某时间发生在随访末2018年年底，也算是考虑进来了)
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
# 处理协变量的分组
# 处理新的血压变量来考虑药物的影响
df$sbp_mean2 <- df$sbp_mean
df[used_blood_pressure_drugs==1]$sbp_mean2 <- df[used_blood_pressure_drugs==1]$sbp_mean2 + 15

# 处理代表基线状态的变量
df$enrollment <- 1

#===================================================================================================
# 剔除基线患有卒中的研究对象，因为我们无法判定这部分人是发生了一次还是多次，并且也无法区分亚型
mltstate.origin.any <- df %>% filter(stroke_or_tia_diag==0&chd_diag==0)
mltstate.origin.sub <- df %>% filter(stroke_or_tia_diag==0&chd_diag==0)
# 96400人

#===================================================================================================
# 导入无亲缘关系的IID，然后仅保留其中没有亲缘关系个体
id_norel <- fread("/public/home/hanyuting/01-GWAS_data/Relatedness/id_norel.txt",h=T)
mltstate.origin.any[mltstate.origin.any$IID %in% id_norel$IID,"no_relatedness"] <- 1
mltstate.origin.sub[mltstate.origin.sub$IID %in% id_norel$IID,"no_relatedness"] <- 1
# mltstate.origin.any %<>% filter(IID %in% id_norel$IID)
# mltstate.origin.sub %<>% filter(IID %in% id_norel$IID)
# 剔除15492人，剩下80908人

# 查看剔除亲缘关系后，首发卒中和复发卒中分布
table(mltstate.origin.any$first_stroke_type)
table(mltstate.origin.any$first_stroke_type,mltstate.origin.any$sec_stroke_type)

#===================================================================================================
# 设置常用的变量，用于后续的处理
PGSlist <- c("PGS002259_metaSTROKE_Lu_EAS","PGS002725_metaISTROKE_EAS","CKB_90104535")
genetics <- c(PGSlist,paste0(PGSlist,".cat"))
PCs <- paste0("PC",1:10)
states <- c("enrollment","first_I60","first_I61","first_I63","first_I64","sec_I60","sec_I61","sec_I63","sec_I64","first_anystroke","sec_anystroke","death")
dates <- c("age_at_study_date","age_first_I60","age_first_I61","age_first_I63","age_first_I64","age_sec_I60","age_sec_I61","age_sec_I63","age_sec_I64",
   "age_first_anystroke","age_sec_anystroke","age_death",
   "study_date","CHD_date","first_anystroke_date","sec_anystroke_date","death_date")
lifestyles <- c("smoking_4groups","alcohol_5groups","diet_5groups","obesity_5groups")
covariates <- c("age_strata","region_code","region_is_urban","is_female","highest_education","education_2groups", "age_3groups","stroke_fh_2groups",
                "has_hypertension","chd_diag","stroke_or_tia_diag","has_diabetes","is_in_gwas_population_subset","gwas_array_type","sbp_mean2",
                "no_relatedness","sbp_mean","used_blood_pressure_drugs","glucose_lowering_2groups")
varlist <- c(genetics,PCs,states,dates,lifestyles,covariates)

fwrite(mltstate.origin.any[,c("surv_28d","age_surv_28d","CHD","age_CHD",..varlist)],"1.Data/rec_stroke_baseline.csv",sep=",",col.names=T)

################################################################################################################################################
####################################                                                        ####################################################
####################################                   2. From anystroke                    ####################################################
####################################                                                        ####################################################
################################################################################################################################################

# 首先分析任何卒中I60~64,及其向复发卒中、死亡和CHD的预后,额外把28d的结局拆分出来
tmat3 <- transMat(x = list(c(2), c(3), c(4,5,6), c(6),c(6), c()), names = c("healthy","firststroke","28d-surv","Rec_stroke","CHD","Death"))

# 转为Multistate数据库:如果同时死亡和发生CHD，优先级：CHD高于死亡，因此目前暂不单独处理CHD和death日期相同的情况了
mltstate.long.any3 <- msprep(data = mltstate.origin.any, trans = tmat3,
                        time = c(NA,"age_first_anystroke","age_surv_28d","age_sec_anystroke","age_CHD","age_death"),
                        status = c(NA,"first_anystroke","surv_28d","sec_anystroke","CHD","death"),
                        keep = c("csid",varlist),
                        start = list(state = mltstate.origin.any$enrollment,time = mltstate.origin.any$age_at_study_date))

# 查看是否有time=0的转移
sum(mltstate.long.any3$time==0)

# 查看转移矩阵
events(mltstate.long.any3)

# 剔除亲缘关系的转移矩阵
trans_matrix <- events(mltstate.long.any3[mltstate.long.any3$no_relatedness==1,])
freq_trans <- data.frame(trans_matrix$Frequencies)
freq_trans <- dcast(freq_trans,from~to,value.var = "Freq")
prop_trans <- data.frame(trans_matrix$Proportions)
prop_trans <- dcast(prop_trans,from~to,value.var = "Freq")
for (i in 2:8){
    freq_trans[,i] <- paste(freq_trans[,i]," (",sprintf(round(prop_trans[,i]*100,1),fmt="%.1f"),")")
}
write.csv(freq_trans, paste("3.Result/CKB/trans_any3_norel",".csv",sep = ""))

# Transition-specific covariates
mltstate.full.any3 <- expand.covs(mltstate.long.any3, c(genetics,PCs,lifestyles,covariates), longnames = FALSE)  # 均为哑变量
# head(mltstate.full.any3)
# names(mltstate.full.any3)

# Drop observation of which time = 0 
message(sum(mltstate.full.any3$time==0)," records was droped")
mltstate.final.any3 <- subset(mltstate.full.any3,time != 0)

fwrite(mltstate.final.any3,"1.Data/rec_stroke_any3.csv",sep=",",col.names=T)
rm(mltstate.final.any3)
message(rep("!",50),'\n',paste0("CKB_rec_stroke_any3.csv"," is generated"),'\n',rep("!",50))


################################################################################################################################################
####################################                                                        ####################################################
####################################                   3. From Subtypes                     ####################################################
####################################                                                        ####################################################
################################################################################################################################################

# 定义转移矩阵
tmat6 <- transMat(x = list(c(2,3,4,5),c(6),c(7),c(8),c(9),c(10,11,12),c(10,11,12),c(10,11,12),c(10,11,12), c(12),c(12), c()), names = c("healthy","SAH","ICH","IS","Uns","surv_28d_I60","surv_28d_I61","surv_28d_I63","surv_28d_I64","Rec_stroke","CHD","Death"))

# 选择要保留的数据

# 转为Multistate数据库
mltstate.long.sub3 <- msprep(data = mltstate.origin.sub, trans = tmat6,
                        time = c(NA,"age_first_I60","age_first_I61","age_first_I63","age_first_I64","age_surv_28d_I60","age_surv_28d_I61","age_surv_28d_I63","age_surv_28d_I64","age_sec_anystroke","age_CHD","age_death"),
                        status = c(NA,"first_I60","first_I61","first_I63","first_I64","surv_28d_I60","surv_28d_I61","surv_28d_I63","surv_28d_I64","sec_anystroke","CHD","death"),
                        keep = c("csid",varlist),
                        start = list(state = mltstate.origin.sub$enrollment,time = mltstate.origin.sub$age_at_study_date))

# 查看是否有time=0的转移
sum(mltstate.long.sub3$time==0)

# 剔除亲缘关系的转移矩阵
trans_matrix <- events(mltstate.long.sub3[mltstate.long.sub3$no_relatedness==1,])
freq_trans <- data.frame(trans_matrix$Frequencies)
freq_trans <- dcast(freq_trans,from~to,value.var = "Freq")
prop_trans <- data.frame(trans_matrix$Proportions)
prop_trans <- dcast(prop_trans,from~to,value.var = "Freq")
for (i in 2:13){
    freq_trans[,i] <- paste(freq_trans[,i]," (",sprintf(round(prop_trans[,i]*100,1),fmt="%.1f"),")")
}
write.csv(freq_trans, paste("3.Result/CKB/trans_sub3_norel",".csv",sep = ""))


# Transition-specific covariates
mltstate.full.sub3 <- expand.covs(mltstate.long.sub3, c(genetics,PCs,lifestyles,covariates), longnames = FALSE)  # 均为哑变量
# head(mltstate.full.sub3)
# names(mltstate.full.sub3)

# Drop observation of which time = 0 
message(sum(mltstate.full.sub3$time==0)," records was droped")
mltstate.final.sub3 <- subset(mltstate.full.sub3,time != 0)

fwrite(mltstate.final.sub3,"1.Data/rec_stroke_sub3.csv",sep=",",col.names=T)
rm(mltstate.final.sub3)
message(rep("!",50),'\n',paste0("CKB_rec_stroke_sub3.csv"," is generated"),'\n',rep("!",50))



################################################################################################################################################
####################################                                                        ####################################################
####################################                   4. same type recurrent               ####################################################
####################################                                                        ####################################################
################################################################################################################################################

# 定义转移矩阵
tmat7 <- transMat(x = list(c(2,3),c(4),c(5),c(6,7,8,9),c(6,7,8,9),c(9),c(9),c(9), c()), names = c("healthy","ICH","IS","surv_28d_I61","surv_28d_I63","Rec_ICH","Rec_IS","CHD","Death"))

# 转为Multistate数据库
mltstate.long.sub.type <- msprep(data = mltstate.origin.sub, trans = tmat7,
                          time = c(NA,"age_first_I61","age_first_I63","age_surv_28d_I61","age_surv_28d_I63","age_sec_I61","age_sec_I63","age_CHD","age_death"),
                          status = c(NA,"first_I61","first_I63","surv_28d_I61","surv_28d_I63","sec_I61","sec_I63","CHD","death"),
                          keep = c("csid",varlist),
                          start = list(state = mltstate.origin.sub$enrollment,time = mltstate.origin.sub$age_at_study_date))
# 查看是否有time=0的转移
sum(mltstate.long.sub.type$time==0)

# 剔除亲缘关系的转移矩阵
trans_matrix <- events(mltstate.long.sub.type[mltstate.long.sub.type$no_relatedness==1,])
freq_trans <- data.frame(trans_matrix$Frequencies)
freq_trans <- dcast(freq_trans,from~to,value.var = "Freq")
prop_trans <- data.frame(trans_matrix$Proportions)
prop_trans <- dcast(prop_trans,from~to,value.var = "Freq")
for (i in 2:11){
    freq_trans[,i] <- paste(freq_trans[,i]," (",sprintf(round(prop_trans[,i]*100,1),fmt="%.1f"),")")
}
write.csv(freq_trans, paste("3.Result/CKB/trans_sub3_type_norel",".csv",sep = ""))


# Transition-specific covariates
mltstate.full.sub.type <- expand.covs(mltstate.long.sub.type, c(genetics,PCs,lifestyles,covariates), longnames = FALSE)  # 均为哑变量
# head(mltstate.full.sub.type)
# names(mltstate.full.sub.type)

# Drop observation of which time = 0 
message(sum(mltstate.full.sub.type$time==0)," records was droped")
mltstate.final.sub.type <- subset(mltstate.full.sub.type,time != 0)

fwrite(mltstate.final.sub.type,"1.Data/rec_stroke_sub3_type.csv",sep=",",col.names=T)
rm(mltstate.final.sub.type)
message(rep("!",50),'\n',paste0("CKB_rec_stroke_sub3_type.csv"," is generated"),'\n',rep("!",50))




