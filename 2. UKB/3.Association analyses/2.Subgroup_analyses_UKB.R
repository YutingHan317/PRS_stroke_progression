################################################################################################################################################
####################################                                                        ####################################################
####################################                       0. Settings                      ####################################################
####################################                                                        ####################################################
################################################################################################################################################
rm(list=ls())

# # # # # # Parameter
args <- commandArgs(trailingOnly = TRUE)
type <- args[1]
message("type is ",type)


# # # # # # Settings
setwd("/public/home/hanyuting/2022-04_CMM_G")
options(width=150)
library(mstate)
library(data.table)
library(dplyr)
library(magrittr)
library(reshape2)

# 多核运算
library(doParallel)
cl_n <- 7
cl <- makeCluster(cl_n)
registerDoParallel(cl)
# stopCluster(cl)

# 加载写好的function
source("~/0.Code/mstate_regress_V5.R")
source("~/0.Code/mstate_GXE_V2.R")

# 设置存在结果的
path_mstate_st <- paste0("3.Result/UKB/ukb_rec_stroke_mstate_GXE/",Sys.Date(),"/")
dir.create(path_mstate_st)

#===================================================================================================
# Set essential parameters
cov_list1 <- c("is_female",paste0("PC",1:10))
cov_list2 <- c(cov_list1,"smoking_status","drinking_6groups","risky_PA","diet_3groups","obesity_5groups")
cov_list3 <- c(cov_list2,"sbp_mean2","has_diabetes")
cov_list4 <- c(cov_list3,"has_AF","TC","LDL_C","HDL_C","TG")

basic_formula <- paste0("Surv(Tstart, Tstop, status)","~","strata(trans)+strata(age_strata)")


################################################################################################################################################
####################################                                                        ####################################################
####################################                       1. Anymodel                      ####################################################
####################################                                                        ####################################################
################################################################################################################################################

#====================================================================================================================
# # # # # # Age groups
if (type==1){

    df_any3 <- fread("1.UKB/ukb_rec_stroke_any3.csv",h=T)
    ex <- "PGS002724_metaISTROKE_EUR.cat"
    gp <- "age_3groups"
    cov <- cov_list1
    data <- df_any3[no_relatedness==1]


    # Subgroup analysis
    Sub_PGS002724_age <- foreach(i=0:2,.combine="rbind") %dopar% {

        library(mstate)
        library(dplyr)
        library(data.table)
        library(lmtest)
        mstate_v5(ex,cov,data[age_3groups==i],F)

    }
    fwrite(Sub_PGS002724_age,paste0(path_mstate_st,"UKB_Age_2724score_any3_hr.csv"),sep=",")

    # GXE
    # 生成mstate方程的formula
    base_formula <- mstate_formula(basic_formula,ex,gp,cov,df_any3[no_relatedness==1])
    # base model
    model_PGS002724_age <- coxph(as.formula(base_formula),data=data,method = "breslow",control = coxph.control(timefix = F))
    # Cal interaction focusing on P value
    GXE_PGS002724_age <- foreach(i=1:7,.combine="rbind") %dopar% {

        library(mstate)
        library(dplyr)
        library(data.table)
        library(lmtest)
        if (grepl(pattern = "cat",x = ex)){

            mstate_GXE_cat(base_formula,model_PGS002724_age,ex,gp,i,data,F)

        }else{

            mstate_GXE_linear(base_formula,model_PGS002724_age,ex,gp,i,data,F)

        }

    }
    fwrite(GXE_PGS002724_age,paste0(path_mstate_st,"UKB_Age_2724score_pvalue.csv"),sep=",")

}


#====================================================================================================================
# # # # # # Sex
if (type==2){

    df_any3 <- fread("1.UKB/ukb_rec_stroke_any3.csv",h=T)
    ex <- "PGS002724_metaISTROKE_EUR.cat"
    gp <- "is_female"
    cov <- cov_list1[!cov_list1=="is_female"]
    data <- df_any3[no_relatedness==1]


    # Subgroup analysis
    Sub_PGS002724_sex <- foreach(i=0:1,.combine="rbind") %dopar% {

        library(mstate)
        library(dplyr)
        library(data.table)
        library(lmtest)
        mstate_v5(ex,cov,data[is_female==i],F)

    }
    fwrite(Sub_PGS002724_sex,paste0(path_mstate_st,"UKB_Sex_2724score_any3_hr.csv"),sep=",")

    # GXE
    # 生成mstate方程的formula
    base_formula <- mstate_formula(basic_formula,ex,gp,cov,df_any3[no_relatedness==1])
    # base model
    model_PGS002724_sex <- coxph(as.formula(base_formula),data=data,method = "breslow",control = coxph.control(timefix = F))
    # Cal interaction focusing on P value
    GXE_PGS002724_sex <- foreach(i=1:7,.combine="rbind") %dopar% {

        library(mstate)
        library(dplyr)
        library(data.table)
        library(lmtest)
        if (grepl(pattern = "cat",x = ex)){

            mstate_GXE_cat(base_formula,model_PGS002724_sex,ex,gp,i,data,F)

        }else{

            mstate_GXE_linear(base_formula,model_PGS002724_sex,ex,gp,i,data,F)

        }

    }
    fwrite(GXE_PGS002724_sex,paste0(path_mstate_st,"UKB_Sex_2724score_pvalue.csv"),sep=",")

}


#====================================================================================================================
# # # # # # Deprivation
if (type==3){

    df_any3 <- fread("1.UKB/ukb_rec_stroke_any3.csv",h=T)
    ex <- "PGS002724_metaISTROKE_EUR.cat"
    gp <- "depri_2g"
    cov <- cov_list1[!cov_list1=="depri_2g"]
    data <- df_any3[no_relatedness==1]


    # Subgroup analysis
    Sub_PGS002724_depri <- foreach(i=0:1,.combine="rbind") %dopar% {

        library(mstate)
        library(dplyr)
        library(data.table)
        library(lmtest)
        mstate_v5(ex,cov,data[depri_2g==i],F)

    }
    fwrite(Sub_PGS002724_depri,paste0(path_mstate_st,"UKB_depri_2724score_any3_hr.csv"),sep=",")

    # GXE
    # 生成mstate方程的formula
    base_formula <- mstate_formula(basic_formula,ex,gp,cov,df_any3[no_relatedness==1])
    # base model
    model_PGS002724_depri <- coxph(as.formula(base_formula),data=data,method = "breslow",control = coxph.control(timefix = F))
    # Cal interaction focusing on P value
    GXE_PGS002724_depri <- foreach(i=1:7,.combine="rbind") %dopar% {

        library(mstate)
        library(dplyr)
        library(data.table)
        library(lmtest)
        if (grepl(pattern = "cat",x = ex)){

            mstate_GXE_cat(base_formula,model_PGS002724_depri,ex,gp,i,data,F)

        }else{

            mstate_GXE_linear(base_formula,model_PGS002724_depri,ex,gp,i,data,F)

        }

    }
    fwrite(GXE_PGS002724_depri,paste0(path_mstate_st,"UKB_depri_2724score_pvalue.csv"),sep=",")

}


#====================================================================================================================
# # # # # # Family history of stroke
if (type==4){

    df_any3 <- fread("1.UKB/ukb_rec_stroke_any3.csv",h=T)
    ex <- "PGS002724_metaISTROKE_EUR.cat"
    gp <- "stroke_fh_2groups"
    cov <- cov_list1[!cov_list1=="stroke_fh_2groups"]
    data <- df_any3[no_relatedness==1]


    # Subgroup analysis
    Sub_PGS002724_fh <- foreach(i=0:1,.combine="rbind") %dopar% {

        library(mstate)
        library(dplyr)
        library(data.table)
        library(lmtest)
        mstate_v5(ex,cov,data[stroke_fh_2groups==i],F)

    }
    fwrite(Sub_PGS002724_fh,paste0(path_mstate_st,"UKB_Fh_2724score_any3_hr.csv"),sep=",")

    # GXE
    # 生成mstate方程的formula
    base_formula <- mstate_formula(basic_formula,ex,gp,cov,df_any3[no_relatedness==1])
    # base model
    model_PGS002724_fh <- coxph(as.formula(base_formula),data=data,method = "breslow",control = coxph.control(timefix = F))
    # Cal interaction focusing on P value
    GXE_PGS002724_fh <- foreach(i=1:7,.combine="rbind") %dopar% {

        library(mstate)
        library(dplyr)
        library(data.table)
        library(lmtest)
        if (grepl(pattern = "cat",x = ex)){

            mstate_GXE_cat(base_formula,model_PGS002724_fh,ex,gp,i,data,F)

        }else{

            mstate_GXE_linear(base_formula,model_PGS002724_fh,ex,gp,i,data,F)

        }

    }
    fwrite(GXE_PGS002724_fh,paste0(path_mstate_st,"UKB_Fh_2724score_pvalue.csv"),sep=",")

}


