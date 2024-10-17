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
if (type %in% 1:4){

    cl_n <- 10
}else{

    cl_n <- 3
}

cl <- makeCluster(cl_n)
registerDoParallel(cl)

# 加载写好的function
source("~/0.Code/mstate_regress_V5.R")
source("~/0.Code/mstate_GXE_V2.R")

# 设置存在结果的
path_mstate_st <- paste0("3.Result/CKB/rec_stroke_mstate_GXE/",Sys.Date(),"/")
dir.create(path_mstate_st)

#===================================================================================================
# Set essential parameters
cov_list1 <- c("is_female",paste0("PC",1:5),"gwas_array_type")

basic_formula <- paste0("Surv(Tstart, Tstop, status)","~","strata(trans)+strata(age_strata)+strata(region_code)")

################################################################################################################################################
####################################                                                        ####################################################
####################################                       1. Anymodel                      ####################################################
####################################                                                        ####################################################
################################################################################################################################################

#====================================================================================================================
# # # # # # Age groups
if (type==1){

    df_any3 <- fread("1.Data/rec_stroke_any3.csv",h=T)
    ex <- "PGS002725_metaISTROKE_EAS"
    gp <- "age_3groups"
    cov <- cov_list1
    data <- df_any3[no_relatedness==1]


    # Subgroup analysis
    Sub_PGS002725_age <- foreach(i=0:2,.combine="rbind") %dopar% {

        library(mstate)
        library(dplyr)
        library(data.table)
        library(lmtest)
        mstate_v5(ex,cov,data[age_3groups==i],F)

    }
    fwrite(Sub_PGS002725_age,paste0(path_mstate_st,"Age_2725score_any3_hr.csv"),sep=",")

    # GXE
    # 生成mstate方程的formula
    base_formula <- mstate_formula(basic_formula,ex,gp,cov,df_any3[no_relatedness==1])
    # base model
    model_PGS002725_age <- coxph(as.formula(base_formula),data=data,method = "breslow",control = coxph.control(timefix = F))
    # Cal interaction focusing on P value
    GXE_PGS002725_age <- foreach(i=1:7,.combine="rbind") %dopar% {

        library(mstate)
        library(dplyr)
        library(data.table)
        library(lmtest)
        if (grepl(pattern = "cat",x = ex)){

            mstate_GXE_cat(base_formula,model_PGS002725_age,ex,gp,i,data,F)

        }else{

            mstate_GXE_linear(base_formula,model_PGS002725_age,ex,gp,i,data,F)

        }


    }
    fwrite(GXE_PGS002725_age,paste0(path_mstate_st,"Age_2725score_pvalue.csv"),sep=",")

}


#====================================================================================================================
# # # # # # Sex: linear&cat 均在复发阶段显著
if (type==2){

    df_any3 <- fread("1.Data/rec_stroke_any3.csv",h=T)
    ex <- "PGS002725_metaISTROKE_EAS.cat"
    gp <- "is_female"
    cov <- cov_list1[!cov_list1=="is_female"]
    data <- df_any3[no_relatedness==1]


    # Subgroup analysis
    Sub_PGS002725_sex <- foreach(i=0:1,.combine="rbind") %dopar% {

        library(mstate)
        library(dplyr)
        library(data.table)
        library(lmtest)
        mstate_v5(ex,cov,data[is_female==i],F)

    }
    fwrite(Sub_PGS002725_sex,paste0(path_mstate_st,"Sex_2725score_any3_hr.csv"),sep=",")

    # GXE
    # 生成mstate方程的formula
    base_formula <- mstate_formula(basic_formula,ex,gp,cov,df_any3[no_relatedness==1])
    # base model
    model_PGS002725_sex <- coxph(as.formula(base_formula),data=data,method = "breslow",control = coxph.control(timefix = F))
    # Cal interaction focusing on P value
    GXE_PGS002725_sex <- foreach(i=1:7,.combine="rbind") %dopar% {

        library(mstate)
        library(dplyr)
        library(data.table)
        library(lmtest)
        if (grepl(pattern = "cat",x = ex)){

            mstate_GXE_cat(base_formula,model_PGS002725_sex,ex,gp,i,data,F)

        }else{

            mstate_GXE_linear(base_formula,model_PGS002725_sex,ex,gp,i,data,F)

        }


    }
    fwrite(GXE_PGS002725_sex,paste0(path_mstate_st,"Sex_2725score_pvalue.csv"),sep=",")

}

#====================================================================================================================
# # # # # # Residence
if (type==3){

    df_any3 <- fread("1.Data/rec_stroke_any3.csv",h=T)
    ex <- "PGS002725_metaISTROKE_EAS.cat"
    gp <- "region_is_urban"
    cov <- cov_list1[!cov_list1=="region_is_urban"]
    basic_formula <- paste0("Surv(Tstart, Tstop, status)","~","strata(trans)+strata(age_strata)")
    data <- df_any3[no_relatedness==1]


    # Subgroup analysis
    Sub_PGS002725_urban <- foreach(i=0:1,.combine="rbind") %dopar% {

        library(mstate)
        library(dplyr)
        library(data.table)
        library(lmtest)
        mstate_v5(ex,cov,data[region_is_urban==i],F)

    }
    fwrite(Sub_PGS002725_urban,paste0(path_mstate_st,"Urban_2725score_any3_hr.csv"),sep=",")

    # GXE
    # 生成mstate方程的formula
    base_formula <- mstate_formula(basic_formula,ex,gp,cov,df_any3[no_relatedness==1])
    # base model
    model_PGS002725_urban <- coxph(as.formula(base_formula),data=data,method = "breslow",control = coxph.control(timefix = F))
    # Cal interaction focusing on P value
    GXE_PGS002725_urban <- foreach(i=1:7,.combine="rbind") %dopar% {

        library(mstate)
        library(dplyr)
        library(data.table)
        library(lmtest)
        if (grepl(pattern = "cat",x = ex)){

            mstate_GXE_cat(base_formula,model_PGS002725_urban,ex,gp,i,data,F)

        }else{

            mstate_GXE_linear(base_formula,model_PGS002725_urban,ex,gp,i,data,F)

        }

    }
    fwrite(GXE_PGS002725_urban,paste0(path_mstate_st,"Urban_2725score_pvalue.csv"),sep=",")

}


#====================================================================================================================
# # # # # # Education:linear&cat no interaction
if (type==3){

    df_any3 <- fread("1.Data/rec_stroke_any3.csv",h=T)
    ex <- "PGS002725_metaISTROKE_EAS.cat"
    gp <- "education_2groups"
    cov <- cov_list1[!cov_list1=="education_2groups"]
    data <- df_any3[no_relatedness==1]


    # Subgroup analysis
    Sub_PGS002725_edu <- foreach(i=0:1,.combine="rbind") %dopar% {

        library(mstate)
        library(dplyr)
        library(data.table)
        library(lmtest)
        mstate_v5(ex,cov,data[education_2groups==i],F)

    }
    fwrite(Sub_PGS002725_edu,paste0(path_mstate_st,"Edu_2725score_any3_hr.csv"),sep=",")

    # GXE
    # 生成mstate方程的formula
    base_formula <- mstate_formula(basic_formula,ex,gp,cov,df_any3[no_relatedness==1])
    # base model
    model_PGS002725_edu <- coxph(as.formula(base_formula),data=data,method = "breslow",control = coxph.control(timefix = F))
    # Cal interaction focusing on P value
    GXE_PGS002725_edu <- foreach(i=1:7,.combine="rbind") %dopar% {

        library(mstate)
        library(dplyr)
        library(data.table)
        library(lmtest)
        if (grepl(pattern = "cat",x = ex)){

            mstate_GXE_cat(base_formula,model_PGS002725_edu,ex,gp,i,data,F)

        }else{

            mstate_GXE_linear(base_formula,model_PGS002725_edu,ex,gp,i,data,F)

        }

    }
    fwrite(GXE_PGS002725_edu,paste0(path_mstate_st,"Edu_2725score_pvalue.csv"),sep=",")

}


#====================================================================================================================
# # # # # # Family history of stroke
if (type==4){

    df_any3 <- fread("1.Data/rec_stroke_any3.csv",h=T)
    ex <- "PGS002725_metaISTROKE_EAS.cat"
    gp <- "stroke_fh_2groups"
    cov <- cov_list1[!cov_list1=="stroke_fh_2groups"]
    data <- df_any3[no_relatedness==1]


    # Subgroup analysis
    Sub_PGS002725_fh <- foreach(i=0:1,.combine="rbind") %dopar% {

        library(mstate)
        library(dplyr)
        library(data.table)
        library(lmtest)
        mstate_v5(ex,cov,data[stroke_fh_2groups==i],F)

    }
    fwrite(Sub_PGS002725_fh,paste0(path_mstate_st,"Fh_2725score_any3_hr.csv"),sep=",")

    # GXE
    # 生成mstate方程的formula
    base_formula <- mstate_formula(basic_formula,ex,gp,cov,df_any3[no_relatedness==1])
    # base model
    model_PGS002725_fh <- coxph(as.formula(base_formula),data=data,method = "breslow",control = coxph.control(timefix = F))
    # Cal interaction focusing on P value
    GXE_PGS002725_fh <- foreach(i=1:7,.combine="rbind") %dopar% {

        library(mstate)
        library(dplyr)
        library(data.table)
        library(lmtest)
        if (grepl(pattern = "cat",x = ex)){

            mstate_GXE_cat(base_formula,model_PGS002725_fh,ex,gp,i,data,F)

        }else{

            mstate_GXE_linear(base_formula,model_PGS002725_fh,ex,gp,i,data,F)

        }

    }
    fwrite(GXE_PGS002725_fh,paste0(path_mstate_st,"Fh_2725score_pvalue.csv"),sep=",")

}
