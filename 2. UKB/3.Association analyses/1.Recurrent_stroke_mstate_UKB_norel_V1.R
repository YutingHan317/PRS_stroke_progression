
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
if (type==2|type==4){

    cl_n <- 1
}else{

    cl_n <- 3
}

cl <- makeCluster(cl_n)
registerDoParallel(cl)

# stopCluster(cl)

# 加载写好的function
source("~/0.Code/mstate_regress_V5.R")
source("~/0.Code/mstate_regress_V6.R")


# 设置存在结果的
path_mstate_st <- paste0("/public/home/hanyuting/2022-04_CMM_G/3.Result/UKB/rec_stroke_mstate_ukb/",Sys.Date(),"/")
dir.create(path_mstate_st)

#===================================================================================================
# Set essential parameters
basic_formula <- paste0("Surv(Tstart, Tstop, status)","~","strata(trans)+strata(age_strata)")

cov_list1 <- c("is_female",paste0("PC",1:10))
cov_list2 <- c(cov_list1,"smoking_4groups","drinking_4groups","diet_5groups","obesity_5groups")
cov_list3 <- c(cov_list2,"sbp_mean2","has_diabetes")
cov_list4 <- c(cov_list3,"has_AF","TC","LDL_C","HDL_C","TG")



PGS_linear <- c("PGS002724_metaISTROKE_EUR","PGS001793_prscsSTROKE_EUR")
PGS_cat <- paste0(PGS_linear,".cat")
PGSlist <- c(PGS_linear, PGS_cat)


################################################################################################################################################
####################################                                                        ####################################################
####################################              1. primary analysis---rec_CHD             ####################################################
####################################                                                        ####################################################
################################################################################################################################################

#===================================================================================================
# anystroke: 这部份对应Figure 1, Sup tab 4-5, Sup fig 4， Sup tab 14.
if (type == 1){

    df_any3 <- fread("1.UKB/ukb_rec_stroke_any3.csv",h=T)
    message("any3 is calculating")
    results <- NULL
    for (m in c(1,2,3,4)){

        cov <- get(paste0("cov_list",m))
        mstate_any3 <- foreach(i=1:length(PGSlist),.combine="rbind") %dopar% {

            library(mstate)
            library(dplyr)
            library(data.table)
            mstate_v5(PGSlist[i],cov,df_any3[no_relatedness==1],F)

        }
        mstate_any3$model <- m
        results <- rbind(results,mstate_any3)
    }
    fwrite(results,paste0(path_mstate_st,"ukb_mstate_any3_norel.csv"),sep=",")

}

# for (i in 1:7){

#     message(paste0("transition ",i," is running"))
#     results <- mstate_v5("PGS002724_metaISTROKE_EUR",cov_list4,df_any3[no_relatedness==1&trans==i],F)
#     print(results)

# }

#===================================================================================================
# stroke_subtype: 对应Fig 4, Sup Fig 5, Sup Tab 7,8,14
if (type==2){

    df_subtype3 <- fread("1.UKB/ukb_rec_stroke_sub3_1.csv",h=T)
    message("subtype3 is calculating")
    results <- NULL
    for (m in c(1,2,3,4)){

        cov <- get(paste0("cov_list",m))
        mstate_subtype3 <- foreach(i=1:length(PGS_linear),.combine="rbind") %dopar% {

            library(mstate)
            library(dplyr)
            library(data.table)
            mstate_v6_2(PGS_linear[i],cov,df_subtype3[no_relatedness==1],F)

        }
        mstate_subtype3$model <- m
        results <- rbind(results,mstate_subtype3)
    }
    fwrite(results,paste0(path_mstate_st,"ukb_mstate_subtype3_norel_linear.csv"),sep=",")

    for (m in c(1,2)){

        cov <- get(paste0("cov_list",m))
        mstate_subtype3 <- foreach(i=1:length(PGS_cat),.combine="rbind") %dopar% {

            library(mstate)
            library(dplyr)
            library(data.table)
            mstate_v6_2(PGS_cat[i],cov,df_subtype3[no_relatedness==1],F)

        }
        mstate_subtype3$model <- m
        results <- rbind(results,mstate_subtype3)
    }
    fwrite(results,paste0(path_mstate_st,"ukb_mstate_subtype3_norel_cat.csv"),sep=",")
    
}

# 因为方便输出结果，所以我写成上面的代码，但上面这些代码跑的时间很长，如果只是核对结果的话，可以考虑一个转移一个转移的跑，比如下面的代码, 推荐后者。特别是对于UKB来说，太慢了
# for (i in c(1:4,9,12,15,18,10,13,16,19,11,14,17,20,21,22)){

#     message(paste0("transition ",i," is running"))
#     results <- mstate_v5("PGS002724_metaISTROKE_EUR",cov_list1,df_subtype3[no_relatedness==1&trans==i],F)
#     print(results)

# }

#===================================================================================================
# Transition pattern C:对应Tab 2, Sup Tab 9-10
if (type==3){

    df_sub_type <- fread("1.UKB/ukb_sub3_type.csv",h=T)
    message("subtype is calculating")
    results <- NULL
    for (m in c(1,2,3)){

        cov <- get(paste0("cov_list",m))
        mstate_subtype3 <- foreach(i=1:length(PGSlist),.combine="rbind") %dopar% {

            library(mstate)
            library(dplyr)
            library(data.table)
            mstate_v5(PGSlist[i],cov,df_sub_type[no_relatedness==1&(trans==5|trans==6|trans==9|trans==10)],F)

        }
        mstate_subtype3$model <- m
        results <- rbind(results,mstate_subtype3)
    }
    fwrite(results,paste0(path_mstate_st,"ukb_mstate_subtype_type_norel.csv"),sep=",")

}
#===================================================================================================
# 剔除28d的any模型
if (type==4){

    df_any_28d <- fread("1.UKB/ukb_rec_stroke_any3_28d.csv",h = T)
    message("subtype is calculating")
    results <- NULL
    for (m in c(1,2,3)){

        cov <- get(paste0("cov_list",m))
        mstate_subtype3 <- foreach(i=1:length(PGSlist),.combine="rbind") %dopar% {

            library(mstate)
            library(dplyr)
            library(data.table)
            mstate_v5(PGSlist[i],cov,df_any_28d[no_relatedness==1],F)

        }
        mstate_subtype3$model <- m
        results <- rbind(results,mstate_subtype3)
    }
    fwrite(results,paste0(path_mstate_st,"ukb_mstate_any_28d_norel.csv"),sep=",")

}

for (i in 1:7){

    message(paste0("transition ",i," is running"))
    results <- mstate_v5("PGS002724_metaISTROKE_EUR",cov_list1,df_any_28d[no_relatedness==1&trans==i],F)
    print(results)

}
#===================================================================================================
# 剔除28d的subtype模型
if (type==5){

    df_sub_28d <- fread("1.UKB/ukb_rec_stroke_sub3_28d.csv",h=T)
    message("subtype is calculating")
    results <- NULL
    for (m in c(1,2,3)){

        cov <- get(paste0("cov_list",m))
        mstate_subtype3 <- foreach(i=1:length(PGS_linear),.combine="rbind") %dopar% {

            library(mstate)
            library(dplyr)
            library(data.table)
            mstate_v6_2(PGS_linear[i],cov,df_sub_28d[no_relatedness==1],F)

        }
        mstate_subtype3$model <- m
        results <- rbind(results,mstate_subtype3)
    }
    fwrite(results,paste0(path_mstate_st,"ukb_mstate_subtype_28d_norel.csv"),sep=",")

}

# for (i in c(1:4,9,12,15,18,10,13,16,19,11,14,17,20,21,22)){

#     message(paste0("transition ",i," is running"))
#     results <- mstate_v5("PGS002724_metaISTROKE_EUR",cov_list1,df_sub_28d[no_relatedness==1&trans==i],F)
#     print(results)

# }


q()
n