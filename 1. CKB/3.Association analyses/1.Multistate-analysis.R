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
if (type == 1|type == 4){

    cl_n <- 10
}else{

    cl_n <- 3
}

cl <- makeCluster(cl_n)
registerDoParallel(cl)

# stopCluster(cl)

# 加载写好的function,v6只是比v5增加了转移途径重新排序的少量代码，两个代码结果是一样。如果全部转移途径一起运行，那就用v6，如果一个转移一个转移的运行，那就用v5
source("~/0.Code/mstate_regress_V5.R")
source("~/0.Code/mstate_regress_V6.R")

# 设置存在结果的文件夹
path_mstate_st <- paste0("3.Result/CKB/rec_stroke_mstate_PRS/",Sys.Date(),"/")
dir.create(path_mstate_st)

#===================================================================================================
# Set essential parameters
basic_formula <- paste0("Surv(Tstart, Tstop, status)","~","strata(trans)+strata(age_strata)+strata(region_code)")

cov_list1 <- c("is_female",paste0("PC",1:5),"gwas_array_type")
cov_list2 <- c(cov_list1,"smoking_4groups","alcohol_5groups","diet_5groups","obesity_5groups")
cov_list3 <- c(cov_list2,"sbp_mean2","has_diabetes")

PGS_linear <- c("PGS002725_metaISTROKE_EAS","PGS002259_metaSTROKE_Lu_EAS","CKB_90104535")
PGS_cat <- paste0(PGS_linear,".cat")
PGSlist <- c(PGS_linear,PGS_cat)

################################################################################################################################################
####################################                                                        ####################################################
####################################              1. primary analysis---rec_CHD             ####################################################
####################################                                                        ####################################################
################################################################################################################################################

#===================================================================================================
# anystroke: 这部份对应Figure 1, Sup tab 4-5, Sup fig 4， Sup tab 11.
if (type==1){

    df_any3 <- fread("1.Data/rec_stroke_any3.csv",h=T)
    results <- NULL
    for (m in c(1,2,3)){

        cov <- get(paste0("cov_list",m))
        mstate_any3 <- foreach(i=1:length(PGSlist),.combine="rbind") %dopar% {

            library(mstate)
            library(dplyr)
            library(data.table)
            mstate_v6_1(PGSlist[i],cov,df_any3[no_relatedness==1],F)

        }
        mstate_any3$model <- m
        results <- rbind(results,mstate_any3)
    }
    fwrite(results,paste0(path_mstate_st,"CKB_G_any3_norel.csv"),sep=",")

}
# 不跑并行的话，也可以直接运行一个模型一个模型的跑，比如下面这些
# mstate_v6_1("PGS002725_metaISTROKE_EAS",cov_list1,df_any3[no_relatedness==1],F)
# mstate_v6_1("PGS002725_metaISTROKE_EAS",cov_list2,df_any3[no_relatedness==1],F)
# mstate_v6_1("PGS002725_metaISTROKE_EAS",cov_list3,df_any3[no_relatedness==1],F)


#===================================================================================================
# stroke_subtype: 对应Fig 4, Sup Fig 5, Sup Tab 7,8,11
if (type==2){

    df_subtype3 <- fread("1.Data/rec_stroke_sub3.csv",h=T)
    # 由于某些trans不收敛，分别计算
    results <- NULL
    for (m in c(1,2,3)){

        cov <- get(paste0("cov_list",m))
        mstate_subtype3 <- foreach(i=1:length(PGSlist),.combine="rbind") %dopar% {

            library(mstate)
            library(dplyr)
            library(data.table)
            mstate_v6_2(PGSlist[i],cov,df_subtype3[no_relatedness==1],F)

        }
        mstate_subtype3$model <- m
        results <- rbind(results,mstate_subtype3)
    }
    fwrite(results,paste0(path_mstate_st,"CKB_G_subtype3_norel.csv"),sep=",")

}
# 因为方便输出结果，所以我写成上面的代码，但上面这些代码跑的时间很长，如果只是核对结果的话，可以考虑一个转移一个转移的跑，比如下面的代码, 推荐后者。特别是对于UKB来说，太慢了
# for (i in c(1:4,9,12,15,18,10,13,16,19,11,14,17,20,21,22)){

#     message(paste0("transition ",i," is running"))
#     results <- mstate_v5("PGS002725_metaISTROKE_EAS",cov_list1,df_subtype3[no_relatedness==1&trans==i],F)
#     print(results)

# }

#===================================================================================================
# Transition pattern C: 对应Tab 2, Sup Tab 9-10
if (type==3){

    df_any3 <- fread("1.Data/rec_stroke_sub3_type.csv",h=T)
    results <- NULL
    for (m in c(1,2,3)){

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
    fwrite(results,paste0(path_mstate_st,"CKB_G_sub3_type_norel.csv"),sep=",")

}

#===================================================================================================
# 剔除了28天的any模型: Sup tab 12
# 这里是图省事儿，跑了很多没画图做表的结果，还是建议复核的话，直接上面备注的循环，那个很快，改一下协变量模型就好
if (type==4){

    df_any3_28d <- fread("1.Data/rec_stroke_any3_28d.csv",h=T)
    results <- NULL
    for (m in c(1,2,3)){

        cov <- get(paste0("cov_list",m))
        mstate_any3 <- foreach(i=1:length(PGS_linear),.combine="rbind") %dopar% {

            library(mstate)
            library(dplyr)
            library(data.table)
            mstate_v5(PGS_linear[i],cov,df_any3_28d[no_relatedness==1],F)

        }
        mstate_any3$model <- m
        results <- rbind(results,mstate_any3)
    }
    fwrite(results,paste0(path_mstate_st,"CKB_G_any_28d_norel.csv"),sep=",")

}

# for (i in 1:7){

#     message(paste0("transition ",i," is running"))
#     results <- mstate_v5("PGS002725_metaISTROKE_EAS",cov_list1,df_any3_28d[no_relatedness==1&trans==i],F)
#     print(results)

# }

# 剔除了28天的sub模型：Sup tab 12
if (type==5){

    df_subtype3_28d <- fread("1.Data/rec_stroke_sub3_28d.csv",h=T)
    # 由于某些trans不收敛，分别计算
    results <- NULL
    for (m in c(1,2,3)){

        cov <- get(paste0("cov_list",m))
        mstate_subtype3 <- foreach(i=1:length(PGS_linear),.combine="rbind") %dopar% {

            library(mstate)
            library(dplyr)
            library(data.table)
            mstate_v6_2(PGS_linear[i],cov,df_subtype3_28d[no_relatedness==1],F)

        }
        mstate_subtype3$model <- m
        results <- rbind(results,mstate_subtype3)
    }
    fwrite(results,paste0(path_mstate_st,"CKB_G_subtype3_28d_norel.csv"),sep=",")

}

# for (i in c(1:4,9,12,15,18,10,13,16,19,11,14,17,20,21,22)){

#     message(paste0("transition ",i," is running"))
#     results <- mstate_v5("PGS002725_metaISTROKE_EAS",cov_list1,df_subtype3_28d[no_relatedness==1&trans==i],F)
#     print(results)

# }

q()