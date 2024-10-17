#############################################################################################
########################            1.Download and harmonizing              #################
#############################################################################################
SSF_PATH="/public/home/hanyuting/0.SSF"

#====================================================================================
# # # # Ischemic stroke
# PGS ID: PGS002724
# PRS for EUR (Including UKB), By Mishra A et al from GIGASTROKE, Nature 2022
# URL="https://ftp.ebi.ac.uk/pub/databases/spot/pgs/scores/PGS002724/ScoringFiles/PGS002724.txt.gz"
# OUTDIR="/public/home/hanyuting/0.SSF/STROKE/Original_files/PGS002724"
# mkdir $OUTDIR
# cd $OUTDIR
# wget $URL 
# gunzip *

# # Harmonizing it
R
setwd("/public/home/hanyuting/0.SSF/STROKE")
#-----------------------
library("data.table")
library("stringr")
library("dplyr")
options(width=150)
#-----------------------
rm(list=ls())
PGS_FILE="./Original_files/PGS002724/PGS002724.txt"
OUT_FILE="PGS002724_metaISTROKE_EUR.ssf"

# # Loading the file: 1213574 variants
base <- fread(PGS_FILE, h=T)

# 重命名
colnames(base) <- c("CHR", "BP", "EA", "NEA", "BETA")

## 增加一些其它列
base$rsid <- NA

## 导出为整理后的数据库
fwrite(base, file=OUT_FILE, sep=" ", quote=F, na="NA")

#====================================================================================
# # # # Stroke
# PGS ID: PGS001793
# PRS for EUR (Excluding UKB), By Wang Y et al. Cell Genom (2023)
URL="https://ftp.ebi.ac.uk/pub/databases/spot/pgs/scores/PGS001793/ScoringFiles/PGS001793.txt.gz"
OUTDIR="/public/home/hanyuting/0.SSF/STROKE/Original_files/PGS001793"
mkdir $OUTDIR
cd $OUTDIR
wget $URL 
gunzip *

# # Harmonizing it
R
setwd("/public/home/hanyuting/0.SSF/STROKE")
#-----------------------
library("data.table")
library("stringr")
library("dplyr")
options(width=150)
#-----------------------
rm(list=ls())
PGS_FILE = "./Original_files/PGS001793/PGS001793.txt"
OUT_FILE="PGS001793_prscsSTROKE_EUR.ssf"

# # Loading the file: 910099 variants
base <- fread(PGS_FILE, h=T)

# 重命名
colnames(base) <- c("rsid","CHR", "BP", "EA", "NEA", "BETA")

## 导出为整理后的数据库
fwrite(base, file=OUT_FILE, sep=" ", quote=F, na="NA")


#############################################################################################
########################          2. Quality control         ################################
#############################################################################################

# 下面这个执行代码是一样的
# vi /public/home/hanyuting/0.Code/PGSFileStandardQC_YH-V1.sh
# #--------------------------------------------------------------------------------------------
# Rscript /public/home/hanyuting/0.Code/PGSFileStandardQC_YH-V1.R \
#     --pgsf ${PGSFILE} \
#     --target ${TARGETFILE} \
#     --delete_ambiguous TRUE \
#     --delete_indel $delete_indel \
#     --ck_info TRUE \
#     --INFO $INFO \
#     --ck_freq TRUE \
#     --FREQ $FREQ \
#     --ck_hwe $ck_hwe \
#     --HWP $HWP \
#     --outdir ${OUTDIR} > ${PGSFILE}.log 2>&1 0>&1
# #--------------------------------------------------------------------------------------------

PGSlist="PGS001798_prscsSTROKE_EUR PGS002724_metaISTROKE_EUR"

for PGS in $PGSlist; do

    TRAIT=STROKE
    PGSFILE=/public/home/hanyuting/0.SSF/${TRAIT}/${PGS}.ssf
    # UKB的info.table太大了，由于现在缺少UKB遗传，就想着不把这个粘贴过来了
    TARGETFILE=/public/home/hanyuting/UKB/GWASdata/Plink_binary/Info.table/UKB_info_table.txt
    delete_indel=FALSE
    INFO=0.8
    FREQ=0.01
    ck_hwe=FALSE
    HWP=1e-6
    OUTDIR=/public/home/hanyuting/0.SSF/${TRAIT}/${PGS}

    qsub -V /public/home/hanyuting/0.Code/PGSFileStandardQC_YH_V1.sh \
         -N PGSFileQC_$PGS \
         -l nodes=1:ppn=20 \
         -l walltime=10:00:00 \
         -m ae \
         -M epi_hanyt@163.com \
         -o /public/home/hanyuting/pbs_log \
         -e /public/home/hanyuting/pbs_log \
         -v PGSFILE=$PGSFILE,TARGETFILE=${TARGETFILE},delete_indel=${delete_indel},INFO=${INFO},FREQ=${FREQ},ck_hwe=${ck_hwe},HWP=${HWP},OUTDIR=${OUTDIR}    
done
