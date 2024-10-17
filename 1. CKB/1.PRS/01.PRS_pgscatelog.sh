
#############################################################################################
########################        1.Download and harmonize       ##############################
#############################################################################################
SSF_harmonizing_PATH="/public/home/hanyuting/0.SSF/0.Code4harmonize"
SSF_PATH="/public/home/hanyuting/0.SSF"

#====================================================================================
# # # # Ischemic stroke
# PGS ID: PGS002725
# PRS for EAS (Including CKB), By Mishra A et al from GIGASTROKE, Nature 2022
# URL="https://ftp.ebi.ac.uk/pub/databases/spot/pgs/scores/PGS002725/ScoringFiles/PGS002725.txt.gz"
# OUTDIR="/public/home/hanyuting/0.SSF/STROKE/Original_files/PGS002725"
# mkdir $OUTDIR
# cd $OUTDIR
# wget $URL 
# gunzip *

# Harmonization
setwd("/public/home/hanyuting/0.SSF/STROKE")
#-----------------------
library("data.table")
library("stringr")
library("dplyr")
options(width=150)
#-----------------------
rm(list=ls())
PGS_FILE = "./Original_files/PGS002725/PGS002725.txt"
OUT_FILE="PGS002725_metaISTROKE_EAS.ssf"

# # Loading the file
base <- fread(PGS_FILE, h=T)

# Rename
colnames(base) <- c("CHR", "BP", "EA", "NEA", "BETA")

## Add other columns
base$rsid <- NA

## Export
fwrite(base, file=OUT_FILE, sep=" ", quote=F, na="NA")


#====================================================================================
# # # # STROKE
# PGS ID: PGS002259
# PRS for EAS, By Lu XF et al from China-PAR, Neurology 2022
# URL="https://ftp.ebi.ac.uk/pub/databases/spot/pgs/scores/PGS002259/ScoringFiles/PGS002259.txt.gz"
# OUTDIR="/public/home/hanyuting/0.SSF/STROKE/Original_files/PGS002259"
# mkdir $OUTDIR
# cd $OUTDIR
# wget $URL 
# gunzip *

setwd("/public/home/hanyuting/0.SSF/STROKE")
#-----------------------
library("data.table")
library("stringr")
library("dplyr")
options(width=150)
#-----------------------
rm(list=ls())
PGS_FILE = "./Original_files/PGS002259/PGS002259.txt"
OUT_FILE="PGS002259_metaSTROKE_Lu_EAS.ssf"

## Loading the file
base <- fread(PGS_FILE, h=T)

# Rename
colnames(base) <- c("rsID","CHR", "BP", "EA","NEA","BETA")

## Export
fwrite(base, file=OUT_FILE, sep=" ", quote=F, na="NA")


#############################################################################################
########################             2.Quality control         ##############################
#############################################################################################

#############################################################################################
########################         standard excute file        ################################
#############################################################################################

vi /public/home/hanyuting/0.Code/PGSFileStandardQC_YH-V1.sh
#--------------------------------------------------------------------------------------------
Rscript /public/home/hanyuting/0.Code/PGSFileStandardQC_YH-V1.R \
    --pgsf ${PGSFILE} \
    --target ${TARGETFILE} \
    --delete_ambiguous TRUE \
    --delete_indel $delete_indel \
    --ck_info TRUE \
    --INFO $INFO \
    --ck_freq TRUE \
    --FREQ $FREQ \
    --ck_hwe $ck_hwe \
    --HWP $HWP \
    --outdir ${OUTDIR} > ${PGSFILE}.log 2>&1 0>&1
#--------------------------------------------------------------------------------------------

### Execute
PGSlist="PGS002259_metaSTROKE_Lu_EAS PGS002725_metaISTROKE_EAS"

for PGS in $PGSlist; do

    TRAIT=STROKE
    PGSFILE=/public/home/hanyuting/0.SSF/${TRAIT}/${PGS}.ssf
    TARGETFILE=/public/home/hanyuting/01-GWAS_data/Info.table/ckb_info_table_nodup.txt
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


#############################################################################################
########################             3.Calculate PRS           ##############################
#############################################################################################

#############################################################################################
########################         standard excute file        ################################
#############################################################################################
vi /public/home/hanyuting/0.Code/wf_cal_PRS.sh
#--------------------------------------------------------------------------------------------
plink2 \
    --bfile $bfile \
    --score $PGSFILE 1 4 6 header \
    --out $OUTFILE \
    --threads 20

#--------------------------------------------------------------------------------------------

# 下面我是把22条染色体合并在一起算的，如果不想合并的话，可以参考UKB，那部分的代码
# 合并的22条染色体的代码见“Gen_nodup_target.sh”
bfile=/public/home/hanyuting/01-GWAS_data/All.nodup
PGSlist="PGS002259_metaSTROKE_Lu_EAS PGS002725_metaISTROKE_EAS"

for PGS in $PGSlist; do

    TRAIT=STROKE
    PGSFILE=/public/home/hanyuting/0.SSF/${TRAIT}/${PGS}.ssf.QC
    OUTFILE=/public/home/hanyuting/2.PRS/${PGS}

    qsub -V /public/home/hanyuting/0.Code/wf_cal_PRS.sh \
         -N cal_PRS_${PGS} \
         -l nodes=1:ppn=20 \
         -l walltime=10:00:00 \
         -m ae \
         -M epi_hanyt@163.com \
         -o /public/home/hanyuting/pbs_log \
         -e /public/home/hanyuting/pbs_log \
         -v bfile=$bfile,PGSFILE=$PGSFILE,OUTFILE=$OUTFILE

done

# 这一步会生成后缀为sscore的文件，就是我们所需要的PRS
