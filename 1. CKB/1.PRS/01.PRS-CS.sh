###########################################################################################################
########################           Download from GWAS catelog              ################################
###########################################################################################################
# for no in {90104534..90104563}; do 
	
# 	URL="http://ftp.ebi.ac.uk/pub/databases/gwas/summary_statistics/GCST90104001-GCST90105000/GCST${no}/GCST${no}_buildGRCh37.tsv.gz"
# 	OUTDIR="/public/home/hanyuting/0.SSF/STROKE/Original_files/GWAS_Nature_2022_STROKE"
# 	# mkdir $OUTDIR
# 	cd $OUTDIR
# 	wget $URL

# done 
# gunzip *

###########################################################################################################
########################                Harmonizing SSF                    ################################
###########################################################################################################
R
options(width=200)
library(data.table)
library(dplyr)
library(magrittr)
setwd("/public/home/hanyuting/0.SSF/STROKE/Original_files/GWAS_Nature_2022_STROKE")

df <- fread(paste0("GCST90104535_buildGRCh37.tsv"))
df %<>% select(CHR=1,BP=2,EA=10,NEA=11,BETA=4,P=6)
fwrite(df,file=paste0("GCST",i,".csv"),sep=" ", quote=F, na="NA")
message(paste0("GCST",i," has been harmonized"))

q()

###########################################################################################################
########################                        QC SSF                     ################################
###########################################################################################################
vi /public/home/hanyuting/0.Code/PGSFileStandardQC_CS_V1.sh
#--------------------------------------------------------------------------------------------
Rscript /public/home/hanyuting/0.Code/PGSFileStandardQC_CS_V1.R \
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

###########################################################################################################
# # # CKB 数据处理
PGS=90104535
PGSFILE=/public/home/hanyuting/0.SSF/STROKE/Original_files/GWAS_Nature_2022_STROKE/GCST${PGS}.csv
TARGETFILE=/public/home/hanyuting/01-GWAS_data/Info.table/ckb_info_table_nodup.txt
delete_indel=FALSE
INFO=0.8
FREQ=0.01
ck_hwe=FALSE
HWP=1e-6
OUTDIR=/public/home/hanyuting/0.SSF/STROKE/GWAS_Nature_2022_STROKE/CKB/GCST${PGS}

qsub -V /public/home/hanyuting/0.Code/PGSFileStandardQC_CS_V1.sh \
     -N PGSFileQC_CKB_$PGS \
     -l nodes=1:ppn=20 \
     -l walltime=10:00:00 \
     -m ae \
     -M epi_hanyt@163.com \
     -o /public/home/hanyuting/pbs_log \
     -e /public/home/hanyuting/pbs_log \
     -v PGSFILE=$PGSFILE,TARGETFILE=${TARGETFILE},delete_indel=${delete_indel},INFO=${INFO},FREQ=${FREQ},ck_hwe=${ck_hwe},HWP=${HWP},OUTDIR=${OUTDIR}    


###########################################################################################################
########################                      snpid to rsid                    ############################
###########################################################################################################

###########################################################################################################
# CKB 
options(width=200)
library(data.table)
library(dplyr)
library(magrittr)

bim <- fread("/public/home/hanyuting/01-GWAS_data/All.nodup.bim",h=F)
info <- fread("/public/home/hanyuting/01-GWAS_data/Info.table/ckb_info_table_nodup.txt",h=T)

info1 <- merge(bim,info[,c("SNP","rsid")],by.x="V2",by.y="SNP")
info1 %>% select(V1,rsid,V3,V4,V5,V6) %>% 
        filter(!is.na(rsid)) %>% 
        fwrite(.,"/public/home/hanyuting/01-GWAS_data/All.nodup.rsid.bim",col.names=F,sep="\t")


###########################################################################################################
########################                PRS-CS--Coef shrinkage             ################################
###########################################################################################################


vi /public/home/hanyuting/0.Code/PRS-CS/Gen_coef_PRSCS_chr.sh
#==================================================================================================================================
cd /public/home/hanyuting/PRS-CS

export MKL_NUM_THREADS=$N_THREADS
export NUMEXPR_NUM_THREADS=$N_THREADS
export OMP_NUM_THREADS=$N_THREADS

python PRScs.py --ref_dir=${ref_dir} \
                --bim_prefix=${bim_prefix} \
                --sst_file=${gwas_file} \
                --n_gwas=${n_gwas} \
                --chrom=$chr \
                --seed=6317 \
                --out_dir=${out_file}
#==================================================================================================================================

# Execute
PGS=90104535
n_gwas=1590566
ETHNIC=eas
DATASOURCE=CKB
N_THREADS=9
ref_dir=/public/home/hanyuting/1000G/ldblk_1kg_${ETHNIC}
bim_prefix=/public/home/hanyuting/01-GWAS_data/All.nodup.rsid
gwas_file=/public/home/hanyuting/0.SSF/STROKE/Original_files/GWAS_Nature_2022_STROKE/GCST${PGS}.csv.${DATASOURCE}.CS.QC
out_file=/public/home/hanyuting/0.SSF/STROKE/GWAS_Nature_2022_STROKE/${DATASOURCE}/GCST${PGS}_${ETHNIC}
for chr in {1..22}; do 
    qsub -V /public/home/hanyuting/0.Code/PRS-CS/Gen_coef_PRSCS_chr.sh \
         -N PRSCS_CKB_${ETHNIC}_${PGS}_${chr} \
         -l nodes=1:ppn=10\
         -l walltime=10:00:00 \
         -m ae \
         -M epi_hanyt@163.com \
         -o /public/home/hanyuting/pbs_log \
         -e /public/home/hanyuting/pbs_log \
         -v N_THREADS=${N_THREADS},ref_dir=${ref_dir},bim_prefix=${bim_prefix},gwas_file=${gwas_file},n_gwas=${n_gwas},chr=${chr},out_file=${out_file}  
done

###########################################################################################################
# CKB
R
rm(list=ls())
options(width=200)
library(data.table)
library(dplyr)
library(magrittr)
setwd("/public/home/hanyuting/0.SSF/STROKE/GWAS_Nature_2022_STROKE/CKB")
info <- fread("/public/home/hanyuting/01-GWAS_data/Info.table/ckb_info_table_nodup.txt",h=T)

files <- list.files(".",full.names = FALSE, recursive = FALSE, pattern = ".txt")

for (f in files){

    df <- fread(f,h=F)
    df <- merge(df,info[,c("SNP","rsid")],by.x="V2",by.y="rsid",all.x=TRUE)
    message(paste0(f," has ",sum(is.na(df$SNP))," missings"))
    df %>% select(SNP,V1,V3,V4,V5,V6) %>% fwrite(.,gsub("txt","ssf",f),col.names=F,sep=" ")
    rm(df)

}


###########################################################################################################
########################                 PRS-CS--cal PRS by Chr            ################################
###########################################################################################################

###########################################################################################################
vi /public/home/hanyuting/0.Code/wf_cal_PRS_chr.sh
#--------------------------------------------------------------------------------------------
plink2 \
    --bfile $bfile \
    --score $PGSFILE 1 4 6 header cols=+scoresums \
    --out $OUTFILE \
    --threads 20

#--------------------------------------------------------------------------------------------

# Execute
PGS=90104535
DATASOURCE=CKB
PGS_DIR=/public/home/hanyuting/0.SSF/STROKE/GWAS_Nature_2022_STROKE/${DATASOURCE}
ETHNIC=eas
for chr in {1..22}; do
    bfile=/public/home/hanyuting/01-GWAS_data/chr${chr}.nodup
    PGSFILE=${PGS_DIR}/GCST${PGS}_${ETHNIC}_pst_eff_a1_b0.5_phiauto_chr${chr}.ssf
    OUTDIR=${PGS_DIR}/GCST${PGS}
    OUTFILE=${OUTDIR}/${DATASOURCE}_CS_chr${chr}
    qsub -V /public/home/hanyuting/0.Code/wf_cal_PRS_chr.sh \
         -N cal_PRS_${DATASOURCE}_${PGS}_chr${chr} \
         -l nodes=1:ppn=10 \
         -l walltime=10:00:00 \
         -m ae \
         -M epi_hanyt@163.com \
         -o /public/home/hanyuting/pbs_log \
         -e /public/home/hanyuting/pbs_log \
         -v bfile=$bfile,PGSFILE=$PGSFILE,OUTFILE=$OUTFILE
done

###########################################################################################################
########################                 Sum chr-specific score            ################################
###########################################################################################################

#--------------------------------------------------------------------------------------------
# # # # CKB
PGS=90104535
DATASOURCE=CKB
count=1
for chr in $(seq 1 22)
do
    file=/public/home/hanyuting/0.SSF/STROKE/GWAS_Nature_2022_STROKE/${DATASOURCE}/GCST${PGS}/${DATASOURCE}_CS_chr${chr}.sscore
    scoreFile=/public/home/hanyuting/2.PRS/${DATASOURCE}_${PGS}
    if [ -s "$file" ] ; then 
        if [ ${count} -eq 1 ] ;then
        # echo "eid chr${chr}" > ${scoreFile}.score
        printf "FID\tIID\tchr${chr}\n" > ${scoreFile}.score
        awk 'NR>1{print $1 "\t" $2 "\t" $6}' ${file} >> ${scoreFile}.score
        else
        paste ${scoreFile}.score <(cut -f6 ${file} | awk -v chr=chr${chr} 'NR==1 {$0=chr} 1') > ${scoreFile}.score.temp&&mv ${scoreFile}.score.temp ${scoreFile}.score
        fi
        count=`expr $count + 1`
     fi
done

# 累加得到总PRS
R
library(data.table)
options(width=150)
setwd("/public/home/hanyuting/2.PRS")
CKB_90104535 <- fread("CKB_90104535.score")

CKB_90104535$score <- rowSums(CKB_90104535[,3:24])

# CKB的34列仅用来填充,方便后续写循环
fwrite(CKB_90104535[,c(1,2,3,4,25)],"CKB_90104535.sscore")

q()