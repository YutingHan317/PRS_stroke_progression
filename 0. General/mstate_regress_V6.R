mstate_v6_1 <- function(ex,cov,data,timefix){

    ntrans <- length(unique(data$trans))
    trans_num <- sort(unique(data$trans))
    model_cov <- NULL
    cov <- cov[cov!=ex]
    for (i in cov) {

        if (nrow(unique(data[,..i])) >10){

            model_cov <- paste(model_cov,"+",paste(i,".",trans_num,sep = "", collapse = " + "))

        }else{

            model_cov <- paste(model_cov,"+",paste("as.factor(",paste0(i,".",trans_num),")",sep = "", collapse = " + "))

        }
    }
    if (grepl(pattern = "cat",x = ex)){
        ex_cat1 <- paste0(ex,"1")    
        ex_cat2 <- paste0(ex,"2")    
        gene <- paste0(paste(ex_cat1,".",trans_num,sep = "", collapse = " + "),"+",paste(ex_cat2,".",trans_num,sep = "", collapse = " + "))
        n=ntrans*2
    }else{
        gene <- paste(ex,".",trans_num,sep = "", collapse = " + ")
        n=ntrans
    }
    single_model1 <- coxph(as.formula(paste0(basic_formula,model_cov,"+",gene)), data = data, method = "breslow",control = coxph.control(timefix = timefix))
    single <- data.frame(round(summary(single_model1)$conf.int,2))
    single[,5] <- paste0(sprintf(single[,1],fmt="%.2f")," (",sprintf(single[,3],fmt="%.2f"),"-",sprintf(single[,4],fmt="%.2f"),")")
    single[,6] <- summary(single_model1)$coefficients[,5]
    single[,7] <- summary(single_model1)$n
    colnames(single) <- c("HR","rev_HR","Lower_CI","Upper_CI","HR_95CI","P","N")
    single$PRS <- rownames(single)
    single$database <- deparse(substitute(data))
    result <- single %>% slice_tail(n=n) %>% select(!rev_HR)
    return(result)

}


mstate_v6_2 <- function(ex,cov,data,timefix){

    ntrans <- length(unique(data$trans))
    trans_num <- sort(unique(data$trans))
    model_cov <- NULL
    cov <- cov[cov!=ex]
    for (i in cov) {

        if (nrow(unique(data[,..i])) >10){

            model_cov <- paste(model_cov,"+",paste(i,".",trans_num,sep = "", collapse = " + "))

        }else{

            model_cov <- paste(model_cov,"+",paste("as.factor(",paste0(i,".",trans_num),")",sep = "", collapse = " + "))

        }
    }
    if (grepl(pattern = "cat",x = ex)){
        ex_cat1 <- paste0(ex,"1")    
        ex_cat2 <- paste0(ex,"2")    
        gene <- paste0(paste(ex_cat1,".",trans_num,sep = "", collapse = " + "),"+",paste(ex_cat2,".",trans_num,sep = "", collapse = " + "))
        n=ntrans*2
    }else{
        gene <- paste(ex,".",trans_num,sep = "", collapse = " + ")
        n=ntrans
    }
    single_model1 <- coxph(as.formula(paste0(basic_formula,model_cov,"+",gene)), data = data, method = "breslow",control = coxph.control(timefix = timefix))
    single <- data.frame(round(summary(single_model1)$conf.int,2))
    single[,5] <- paste0(sprintf(single[,1],fmt="%.2f")," (",sprintf(single[,3],fmt="%.2f"),"-",sprintf(single[,4],fmt="%.2f"),")")
    single[,6] <- summary(single_model1)$coefficients[,5]
    single[,7] <- summary(single_model1)$n
    colnames(single) <- c("HR","rev_HR","Lower_CI","Upper_CI","HR_95CI","P","N")
    single$PRS <- rownames(single)
    single$database <- deparse(substitute(data))
    result <- single %>% slice_tail(n=n) %>% select(!rev_HR)
    if (grepl(pattern = "cat",x = ex)){

        result <- result[c(1:8, 9,12,15,18,10,13,16,19,11,14,17,20,21,22,23:30,31,34,37,40,32,35,38,41,33,36,39,42,43,44),]

    }else{

        result <- result[c(1:8,9,12,15,18,10,13,16,19,11,14,17,20,21,22),]

    }
    return(result)

}