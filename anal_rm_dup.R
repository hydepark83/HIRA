library(sm)
library(data.table)

YNO.data <- read.table("../data/ykiho.txt", sep=",", header=T)
YNO.dt <- data.table(YNO.data)
setkey(YNO.dt, YNO)
## Load data
#r.data <- read.table("./data/proc_ED_nps_20", header=T,  sep=",") #2009.01.01 ~ 2009.12.31
load("../data/proc_ED_nps_20.rda")

## Time period:
min(r.data$RECU_FR_DT)
max(r.data$RECU_FR_DT)

### There are some duplicated records (dates) for some patients 11/14/2013 ###############
### Should count as a one visit when count the number of visit.
uni.NO <- unique(r.data$NO)
temp.idx <- apply(as.matrix(uni.NO), 1, temp <- function(id){
			temp <- r.data[which(r.data$NO==id),]
			rs <- rownames(temp[which(duplicated(temp$RECU_FR_DT)==F),])
			return(as.numeric(rs))
			})
temp.idx <- unlist(temp.idx)
uni.date.idx <- rep(0, nrow(r.data))
uni.date.idx[temp.idx] <- 1

## ED visitors
ed.idx <- apply(as.matrix(r.data$IN_PAT_CORS_TYPE), 1, temp <- function(yy){
					temp <- strsplit(as.character(yy), "")[[1]]
					if(temp[2] == "1"){return(1)}else{return(0)}
					})

r.dt <- data.table(r.data, ed.idx=ed.idx)
setkey(r.dt, NO, ed.idx)
ed.data <- r.data[which(ed.idx==1),]
ed.dt <- data.table(ed.data)
setkey(ed.dt, NO)

## multiple ED visitors

## Frequent users definition: 
#When defined as 4 or more ED visits per year, frequent users accounted for 4.5% to 8% of all ED patients
ed.count.data <- r.data[which(ed.idx==1 & uni.date.idx==1),]
num.ed.record.FF <- sum(table(ed.count.data$NO)[which(table(ed.count.data$NO)>=4)])
num.ed.record.all <- sum(table(ed.count.data$NO))
#> num.ed.record.FF/num.ed.record.all
#[1] 0.140102 <<<<<<<< CH


FF.NO <- names(table(ed.count.data$NO)[which(table(ed.count.data$NO)>=4)]) 
NO.FF.NO <- setdiff(unique(ed.count.data$NO), FF.NO)
num.ed.patients.FF <- length(FF.NO)
num.ed.patients.all <- length(FF.NO) + length(NO.FF.NO)
# >num.ed.patients.FF/num.ed.patients.all
# >0.0309447 <<<<< CH

### Trend ED visits vs. Disease
temp.DN <- list()
for(edc in c(1:10)){
	print(edc)
	temp.NO <- names(table(ed.count.data$NO)[which(table(ed.count.data$NO)==edc)])
	if(edc==10){
		temp.NO <- names(table(ed.count.data$NO)[which(table(ed.count.data$NO)>=edc & table(ed.count.data$NO)<=26)])
		}
	flush <- apply(as.matrix(temp.NO), 1, temp <- function(nn){
				temp <- as.matrix(ed.dt[J(as.numeric(nn))])
				temp.main.sick <- unique(temp[,"MAIN_SICK"])
				return(temp.main.sick)
				})	
	temp.DN[[edc]] <- unlist(flush)
	}

## frequency >1000
DN.all <- names(table(r.data[,"MAIN_SICK"]))
rs <- c()
for(dn in DN.all){
	temp.prop <- c()
	for(edc in c(1:10)){
		temp.prop <- c(temp.prop, length(which(temp.DN[[edc]]== dn))/length(temp.DN[[edc]]))
		}
	num.ed.visit <- c(1:10)
	temp.rs <- c(dn, anova(lm(num.ed.visit~temp.prop))[[5]][1])
	rs <- rbind(rs, temp.rs)
	}
rs.adjp <- p.adjust(as.numeric(rs[,2]))
rs <- cbind(rs, rs.adjp)
rs <- rs[sort(as.numeric(rs[,2]), decreasing=F, index.return=T)$ix,]

## Disease significantly associated with ED visits
sig.d <- rs[which(as.numeric(rs[,3])<0.05),1]
for(dn in sig.d){
	temp.prop <- c()
        for(edc in c(1:10)){
                temp.prop <- c(temp.prop, length(which(temp.DN[[edc]]== dn))/length(temp.DN[[edc]]))
                }
	num.ed.visit <- c(c(1:9), ">10")
	print(cbind(num.ed.visit, temp.prop))
	}
length(DN.all)
#8425
#rs[which(as.numeric(rs[,3])<0.05),]
#temp.rs "F102" "1.65509315088692e-07" "0.000871241034626875"
#temp.rs "I219" "9.31761339711051e-06" "0.0490385993089926" 

## F102
## Chronic alcoholism
## Alcohol addiction
## Mental and behavioral disorders due to alcohol dependence syndrome

# [1,] "1"          "0.00139041005445125"
# [2,] "2"          "0.00343665322847135"
# [3,] "3"          "0.0081573896353167" 
# [4,] "4"          "0.0108108108108108" 
# [5,] "5"          "0.0168654874537227" 
# [6,] "6"          "0.0231124807395994" 
# [7,] "7"          "0.0335365853658537" 
# [8,] "8"          "0.0328849028400598" 
# [9,] "9"          "0.0349344978165939" 
#[10,] ">10"        "0.0441603718768158" 

# I219 
#Acute myocardial infarction, unspecified
#Myocardial infarction(acute) NOS
      
#num.ed.visit temp.prop            
# [1,] "1"          "0.00653259695974025"
# [2,] "2"          "0.00650981428854669"
# [3,] "3"          "0.00575815738963532"
# [4,] "4"          "0.00627027027027027"
# [5,] "5"          "0.00411353352529823"
# [6,] "6"          "0.00385208012326656"
# [7,] "7"          "0.00304878048780488"
# [8,] "8"          "0.00149476831091181"
# [9,] "9"          "0.00218340611353712"
#[10,] ">10"        "0"  	



# data structure conversion for boost computational speed
print(Sys.time())
FF.info <- apply(as.matrix(FF.NO), 1, temp <- function(nn){
		temp <- as.matrix(ed.dt[J(as.numeric(nn))])
		temp.age <- as.numeric(temp[,"age"][1])
		# there is an issue in age column, age sometimes varies
		temp.gen <- temp[,"gen"][1]
                temp.insurance.pay <- sum(as.numeric(temp[,"DMD_JBRDN_AMT"]))/nrow(temp)
		temp.total.pay <- sum(as.numeric(temp[,"DMD_TRAMT"]))/nrow(temp)
                temp.self.pay <- temp.total.pay - temp.insurance.pay
		# there is an issue in DMD_SBRDN_AMT columns (all zero in some cases)
		temp.dur <- sum(as.numeric(temp[,"RECN"]))/nrow(temp)
		temp.vscn <- sum(as.numeric(temp[,"VSCN"]))/nrow(temp)
		temp.se.class <- temp[,"MACL_CD"][1]
		temp.main.sick <- paste(unique(temp[,"MAIN_SICK"]), collapse="|")
		tt <- temp[,"DIAG_RSLT_TYPE"]
		if(length(which(tt=="4"))>0){
			 temp.death=1
			}else{
			temp.death=0
			}
		tt <- temp[,"SUB_SICK"]
		if(length(which(tt!=""))>0){
                        temp.comorbidity.idx=1
			temp.comorbidity=paste(tt[which(tt!="")], collapse="|")
                        }else{
                        temp.comorbidity.idx=0
			temp.comorbidity=NA
                        }
		if(length(which(temp[,"OPR_YN"]==9))>0){
			temp.opr <- 1
			}else{
			temp.opr <- 0
			}
		kk <- temp[,"YNO"]
		temp.YNO <- apply(as.matrix(kk), 1, temp <- function(yyn){
					temp.yno <- YNO.dt[J(as.numeric(yyn))]
					return(temp.yno[,bed_grade])}
					)
		temp.YNO <- mean(temp.YNO)
		temp.outPD.visit <- nrow(r.dt[J(as.numeric(nn),0),nomatch=0])
		return(c(nn, temp.age, temp.gen, temp.insurance.pay, temp.self.pay, temp.dur, temp.se.class, temp.main.sick, temp.death, temp.comorbidity.idx, temp.comorbidity, temp.outPD.visit, temp.vscn, temp.opr, temp.YNO))
		})
print(Sys.time())


NO.FF.info <- apply(as.matrix(NO.FF.NO), 1, temp <- function(nn){
		temp <- as.matrix(ed.dt[J(as.numeric(nn))])
		if(nrow(ed.dt[J(as.numeric(nn))])>1){
 	                temp.age <- as.numeric(temp[,"age"][1])
	                temp.gen <- temp[,"gen"][1]
	                temp.insurance.pay <- sum(as.numeric(temp[,"DMD_JBRDN_AMT"]))/nrow(temp)
	                temp.total.pay <- sum(as.numeric(temp[,"DMD_TRAMT"]))/nrow(temp)
	                temp.self.pay <- temp.total.pay - temp.insurance.pay
			temp.dur <- sum(as.numeric(temp[,"RECN"]))/nrow(temp)
			temp.vscn <- sum(as.numeric(temp[,"VSCN"]))/nrow(temp)
			temp.se.class <- temp[,"MACL_CD"][1]
			temp.main.sick <- paste(unique(temp[,"MAIN_SICK"]), collapse="|")
	                tt <- temp[,"DIAG_RSLT_TYPE"]
	                if(length(which(tt=="4"))>0){
        	                 temp.death=1
                	        }else{  
                        	temp.death=0
                        	}
			tt <- temp[,"SUB_SICK"]
                	if(length(which(tt!=""))>0){
                         	temp.comorbidity.idx=1
				temp.comorbidity=paste(tt[which(tt!="")], collapse="|")
                        	}else{
                        	temp.comorbidity.idx=0
				temp.comorbidity=NA
                        	}
			if(length(which(temp[,"OPR_YN"]==9))>0){
                        temp.opr <- 1
                        }else{
                        temp.opr <- 0
                        }
			kk <- temp[,"YNO"]
	                temp.YNO <- apply(as.matrix(kk), 1, temp <- function(yyn){
                                        temp.yno <- YNO.dt[J(as.numeric(yyn))]
                                        return(temp.yno[,bed_grade])}
                                        )
        	        temp.YNO <- mean(temp.YNO)
			}else{
			names(temp) <- colnames(ed.data)
			temp.age <- as.numeric(temp["age"])
			temp.gen <- temp["gen"]
			temp.insurance.pay <- as.numeric(temp["DMD_JBRDN_AMT"])
			temp.total.pay <-as.numeric(temp["DMD_TRAMT"])
			temp.self.pay <- temp.total.pay - temp.insurance.pay
			temp.dur <- as.numeric(temp["RECN"])
			temp.vscn <- as.numeric(temp["VSCN"])
			temp.se.class <- temp["MACL_CD"]
			temp.main.sick <- temp["MAIN_SICK"]
				
			if(is.na(temp["DIAG_RSLT_TYPE"])==T){
				temp.death=0
				}else if(temp["DIAG_RSLT_TYPE"]=="4"){
				temp.death=1
                                }else{
                                temp.death=0
                                }
			if(temp["SUB_SICK"]!=""){
				temp.comorbidity.idx=1
				temp.comorbidity=temp["SUB_SICK"]
                                }else{
                                temp.comorbidity.idx=0
				temp.comorbidity=NA
                                }
			if(temp["OPR_YN"]==9){
                        temp.opr <- 1
                        }else{
                        temp.opr <- 0
                        }
                	yyn <-temp["YNO"]
                        temp.yno <- YNO.dt[J(as.numeric(yyn))]
                        temp.YNO <- temp.yno[,bed_grade]
			}
		temp.outPD.visit <- nrow(r.dt[J(as.numeric(nn),0),nomatch=0])
		return(c(nn, temp.age, temp.gen, temp.insurance.pay, temp.self.pay, temp.dur, temp.se.class, temp.main.sick, temp.death, temp.comorbidity.idx, temp.comorbidity, temp.outPD.visit, temp.vscn, temp.opr, temp.YNO))
                })
print(Sys.time())


FF.info <- t(FF.info); FF.info <- as.data.frame(FF.info)
colnames(FF.info) <- c("NO", "age", "gen", "insurance.pay", "patient.pay", "days.in.hospitals", "class", "main.sick", "death", "comorbidity.idx", "comorbidity", "out.patient.department.visits", "treat.days", "OPRYN", "YNO")
FF.info$age <- as.numeric(as.matrix(FF.info$age))
FF.info$insurance.pay <- as.numeric(as.matrix(FF.info$insurance.pay))
FF.info$patient.pay <- as.numeric(as.matrix(FF.info$patient.pay))
FF.info$days.in.hospitals <- as.numeric(as.matrix(FF.info$days.in.hospitals))
FF.info$main.sick <- as.character(as.matrix(FF.info$main.sick))
FF.info$comorbidity <- as.character(as.matrix(FF.info$comorbidity))
FF.info$out.patient.department.visits <- as.numeric(as.matrix(FF.info$out.patient.department.visits))
FF.info$treat.days <- as.numeric(as.matrix(FF.info$treat.days))
FF.info$YNO <- as.numeric(as.matrix(FF.info$YNO))
FF.info$gen <- as.factor(as.matrix(FF.info$gen))
FF.info$class <- as.factor(as.matrix(FF.info$class))
FF.info$death <- as.factor(as.matrix(FF.info$death))
FF.info$OPRYN <- as.factor(as.matrix(FF.info$OPRYN))

NO.FF.info <- t(NO.FF.info); NO.FF.info <- data.frame(NO.FF.info)
colnames(NO.FF.info) <- c("NO", "age", "gen", "insurance.pay", "patient.pay", "days.in.hospitals", "class", "main.sick", "death", "comorbidity.idx", "comorbidity", "out.patient.department.visits", "treat.days", "OPRYN", "YNO")
NO.FF.info$age <- as.numeric(as.matrix(NO.FF.info$age))
NO.FF.info$insurance.pay <- as.numeric(as.matrix(NO.FF.info$insurance.pay))
NO.FF.info$patient.pay <- as.numeric(as.matrix(NO.FF.info$patient.pay))
NO.FF.info$days.in.hospitals <- as.numeric(as.matrix(NO.FF.info$days.in.hospitals))
NO.FF.info$main.sick <- as.character(as.matrix(NO.FF.info$main.sick))
NO.FF.info$comorbidity <- as.character(as.matrix(NO.FF.info$comorbidity))
NO.FF.info$out.patient.department.visits <- as.numeric(as.matrix(NO.FF.info$out.patient.department.visits))
NO.FF.info$treat.days <- as.numeric(as.matrix(NO.FF.info$treat.days))
NO.FF.info$YNO <- as.numeric(as.matrix(NO.FF.info$YNO))
NO.FF.info$gen <- as.factor(as.matrix(NO.FF.info$gen))
NO.FF.info$class <- as.factor(as.matrix(NO.FF.info$class))
NO.FF.info$death <- as.factor(as.matrix(NO.FF.info$death))
NO.FF.info$OPRYN <- as.factor(as.matrix(NO.FF.info$OPRYN))

t.test(FF.info[,"out.patient.department.visits"]);t.test(NO.FF.info[,"out.patient.department.visits"])
t.test(FF.info[,"out.patient.department.visits"], NO.FF.info[,"out.patient.department.visits"])
t.test(FF.info[,"age"]);t.test(NO.FF.info[,"age"])
t.test(FF.info[,"age"], NO.FF.info[,"age"])
gender.table <- rbind(table(FF.info[,"gen"]), table(NO.FF.info[,"gen"])); gender.table
chisq.test(gender.table)

##3 NEW!!!
t.test(FF.info[,"treat.days"], NO.FF.info[,"treat.days"])
t.test(FF.info[,"YNO"], NO.FF.info[,"YNO"])
opryn.table <- rbind(table(FF.info[,"OPRYN"]), table(NO.FF.info[,"OPRYN"]))
chisq.test(opryn.table)

### Multiple logistic regression
mr.data <- cbind(rbind(FF.info, NO.FF.info), FREQ.EDusers=c(rep(1, length(FF.info)), rep(0, length(NO.FF.info))))
mr.data <- mr.data[,c("FREQ.EDusers", "age", "gen", "days.in.hospitals", "class", "death", "out.patient.department.visits", "treat.days", "OPRYN", "YNO", "YNO")]
mylogit <- glm(FREQ.EDusers~., data=mr.data, family = "binomial")
summary(mylogit)
confint.default(mylogit)
exp(coef(mylogit)) #OR
exp(cbind(OR = coef(mylogit), confint(mylogit)))

### Prediction
library(ROCR)
# 10 permutation
bitmap("ROCcurves.png", res=300)
p.auc <- c()
for(i in c(1:10)){
	p.idx <- sample(c(1:nrow(mr.data)), nrow(mr.data), replace=T)
	p.mr.data<- mr.data[p.idx,]
	p.pred <- predict(mylogit, p.mr.data, type="response")
	preds<- prediction(as.numeric(p.pred), as.numeric(p.mr.data[,"FREQ.EDusers"]))
	perf <- performance(preds, "tpr", "fpr") 
	perf2 <- performance(preds, "auc") 
	p.auc <- c(p.auc, perf2)
	if(i==1){
		plot(perf, main="ROC Curves from ten permutations",  lwd=2, col="darkgrey")
		}else{
		lines(perf, lwd=2, col="darkgrey")
		}
	}
legend("bottomright", paste("Average AUC values: ", round(mean(p.auc), 1), sep=""), col="red", cex=1)
graphics.off()

	







#medical protection proportion
length(which(is.na(FF.info$class)==F))/nrow(FF.info)
length(which(is.na(NO.FF.info$class)==F))/nrow(NO.FF.info)
length(which(is.na(NO.FF.info$class)==T))/nrow(NO.FF.info)
length(which(is.na(FF.info$class)==T))/nrow(FF.info)
length(which(is.na(FF.info$class)==F))
length(which(is.na(FF.info$class)==T))
length(which(is.na(NO.FF.info$class)==F))
length(which(is.na(NO.FF.info$class)==T))
#payment
FF.pay.total <- apply(as.matrix(FF.NO), 1, temp <- function(nn){
                 temp <- as.matrix(ed.dt[J(as.numeric(nn))])
                temp.pay <- sum(as.numeric(temp[,"DMD_TRAMT"]))
                return(temp.pay)})
FF.pay.total <- sum(FF.pay.total)
pay.total <- sum(as.numeric(ed.data[,"DMD_TRAMT"]))
FF.pay.total/pay.total
t.test(FF.info[,"insurance.pay"]+ FF.info[,"patient.pay"]);t.test(NO.FF.info[,"insurance.pay"] + NO.FF.info[,"patient.pay"])
FF.insurance.pay.total <- apply(as.matrix(FF.NO), 1, temp <- function(nn){
                temp <- ed.data[which(ed.data[,"NO"]==nn),]
                temp.insurance.pay <- sum(as.numeric(temp[,"DMD_JBRDN_AMT"]))
                return(temp.insurance.pay)})
FF.insurance.pay.total <- sum(FF.insurance.pay.total);FF.insurance.pay.total 
insurance.pay.total <- sum(as.numeric(ed.data[,"DMD_JBRDN_AMT"]));insurance.pay.total
FF.insurance.pay.total/insurance.pay.total
t.test(FF.info[,"days.in.hospitals"]);t.test(NO.FF.info[,"days.in.hospitals"])
t.test(FF.info[,"days.in.hospitals"], NO.FF.info[,"days.in.hospitals"])
#### Main Sickness
FF.ms <- c()
for(temp in FF.info[,"main.sick"]){
        FF.ms <- c(FF.ms, strsplit(temp, "\\|")[[1]])
        }
print(as.matrix(sort(table(FF.ms),decreasing=T)[1:10]))
NO.FF.ms <- c()
for(temp in NO.FF.info[,"main.sick"]){
        NO.FF.ms <- c(NO.FF.ms, strsplit(temp, "\\|")[[1]])
        }
print(as.matrix(sort(table(NO.FF.ms),decreasing=T)[1:10]))
#### Subsickness
FF.ss <- c()
for(temp in FF.info[,"comorbidity"]){
        if(is.na(temp)==T){next}
        FF.ss <- c(FF.ss, strsplit(temp, "\\|")[[1]])
        }
print(as.matrix(sort(table(FF.ss),decreasing=T)[1:10]))
NO.FF.ss <- c()
for(temp in NO.FF.info[,"comorbidity"]){
        if(is.na(temp)==T){next}
        NO.FF.ss <- c(NO.FF.ss, strsplit(temp, "\\|")[[1]])
        }
print(as.matrix(sort(table(NO.FF.ss),decreasing=T)[1:10]))
##
death.prop.FF <- length(which(FF.info[,"death"]==1))/nrow(FF.info)*100
death.prop.NO.FF <- length(which(NO.FF.info[,"death"]==1))/nrow(NO.FF.info)*100



############################
com.prop.FF <- length(which(FF.info[,"comorbidity.idx"]==1))/nrow(FF.info)*100
com.prop.NO.FF <- length(which(NO.FF.info[,"comorbidity.idx"]==1))/nrow(NO.FF.info)*100
com.prop.FF; com.prop.NO.FF





bmp("Num_OutPatient_visits.png")
par(mar=c(5,7,5,2))
par(lwd=3)
par(cex.axis=1)
par(cex.lab=1)
par(cex.main=1)
temp.idx <- c(rep(1,length(FF.info[,"out.patient.department.visits"])), rep(2,length(NO.FF.info[,"out.patient.department.visits"])))
sm.density.compare(c(FF.info[,"out.patient.department.visits"], NO.FF.info[,"out.patient.department.visits"]), as.factor(temp.idx), xlab="Outpatient Department Visits / year", ylab="Density")
colfill<-c(2:(2+length(levels(as.factor(temp.idx)))))
graphics.off()
bmp("Num_OutPatient_visits_BARPLOT.png",640, 480)
b<-boxplot(list(FF=FF.info[,"out.patient.department.visits"],Non.FF=NO.FF.info[,"out.patient.department.visits"]), col=c("Red", "pink"),  ylab="Number of Visits / year", main="Outpatient Department visits", outline = FALSE)
graphics.off()


bmp("AGE_FF_NFF.png")
par(mar=c(5,7,5,2))
par(lwd=3)
par(cex.axis=1)
par(cex.lab=1)
par(cex.main=1)
temp.idx <- c(rep(1,length(FF.info[,"age"])), rep(2,length(NO.FF.info[,"age"])))
sm.density.compare(c(FF.info[,"age"], NO.FF.info[,"age"]), as.factor(temp.idx), xlab="Age", ylab="Density")
title(main="Age Distribution between FF(RED) & Non-FF(GREEN)")
colfill<-c(2:(2+length(levels(as.factor(temp.idx)))))
graphics.off()


## Sex
bmp("Gender_FF_NFF.png")
par(mar=c(5,5,2,5))
par(mfrow=c(1,2))
barplot(table(FF.info[,"gen"])/length(FF.info[,"gen"]), main="Gender of FF", col=c("blue","red"), ylab="Proportion",names.arg=c("Male", "Female"))
barplot(table(NO.FF.info[,"gen"])/length(NO.FF.info[,"gen"]), main="Gender of Non-FF", col=c("blue","red"), ylab="Proportion",names.arg=c("Male", "Female"))
graphics.off()
gender.table <- rbind(table(FF.info[,"gen"]), table(NO.FF.info[,"gen"]))
colnames(gender.table) <- c("Male", "Female")
rownames(gender.table) <- c("FF","Non-FF")
chisq.test(gender.table)



## Payment
bmp("Pay-perVisit_FF_NFF.png", 840, 480)
par(mar=c(5,5,2,5))
par(mfrow=c(1,2))
boxplot(list(Frequent.Users=FF.info[,"insurance.pay"]/1072, Occasional.Visitors=NO.FF.info[,"insurance.pay"]/1072), col=c("darkgrey", "white"), main="Pay by National Heahlth Insurance / visit",outline = FALSE, ylab="payment($)")
boxplot(list(Frequent.Users=FF.info[,"patient.pay"]/1072, Occasional.Visitors=NO.FF.info[,"patient.pay"]/1072), col=c("darkgrey", "white"), main="Pay by Patient / visit",outline = FALSE, ylab="payment($)")
graphics.off()
t.test(FF.info[,"insurance.pay"], NO.FF.info[,"insurance.pay"])
t.test(FF.info[,"patient.pay"], NO.FF.info[,"patient.pay"])


FF.pay.total <- apply(as.matrix(FF.NO), 1, temp <- function(nn){
                 temp <- as.matrix(ed.dt[J(as.numeric(nn))])
		temp.pay <- sum(as.numeric(temp[,"DMD_TRAMT"]))
		return(temp.pay)})
FF.pay.total <- sum(FF.pay.total)
pay.total <- sum(as.numeric(ed.data[,"DMD_TRAMT"]))
FF.pay.total/pay.total
		
FF.insurance.pay.total <- apply(as.matrix(FF.NO), 1, temp <- function(nn){
		 temp <- as.matrix(ed.dt[J(as.numeric(nn))])
                temp.insurance.pay <- sum(as.numeric(temp[,"DMD_JBRDN_AMT"]))
		return(temp.insurance.pay)})
FF.insurance.pay.total <- sum(FF.insurance.pay.total)
insurance.pay.total <- sum(as.numeric(ed.data[,"DMD_JBRDN_AMT"]))
FF.insurance.pay.total/insurance.pay.total


## Treatment duration
bmp("RECN_FF_NFF_rangeall.png")
par(mar=c(5,7,5,2))
par(lwd=3)
par(cex.axis=1)
par(cex.lab=1)
par(cex.main=1)
temp.idx <- c(rep(1,length(FF.info[,"days.in.hospitals"])), rep(2,length(NO.FF.info[,"days.in.hospitals"])))
sm.density.compare(c(FF.info[,"days.in.hospitals"], NO.FF.info[,"days.in.hospitals"]), as.factor(temp.idx), xlab="Duration(days)", ylab="Density")
title(main="Treatment duration/visit between FF(RED) & Non-FF(GREEN)")
colfill<-c(2:(2+length(levels(as.factor(temp.idx)))))
graphics.off()





### Class distribution -NEW
# 0 class is not established so make it NA
FF.info[which(FF.info[,"class"]==0),"class"] <-NA
NO.FF.info[which(NO.FF.info[,"class"]==0),"class"] <-NA
FF.info$class <- as.character(FF.info$class)
NO.FF.info$class <- as.character(NO.FF.info$class)

bmp("CLASS_FF_NFF.png",1280, 480)
par(mfrow=c(1,2))
lowclass.prop.FF <- sum(table(FF.info[,"class"]))/nrow(FF.info)*100
lowclass.prop.NO.FF <- sum(table(NO.FF.info[,"class"]))/nrow(NO.FF.info)*100
b<-barplot(round(c(lowclass.prop.FF, lowclass.prop.NO.FF),2), col=c("red", "pink"), width=0.5, ylab="Proportion(%)", main="Proportion of person for medical protection", names.arg=c("FF population","Non-FF population"))
text(x=b, y=round(c(lowclass.prop.FF, lowclass.prop.NO.FF),2), labels=paste(round(c(lowclass.prop.FF, lowclass.prop.NO.FF),2), "%", sep=""), pos=1, col="black", cex=1.5)

par(las=2)
FF.sec <- table(FF.info[,"class"])/length(which(is.na(FF.info[,"class"])!=T))*100
NO.FF.sec <- table(NO.FF.info[,"class"])/length(which(is.na(NO.FF.info[,"class"])!=T))*100
FF.sec <- FF.sec
NO.FF.sec <- NO.FF.sec
b<-barplot(rbind(FF.sec, NO.FF.sec), main="Medical protection classes in FF/Non-FF", col=c("red", "pink"), legend=c("FF", "Non-FF"), beside=T, ylim=c(0,100), names.arg=paste("Class-", names(FF.sec), sep=""), ylab="Proportion(%)")
text(x=b, y=c(FF.sec[1],NO.FF.sec[1],FF.sec[2],NO.FF.sec[2], FF.sec[3], NO.FF.sec[3], FF.sec[4], NO.FF.sec[4], FF.sec[5], NO.FF.sec[5], FF.sec[6], NO.FF.sec[6]), labels=paste(round(c(FF.sec[1],NO.FF.sec[1],FF.sec[2],NO.FF.sec[2], FF.sec[3], NO.FF.sec[3], FF.sec[4], NO.FF.sec[4], FF.sec[5], NO.FF.sec[5], FF.sec[6], NO.FF.sec[6]),0), "%", sep=""), pos=3,col="black",cex=1)
graphics.off()


save.image("temp2.rda")


#### Main Sickness
FF.ms <- c()
for(temp in FF.info[,"main.sick"]){
	FF.ms <- c(FF.ms, strsplit(temp, "\\|")[[1]])
	}
print(as.matrix(sort(table(FF.ms),decreasing=T)[1:10]))

NO.FF.ms <- c()
for(temp in NO.FF.info[,"main.sick"]){
        NO.FF.ms <- c(NO.FF.ms, strsplit(temp, "\\|")[[1]])
        }
print(as.matrix(sort(table(NO.FF.ms),decreasing=T)[1:10]))


#### Subsickness
FF.ss <- c()
for(temp in FF.info[,"comorbidity"]){
	if(is.na(temp)==T){next}
        FF.ss <- c(FF.ss, strsplit(temp, "\\|")[[1]])
        }
print(as.matrix(sort(table(FF.ss),decreasing=T)[1:10]))
NO.FF.ss <- c()
for(temp in NO.FF.info[,"comorbidity"]){
	if(is.na(temp)==T){next}
        NO.FF.ss <- c(NO.FF.ss, strsplit(temp, "\\|")[[1]])
        }
print(as.matrix(sort(table(NO.FF.ss),decreasing=T)[1:10]))



### Comorbidity Frequency

bmp("comorbidity_FF_NFF.png",640, 480)
com.prop.FF <- length(which(FF.info[,"comorbidity.idx"]==1))/nrow(FF.info)*100
com.prop.NO.FF <- length(which(NO.FF.info[,"comorbidity.idx"]==1))/nrow(NO.FF.info)*100
b<-barplot(round(c(com.prop.FF, com.prop.NO.FF),2), col=c("darkgreen", "green"), width=0.5, ylab="Proportion(%)", main="Proportion of Comorbidity", names.arg=c("FF population","Non-FF population"), ylim=c(1,100))
text(x=b, y=round(c(com.prop.FF, com.prop.NO.FF),2), labels=paste(round(c(com.prop.FF, com.prop.NO.FF),2), "%", sep=""), pos=1, col="black", cex=1.5)
graphics.off()

### Mortality

bmp("Death_FF_NFF.png",640, 480)
death.prop.FF <- length(which(FF.info[,"death"]==1))/nrow(FF.info)*100
death.prop.NO.FF <- length(which(NO.FF.info[,"death"]==1))/nrow(NO.FF.info)*100
b<-barplot(round(c(death.prop.FF, death.prop.NO.FF),2), col=c("blue", "lightblue"), width=0.5, ylab="Proportion(%)", main="Proportion of DEATH events", names.arg=c("FF population","Non-FF population"), ylim=c(1,100))
text(x=b, y=round(c(death.prop.FF, death.prop.NO.FF),2), labels=paste(round(c(death.prop.FF, death.prop.NO.FF),2), "%", sep=""), pos=3, col="black", cex=1.5)
graphics.off()
