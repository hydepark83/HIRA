library(sm)

## Load data
#r.data <- read.table("./data/proc_ED_nps_20", header=T,  sep=",") #2009.01.01 ~ 2009.12.31
load("./data/proc_ED_nps_20.rda")

## Time period:
min(r.data$RECU_FR_DT)
max(r.data$RECU_FR_DT)

## ED visitors
ed.idx <- apply(as.matrix(r.data$IN_PAT_CORS_TYPE), 1, temp <- function(yy){
					temp <- strsplit(as.character(yy), "")[[1]]
					if(temp[2] == "1"){return(1)}else{return(0)}
					})
table(ed.idx) # proportion of ED patients
ed.data <- r.data[which(ed.idx==1),]

## multiple ED visitors
as.matrix(table(table(ed.data$NO)))

## Frequent users definition: 
#When defined as 4 or more ED visits per year, frequent users accounted for 4.5% to 8% of all ED patients
num.ed.record.FF <- sum(table(ed.data$NO)[which(table(ed.data$NO)>=4)])
num.ed.record.all <- sum(table(ed.data$NO))
#> num.ed.record.FF/num.ed.record.all
#[1] 0.1710216

FF.NO <- names(table(ed.data$NO)[which(table(ed.data$NO)>=4)]) 
NO.FF.NO <- setdiff(unique(ed.data$NO), FF.NO)
num.ed.patients.FF <- length(FF.NO)
num.ed.patients.all <- length(FF.NO) + length(NO.FF.NO)
# >num.ed.patients.FF/num.ed.patients.all
# >0.03622949
#flush <- apply(as.matrix(FF.NO), 1, temp <- function(nn){
#	temp.r.data <- r.data[which(r.data$NO==nn&ed.idx==1), ]
#	if(nrow(temp.r.data)>=10){
#	print(nn)
#	print("Number of visits:")
#	print(nrow(temp.r.data))
#	print("Date:")
#	print(temp.r.data$RECU_FR_DT)
#	print("Disease:")
#	print(temp.r.data$INJ_OUT_PRY_FACTOR)
#	}
#	})

# data structure conversion for boost computational speed
ed.data <- sapply(ed.data,as.character)

print(Sys.time())
FF.info <- apply(as.matrix(FF.NO), 1, temp <- function(nn){
		temp <- ed.data[which(ed.data[,"NO"]==nn),]
		temp.age <- as.numeric(temp[,"age"][1])
		# there is an issue in age column, age sometimes varies
		temp.gen <- temp[,"gen"][1]
                temp.insurance.pay <- sum(as.numeric(temp[,"DMD_JBRDN_AMT"]))/nrow(temp)
		temp.total.pay <- sum(as.numeric(temp[,"DMD_TRAMT"]))/nrow(temp)
                temp.self.pay <- temp.total.pay - temp.insurance.pay
		# there is an issue in DMD_SBRDN_AMT columns (all zero in some cases)
		temp.dur <- sum(as.numeric(temp[,"RECN"]))/nrow(temp)
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
		temp.outPD.visit <- length(which(ed.idx!=1 & r.data[,"NO"]==nn))
		return(c(nn, temp.age, temp.gen, temp.insurance.pay, temp.self.pay, temp.dur, temp.se.class, temp.main.sick, temp.death, temp.comorbidity.idx, temp.comorbidity, temp.outPD.visit))
		})
print(Sys.time())


NO.FF.info <- apply(as.matrix(NO.FF.NO), 1, temp <- function(nn){
		temp <- ed.data[which(ed.data[,"NO"]==nn),]
		if(length(which(ed.data[,"NO"]==nn))>1){
 	                temp.age <- as.numeric(temp[,"age"][1])
	                temp.gen <- temp[,"gen"][1]
	                temp.insurance.pay <- sum(as.numeric(temp[,"DMD_JBRDN_AMT"]))/nrow(temp)
	                temp.total.pay <- sum(as.numeric(temp[,"DMD_TRAMT"]))/nrow(temp)
	                temp.self.pay <- temp.total.pay - temp.insurance.pay
			temp.dur <- sum(as.numeric(temp[,"RECN"]))/nrow(temp)
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
			}else{
			names(temp) <- colnames(ed.data)
			temp.age <- as.numeric(temp["age"])
			temp.gen <- temp["gen"]
			temp.insurance.pay <- as.numeric(temp["DMD_JBRDN_AMT"])
			temp.total.pay <-as.numeric(temp["DMD_TRAMT"])
			temp.self.pay <- temp.total.pay - temp.insurance.pay
			temp.dur <- as.numeric(temp["RECN"])
			temp.se.class <- temp["MACL_CD"]
			temp.main.sick <- temp["MAIN_SICK"]
			if(temp["DIAG_RSLT_TYPE"]=="4"){
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
			}
		temp.outPD.visit <- length(which(ed.idx!=1 & r.data[,"NO"]==nn))
                return(c(nn, temp.age, temp.gen, temp.insurance.pay, temp.self.pay, temp.dur, temp.se.class, temp.main.sick, temp.death, temp.comorbidity.idx, temp.comorbidity, temp.outPD.visit))
                })
print(Sys.time())


FF.info <- t(FF.info); FF.info <- as.data.frame(FF.info)
colnames(FF.info) <- c("NO", "age", "gen", "insurance.pay", "patient.pay", "days.in.hospitals", "class", "main.sick", "death", "comorbidity.idx", "comorbidity", "out.patient.department.visits")
FF.info$age <- as.numeric(as.matrix(FF.info$age))
FF.info$insurance.pay <- as.numeric(as.matrix(FF.info$insurance.pay))
FF.info$patient.pay <- as.numeric(as.matrix(FF.info$patient.pay))
FF.info$days.in.hospitals <- as.numeric(as.matrix(FF.info$days.in.hospitals))
FF.info$main.sick <- as.character(as.matrix(FF.info$main.sick))
FF.info$comorbidity <- as.character(as.matrix(FF.info$comorbidity))
FF.info$out.patient.department.visits <- as.numeric(as.matrix(FF.info$out.patient.department.visits))

NO.FF.info <- t(NO.FF.info); NO.FF.info <- data.frame(NO.FF.info)
colnames(NO.FF.info) <- c("NO", "age", "gen", "insurance.pay", "patient.pay", "days.in.hospitals", "class", "main.sick", "death", "comorbidity.idx", "comorbidity", "out.patient.department.visits")
NO.FF.info$age <- as.numeric(as.matrix(NO.FF.info$age))
NO.FF.info$insurance.pay <- as.numeric(as.matrix(NO.FF.info$insurance.pay))
NO.FF.info$patient.pay <- as.numeric(as.matrix(NO.FF.info$patient.pay))
NO.FF.info$days.in.hospitals <- as.numeric(as.matrix(NO.FF.info$days.in.hospitals))
NO.FF.info$main.sick <- as.character(as.matrix(NO.FF.info$main.sick))
NO.FF.info$comorbidity <- as.character(as.matrix(NO.FF.info$comorbidity))
NO.FF.info$out.patient.department.visits <- as.numeric(as.matrix(NO.FF.info$out.patient.department.visits))


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

#t.test(FF.info[,"age"], NO.FF.info[,"age"])
#> mean(FF.info[,"age"])
#[1] 58.30557
#> mean(NO.FF.info[,"age"])
#[1] 46.31945
# p-value < 2.2e-16


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
#> gender.table
#        Male Female
#FF      3355   2336
#Non-FF 77887  73504
chisq.test(gender.table)
#p-value < 2.2e-16



## Payment
bmp("Pay-perVisit_FF_NFF.png", 840, 480)
par(mar=c(5,5,2,5))
par(mfrow=c(1,2))
boxplot(list(Frequent.Users=FF.info[,"insurance.pay"]/1072, Occasional.Visitors=NO.FF.info[,"insurance.pay"]/1072), col=c("darkgrey", "white"), main="Pay by National Heahlth Insurance / visit",outline = FALSE, ylab="payment($)")
boxplot(list(Frequent.Users=FF.info[,"patient.pay"]/1072, Occasional.Visitors=NO.FF.info[,"patient.pay"]/1072), col=c("darkgrey", "white"), main="Pay by Patient / visit",outline = FALSE, ylab="payment($)")
graphics.off()
t.test(FF.info[,"insurance.pay"], NO.FF.info[,"insurance.pay"])
t.test(FF.info[,"patient.pay"], NO.FF.info[,"patient.pay"])

FF.insurance.pay.total <- apply(as.matrix(FF.NO), 1, temp <- function(nn){
                temp <- ed.data[which(ed.data[,"NO"]==nn),]
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
#J189  391
#F102  292
#I639  262
#F209  207
#I10   198
#nncd  184
#C349  173
#C220  159
#A419  157
#N180  150

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
