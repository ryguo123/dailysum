
#' dailysum
#'
#' deal the data of
#'
#' @param x cageNo of each cage
#'
#' @return write.xlsx
#' @export
#'x<-c(775,624,123,2525,324,NA,NA)
#' @examples
day<-function(x){
  fileN <- readline(prompt = "输入文件名 : ")
  fileD <- readline(prompt = "文件储存地址 : ")
  cageNo <- c()
  done <- FALSE
  while (!done) {
    x <- readline(prompt = "请输入鸡舍笼位 (输入 q 结束): ")
    if (x == "q") {
      done <- TRUE
    } else {
      cageNo <- c(cageNo, x)
    }
  }
  cageNo<-as.numeric(cageNo)
  fileY <- unlist(strsplit(date()," "))[5]
  fileM <- as.numeric(unlist(strsplit(fileN,"[.]"))[1])
  if(fileM<9){
    fileM<-paste0("0",fileM)
  }

  library(writexl)
  fileDir<-paste0(fileD,fileY,"\\",fileM,"\\")
  inputN<-paste0(fileDir,fileN,".csv")
  input<-read.csv2(inputN,header=F,skip=1,sep = ",",stringsAsFactors=F)
  input<-input[!is.na(input[,3]),]
  input<-input[grepl("^[[:digit:]][[:upper:]][-][[:digit:]]+$",input[,2]),]
  reIn<-c()
  for(rowNo in 1:nrow(input)){
    splitD<-unlist(strsplit(input[rowNo,1]," +"))
    reIn<-rbind(reIn,splitD)
  }

  rownames(reIn)<-1:nrow(reIn)
  reIn<-cbind(reIn,input[,-1])
  library(tidyr)
  new<-separate(reIn,1,into= c("year","month","day"),sep = "-")
  new2<-unite(new,"precise",month,day,sep = ".")
  for (i in fileN){
    b=subset(new2,new2$precise==i)
  }
  new3<-separate(b,2,into = c("month","day"),sep = "\\.")
  reIn<-unite(new3,"1",year,month,day,sep = "-")
  dates<-unique(reIn[,1])
  wrong1<-subset(reIn,reIn$V3=="2.00")
  wrong2<-subset(reIn,reIn$V3!="1.00"|reIn$V3!="2.00")
  wrong<-subset(reIn,reIn$V3!="1.00")
  reIn<-subset(reIn,reIn$V3=="1.00")

  for(dd in dates){
    subD<-reIn[reIn[,1]%in%dd,]
    year<-unlist(strsplit(dd,"[-/]"))[1]
    yearB<-substr(year,3,4)
    month<-unlist(strsplit(dd,"[-/]"))[2]
    day<-unlist(strsplit(dd,"[-/]"))[3]
    if(length(grep(year,dir(fileD)))==0){
      print(paste0("需要在桌面/ChanDan/中创建",year,"年的新文件夹！"))
      dir.create(paste0(fileD,fileY))
    }

    if(length(grep(month,dir(paste0(fileD,fileY))))==0){
      print(paste0("需要在桌面/ChanDan/",year,"/文件夹中再创建",month,"月的新文件夹！"))
      dir.create(paste0(fileD,fileY,"\\",fileM))
    }

    hNo<-as.numeric(unique(substr(subD[,3],1,1)))
    hNo<-hNo[!is.na(hNo)]
    houseNo<-paste0(hNo,"号舍")

    files<-dir(paste0(fileDir,"sum"))
    if(length(files)>0){
      SumH<-substr(files,1,3)
      if(!all(houseNo%in%SumH)){
        sheetN<-unique(c(houseNo,SumH))
        sheetN<-sheetN[order(sheetN)]
      }else{
        sheetN<-SumH
      }
      sheets<-vector(mode="list",length=length(sheetN))
      names(sheets)<-sheetN

      for(ff in sheetN){
        if(length(grep(ff,files))==1){
          inD<-read.delim(paste0(fileDir,"sum\\",files[grep(ff,files)]),sep="\t",stringsAsFactors=F)
          names(inD)<-sub("^X","",names(inD))
          sheets[[grep(ff,sheetN)]]<-inD
        }
      }
    }else{
      dir.create(paste0(fileDir,"sum\\"))
      sheetN<-houseNo
      sheets<-vector(mode="list",length=length(houseNo))
      names(sheets)<-houseNo
    }

    for(hn in hNo){
      subDH<-subD[grep(paste0("^",hn),subD[,3]),]
      if(is.null(sheets[[grep(paste0(hn,"号舍"),sheetN)]])){
        cageN<-1:cageNo[hn]
        cageN[c(1:cageNo[hn])<10]<-paste0("00",1:9)
        cageN[c(1:cageNo[hn])<100 & c(1:cageNo[hn])>9]<-paste0("0",10:99)
        cages<-paste0(hn,rep(LETTERS[1:3],each=cageNo[hn]),"-",cageN)

        eggs<-as.data.frame(c("产蛋数",cages))
        names(eggs)<-"笼号"
        sumP<-as.data.frame(rep(0,nrow(eggs)))
        names(sumP)<-paste(as.numeric(month),day,sep=".")

        if(sum(is.na(match(subDH[,3],eggs$"笼号")))>0){
          idxDE<-which(is.na(match(subDH[,3],eggs$"笼号")))
          subDH<-subDH[-idxDE,]
        }

        eggC<-as.numeric(subDH[,4])
        sumP[match(subDH[,3],eggs$"笼号"),1]<-eggC

        sumP[1,1]<-sum(eggC)
        sheets[[grep(paste0(hn,"号舍"),sheetN)]]<-cbind(eggs,sumP)
        write.table(cbind(eggs,sumP),paste0(fileDir,"sum\\",paste0(hn,"号舍"),".txt"),sep="\t",col.names=c("笼号",names(sumP)),row.names=F,quote=F)
      }else{
        eggs<-sheets[[grep(paste0(hn,"号舍"),sheetN)]]
        if(any(grepl(paste(as.numeric(month),day,sep="."),names(eggs)))){
          eggs<-eggs[,-grep(paste(as.numeric(month),day,sep="."),names(eggs))]
        }
        sumP<-as.data.frame(rep(0,nrow(eggs)))
        names(sumP)<-paste(as.numeric(month),day,sep=".")
        if(sum(is.na(match(subDH[,3],eggs$"笼号")))>0){
          idxDE<-which(is.na(match(subDH[,3],eggs$"笼号")))
          subDH<-subDH[-idxDE,]
        }
        eggC<-as.numeric(subDH[,4])
        sumP[match(subDH[,3],eggs$"笼号"),1]<-eggC

        sumP[1,1]<-sum(eggC)
        sumD<-cbind(eggs[,-1],sumP)
        names(sumD)<-c(names(eggs)[-1],names(sumP))
        ordN<-as.numeric(sub(paste0(as.numeric(month),"."),"",names(sumD)))
        sheets[[grep(paste0(hn,"号舍"),sheetN)]]<-cbind(eggs[,1],sumD[,order(ordN)])
        names(sheets[[grep(paste0(hn,"号舍"),sheetN)]])<-c("笼号",names(sumD)[order(ordN)])
        write.table(cbind(eggs[,1],sumD[,order(ordN)]),paste0(fileDir,"sum\\",paste0(hn,"号舍"),".txt"),sep="\t",col.names=c("笼号",names(sumD)[order(ordN)]),row.names=F,quote=F)
      }
    }

    if(as.numeric(month)<10){
      month<-paste0("0",as.numeric(month))
    }

    write_xlsx(sheets,paste0(fileDir,yearB,month,"总结表.xlsx"))
  }

  wrong$V3<-"1"

  for(dd in dates){
    subD<-wrong[wrong[,1]%in%dd,]
    year<-unlist(strsplit(dd,"[-/]"))[1]
    yearB<-substr(year,3,4)
    month<-unlist(strsplit(dd,"[-/]"))[2]
    day<-unlist(strsplit(dd,"[-/]"))[3]
    if(length(grep(year,dir(fileD)))==0){
      print(paste0("需要在桌面/ChanDan/中创建",year,"年的新文件夹！"))
      dir.create(paste0(fileD,fileY))
    }

    if(length(grep(month,dir(paste0(fileD,fileY))))==0){
      print(paste0("需要在桌面/ChanDan/",year,"/文件夹中再创建",month,"月的新文件夹！"))
      dir.create(paste0(fileD,fileY,"\\",fileM))
    }

    hNo<-as.numeric(unique(substr(subD[,3],1,1)))
    hNo<-hNo[!is.na(hNo)]
    houseNo<-paste0(hNo,"号舍")
    files<-dir(paste0(fileDir,"wrongsum"))
    if(length(files)>0){
      SumH<-substr(files,1,3)
      if(!all(houseNo%in%SumH)){
        sheetN<-unique(c(houseNo,SumH))
        sheetN<-sheetN[order(sheetN)]
      }else{
        sheetN<-SumH
      }
      sheets<-vector(mode="list",length=length(sheetN))
      names(sheets)<-sheetN

      for(ff in sheetN){
        if(length(grep(ff,files))==1){
          inD<-read.delim(paste0(fileDir,"wrongsum\\",files[grep(ff,files)]),sep="\t",stringsAsFactors=F)
          names(inD)<-sub("^X","",names(inD))
          sheets[[grep(ff,sheetN)]]<-inD
        }
      }
    }else{
      dir.create(paste0(fileDir,"wrongsum\\"))
      sheetN<-houseNo
      sheets<-vector(mode="list",length=length(houseNo))
      names(sheets)<-houseNo
    }

    for(hn in hNo){
      subDH<-subD[grep(paste0("^",hn),subD[,3]),]


      if(is.null(sheets[[grep(paste0(hn,"号舍"),sheetN)]])){
        cageN<-1:cageNo[hn]
        cageN[c(1:cageNo[hn])<10]<-paste0("00",1:9)
        cageN[c(1:cageNo[hn])<100 & c(1:cageNo[hn])>9]<-paste0("0",10:99)
        cages<-paste0(hn,rep(LETTERS[1:3],each=cageNo[hn]),"-",cageN)

        eggs<-as.data.frame(c("出错数",cages))
        names(eggs)<-"笼号"
        sumP<-as.data.frame(rep(0,nrow(eggs)))
        names(sumP)<-paste(as.numeric(month),day,sep=".")

        if(sum(is.na(match(subDH[,3],eggs$"笼号")))>0){
          idxDE<-which(is.na(match(subDH[,3],eggs$"笼号")))
          subDH<-subDH[-idxDE,]
        }

        eggC<-as.numeric(subDH[,4])
        sumP[match(subDH[,3],eggs$"笼号"),1]<-eggC

        sumP[1,1]<-sum(eggC)
        sheets[[grep(paste0(hn,"号舍"),sheetN)]]<-cbind(eggs,sumP)
        #write.table(cbind(eggs,sumP),paste0(fileDir,"sum/",paste0(hn,"号舍"),".txt"),sep="\t",col.names=c("笼号",names(sumP)),row.names=F,quote=F)
        write.table(cbind(eggs,sumP),paste0(fileDir,"wrongsum\\",paste0(hn,"号舍错误数据"),".txt"),sep="\t",col.names=c("笼号",names(sumP)),row.names=F,quote=F)
      }else{
        eggs<-sheets[[grep(paste0(hn,"号舍"),sheetN)]]
        if(any(grepl(paste(as.numeric(month),day,sep="."),names(eggs)))){
          eggs<-eggs[,-grep(paste(as.numeric(month),day,sep="."),names(eggs))]
        }
        sumP<-as.data.frame(rep(0,nrow(eggs)))
        names(sumP)<-paste(as.numeric(month),day,sep=".")
        if(sum(is.na(match(subDH[,3],eggs$"笼号")))>0){
          idxDE<-which(is.na(match(subDH[,3],eggs$"笼号")))
          subDH<-subDH[-idxDE,]
        }
        eggC<-as.numeric(subDH[,4])
        sumP[match(subDH[,3],eggs$"笼号"),1]<-eggC

        sumP[1,1]<-sum(eggC)
        sumD<-cbind(eggs[,-1],sumP)
        names(sumD)<-c(names(eggs)[-1],names(sumP))
        ordN<-as.numeric(sub(paste0(as.numeric(month),"."),"",names(sumD)))
        sheets[[grep(paste0(hn,"号舍"),sheetN)]]<-cbind(eggs[,1],sumD[,order(ordN)])
        names(sheets[[grep(paste0(hn,"号舍"),sheetN)]])<-c("笼号",names(sumD)[order(ordN)])
        write.table(cbind(eggs[,1],sumD[,order(ordN)]),paste0(fileDir,"wrongsum\\",paste0(hn,"号舍错误数据"),".txt"),sep="\t",col.names=c("笼号",names(sumD)[order(ordN)]),row.names=F,quote=F)
      }
    }

    if(as.numeric(month)<10){
      month<-paste0("0",as.numeric(month))
    }

    write_xlsx(sheets,paste0(fileDir,yearB,month,"错误数据总结表.xlsx"))
  }
}
print("------------------------------------- data has been deal --------------------------------------")
print("--------------------------- all the data are store in C:\\Users\\Administrator\\Desktop\\chandan ---------------------------")
