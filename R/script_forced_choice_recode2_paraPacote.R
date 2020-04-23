checkTypo<-function(data, RecSpc=T, Cd=c("a","b","c")) {
  if(RecSpc) {
    data<-as.data.frame(apply(data,2, function(x) gsub('\\s+', '',x)))
  }
  errorTyp<-NULL
  for (col in 1:ncol(data)){
    for(e in 1:nrow(data)){
      if( (data[e,col] %in% Cd) | (is.na(data[e,col]))  ) {NULL
      } else {
        errorTyp<-paste0(errorTyp, paste0("r",rownames(data[e,]),"col",col), sep=",")
      }
    }
  }
  if(length(errorTyp) == 0) {
    print ("Cool, there is no typos in your dataset")}
  if(length(errorTyp) >= 1) {
    print(as.data.frame(strsplit(errorTyp, ","),col.names="cell_typo"))}
}
checkTie<-function(data, type="mole") {
  if(type=="mole") {
    errorTie<-NULL
    bl<-c(1,2)
    ntripl=ncol(data)/2
    for (trl in 1:ntripl){
      for(e in 1:nrow(data)){
        if ((data[e,bl[1]] %in% data[e,bl[2]]) &
            (!is.na(data[e,bl[1]]) | !is.na(data[e,bl[2]]))) {
          errorTie<- paste0(errorTie, paste0("r",rownames(data[e,]),"Bl",trl), sep=",")
        } else {NULL}
      }
      bl<-bl+2
    }}
  if(type=="rank"){
    errorTie<-NULL
    bl<-c(1,2,3)
    ntripl=ncol(data)/3
    for (trl in 1:ntripl){
      for(e in 1:nrow(data)){
        if (((data[e,bl[1]] %in% data[e,c(bl[2],bl[3])]) |
             (data[e,bl[2]] %in% data[e,c(bl[1],bl[3])])) &
            (sum(is.na(data[e,c(bl[1],bl[2],bl[3])])) < 2)) {
          errorTie<- paste0(errorTie, paste0("r",rownames(data[e,]),"Trl",trl), sep=",")
        } else {NULL}
      }
      bl<-bl+3
    }}
  if(length(errorTie) == 0) {
    print ("Cool, there is no ties in your dataset")}
  if(length(errorTie) >= 1) {
    print(as.data.frame(strsplit(errorTie, ","),col.names="block_tied_answer"))}
}
recodeErrors<- function(data, type="mole", RecTie=T, RecTyp=T, Cd=c("a","b","c")) {
  if(type=="mole") {
    if(RecTie) {
      bl<-c(1,2)
      ntripl=ncol(data)/2
      for (trl in 1:ntripl){
        for(e in 1:nrow(data)){
          if ((data[e,bl[1]] %in% data[e,bl[2]]) &
              (!is.na(data[e,bl[1]]) | !is.na(data[e,bl[2]]))) {
            data[e,bl[1]]<-NA
            data[e,bl[2]]<-NA
          } else {NULL}
        }
        bl<-bl+2
      }
    }
    if(RecTyp) {
      for (col in 1:ncol(data)){
        for(e in 1:nrow(data)){
          if (data[e,col] %in% Cd | is.na(data[e,col]))  {
            NULL}
          else{
            data[e,col]<-NA }
        }
      }
    }
    return(data)
  }
  if(type=="rank"){
    if(RecTie) {
      bl<-c(1,2,3)
      ntripl=ncol(data)/3
      for (trl in 1:ntripl){
        for(e in 1:nrow(data)){
          if (sum(is.na(data[e,c(bl[1],bl[2],bl[3])])) < 2) {
            if(!is.na(data[e,bl[1]]) &  data[e,bl[1]] %in% data[e,bl[2]]){
              data[e,c(bl[1],bl[2])]<-NA}
            else if(!is.na(data[e,bl[1]]) &  data[e,bl[1]] %in% data[e,bl[3]]){
              data[e,c(bl[1],bl[3])]<-NA}
            else if(!is.na(data[e,bl[2]]) &  data[e,bl[2]] %in% data[e,bl[3]]){
              data[e,c(bl[2],bl[3])]<-NA}
            else {NULL}
          }
          else {NULL}
        }
        bl=bl+3
      }
    }
    if(RecTyp) {
      for (col in 1:ncol(data)){
        for(e in 1:nrow(data)){
          if (data[e,col] %in% Cd | is.na(data[e,col]))  {
            NULL}
          else{
            data[e,col]<-NA }
        }
      }
    }
    return(data)
  }
}
recodeData<-function(data, type="mole", Cd=c(1,2,3), na=T, rk=2) {
  if(type=="mole") {
    ntripl=ncol(data)/2
    dataTemp<-as.data.frame(matrix(data=NA, nrow =nrow(data) , ncol=ntripl*3))
    Bor<-c(1,2)
    Bn<-c(1,2,3)
    for (trl in 1:ntripl){
      for(s in 1:nrow(data)){
        if ( (is.na(data[s,Bor[1]]) & is.na(data[s,Bor[2]])) |
             (is.na(data[s,Bor[1]]) & data[s,Bor[2]]==Cd[3]) |
             (data[s,Bor[1]]==Cd[3] & is.na(data[s,Bor[2]]))) {
          dataTemp[s,Bn[1]]<-NA
        } else if ((Cd[2] %in% data[s,Bor[1]]) | (Cd[1] %in% data[s,Bor[2]])) {
          dataTemp[s,Bn[1]]<-1
        } else {dataTemp[s,Bn[1]]<-0}
      }
      for(s in 1:nrow(data)){
        if ( (is.na(data[s,Bor[1]]) & is.na(data[s,Bor[2]])) |
             (is.na(data[s,Bor[1]]) & data[s,Bor[2]]==Cd[2]) |
             (data[s,Bor[1]]==Cd[2] & is.na(data[s,Bor[2]]))) {
          dataTemp[s,Bn[2]]<-NA
        } else if ((Cd[3] %in% data[s,Bor[1]]) | (Cd[1] %in% data[s,Bor[2]])) {
          dataTemp[s,Bn[2]]<-1
        } else {dataTemp[s,Bn[2]]<-0}
      }
      for(s in 1:nrow(data)){
        if ( (is.na(data[s,Bor[1]]) & is.na(data[s,Bor[2]])) |
             (is.na(data[s,Bor[1]]) & data[s,Bor[2]]==Cd[1]) |
             (data[s,Bor[1]]==Cd[1] & is.na(data[s,Bor[2]]))) {
          dataTemp[s,Bn[3]]<-NA
        } else if ((Cd[3] %in% data[s,Bor[1]]) | (Cd[2] %in% data[s,Bor[2]])) {
          dataTemp[s,Bn[3]]<-1
        } else {dataTemp[s,Bn[3]]<-0}
      }
      Bor<-Bor+2
      Bn<-Bn+3
    }}
  if(type=="rank"){
    ntripl=ncol(data)/3
    dataTemp<-as.data.frame(matrix(data=NA, nrow =nrow(data) , ncol=ntripl*3))
    Loc<-c(1,2,3)
    if(rk==1)   {
      data <- as.data.frame(matrix(mapply(function(x){4 - x}, data),ncol = ncol(data)))
    }
    if(!na){
      for (trl in 1:ntripl){
        for(s in 1:nrow(data)){
          if( data[s,Loc[1]] < data[s,Loc[2]]) {
            dataTemp[s,Loc[1]]<-1
          } else {
            dataTemp[s,Loc[1]]<-0}
        }
        for(s in 1:nrow(data)){
          if( data[s,Loc[1]] < data[s,Loc[3]]) {
            dataTemp[s,Loc[2]]<-1
          } else {
            dataTemp[s,Loc[2]]<-0}
        }
        for(s in 1:nrow(data)){
          if( data[s,Loc[2]] < data[s,Loc[3]]) {
            dataTemp[s,Loc[3]]<-1
          } else {
            dataTemp[s,Loc[3]]<-0}
        }
        Loc<-Loc+3
      }}
    if(na){
      for (trl in 1:ntripl){
        for(s in 1:nrow(data)){
          if(is.na(data[s,Loc[1]]) & is.na(data[s,Loc[2]])) {
            dataTemp[s,Loc[1]]<-NA
          } else if(data[s,Loc[1]] %in% data[s,Loc[2]]) {
            dataTemp[s,Loc[1]]<-NA
          } else if (sum(is.na(as.numeric(data[s,c(Loc[1],Loc[2], Loc[3])])))==2 & 2 %in% as.numeric(data[s,c(Loc[1],Loc[2])])) {
            dataTemp[s,Loc[1]]<-NA
          } else if (sum(is.na(as.numeric(data[s,c(Loc[1],Loc[2], Loc[3])])))==1 &
                     ( 2 %in% as.numeric(data[s,Loc[2]]) & 3 %in% as.numeric(data[s,Loc[3]]) |
                       2 %in% as.numeric(data[s,Loc[1]]) & 1 %in% as.numeric(data[s,Loc[3]]))) {
            dataTemp[s,Loc[1]]<-1
          } else if (sum(is.na(as.numeric(data[s,c(Loc[1],Loc[2],Loc[3])])))==1 &
                     ( 2 %in% as.numeric(data[s,Loc[2]]) & 1 %in% as.numeric(data[s,Loc[3]]) |
                       2 %in% as.numeric(data[s,Loc[1]]) & 3 %in% as.numeric(data[s,Loc[3]]))) {
            dataTemp[s,Loc[1]]<-0
          } else if ( ((1 %in% data[s,Loc[1]]) | (3 %in% data[s,Loc[2]]))) {
            dataTemp[s,Loc[1]]<-1
          } else if ( ((1 %in% data[s,Loc[2]]) | (3 %in% data[s,Loc[1]]))) {
            dataTemp[s,Loc[1]]<-0
          } else {dataTemp[s,Loc[1]]<-"error"}
        }
        for(s in 1:nrow(data)){
          if(is.na(data[s,Loc[1]]) & is.na(data[s,Loc[3]])) {
            dataTemp[s,Loc[2]]<-NA
          } else if(data[s,Loc[1]] %in% data[s,Loc[3]]) {
            dataTemp[s,Loc[2]]<-NA
          } else if (sum(is.na(as.numeric(data[s,c(Loc[1],Loc[2], Loc[3])])))==2 & 2 %in% as.numeric(data[s,c(Loc[1],Loc[3])])) {
            dataTemp[s,Loc[2]]<-NA
          } else if (sum(is.na(as.numeric(data[s,c(Loc[1],Loc[2], Loc[3])])))==1 &
                     ( 3 %in% as.numeric(data[s,Loc[2]]) & 2 %in% as.numeric(data[s,Loc[3]]) |
                       2 %in% as.numeric(data[s,Loc[1]]) & 1 %in% as.numeric(data[s,Loc[2]]))) {
            dataTemp[s,Loc[2]]<-1
          } else if (sum(is.na(as.numeric(data[s,c(Loc[1],Loc[2],Loc[3])])))==1 &
                     ( 1 %in% as.numeric(data[s,Loc[2]]) & 2 %in% as.numeric(data[s,Loc[3]]) |
                       2 %in% as.numeric(data[s,Loc[1]]) & 3 %in% as.numeric(data[s,Loc[2]]))) {
            dataTemp[s,Loc[2]]<-0
          } else if ( ((1 %in% data[s,Loc[1]]) | (3 %in% data[s,Loc[3]]))) {
            dataTemp[s,Loc[2]]<-1
          } else if ( ((1 %in% data[s,Loc[3]]) | (3 %in% data[s,Loc[1]]))) {
            dataTemp[s,Loc[2]]<-0
          } else {dataTemp[s,Loc[1]]<-"error"}
        }
        for(s in 1:nrow(data)){
          if(is.na(data[s,Loc[2]]) & is.na(data[s,Loc[3]])) {
            dataTemp[s,Loc[3]]<-NA
          } else if(data[s,Loc[2]] %in% data[s,Loc[3]]) {
            dataTemp[s,Loc[3]]<-NA
          } else if (sum(is.na(as.numeric(data[s,c(Loc[1],Loc[2], Loc[3])])))==2 & 2 %in% as.numeric(data[s,c(Loc[2],Loc[3])])) {
            dataTemp[s,Loc[3]]<-NA
          } else if (sum(is.na(as.numeric(data[s,c(Loc[1],Loc[2], Loc[3])])))==1 &
                     ( 3 %in% as.numeric(data[s,Loc[1]]) & 2 %in% as.numeric(data[s,Loc[3]]) |
                       1 %in% as.numeric(data[s,Loc[1]]) & 2 %in% as.numeric(data[s,Loc[2]]))) {
            dataTemp[s,Loc[3]]<-1
          } else if (sum(is.na(as.numeric(data[s,c(Loc[1],Loc[2],Loc[3])])))==1 &
                     ( 1 %in% as.numeric(data[s,Loc[1]]) & 2 %in% as.numeric(data[s,Loc[3]]) |
                       3 %in% as.numeric(data[s,Loc[1]]) & 2 %in% as.numeric(data[s,Loc[2]]))) {
            dataTemp[s,Loc[3]]<-0
          } else if ( ((1 %in% data[s,Loc[2]]) | (3 %in% data[s,Loc[3]]))) {
            dataTemp[s,Loc[3]]<-1
          } else if ( ((1 %in% data[s,Loc[3]]) | (3 %in% data[s,Loc[2]]))) {
            dataTemp[s,Loc[3]]<-0
          } else {dataTemp[s,Loc[1]]<-"error"}
        }
        Loc<-Loc+3
      } #close loop
    } # close if na
  } # close type rank
  vetorTrpl<-rep(NA, ntripl*3)
  contTrpl<-c(1,2,3)
  for(i in 1:ntripl){
    vetorTrpl[contTrpl[1]]<-paste("i",contTrpl[1],"i",contTrpl[2], sep="")
    vetorTrpl[contTrpl[2]]<-paste("i",contTrpl[1],"i",contTrpl[3], sep="")
    vetorTrpl[contTrpl[3]]<-paste("i",contTrpl[2],"i",contTrpl[3], sep="")
    contTrpl<-contTrpl+3
  } #close loop
  row.names(dataTemp)<-row.names(data)
  colnames(dataTemp)<-vetorTrpl
  return(dataTemp)
}

