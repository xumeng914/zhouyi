guaname <- c("Ǭ","��","����","��׳","С��","��","����","̩",
             "��","��","�","����","����","��","��","��",
             "ͬ��","��","��","��","����","�ȼ�","��","����",
             "����","��","���","��","��","��","��","��",
             "��","���","��","��","��","��","��","��",
             "��","��","δ��","��","��","��","��","ʦ",
             "��","��","��","С��","��","�","��","ǫ",
             "��","��","��","ԥ","��","��","��","��")

yao <- function(x) {
  set.seed(x)
  nn <- 49
  i <- 1
  for (i in 1:3) {
    x <- c(1:nn)
    yuzhi <- runif(1,min=1,max=nn)
    leb <- x>=yuzhi
    u <- length(leb[leb==T])
    v <- length(leb[leb==F])-1
    uy <- u%%4
    if(uy==0) uy <- 4
    vy <- v%%4
    if(vy==0) vy <- 4
    u <- u-uy
    v <- v-vy
    nn <- u+v
    i <- i+1
  }
  
  return(nn/4)
}


#y <- c()
#for (i in 1:10000) {
#y[i] <- yao()
#}
#table(y)

gua <- function(date){
  d <- c()
  d[1]<-as.numeric(substr(date,1,2))
  d[2]<-as.numeric(substr(date,3,4))
  d[3]<-as.numeric(substr(date,5,5))
  d[4]<-as.numeric(substr(date,6,6))
  d[5]<-as.numeric(substr(date,7,7))
  d[6]<-as.numeric(substr(date,8,8))
  yy <- c("������������","����  ����")
  g <- c()
  for(i in 1:6){
    g[i] <- yao(d[i])
    
  }
  zg <- g%%2!=0   #����
  pd <- as.logical((g==9)+(g==6))
  m <- sum(pd)
  bg <- (zg+pd)%%2!=0  #����
  
  xulie<-as.numeric(zg)
  zgn <- 64-(xulie[6]*1+xulie[5]*2+xulie[4]*4+xulie[3]*8+xulie[2]*16+xulie[1]*32)
  xulie<-as.numeric(bg)
  bgn <- 64-(xulie[6]*1+xulie[5]*2+xulie[4]*4+xulie[3]*8+xulie[2]*16+xulie[1]*32)
  
  zgt <- yy[as.numeric(!zg)+1]
  
  #zgtt <- paste(zgt[6],zgt[5],zgt[4],zgt[3],zgt[2],zgt[1],sep="\n")
  
  bgt <- yy[as.numeric(!bg)+1]
  
  #bgtt <- paste(bgt[6],bgt[5],bgt[4],bgt[3],bgt[2],bgt[1],sep="\n")
  
  guaxiang <- data.frame(zg=c(zgt[6],zgt[5],zgt[4],zgt[3],zgt[2],zgt[1]),
                         bg=c(bgt[6],bgt[5],bgt[4],bgt[3],bgt[2],bgt[1]))
  guaxiang$p <- ifelse(pd[6:1],"  ***  ","")
  
  colnames(guaxiang)[1] <- "����"
  colnames(guaxiang)[2] <- "����"
  colnames(guaxiang)[3] <- "��س"
  rownames(guaxiang) <- c("6","5","4","3","2","1")
  result <- list()
  if(m==0){result <- list(gx=guaxiang,n=c(guaname[zgn],guaname[bgn]),"����")}
  if(m==1){result <- list(gx=guaxiang,n=c(guaname[zgn],guaname[bgn]),y=which(pd==1),"����")}
  if(m==2){result <- list(gx=guaxiang,n=c(guaname[zgn],guaname[bgn]),y=which(pd==1),"����")}
  if(m==3){result <- list(gx=guaxiang,n=c(guaname[zgn],guaname[bgn]),"����Ϊ��,�ο�����")}
  if(m==4){result <- list(gx=guaxiang,n=c(guaname[zgn],guaname[bgn]),y=which(pd==0),"����")}
  if(m==5){result <- list(gx=guaxiang,n=c(guaname[zgn],guaname[bgn]),y=which(pd==0),"����")}
  if(m==6){result <- list(gx=guaxiang,n=c(guaname[zgn],guaname[bgn]),"Ǭ�����ã��ο�����")}
  
  return(result)
}


gua(19880914)

