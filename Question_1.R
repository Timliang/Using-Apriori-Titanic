#preparation for algorithm

titanic <- read.table("Dataset.data", header=F)
names(titanic) <- c("Class", "Age", "Sex", "Survived")
minsupport<- 1/10*nrow(titanic)
titanic[,1] <- paste('Class',titanic[,1],sep = '=')
titanic[,2] <- paste('Age',titanic[,2],sep = '=')
titanic[,3] <- paste('Sex',titanic[,3],sep = '=')
titanic[,4] <- paste('Survived',titanic[,4],sep = '=')
tag = c('Class=1st','Class=2nd','Class=3rd','Class=crew')
tag_1 = c('Age=adult','Age=child')
tag_2 = c('Sex=female','Sex=male')
tag_3 = c('Survived=no','Survived=yes')
tag = cbind(tag,tag_1,tag_2,tag_3)
tag[3:4,2:4] = 'NULL'
c<-list()
l<-list()
num=0
Rule <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(Rule) <- c('rule','support','confidence','lift')

# the algorithm
# create the only one rule
for(i in 1:50)
{
  c[[i]]<- data.frame(item=character(0),count=numeric(0))
  l[[i]]<- data.frame(item=character(0),count=numeric(0))
}

for(i in 1:ncol(titanic)){
  for (j in tag[,i]) {
    m<-length(which(titanic[,i]==j))
    c[[1]]<-rbind(c[[1]],cbind(item=j,count=m))    
    if(m>=minsupport)
    {
      l[[1]]<-rbind(l[[1]],cbind(item=j,count=m))
    }
  }
}
print(l[[1]])

# create the rules whose length between 2-50 
for(k in 2:50)
{
  if(k==2){
    for(i in 1:(nrow(l[[k-1]])-1))
    {
      for(j in (i+1):nrow(l[[k-1]]))
      {
        temp1 <- as.character(l[[k-1]][i,1])
        temp2 <- as.character(l[[k-1]][j,1])
        x<-c(temp1,temp2)
        for (col_1 in 1:3){
          for (col_2 in 2:4) {
            m<-length(which(titanic[,col_1]==temp1 & titanic[,col_2]==temp2))
            c[[k]]<-rbind(c[[k]],cbind(item=list(x),count=m))    
            if(m>=minsupport)
            {
              l[[k]]<-rbind(l[[k]],cbind(item=list(x),count=m))
              
              print(paste(x, collapse=", "))
              
            }
          }
          
        }
        
      }  
    }
  }
  else
  {
    
    for(i in 1:(nrow(l[[k-1]])-1))
    {
      for(j in (i+1):nrow(l[[k-1]]))
      {
        a<-l[[k-1]][i,1]$item[-(k-1)]
        b<-l[[k-1]][j,1]$item[-(k-1)]
        if(length(setdiff(a,b))==0)
        { 
          x<- c(a,l[[k-1]][i,1]$item[k-1],l[[k-1]][j,1]$item[k-1])
          sub_data<-titanic
          m = 0
          for(r in 1:nrow(titanic))
          { y <- as.matrix(titanic[r,])
            y <- as.vector(y)
            if (length(setdiff(x,y))==0){
             m = m +1 
            }
          }
          c[[k]]<-rbind(c[[k]],cbind(item=list(x),count=m))    
          if(m>=minsupport)
          {  l[[k]]<-rbind(l[[k]],cbind(item=list(x),count=m))
          print(paste(x, collapse=", "))
          
          }
        }
      }  
    }
  }
  
  if(nrow(l[[k]])==0 | nrow(l[[k]])==1)
  {
    no_of_sets <- k-1 
    break;  
  }
}  
maximal<-function(a,b,k)
{
  
  for(i in 1:nrow(a))
  {
    n<-as.vector(a[i,1])
    for(j in 1:nrow(b))
    {
      m<-as.vector(b[j,1])
      o<-which(n[[1]] %in% m[[1]])
      
      if(length(o)!=0)
        break;
    }
    if(length(o)==0)
    {  
      if(k==1)
        print(paste(a[i,1]))
      else
        print(paste(a[i,1]$item, collapse=", "))
    }
  }
  
}
#rules generation
#exam for the confidence and lift

combine<-function(g,p)
{
  combn(g, p)
}

for(k in 2:no_of_sets)
{
  for(n in 1:nrow(l[[k]]))
  {
    for(i in 1:(k-1))  
    {
      t <- combine(l[[k]][n,1]$item,i)
      for(j in 1:ncol(t))  
      {
        left<-t[,j]
        numerator <- l[[k]][n,2]$count
        
        s<-match(list(left),l[[length(left)]][1]$item)
        
        if(length(left) == 1)
        { denominator <- as.character(l[[length(left)]][s,2])
          denominator <- as.numeric(denominator)
        }
        else{  
          denominator <- as.character(l[[length(left)]][s,2]$count)
          denominator <- as.numeric(denominator)
        }
        
        confidence <- numerator / denominator
        confidence <- confidence
        if(confidence >= 0.8)
        {
          right <- setdiff(l[[k]][n,1]$item,left)
          if(length(left)>0)
            {le <- paste(left, collapse=", ")}
          if(length(right)>0){
            ri <- paste(right, collapse=", ")}
          right_location <- match(list(right),l[[length(right)]][1]$item)
          if(length(right) == 1)
          { right_count <- as.character(l[[length(right)]][right_location,2])
          right_count <- as.numeric(right_count)
          }
          else{  
            right_count <- as.character(l[[length(right)]][right_location,2]$count)
            right_count <- as.numeric(right_count)
          }
          lift <- confidence/(right_count/nrow(titanic))
          rule <- paste(c("{",le,"}", "=>","{",ri,"}"),collapse = '')
          Rule[nrow(Rule) + 1,] <- c(rule,numerator/nrow(titanic),confidence,lift)
          q <- matrix(c(numerator,(denominator-numerator),(right_count-numerator),(nrow(titanic)-denominator-right_count+numerator)),2,2)
          print(q)
        }
      }
    }
  }
  
}
View(Rule)
