data = read.csv('R_Input.csv')

wordlist = data.frame(Paragraph=NULL,Source = NULL, Target = NULL)

for (i in 1:max(data$Paragraph)){
  idx = data[data$Paragraph==i,]
  if(nrow(idx)>1){
  for(j in 1:(nrow(idx)-1)){
    for (k in (j+1):nrow(idx)) {
      rw = c(i,idx$Id[j],idx$Id[k])
      wordlist = rbind(wordlist,rw)
    }
    
  }}
  
}
colnames(wordlist) = c('Paragraph','Source','Target')
wordlist$Type = replicate(nrow(wordlist),'undirected')
wordlist$Weight = replicate(nrow(wordlist),1)

new_list = wordlist[,c(2,3,5)]
new_list2 = aggregate(Weight~Source+Target,data = new_list,FUN = sum)
new_list2$Type = replicate(nrow(new_list2),'undirected')
new_list2 = new_list2[,c(1,2,4,3)]
write.csv(new_list2,'final_data.csv',row.names = F)




