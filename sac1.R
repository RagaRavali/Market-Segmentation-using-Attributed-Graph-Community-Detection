#Collaborators : Raga Ravali Pothireddy - rpothir, Abhinav Pothuganti-apothug
					

library(igraph)
library('proxy')


##inspired from stack overflow
#args <- commandArgs(trailingOnly = TRUE)
#alpha=as.numeric(args[1])

alpha_val = 0

######getting data###change the data path here
edges_path <- "E:/BI/community/06.Topic-7.Project-6.MarketSegmentation.AttributedGraphCommunityDetection/data/fb_caltech_small_edgelist.txt"
graph <- read.graph(file=edges_path, format = c("edgelist"))

atr_path <- "E:/BI/community/06.Topic-7.Project-6.MarketSegmentation.AttributedGraphCommunityDetection/data/fb_caltech_small_attrlist.csv"
atr<- read.csv(atr_path)

community <- 1:vcount(graph)

##modularity value
modularity_val <- function(j, graph_temp, community, i){
  
  community[i] <- j
  modularity(graph_temp, community)
}


##cosine similarity..directly inspired from stack overflow using dist method.
similarity_cos <- function(j, data, community, i){
  
  whi_comm = community[which(community == j)]
  whi_comm = apply(data[whi_comm,], 2, mean)    ##average of distance in a community        
  dist(rbind(data[i,], whi_comm), method="cosine")
}

##using only 15 iterations
for(x in 1:15){
  print(x)
  print(length(unique(community)))
  total_comm = 0
  for(i in 1:vcount(graph)){
    
    modularity_old<-modularity(graph,membership = community)##igraph doc
    neigh<-neighbors(graph,i)
    
    if(length(neigh)){
      ##calculated delta from paper as give...but sapply function is inspired from peterwerner.blogspot.
      
      simil_cos = sapply(unique(community[neigh]), similarity_cos , atr, community, i)
      vect = sapply(unique(community[neigh]), modularity_val , graph, community, i)
      delta = alpha_val * (vect - modularity_old) + ((1 - alpha_val) * simil_cos)
      
      max_delta <- max(delta)
      if(max_delta > 0){
        uniq <- unique(community[neigh])
        total_comm = total_comm + 1
        
        community[i] <- uniq[which.max(delta)]
      }  
    }
  }
}


prev_comm=community
# using only 15 iterations
for(i in 1:15)
{
  for(j in 1:15){
    print(j)
    print(length(unique(community)))
    total_comm <- 0
    for(k in 1:vcount(graph)){
      modularity_old<-modularity(graph,membership = community)
      neigh<-neighbors(graph,j)
      
      if(length(neigh)){
        simil_cos = sapply(unique(community[neigh]), similarity_cos , atr, community, k)
        vect = sapply(unique(community[neigh]), modularity_val , graph, community, k)
        delta = alpha_val * (vect - modularity_old) + ((1 - alpha_val) * simil_cos)
        max_delta <- max(delta)
        if(max_delta > 0){
          uniq <- unique(community[neigh])
          
          total_comm = total_comm + 1
          
          community[k] <- uniq[which.max(delta)]
        }  
      }
    }
  }
  if(isTRUE(all.equal(prev_comm,community)))
  {
    break
  }
  prev_comm=community
}

####writing into file


fil<-paste("communities",alpha_val,sep="_")
fil<-paste(fileName,"txt",sep=".")
final_file<-file(fil,"w")


for(i in 1:length(unique(community)))
{
  final <- vector("numeric")
  
  
  for(j in 1:vcount(graph))
  {
    
    if(community[j]==unique(community)[i]){
      
      final <- append(final, j,  after = length(final))
    }
  }
  cat(as.character(final), file=final_file,sep = ",")
  cat("\n", file=final_file)
}

close(fileDes)

