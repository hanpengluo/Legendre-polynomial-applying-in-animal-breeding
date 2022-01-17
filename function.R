# function 1
lengder_stand <- function(x,n){
  xmin <- min(x)
  xmax <- max(x)
  w <- (2*(x-xmin)/(xmax-xmin))-1
  # L0=0.7071*w^0
  # L1=(1.5^0.5)*w^1=1.2247*w
  L1<-1.2247*w
  # L2=[sqrt(45/8)*k^2-sqrt(5/8)]  L2=-0.7906+2.3717*k^2
  L2<--0.7906+2.3717*w^2
  # L3=[sqrt(175/8)*k^3-sqrt(63/8)*k] L3=-2.8062*k+4.6771*k^3
  L3<--2.8062*w+4.6771*w^3
  # L4=0.7955-7.955*k^2+9.2808*k^4
  L4<-0.7955-7.955*w^2+9.2808*w^4
  # L5=4.3973*k-20.5206*k^3+18.4685*k^5
  L5 <- 4.3973*w-20.5206*w^3+18.4685*w^5
  L_all <-cbind.data.frame(w,L1,L2,L3,L4,L5) 
  return(L_all)
}

# function 2
time_matrix<-function(t_min,t_max,t_interval,t_order){
  sequence_t=seq(t_min,t_max,t_interval)
  k=((2*(sequence_t-min(sequence_t)))/(max(sequence_t)-min(sequence_t))-1)
  t_matrix=matrix(data = NA,nrow = length(sequence_t),ncol = 8)
  t_matrix[,1]=sequence_t
  t_matrix[,2]=k                                               # k
  t_matrix[,3]=1                                               #A0
  t_matrix[,4]=1.2247*k                                        #L1
  t_matrix[,5]=-0.7906+2.3717*k^2                              #L2
  t_matrix[,6]=-2.8062*k+4.6771*k^3                            #L3
  t_matrix[,7]=0.7955-7.955*k^2+9.2808*k^4                    #L4
  t_matrix[,8]=4.3973*k-20.5206*k^3+18.4685*k^5               #L5
  #t_matrix<-as.data.frame(t_matrix)
  names(t_matrix)=c("k_w","a","L1","L2","L3","L4","L5")
  t_matrix<-t_matrix[,c(1,3:(t_order+3))]
  return(t_matrix)
}

# function 3

Legendre_plot <- function(len_order_matrix,t_min,t_max,t_interval,t_order){
  if(!require(ggplot2)) install.packages('ggplot2')
  print(paste0('Legendre polynomials order: ',t_order))
  mytheme_gg<-theme(
    plot.subtitle = element_text(vjust = 1,size = 20, hjust = 0.5,colour = "black"),
    plot.caption = element_text(vjust = 1,size = 20, hjust = 0.5,colour = "black"),                                            
    axis.line = element_line(size = 1,colour = "black"),
    axis.ticks = element_line(colour = "black", size = 1),
    axis.title = element_text(size = 20, hjust = 0.5,colour = "black"),
    axis.text = element_text(size = 20,colour = "black", hjust = 0.5),#label of axis
    plot.title = element_text(size = 20, colour = "black",hjust = 0.5),
    legend.text = element_text(size = 15, hjust = 0.5,colour = "black"),
    legend.title = element_text(size = 15, hjust = 0.5,colour = "black"),
    panel.background = element_rect(fill = NA),#backgroud
    plot.background = element_rect(colour = NA), #background
    panel.grid = element_line(colour = "grey90"))# background line
  time_matrix<-function(t_min,t_max,t_interval,t_order){
    sequence_t=seq(t_min,t_max,t_interval)
    k=((2*(sequence_t-min(sequence_t)))/(max(sequence_t)-min(sequence_t))-1)
    t_matrix=matrix(data = NA,nrow = length(sequence_t),ncol = 8)
    t_matrix[,1]=sequence_t
    t_matrix[,2]=k                                               # k
    t_matrix[,3]=1                                               #A0
    t_matrix[,4]=1.2247*k                                        #L1
    t_matrix[,5]=-0.7906+2.3717*k^2                              #L2
    t_matrix[,6]=-2.8062*k+4.6771*k^3                            #L3
    t_matrix[,7]=0.7955-7.955*k^2+9.2808*k^4                    #L4
    t_matrix[,8]=4.3973*k-20.5206*k^3+18.4685*k^5               #L5
    #t_matrix<-as.data.frame(t_matrix)
    names(t_matrix)=c("k_w","a","L1","L2","L3","L4","L5")
    t_matrix<-t_matrix[,c(1,3:(t_order+3))]
    return(t_matrix)
  }
  r_t <- time_matrix(t_min,t_max,t_interval,t_order)
  cur <- cbind.data.frame(Env_variable=r_t[,1],Value=r_t[,3:(t_order+2)]%*%len_order_matrix)
  #names(cur) <- c('Env_variable','Value')
  write.csv(cur,paste0('Legendre polynomials result order_',t_order,'.csv'),row.names = F)
  cur_plot <- ggplot(cur,aes(Env_variable,Value))+geom_point()+geom_line()+theme_bw()+mytheme_gg
  ggsave(paste0('Legendre polynomials result order_',t_order,'.pdf'),cur_plot)
}