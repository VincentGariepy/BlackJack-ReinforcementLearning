#Project - BlackJack using REINFORCE 

#We will be studying the game of BlackJack
#We will want want to find an optimal policy (when to ask for a card and when not to).
#To get an optimal policy, instead of having a binary take or don't take, we will have a
#Probability of taking a card or not. For this case we will note really need to be epsilon greedy
#Since there will already be exploration

#We will use the REINFORCE Method

#First set up the black jack game like in the assignment
#First write a function to draw a card
GetCard<-function()
{
  value<-0
  value<-sample(1:13,1)
  if(value==1)
  {
    value<-11
  }else if(value<=13 & value>=11)
  {
    value<-10
  }
  return(value)
}

SimulateBlackJackEpisodeStick20<-function(theseed,policy)
{
  #return values
  reward<-0
  cardsplayer<-0
  totalsplayer<-0
  usableacesplayer<-0
  cardsdealer<-0
  totalsdealer<-0
  usableacesdealer<-0
  
  #This is a vector of the action taken
  Take<-rep(0,1)
  
  set.seed(theseed)
  
  #Deal the cards
  cardsplayer<-c(GetCard(),GetCard())
  cardsdealer<-c(GetCard(),GetCard())
  
  #Record the dealer visible card
  visibledealer<-cardsdealer[2]
  
  #Verify the aces
  if(cardsplayer[1]==11 & cardsplayer[2]==11)
  {
    cardsplayer[2]<-1
  }
  if(cardsdealer[1]==11 & cardsdealer[2]==11)
  {
    cardsdealer[2]<-1
  }
  
  #Get the totals 
  totalsplayer<-c(cardsplayer[1],sum(cardsplayer))
  totalsdealer<-c(cardsdealer[1],sum(cardsdealer))
  
  #Get usable aces for player and dealer
  if(cardsplayer[1]==11)
  {
    usableacesplayer<-c(1,1)
  }else if(cardsplayer[2]==11)
  {
    usableacesplayer<-c(0,1)
  }else{
    usableacesplayer<-c(0,0)
  }
  
  if(cardsdealer[1]==11)
  {
    usableacesdealer<-c(1,1)
  }else if(cardsdealer[2]==11)
  {
    usableacesdealer<-c(0,1)
  }else{
    usableacesdealer<-c(0,0)
  }
  
  #Get the probability of asking for a card for a certain state
  i<-2
  #if less than 12 always ask for a card
  if(totalsplayer[i]<12)
  {
    Take<-append(Take,0)
  }else{
    prob<-policy[(totalsplayer[i]-11),(visibledealer-1),(usableacesplayer[i]+1)]
    Take<-append(Take,rbinom(1,1,(1-prob)))
  }
  #If 0 then we take, if 1 stop
  while(Take[i]==0)
  {
    i<-i+1
    #Deal another card and check if it is a second ace, if not check if it is an ace
    #and add 1 to usableaces, if it not an ace then just add same state to the list
    cardsplayer<-c(cardsplayer,GetCard())
    if(cardsplayer[i]==11 & usableacesplayer[i-1] == 1)
    {
      cardsplayer[i]<-1
      usableacesplayer<-c(usableacesplayer,1)
    }else if(cardsplayer[i]==11)
    {
      usableacesplayer<-c(usableacesplayer,1)
    }else{
      usableacesplayer<-c(usableacesplayer,usableacesplayer[i-1])
    }
    
    #if bust and ace is usable then transform to 1
    if(sum(cardsplayer)>21 & usableacesplayer[i] == 1)
    {
      cardsplayer[cardsplayer==11]<-1
      usableacesplayer[i]<-0
    }
    #Add to total
    totalsplayer<-c(totalsplayer,sum(cardsplayer))
    
    if(totalsplayer[i]<12)
    {
      Take<-append(Take,0)
    }else if(totalsplayer[i]>21)
    {
      Take<-append(Take,1)
    }else
    {
      prob<-policy[(totalsplayer[i]-11),(visibledealer-1),(usableacesplayer[i]+1)]
      Take<-append(Take,rbinom(1,1,(1-prob)))
    }
  }
  
  #check if busted else play dealer
  if(tail(totalsplayer, n=1)>21)
  {
    reward<--1
  }else{
    #Take policy of stopping when dealer total is 17 or over
    i<-2
    while(totalsdealer[i]<17)
    {
      i<-i+1
      #Deal another card and check if it is a second ace, if not check if it is an ace
      #and add 1 to usableaces, if it not an ace then just add same state to the list
      cardsdealer<-c(cardsdealer,GetCard())
      if(cardsdealer[i]==11 & usableacesdealer[i-1] == 1)
      {
        cardsdealer[i]<-1
        usableacesdealer<-c(usableacesdealer,1)
      }else if(cardsdealer[i]==11)
      {
        usableacesdealer<-c(usableacesdealer,1)
      }else{
        usableacesdealer<-c(usableacesdealer,usableacesdealer[i-1])
      }
      
      #if bust and ace is usable then transform to 1
      if(sum(cardsdealer)>21 & usableacesdealer[i] == 1)
      {
        cardsdealer[cardsdealer==11]<-1
        usableacesdealer[i]<-0
      }
      #Add to total
      totalsdealer<-c(totalsdealer,sum(cardsdealer))
    }
    #Check if dealer busted
    if(tail(totalsdealer,n=1)>21)
    {
      reward<-1
    }else if(tail(totalsdealer,n=1)>tail(totalsplayer,n=1))
    {
      reward<--1
    }else if(tail(totalsdealer,n=1)<tail(totalsplayer,n=1))
    {
      reward<-1
    }else if(tail(totalsdealer,n=1)==tail(totalsplayer,n=1))
    {
      reward<-0       
    }
  }
  #Get card number when total is 12 or over for first time
  k<-min(which(totalsplayer>11))
  
  lt<-c(totalsplayer[k],visibledealer,usableacesplayer[k],reward,Take[k])
}

#generate 100,000 blackjack games and get the optimal policy
n=500000

#The 3 alphas we will test
alpha<-c(0.1,0.05,0.01)

OptimalPolicy<-array(0.5,dim=c(10,10,2,(n+1),3)) #Start them all at 50% of taking card

#simulate 100,000 blackjack games each with a different seed
for(j in 1:3)
{
  #x=value of player total after 2 cards (12,13,14,...,21)
  #y=value of dealer visible card (2,3,4,..,11)
  #z=usable ace or not after 2 cards (0,1)
  #l=action taken (0,1) (0 being asking for card)
  Nsamplebystate<-array(0,dim=c(10,10,2,2))
  ActionFunctionEstim<-array(0,dim=c(10,10,2,2))
  
  #Take parameters theta to get the policy : p(1)=e^(theta1)/(e^(theta1)+e^(theta2))
  Theta<-array(0,dim=c(10,10,2,2)) #to start with prob of 0.5
  for(i in 1:n)
  {
    #Get V and N matrix of episode i-1
    PreviousQ<-ActionFunctionEstim
    PreviousN<-Nsamplebystate
    
    #Simulate episode
    lt<-SimulateBlackJackEpisodeStick20(i,OptimalPolicy[,,,i,j])
    
    #Get state
    x<-lt[1]-11
    y<-lt[2]-1
    z<-lt[3]+1
    reward<-lt[4]
    l<-lt[5]+1
    
    #Gradient 
    gradientpi<-c((l-2)/(-1)-OptimalPolicy[x,y,z,i,j],(l-1)-(1-OptimalPolicy[x,y,z,i,j]))
    
    #Update the theta
    Theta[x,y,z,]<-Theta[x,y,z,]+alpha[j]*reward*gradientpi
    
    #Update Q and N matrix for state and action picked
    #ActionFunctionEstim[x,y,z,l]<-PreviousQ[x,y,z,l]+(1/(PreviousN[x,y,z,l]+1))*(reward-PreviousQ[x,y,z,l])
    #Nsamplebystate[x,y,z,l]<-PreviousN[x,y,z,l]+1
    
    #copy the previous results to new time step the update the new value fo state visited
    OptimalPolicy[,,,(i+1),j]<-OptimalPolicy[,,,i,j]
    #Update the optimal policy (probability of picking card) for the state visited
    OptimalPolicy[x,y,z,(i+1),j]<-exp(Theta[x,y,z,1])/(exp(Theta[x,y,z,1])+exp(Theta[x,y,z,2]))
  }
}
#Now let us find the optimal alpha, let us look at the convergence of the optimal policy
#Let us compare three values of alpha for the case where your total is 15, the dealer visible card is 6
#And you have don't have a visible ace
#We will look at alpha=0.1,0.01,0.001
plot(1,main='Convergence of optimal policy when player total is 15\n and visible card is 6 with usable ace'
     ,xlab='Episode',ylab='Percentage of asking for card',xlim=c(0,n),yli=c(0,100))
lines(100*OptimalPolicy[4,5,2,,1],type='l',col='blue')
lines(100*OptimalPolicy[4,5,2,,2],type='l',col='red')
lines(100*OptimalPolicy[4,5,2,,3],type='l',col='purple')
legend(0, 20, legend=c('alpha=0.1', 'alpha=0.05', 'alpha=0.01'),col=c("blue", "red", 'purple'), lty=1:2, cex=0.8)

plot(1,main='Convergence of optimal policy when player total is 21\n and visible card is 3 with no usable aces',
     xlab='Episode',ylab='Percentage of asking for card',xlim=c(0,n),yli=c(0,100))
lines(100*OptimalPolicy[10,2,1,,1],type='l',col='blue')
lines(100*OptimalPolicy[10,2,1,,2],type='l',col='red')
lines(100*OptimalPolicy[10,2,1,,3],type='l',col='purple')
legend(70, 20, legend=c('alpha=0.1', 'alpha=0.05', 'alpha=0.01'),col=c("blue", "red", 'purple'), lty=1:2, cex=0.8)

plot(1,main='Convergence of optimal policy when player total is 12\n and visible card is 9 with no usable aces',
     xlab='Episode',ylab='Percentage of asking for card',xlim=c(0,n),yli=c(0,100))
lines(100*OptimalPolicy[1,8,1,,1],type='l',col='blue')
lines(100*OptimalPolicy[1,8,1,,2],type='l',col='red')
lines(100*OptimalPolicy[1,8,1,,3],type='l',col='purple')
legend(0, 20, legend=c('alpha=0.1', 'alpha=0.05', 'alpha=0.01'),col=c("blue", "red", 'purple'), lty=1:2, cex=0.8)


#From this graph it seems clear that the alpha of 0.05 is the best so lets delete the rest
OptimalPolicyNew<-OptimalPolicy[,,,,2]
#Make some space on ram
rm(OptimalPolicy)
gc()

#Now let us look at optimal policy, we will look at when what is the optimal policy
#to ask vs not ask for a card for each of your totals, we will look at dealer visible card 5 and 10
#and usable and non usable aces to get a general picture with. We will also look at the over average with no usable ace
policy5nousable<-matrix(0,nrow=2,ncol=10)
colnames(policy5nousable)<-c('12','13','14','15','16','17','18','19','20','21')
policy5usable<-matrix(0,nrow=2,ncol=10)
colnames(policy5usable)<-c('12','13','14','15','16','17','18','19','20','21')
policy10nousable<-matrix(0,nrow=2,ncol=10)
colnames(policy10nousable)<-c('12','13','14','15','16','17','18','19','20','21')
policy10usable<-matrix(0,nrow=2,ncol=10)
colnames(policy10usable)<-c('12','13','14','15','16','17','18','19','20','21')
policyallnousable<-matrix(0,nrow=2,ncol=10)
colnames(policyallnousable)<-c('12','13','14','15','16','17','18','19','20','21')
policyallusable<-matrix(0,nrow=2,ncol=10)
colnames(policyallusable)<-c('12','13','14','15','16','17','18','19','20','21')

#Get the percentages of optimal policy
policy5nousable[1,]<-OptimalPolicyNew[,4,1,(n+1)]
policy5nousable[2,]<-(1-OptimalPolicyNew[,4,1,(n+1)])

#Get the percentages of optimal policy
policy5usable[1,]<-OptimalPolicyNew[,4,2,(n+1)]
policy5usable[2,]<-(1-OptimalPolicyNew[,4,2,(n+1)])

#Get the percentages of optimal policy
policy10nousable[1,]<-OptimalPolicyNew[,9,1,(n+1)]
policy10nousable[2,]<-(1-OptimalPolicyNew[,9,1,(n+1)])

#Get the percentages of optimal policy
policy10usable[1,]<-OptimalPolicyNew[,9,2,(n+1)]
policy10usable[2,]<-(1-OptimalPolicyNew[,9,2,(n+1)])

for(i in 1:10)
{
  #Get the percentages of optimal policy
  policyallnousable[1,i]<-mean(OptimalPolicyNew[i,,1,(n+1)])
  policyallnousable[2,i]<-mean((1-OptimalPolicyNew[i,,1,(n+1)]))
  
  policyallusable[1,i]<-mean(OptimalPolicyNew[i,,2,(n+1)])
  policyallusable[2,i]<-mean((1-OptimalPolicyNew[i,,2,(n+1)]))
}

#Transform totals to percentages to do nice stacked barplot
data_percentage5nouse<-apply(policy5nousable, 2, function(x){x*100/sum(x,na.rm=T)})
data_percentage5use<-apply(policy5usable, 2, function(x){x*100/sum(x,na.rm=T)})
data_percentage10nouse<-apply(policy10nousable, 2, function(x){x*100/sum(x,na.rm=T)})
data_percentage10use<-apply(policy10usable, 2, function(x){x*100/sum(x,na.rm=T)})
data_percentageallnouse<-apply(policyallnousable, 2, function(x){x*100/sum(x,na.rm=T)})
data_percentagealluse<-apply(policyallusable, 2, function(x){x*100/sum(x,na.rm=T)})


barplot(data_percentage5nouse, col=c('green','red'), main='When to ask for card if no usable aces and dealer total 5',
        xlab='Player total', ylab='Percentage of counts it is \noptimal to ask for a card')
barplot(data_percentage5use, col=c('green','red'), main='When to ask for card if 1 usable ace and dealer total 5',
        xlab='Player total', ylab='Percentage of counts it is \noptimal to ask for a card')
barplot(data_percentage10nouse, col=c('green','red'), main='When to ask for card if no usable aces and dealer total 10',
        xlab='Player total', ylab='Percentage of counts it is \noptimal to ask for a card')
barplot(data_percentage10use, col=c('green','red'), main='When to ask for card if 1 usable ace and dealer total 10',
        xlab='Player total', ylab='Percentage of counts it is \noptimal to ask for a card')
barplot(data_percentageallnouse, col=c('green','red'), main='When to ask for card if no usable aces',
        xlab='Player total', ylab='Percentage of counts it is \noptimal to ask for a card')
barplot(data_percentagealluse, col=c('green','red'), main='When to ask for card if 1 usable ace',
        xlab='Player total', ylab='Percentage of counts it is \noptimal to ask for a card')


