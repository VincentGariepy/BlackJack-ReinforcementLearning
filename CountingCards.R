#Project
#Vincent Gariepy

#We will be studying the game of BlackJack
#We will want want to find an optimal policy (when to ask for a card and when not to).
#However, compared to assignment we will also take into consideration counting cards. The method used to count cards 
#is explained in the report.

#First we must simulate a game of blackjack, we will only consider the dealer and player.
#Most casino's will use between 4 and 8 decks for this, we will use 6 for our simulation.
#If there are 15 or less cards in the deck the simulation will end.

#First write a function to simulate the whole 6 decks a card
#cards will be (2,11), 11 being ace, jack/queen/king being 10 
GetDeck<-function(theseed,numdeck)
{
  set.seed(theseed)
  numcards<-52*numdeck
  unshuffleddeck<-rep(c(2,3,4,5,6,7,8,9,10,10,10,10,11),(numcards/13))
  
  #sample 1 to 312 without replacement to shuffle the decks
  deck<-rep(0,numcards)
  ordercards<-sample(numcards,numcards,replace=F)
  
  deck<-unshuffleddeck[ordercards]
  
  return(deck)
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

#Create a function to add to the count tally
counting<-function(count,card)
{
  newcount<-0
  #if card between 2 to 6 then add 1 to count
  if(card<=6 & card!=1)
  {
    newcount<-count+1  
  }
  #If card value 10 or an ace substract 1 to count
  if(card>=10 | card==1)
  {
    newcount<-count-1
  }
  return(newcount)
}

#To play the game of blackjack we must have a policy.
#In this case the policy will be an array [i,j,k,l]
#in each case of value we will indicate whether to ask for card (1) or not (0)
#i:total of your cards between 11 and 21
#j:value of card seen from dealer between 2 and 11
#k:usable ace (1 for yes, 0 for no)
#l:card count, to simplify we will use 0.5 increment estimate between -10 to 10, 
# this will be a value already divided by number of decks
# in the vector 1 will correspond to -10, 2 to -9.5, 3 to -9, etc so (0,41)

#This function will return a list with the following vectors
#each increment in the vector will be a different game but using the same 6 decks
#totaslplayer : the total of the player when it is over 12
#visibledealers : the value of the visible card of the dealer
#usableaces : if there is a usable ace when the value of total of player is taken
#cardcounts : the count of cards divided by number of decks, rounded to nearest 0.5,
#             this will be the value of the count at the beginning of each game
#actions : the action taken in each state, follows the policy
#rewards : reward of the game -1 if loss, 0 if draw, 1 if win
SimulateBlackJackEpisodes<-function(theseed,numdeck,policy,epsilon)
{
  #return values (vectors)
  rewards<-vector()
  totalsplayer<-vector()
  visibledealers<-vector()
  usableaces<-vector()
  actions<-vector()
  cardcounts<-vector()
  
  #set seed for consistency
  set.seed(theseed)
  
  #Get the deck
  deck<-GetDeck(theseed,numdeck)
  
  #Start the count of cards at 0
  count<-0
  
  #play the games until there are 15 or less cards left before you start a game
  numcards<-0
  #Loop to go through deck
  while(length(deck)-numcards>15)
  {
    #Now we will go through one game
  
    #Temp variable only for this game
    tempreward<-0
    tempcardsplayer<-0
    temptotalsplayer<-0
    tempusableacesplayer<-0
    tempcardsdealer<-0
    temptotalsdealer<-0
    tempusableacesdealer<-0
    limit<-0
    
    #Record the actual count at the beginning of the game
    decksleft<-max(c(1,(length(deck)-numcards)/52))
    tempactualcount<-round((count/decksleft)/0.5)*0.5
    
    #To limit the number of state we will limit it to -3,+3
    if(tempactualcount<(-3))
    {
      tempactualcount<-(-3)
    }else if(tempactualcount>3)
    {
      tempactualcount<-3
    }
    
    #Give cards 
    tempcardsplayer<-c(deck[numcards+1],deck[(numcards+3)])
    tempcardsdealer<-c(deck[(numcards+2)],deck[(numcards+4)])
    numcards<-numcards+4
    
    #Record value of seen card
    tempvisibledealer<-tempcardsdealer[2]
    
    #Verify the aces
    if(tempcardsplayer[1]==11 & tempcardsplayer[2]==11)
    {
      tempcardsplayer[2]<-1
    }
    if(tempcardsdealer[1]==11 & tempcardsdealer[2]==11)
    {
      tempcardsdealer[2]<-1
    }
    
    #Get the totals 
    temptotalsplayer<-c(tempcardsplayer[1],sum(tempcardsplayer))
    temptotalsdealer<-c(tempcardsdealer[1],sum(tempcardsdealer))
    
    #Get usable aces for player and dealer
    if(tempcardsplayer[1]==11)
    {
      tempusableacesplayer<-c(1,1)
    }else if(tempcardsplayer[2]==11)
    {
      tempusableacesplayer<-c(0,1)
    }else{
      tempusableacesplayer<-c(0,0)
    }
    
    if(tempcardsdealer[1]==11)
    {
      tempusableacesdealer<-c(1,1)
    }else if(tempcardsdealer[2]==11)
    {
      tempusableacesdealer<-c(0,1)
    }else{
      tempusableacesdealer<-c(0,0)
    }
    
    #Take policy given depending on state
    i<-2
    #If we are below 12 we ask for a card for sure 
    if(temptotalsplayer[i]<12)
    {
      limit<-append(limit,1)
    }else
    {
      #Check if we explore or use optimal policy
      templimit<-policy[(temptotalsplayer[i]-11),(tempvisibledealer-1),(tempusableacesplayer[i]+1),(2*(tempactualcount)+7)]
      #Use 1-e+e/A(s) and binomial, so if 1 explore else use optimal
      ep<-epsilon-epsilon/2
      choice<-rbinom(n=1,size=1,prob=ep)
      if(choice==1)
      {
        #If explore we use abs(optimal-1), so if 1 you get |1-3|=2 and if 2 you get |2-3|=1
        limit<-append(limit,abs(templimit-3))
      }else
      {
        limit<-append(limit,templimit)
      }
    }
    #play until policy says to stop, or you bust
    while(tail(limit,n=1)==1)
    {
      i<-i+1
      numcards<-numcards+1
      #Deal another card and check if it is a second ace, if not check if it is an ace
      #and add 1 to usableaces, if it not an ace then just add same state to the list
      tempcardsplayer<-c(tempcardsplayer,deck[numcards])
      if(tempcardsplayer[i]==11 & tempusableacesplayer[i-1] == 1)
      {
        tempcardsplayer[i]<-1
        tempusableacesplayer<-c(tempusableacesplayer,1)
      }else if(tempcardsplayer[i]==11)
      {
        tempusableacesplayer<-c(tempusableacesplayer,1)
      }else{
        tempusableacesplayer<-c(tempusableacesplayer,tempusableacesplayer[i-1])
      }
      
      #if bust and ace is usable then transform to 1
      if(sum(tempcardsplayer)>21 & tempusableacesplayer[i] == 1)
      {
        tempcardsplayer[tempcardsplayer==11]<-1
        tempusableacesplayer[i]<-0
      }
      #Add to total
      temptotalsplayer<-c(temptotalsplayer,sum(tempcardsplayer))
      
      #If we are below 12 we ask for a card for sure, if above 21 we stop 
      if(temptotalsplayer[i]<12)
      {
        limit<-append(limit,1)
      }else if(temptotalsplayer[i]>21)
      {
        limit<-append(limit,2)
      }else
      {
        #Check if we explore or use optimal policy
        templimit<-policy[(temptotalsplayer[i]-11),(tempvisibledealer-1),(tempusableacesplayer[i]+1),(2*(tempactualcount)+7)]
        #Use 1-e+e/A(s) and binomial, so if 1 explore else use optimal
        ep<-epsilon-epsilon/2
        choice<-rbinom(n=1,size=1,prob=ep)
        if(choice==1)
        {
          #If explore we use abs(optimal-1), so if 1 you get |1-3|=2 and if 2 you get |2-3|=1
          limit<-append(limit,abs(templimit-3))
        }else
        {
          limit<-append(limit,templimit)
        }
      }
    }
    
    #check if busted else play dealer
    if(tail(temptotalsplayer, n=1)>21)
    {
      #if busted we will count the cards we can see(not the first card of dealer)
      tempreward<--1
      
      count<-counting(count,tempcardsdealer[2])
      
      for(j in 1:length(tempcardsplayer))
      {
        count<-counting(count,tempcardsplayer[j])
      }
      
    }else{
      #Take policy of stopping when dealer total is 17 or over
      i<-2
      while(temptotalsdealer[i]<17)
      {
        i<-i+1
        numcards<-numcards+1
        #Deal another card and check if it is a second ace, if not check if it is an ace
        #and add 1 to usableaces, if it not an ace then just add same state to the list
        tempcardsdealer<-c(tempcardsdealer,deck[numcards])
        if(tempcardsdealer[i]==11 & tempusableacesdealer[i-1] == 1)
        {
          tempcardsdealer[i]<-1
          tempusableacesdealer<-c(tempusableacesdealer,1)
        }else if(tempcardsdealer[i]==11)
        {
          tempusableacesdealer<-c(tempusableacesdealer,1)
        }else{
          tempusableacesdealer<-c(tempusableacesdealer,tempusableacesdealer[i-1])
        }
        
        #if bust and ace is usable then transform to 1
        if(sum(tempcardsdealer)>21 & tempusableacesdealer[i] == 1)
        {
          tempcardsdealer[tempcardsdealer==11]<-1
          tempusableacesdealer[i]<-0
        }
        #Add to total
        temptotalsdealer<-c(temptotalsdealer,sum(tempcardsdealer))
      }
      
      #Check if dealer busted, if not get who won
      if(tail(temptotalsdealer,n=1)>21)
      {
        tempreward<-1
      }else if(tail(temptotalsdealer,n=1)>tail(temptotalsplayer,n=1))
      {
        tempreward<--1
      }else if(tail(temptotalsdealer,n=1)<tail(temptotalsplayer,n=1))
      {
        tempreward<-1
      }else if(tail(temptotalsdealer,n=1)==tail(temptotalsplayer,n=1))
      {
        tempreward<-0       
      }
      
      #Count the cards, this time you can see all the cards of the dealer
      for(j in 1:length(tempcardsplayer))
      {
        count<-counting(count,tempcardsplayer[j])
      }
      for(j in 1:length(tempcardsdealer))
      {
        count<-counting(count,tempcardsdealer[j])
      }
    }
    #Now we will update our result vectors, 
    #we will not just consider the first time the player total is 12 or more, but everytime it is over 12
    
    #Get card number when total is 12 or over, and then when it is over 21
    k<-min(which(temptotalsplayer>11))
    p<-max(which(temptotalsplayer<=21))
  
    for(j in k:p)
    {
      totalsplayer<-append(totalsplayer,temptotalsplayer[j])
      visibledealers<-append(visibledealers,tempvisibledealer)
      usableaces<-append(usableaces,tempusableacesplayer[j])
      cardcounts<-append(cardcounts,tempactualcount)
      rewards<-append(rewards,tempreward)
      
      actions<-append(actions,limit[j])
    }
  }
  lt<-list(totalsplayer,visibledealers,usableaces,cardcounts,actions,rewards)
}

#Num of decks is 6
numdeck<-6
#Number of 6 deck simulations
n<-10000
#Epsilon for exploration, we want to explore a lot at the begining then slowly reduce it
e<-0.5

#Set initial policy to ask for cards in each state
#Optimal policy array, this will be kept updated with action who has maxQ
optimalpolicy<-array(1,dim=c(10,10,2,13))

#x=value of player total after 2 cards (12,13,14,...,21)
#y=value of dealer visible card (2,3,4,..,11)
#z=usable ace or not after 2 cards (0,1)
#k=value of actual count (-10,10) by 0.5 increments
#l=action, 0 for stop and 1 for ask for card
Nsamplebystate<-array(0,dim=c(10,10,2,13,2))
ActionFunctionEstim<-array(0,dim=c(10,10,2,13,2))
StateFunctionEstim<-array(0,dim=c(10,10,2,13,n))

#simulate 10,000 blackjack games each with a different seed
for(i in 1:n)
{
  #for the first half of simulation explore with epsilon 0.5, then every 1/10 reduce it by 0.1
  if(i==(n/2+1))
  {
    e<-0.4
  }else if(i==(n/2+1+n/10))
  {
    e<-0.3
  }else if(i==(n/2+1+2*n/10))
  {
    e<-0.2
  }
  else if(i==(n/2+1+3*n/10))
  {
    e<-0.1
  }else if(i==(n/2+1+4*n/10))
  {
    e<-0
  }
  #Simulate episodes with all different actions (we will use e-greedy policy to get the action)
  lt<-SimulateBlackJackEpisodes(i,numdeck,optimalpolicy,e)

  totalsplayer<-lt[[1]]
  visibledealer<-lt[[2]]
  usableacesplayer<-lt[[3]]
  cardcounts<-lt[[4]]
  actions<-lt[[5]]
  rewards<-lt[[6]]
  
  #initialise the temp state function for this episode
  if(i!=1)
  {
    tempstatefunction<-StateFunctionEstim[,,,,(i-1)]
  }else
  {
    tempstatefunction<-StateFunctionEstim[,,,,1]
  }

  
  #Loop through every step of the episode and update function
  for(j in 1:(length(rewards)-1))
  {
    #Get Q and N matrix of episode i-1
    PreviousQ<-ActionFunctionEstim
    PreviousN<-Nsamplebystate
    
    #Get state action pairs as indexes for the array
    x<-totalsplayer[j]-11
    y<-visibledealer[j]-1
    z<-usableacesplayer[j]+1
    k<-2*cardcounts[j]+7
    l<-actions[j]
    

    #Update Q and N matrix for state picked
    Nsamplebystate[x,y,z,k,l]<-PreviousN[x,y,z,k,l]+1
    ActionFunctionEstim[x,y,z,k,l]<-PreviousQ[x,y,z,k,l]+(rewards[j]-PreviousQ[x,y,z,k,l])/Nsamplebystate[x,y,z,k,l]
    
    #Update the temp state function estimate for this step
    tempstatefunction[x,y,z,k]<-max(ActionFunctionEstim[x,y,z,k,1:2])
    
    #Update the optimal policy and state value function
    optimalpolicy[x,y,z,k]<-which.max(ActionFunctionEstim[x,y,z,k,1:2])
  }

  StateFunctionEstim[,,,,i]<-tempstatefunction
}

#Let us see the number of visits per card count
Ncount<-matrix(0,nrow=2,ncol=13)
for(i in 1:13)
{
  Ncount[1,i]<-sum(Nsamplebystate[,,,i,])
}
Ncount[2,]<-c(-3,-2.5,-2,-1.5,-1,-0.5,0,0.5,1,1.5,2,2.5,3)

barplot(Ncount[1,],main='Number of visits to each count of cards',xlab='Card count',ylab='Number of visits',
        names.arg=c('-3','-2.5','-2','-1.5','-1','-0.5','0','0.5','1','1.5','2','2.5','3'))

#Now let us average the state value function estimate for each card count and see if there is 
#card count which is advantageous to us
avgSVF<-matrix(0,nrow=2,ncol=13)
for(i in 1:13)
{
  avgSVF[1,i]<-mean(StateFunctionEstim[,,,i,n])
}
avgSVF[2,]<-c(-3,-2.5,-2,-1.5,-1,-0.5,0,0.5,1,1.5,2,2.5,3)

plot(avgSVF[2,],avgSVF[1,],main='Average State value function for each count of cards',
     xlab='Card count',ylab='Average State value function estimate')


#Now we will replicate the graph from assignment 2, with and without usable aces
#and with count of 0, we can see we get a similar graph
library(plot3D)
gridPlayerTotal = 12:21
gridDealerTotal = 2:11
M <- mesh(gridDealerTotal, gridPlayerTotal)
surf3D(x = M$x, y = M$y, z = t(StateFunctionEstim[,,1,7,n]),
       xlab="Dealer total",ylab="Player total", zlab="Reward",
       theta=-60, phi=25, bty="g", ticktype="detailed", colkey=FALSE,
       zlim=range(-1,1), main="Reward for player depending on dealer and player \ntotal, when no usable aces")

surf3D(x = M$x, y = M$y, z = t(StateFunctionEstim[,,2,7,n]),
       xlab="Dealer total",ylab="Player total", zlab="Reward",
       theta=-60, phi=25, bty="g", ticktype="detailed", colkey=FALSE,
       zlim=range(-1,1), main="Reward for player depending on dealer and player \ntotal, when one usable ace")

#Now let us look at optimal policy, we will look at when what percentage of card counts is it
#optimal to ask vs not ask for a card for each of your totals, we will look at dealer visible card 5 and 10
#and usable and non usable aces to get a general picture with. We will also look at the over average with no usable ace
policy7nousable<-matrix(0,nrow=2,ncol=10)
colnames(policy7nousable)<-c('12','13','14','15','16','17','18','19','20','21')
policy7usable<-matrix(0,nrow=2,ncol=10)
colnames(policy7usable)<-c('12','13','14','15','16','17','18','19','20','21')
policy10nousable<-matrix(0,nrow=2,ncol=10)
colnames(policy10nousable)<-c('12','13','14','15','16','17','18','19','20','21')
policy10usable<-matrix(0,nrow=2,ncol=10)
colnames(policy10usable)<-c('12','13','14','15','16','17','18','19','20','21')
policyallusable<-matrix(0,nrow=2,ncol=10)
colnames(policyallusable)<-c('12','13','14','15','16','17','18','19','20','21')

for(i in 1:10)
{
  #Get the number of 1 and the number of 2 in optimal policy
  policy7nousable[1,i]<-sum(optimalpolicy[i,6,1,]==1)
  policy7nousable[2,i]<-sum(optimalpolicy[i,6,1,]==2)
  
  #Get the number of 1 and the number of 2 in optimal policy
  policy7usable[1,i]<-sum(optimalpolicy[i,6,2,]==1)
  policy7usable[2,i]<-sum(optimalpolicy[i,6,2,]==2)
  
  #Get the number of 1 and the number of 2 in optimal policy
  policy10nousable[1,i]<-sum(optimalpolicy[i,9,1,]==1)
  policy10nousable[2,i]<-sum(optimalpolicy[i,9,1,]==2)
  
  #Get the number of 1 and the number of 2 in optimal policy
  policy10usable[1,i]<-sum(optimalpolicy[i,9,2,]==1)
  policy10usable[2,i]<-sum(optimalpolicy[i,9,2,]==2)
  
  #Get the number of 1 and the number of 2 in optimal policy
  policyallusable[1,i]<-sum(optimalpolicy[i,,2,]==1)
  policyallusable[2,i]<-sum(optimalpolicy[i,,2,]==2)
  
}
#Transform totals to percentages to do nice stacked barplot
data_percentage7nouse<-apply(policy7nousable, 2, function(x){x*100/sum(x,na.rm=T)})
data_percentage7use<-apply(policy7usable, 2, function(x){x*100/sum(x,na.rm=T)})
data_percentage10nouse<-apply(policy10nousable, 2, function(x){x*100/sum(x,na.rm=T)})
data_percentage10use<-apply(policy10usable, 2, function(x){x*100/sum(x,na.rm=T)})
data_percentagealluse<-apply(policyallusable, 2, function(x){x*100/sum(x,na.rm=T)})

barplot(data_percentage7nouse, col=c('green','red'), main='When to ask for card if no usable aces and dealer total 7',
        xlab='Player total', ylab='Percentage of counts it is \noptimal to ask for a card')
barplot(data_percentage7use, col=c('green','red'), main='When to ask for card if 1 usable ace and dealer total 7',
        xlab='Player total', ylab='Percentage of counts it is \noptimal to ask for a card')
barplot(data_percentage10nouse, col=c('green','red'), main='When to ask for card if no usable aces and dealer total 10',
        xlab='Player total', ylab='Percentage of counts it is \noptimal to ask for a card')
barplot(data_percentage10use, col=c('green','red'), main='When to ask for card if 1 usable ace and dealer total 10',
        xlab='Player total', ylab='Percentage of counts it is \noptimal to ask for a card')
barplot(data_percentagealluse, col=c('green','red'), main='When to ask for card if 1 usable ace',
        xlab='Player total', ylab='Percentage of counts it is \noptimal to ask for a card')

#Now let us look at the convergence of the state value function over the n episodes
#We will average across all totals for player and dealer and usable aces.
#We will compare the count of 0 vs count of 1 and -1, also average across all counts
avgSVF<-matrix(0,nrow=4,ncol=n)
for(i in 1:n)
{
  avgSVF[1,i]<-mean(StateFunctionEstim[,,,7,i])
  avgSVF[2,i]<-mean(StateFunctionEstim[,,,5,i])
  avgSVF[3,i]<-mean(StateFunctionEstim[,,,9,i])
  avgSVF[4,i]<-mean(StateFunctionEstim[,,,,i])
}

plot(1,main='Convergence of state value function', xlab='Episode',ylab='Average State value function estimate',
     xlim=c(0,n),ylim=c(-0.05,0.15))
lines(avgSVF[1,], type='l', col='green', lwd=1)
lines(avgSVF[2,], type='l', col='blue', lwd=1)
lines(avgSVF[3,], type='l', col='red', lwd=1)
lines(avgSVF[4,], type='l',col='purple', lwd=1)
legend((n-3*n/10), 0.15, legend=c('Count = 0', 'count = -1', 'count = 1', 'All counts'),col=c("green","blue", "red", 'purple'), lty=1:2, cex=0.8)
