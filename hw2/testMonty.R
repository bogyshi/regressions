#this is where i never switch
oDoor=sample(c(1,2,3),100,1)
results=table(oDoor)
myPick=sample(c(1,2,3),100,1)
correct = 0
for(i in 1:100){
  if(oDoor[i]==myPick[i]){
    correct = correct+1
    #this shows the chance I got it right the first time
  }
}
print(correct/100.0)
#this is when I always switch
oDoor=sample(c(1,2,3),100,1)
results=table(oDoor)
myPick=sample(c(1,2,3),100,1)
correct = 0
for(i in 1:100){
  reveal=1:3
  if(oDoor[i]==myPick[i]){
    #i initially picked the correct door, but I am going to switch, so I dont get any correct
  }
  else
  {
    #now I pick the correct door! he revealed the other bogus door and I switch to choose the prize!
    correct=correct + 1
  }
}
print(correct/100.0)