# Gambler's Problem
# Solution to Example 4.3 and Exercise 4.8 in Reinforcement Learning by Sutton an Barto
# 
# Written by David Anisman, Feb 2015
#
# Comments and questions : quantizimo@gmail.com
#
# Implemented using in-place Value Iteration.
#

coinHeadsProb = 0.4

states = 1:99

policy = rep(0, length(states))

coinTailsProb = 1 - coinHeadsProb

stateValueFunction = rep(0, length(states))



CalcBestValueAction = function (currState, actions, valueFunction, coinHeadsProb, coinTailsProb){
  
  numActions = length(actions)
  
  actionValues = array(0, dim=c(1, numActions))
  
  
  for (i in 1:numActions){
    
    currAction = actions[i]
    
    nextStateHeads = currState + currAction
    
    nextStateTails = currState - currAction
    
    
    rewardHeads = ifelse(nextStateHeads == 100, 1, 0)
    rewardTails = 0
    
    valueTermHeads = ifelse(rewardHeads == 1, 1, valueFunction[nextStateHeads])
    valueTermTails = ifelse(nextStateTails > 0, valueFunction[nextStateTails], 0)
    
    actionValues[i] = coinHeadsProb * valueTermHeads + coinTailsProb * valueTermTails                                 
  }
  
  maxValue = max(actionValues)
  
  bestAction = actions[which(abs(actionValues - maxValue) < 0.0001)[1]]
  
  result = list()
  result$maxValue = maxValue
  result$bestAction = bestAction
  
  return(result)
}



maxIters = 1000
numIters = 0
delta = 10
tolerance = 0.00000000001

while (numIters < maxIters && delta > tolerance){
  
  numIters = numIters + 1
  
  delta = 0
  for (currState in states){
    
    actions = 1:min(currState, (100 - currState))
    
    saveV = stateValueFunction[currState]
    
    bestValueAction = CalcBestValueAction(currState, actions, stateValueFunction, coinHeadsProb, coinTailsProb)
    
    stateValueFunction[currState] = bestValueAction$maxValue
    
    policy[currState] = bestValueAction$bestAction
    
    delta = max(delta, abs(saveV - stateValueFunction[currState]))
  }
}






plot(policy)
#plot(stateValueFunction)








