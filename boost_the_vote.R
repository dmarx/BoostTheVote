boost_the_vote = function(
  pop_size = 1e3,
  a=2, b=2, # on average, people vote no better than a coin flips.
  boosting_factor = .2, # penalty/bonus for predicting good outcomes
  elections = 10, # number of elections
  churn_rate=.1 # births/deaths voter churn
){
  # Let's simulate "boosting the vote"
  #
  # ~~VOTING POPULATION~~
  voters = rbeta(pop_size, a, b)
  # initialize all voters as having equal contributions to the process
  voting_power = rep(1, pop_size)
  # define a linear penalty/bonus for making a good/bad decision
  penalty = 1 - boosting_factor
  bonus = 1 + boosting_factor
  # after each election, update everyones voting power relative to whether or not they made the right choice
  result = rep(NA, elections)
  voter_age = rep(0, pop_size) # Track the number of elections a voter has participated in
  n_churn = pop_size * churn_rate
  for(i in 1:elections){
    # Run the election
    outcomes = rbinom(pop_size, 1, voters)
    result[i] = sum(voting_power*outcomes)/sum(voting_power)
    # weighted average
    # Update voting power based on results
    voting_power[outcomes==1] = voting_power[outcomes==1] * bonus
    voting_power[outcomes==0] = voting_power[outcomes==0] * penalty
    
    # Kill of oldest voters, replace with fresh voters
    #ix = order(voter_age, ascending=False)[1:n_churn]
    voter_age = voter_age + 1
    ix = sample(pop_size, n_churn, prob = voter_age/sum(voter_age))
    voters[ix] = rbeta(n_churn, a, b)
    voting_power[ix] = 1
  }
  list(voters=voters, voting_power=voting_power,outcomes=result)
}

elections_sensitivity_plot = function(k = 100,
                                      boosting_factors = c(0, 0.2,
                                                           0.5, 0.8), 
                                      #q = c(0.2, 0.5, 0.8), #60% CI
                                      #q = c(0.05, 0.5, 0.95) # 90% CI
                                      conf_intv = .9,
                                      n = 30, 
                                      a = 1, 
                                      b = 1) {
  q_delta = (1-conf_intv)/2
  q = c(q_delta, .5, 1-q_delta)
  votes = lapply(boosting_factors, function(x) {
    v = replicate(k, 
                  boost_the_vote(boosting_factor = x,
                                 elections = n,
                                 a = a, b = b)$outcomes
    )
    apply(v, 1, function(row) {
      quantile(row, q)
    })
  })
  plot_title = paste("Mortality Model: Beta(a=",a,", b=",b,")", sep="")
  plot(0, 0, xlim = c(0, n), ylim = c(0, 1), xlab = "Elections", ylab = "Outcome", main=plot_title)
  for (i in 1:length(votes)) {
    lines(1:n, votes[[i]][1, ], col = i, lty = 2)
    lines(1:n, votes[[i]][2, ], col = i, lty = 1)
    lines(1:n, votes[[i]][3, ], col = i, lty = 2)
  }
}

##################################################

elections_sensitivity_plot(conf_intv=.8)

xv = seq(0,1,.01)
plot(xv, dbeta(xv, 2,5), type='l')
elections_sensitivity_plot(k=100, a=2, b=2, conf_intv=.7)
