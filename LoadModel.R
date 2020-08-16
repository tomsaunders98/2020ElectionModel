library(rstan)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(lubridate)
library(ggplot2)

rstan_options(javascript = FALSE)
########################################
# Useful functions (Taken from PKremp) #
########################################


corr_matrix <- function(m){
  (diag(m)^-.5 * diag(nrow = nrow(m))) %*% m %*% (diag(m)^-.5 * diag(nrow = nrow(m))) 
}

cov_matrix <- function(n, sigma2, rho){
  m <- matrix(nrow = n, ncol = n)
  m[upper.tri(m)] <- rho
  m[lower.tri(m)] <- rho
  diag(m) <- 1
  (sigma2^.5 * diag(n))  %*% m %*% (sigma2^.5 * diag(n))
}

logit <- function(x) log(x/(1-x))

inv_logit <- function(x) 1/(1 + exp(-x))

#######################
# Important Variables #
#######################


PredictElection <- 2012

TimeForChange <- tibble(Years = c(2008, 2012, 2016, 2020), TFC = c(0, 0.505, 0.486, 0)) #Time For Change vals across years


start_date <- as.Date("2012-04-01")
election_day <- as.Date("2012-11-06") 
PredictFrom <- as.Date("2012-10-06")

Polls2012 <- read.csv("PollData/all_polls_2012.csv")
Polls2008 <- read.csv("PollData/all_polls_2008.csv")

################
# Wrangle Data #
################


Polls2012 <- Polls2012 %>%
  rename(pop = number.of.observations,
         vtype = population,
         method = mode,
        dem = obama,
        gop = romney) %>%
  mutate(begin = as.Date(start.date, format="%m/%d/%y"),
         end   = as.Date(end.date, format="%m/%d/%y"),
         t = end - (1 + as.numeric(end-begin)) %/% 2) %>% #MidPoint of Poll
           filter( t>= start_date & #Exclude Polls Before Start Date or if not Likely Voters
                     !is.na(t) &
           vtype == "Likely Voters" &
           pop > 1 &
             t <= PredictFrom) %>% 
  subset(state != "--") %>%
  mutate(
    undecided = coalesce(undecided, 0L), #Remove NAs for Undecided/ 3rd Party Candidates
    other = coalesce(other, 0),
    sum = dem + gop,
    week = floor_date(t - days(2), unit = "week") + days(2), # weeks start on Tuesdays. (As in Pkremp Implementation)
    day_of_week = as.integer(t - week), # number of days since beginnning of the week
    n_dem = round(pop * dem/100), #Number of Clinton Voters in Poll
    n_respondents = round(pop*(dem+gop)/100), # Way to exclude 3rd Party/Undecided from pop
    p_dem = dem/(dem+gop), #Way  to exclude 3rd Party from Clinton results
    #Indexes For Stan
    index_s = as.numeric(as.factor(as.character(state))), #State as Factor
    # Factors are alphabetically sorted: 1 = --, 2 = AL, 3 = AK, 4 = AZ...
    index_t = 1 + as.numeric(t) - min(as.numeric(t)), #Distance in Days from the first poll
    index_w = as.numeric(as.factor(week)), # Week as Factor
    index_p = as.numeric(as.factor(as.character(pollster))) #Pollster as Factor
  ) %>%
  arrange(state, t, sum) %>% 
  distinct(state, t, pollster, .keep_all = TRUE) %>% #Keep Unique Rows
  select(state, t, begin, end, pollster, method, 
         p_dem, n_respondents, n_dem, week, day_of_week, starts_with("index_")) #Only Relevant Columns

Polls2012$state <- factor(Polls2012$state)

#Remove Overlaping Polls 

Polls2012$OV <- FALSE

Polls2012 <- Polls2012 %>%
  arrange(state, pollster, end)
  
for(i in 1:length(Polls2012$end)){
  if (!is.na(Polls2012$end[i]) & !is.na(Polls2012$begin[i+1])){
    if (Polls2012$end[i] > Polls2012$begin[i+1] & Polls2012$pollster[i] == Polls2012$pollster[i+1] & Polls2012$state[i] == Polls2012$state[i+1]){
      Polls2012$OV[i+1] <- TRUE
    }
  }
}

Polls2012 <- filter(Polls2012, Polls2012$OV == FALSE)


###########
# Vectors #
###########

ndays <- max(Polls2012$t) - min(Polls2012$t) #Days between first and last poll
all_t <- min(Polls2012$t) + days(0:(ndays)) #? All Days on Which Polls are
all_t_until_election <- min(all_t) + days(0:(election_day - min(all_t))) # Days from first poll to election day
week_for_all_t <- floor_date(all_t - days(2), unit="week") + days(2) 

all_weeks <- (floor_date(all_t - days(2), unit = "week") + days(2)) %>% unique # weeks start on Tuesdays
all_weeks_until_election <- (floor_date(all_t_until_election - days(2), unit = "week") + days(2)) %>% unique
all_pollsters <- levels(as.factor(Polls2012$pollster)) #List of Pollsters

######################################
# Wrangle Prior Election Data (2008) #
######################################


PollsPrior <- read.csv("PollData/2008.csv")


PollsPrior <- PollsPrior %>% 
  mutate(score = obama_count / (obama_count + mccain_count),
         national_score = sum(obama_count)/sum(obama_count + mccain_count),
         diff_score = score - national_score,
         share_national_vote = (total_count*(1+adult_pop_growth_2011_15))
         /sum(total_count*(1+adult_pop_growth_2011_15))) %>%
  arrange(state)
rownames(PollsPrior) <- PollsPrior$state 




# Important Values

all_states <- PollsPrior$state
state_name <- PollsPrior$state_name
names(state_name) <- PollsPrior$state

# Electoral votes, by state:

ev_state <- PollsPrior$ev
names(ev_state) <- PollsPrior$state


#################################################
# Create Priors (mu_b and mu_a) from Prior Data #
#################################################


TimeVal <- as.numeric(subset(TimeForChange, Years == PredictElection, select = TFC)) #Get Time for Change Value


# Get Values for states that weren't polled
states2008 <- PollsPrior 

all_polled_states <- as.integer(Polls2012$state %>% unique %>% sort)
PollsPrior <- PollsPrior %>%
  filter(as.integer(state) %in% all_polled_states)

prior_diff_score <- PollsPrior$diff_score #Needs Examing
names(prior_diff_score) <- PollsPrior$state #Needs help

state_weights <- c(PollsPrior$share_national_vote / sum(PollsPrior$share_national_vote))
names(state_weights) <- c(PollsPrior$state)

score_among_polled <- sum(PollsPrior$obama_count)/
  sum(PollsPrior$obama_count + 
        PollsPrior$mcain_count)
alpha_prior <- log(PollsPrior$national_score[1]/score_among_polled)

sigma_mu_b_end <-cov_matrix(n = length(mu_b_prior), sigma2 = 1/20, rho = 0.5)
sigma_walk_b_forecast <- cov_matrix(length(mu_b_prior), 7*(0.015)^2, 0.75)


mu_b_prior <- logit(TimeVal + prior_diff_score)

##################################################
# Passing the data to Stan and running the model #
##################################################

out <- stan("RevisedPolls.stan", 
            data = list(N = nrow(Polls2012),                  # Number of polls
                        S = max(Polls2012$index_s),           # Number of states
                        T = length(all_t_until_election),           # Number of days
                        W = length(all_weeks_until_election),           # Number of weeks
                        P = max(Polls2012$index_p),           # Number of pollsters
                        last_poll_T = length(all_t), #the day of last poll
                        last_poll_W = length(all_weeks), #Week of last poll
                        s = Polls2012$index_s, #State of Poll (As Factor)
                        t = Polls2012$index_t, #Days since first poll
                        w = Polls2012$index_w, #Week as Factor
                        p = Polls2012$index_p, #Pollster as Factor
                        n_dem = Polls2012$n_dem, #Number of Dem Voters
                        n_respondents = Polls2012$n_respondents, #Number of respondents
                        state_weights = state_weights, #Needs Fixing
                        T_unique = max(Polls2012$index_t), 
                        t_unique = Polls2012$index_t,
                        mu_b_prior =  mu_b_prior, #TFC + Change From Obama
                        sigma_mu_b_end = sigma_mu_b_end, #Must understnand this coveriance matrix
                        sigma_walk_b_forecast = sigma_walk_b_forecast, #same here
                        week = as.integer(as.factor(week_for_all_t)), #unclear
                        day_of_week = as.integer(all_t - week_for_all_t)), #unclear
            chains = 4, iter = 4000)

###############################
# Extracting Values from Stan #
###############################


p <- rstan::extract(out, pars = "predicted_score")[[1]]
mu_a <- rstan::extract(out, pars = "mu_a")[[1]]
mu_b <- rstan::extract(out, pars = "mu_b")[[1]]
sigma_walk_b_past <- rstan::extract(out, pars = "sigma_walk_b_past")[[1]]
sigma_walk_a_past <- rstan::extract(out, pars = "sigma_walk_a_past")[[1]]

#############################
# Wrange Simulation Results #
#############################

dates <- sort(c(all_t[all_t <= all_weeks[length(all_weeks)]], 
                unique(setdiff(all_weeks_until_election + days(3), all_weeks + days(3)))))
dates <- c(dates[-length(dates)], election_day)


dimnames(p) <- list(1:nrow(p), as.character(dates), PollsPrior$state)
dimnames(mu_a) <- list(1:nrow(mu_a), as.character(all_t))
dimnames(mu_b) <- list(1:dim(mu_b)[1],
                       as.character(all_weeks_until_election + days(3)),
                       PollsPrior$state)
sim_forecast <- p[,as.character(election_day),-1]
sim_win <- sim_forecast > 0.5



all_non_polled_states <- setdiff(all_states, Polls2012$state)
non_polled_win        <- states2008[all_non_polled_states,]$score > .5
names(non_polled_win) <- all_non_polled_states

non_polled_win_matrix <- rep(non_polled_win, nrow(sim_win)) %>%
  matrix(nr = nrow(sim_win), byrow = TRUE,
         dimnames = list(1:nrow(sim_win), all_non_polled_states))

sim_win_all_states <- cbind(sim_win, non_polled_win_matrix)

result_ev_all_states <- sim_win_all_states %*% ev_state[colnames(sim_win_all_states)]

########################
# Simple Model Results #
########################


mean(result_ev_all_states >= 270) # P(win electoral college)

qplot(result_ev_all_states, geom="histogram")






