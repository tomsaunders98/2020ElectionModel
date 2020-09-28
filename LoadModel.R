#Load Libraries
library(rstan, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(tidyr, quietly = TRUE)
library(purrr, quietly = TRUE)
library(stringr, quietly = TRUE)
library(lubridate, quietly = TRUE)
library(ggplot2, quietly = TRUE)
#Set Stan Options
rstan_options(javascript = FALSE)
options(mc.cores = 6)

#helpful function for building covariance matrix in R
cov_matrix <- function(n, sigma2, rho){
  m <- matrix(nrow = n, ncol = n)
  m[upper.tri(m)] <- rho
  m[lower.tri(m)] <- rho
  diag(m) <- 1
  (sigma2^.5 * diag(n))  %*% m %*% (sigma2^.5 * diag(n))
}


#####################################################################
# Data selection (makes it easier to backtest on earlier elections) #
#####################################################################
Election <- 2020
from_date <- Sys.Date() #as.Date("2012-10-15")

#2020 Model using Ray Fair https://fairmodel.econ.yale.edu/vote2020/index2.htm
ElectionDates <- tibble(Year = c(2008,2012,2016,2020), EndDate = c("0", "06/11/2012", "08/11/2016", "03/11/2020"), StartDate = c("0", "01/03/2012", "01/03/2016", "01/03/2020"), TFC = c(0, 0.505, 0.486, 0.53), dem = c("obama","obama", "clinton", "biden"), gop = c("mccain", "romney", "trump", "trump"))

Electiondate <- as.Date(as.character(subset(ElectionDates, Year == Election, select = EndDate)), format="%d/%m/%Y")
StartDate <- as.Date(as.character(subset(ElectionDates, Year == Election, select = StartDate)), format="%d/%m/%Y")
TFC <- as.numeric(subset(ElectionDates, Year == Election, select = TFC))
demman <- as.character(subset(ElectionDates, Year == Election, select = dem))
gopman <- as.character(subset(ElectionDates, Year == Election, select = gop))
demmanprior <- as.character(subset(ElectionDates, Year == Election-4, select = dem))
gopmanprior <- as.character(subset(ElectionDates, Year == Election-4, select = gop))
PriorData <- read.csv(paste("PollData/", as.character(Election-4), ".csv", sep=""))

if (Election != 2020){
  PollData <- read.csv(paste("PollData/all_polls_", as.character(Election), ".csv", sep=""))
}else{ #Poll Data from The Economist
  PollData <-read.csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vQ56fySJKLL18Lipu1_i3ID9JE06voJEz2EXm6JW4Vh11zmndyTwejMavuNntzIWLY0RyhA1UsVEen0/pub?gid=0&single=true&output=csv')
}

################################################
# Wrangle poll data and build vectors for stan #
################################################

PollData <- PollData %>%
  rename(
    pop = number.of.observations,
    type = population,
    dem = all_of(demman),
    gop = all_of(gopman)
  ) %>%
  subset(state != "--" &
         as.Date(start.date, format="%m/%d/%Y", origin = lubridate::origin) > StartDate &
         type == "lv" &
         include == TRUE) %>%
  mutate(
    start = as.Date(start.date, format="%m/%d/%y"),
    end = as.Date(end.date, format="%m/%d/%y", origin = lubridate::origin),
    n_dem = round(pop * dem/100),
    n_respondents = round(pop*(dem+gop)/100),
    date = end - (1 + as.numeric(end-start)) %/% 2,
    #Indexes
    date_index = 1 + as.numeric(date) - min(as.numeric(date)),
    state_index = as.numeric(as.factor(as.character(state)))
  ) %>%
  select(state, pop, dem, gop, start, end, n_dem, n_respondents, date, state_index, date_index) %>%
  drop_na()

# Create important vectors:
date_range <- Electiondate - min(PollData$date)
all_D <- min(PollData$date) + days(0:(max(PollData$date)-min(PollData$date)))
until_election <- min(PollData$date) + days(0:date_range)
states_w_polls <- PollData$state %>% unique %>% sort

#####################################
# Load prior data and create priors #
#####################################


# Save Prior Data for later before its filtered to remove non-polled states
AllData <- PriorData %>%
  rename(
    dem = all_of(demmanprior),
    gop = all_of(gopmanprior)
  ) %>%
  mutate(
    vote = as.numeric(dem/ (dem + gop))
  )
states_no_polls <- setdiff(AllData$state, states_w_polls)

# Get Prior Poll Data to create our state-level forecast h_i
PriorData <- PriorData %>%
  rename(
    dem = all_of(demmanprior),
    gop = all_of(gopmanprior)
  ) %>%
  filter(
    state %in% states_w_polls
  ) %>%
  mutate(
    state_index = as.numeric(as.factor(as.character(state))),
    vote = as.numeric(dem/ (dem + gop)),
    nationalvote = sum(dem)/ (sum(dem) + sum(gop)),
    diff = vote - nationalvote,
    raw_beta_prior = TFC + diff,
    Beta_prior = qlogis(TFC + diff)
  )


# Create Priors
mu_state_prior <- PriorData %>%
  pull(Beta_prior)

sigma_mu_state_end <-cov_matrix(n = length(mu_state_prior), sigma2 = 0.05, rho = 0.5)
sigma_walk_state_forecast <- cov_matrix(length(mu_state_prior), (0.057)^2, 0.75)




################
# Send to Stan #
################



out <- stan("Polls.stan", 
            data = list(N = nrow(PollData),  #Number of Polls
                        #Get totals for important values
                        S = max(PollData$state_index),   
                        D = length(until_election), 
                        last_poll_D = max(PollData$date_index),
                        #Now we have totals, onto vectors:
                        state = PollData$state_index,
                        day = PollData$date_index,
                        #Poll Values
                        n_dem = PollData$n_dem,
                        n_respondents = PollData$n_respondents,
                        #Priors
                        sigma_mu_state_end = sigma_mu_state_end, #covariance for prior
                        sigma_walk_state_forecast = sigma_walk_state_forecast, #covariance for forecast
                        mu_state_prior = mu_state_prior), #historical forecast
            chains  = 6,  iter = 2500, warmup = 500)

# Extract Predicted Score from STAN
p <- rstan::extract(out, pars = "predicted_score")[[1]]
dimnames(p) <- list(1:nrow(p), as.character(until_election), states_w_polls)

#############################
# Calculate Electoral Votes #
#############################

sim_forecast <- p[,as.character(Electiondate),]
PredVote <- colMeans(sim_forecast)
sim_win <- sim_forecast > 0.5


# Including states where there is no polling
non_polled_win <- AllData %>%
  subset(
    state %in% states_no_polls
  ) %>%
  mutate(
    score = ifelse(vote > 0.5, TRUE, FALSE)
  ) %>%
  pull(score)
names(non_polled_win) <- states_no_polls

non_polled_matrix = rep(non_polled_win, nrow(sim_win)) %>%
  matrix(nr = nrow(sim_win), byrow = TRUE, dimnames = list(1:nrow(sim_win), states_no_polls))


# Combine no polls + polls
sim_win_all_states <- cbind(sim_win, non_polled_matrix)
ev_state <- AllData$ev
names(ev_state) <- AllData$state
result_ev_all_states <- as.numeric(sim_win_all_states %*% ev_state[colnames(sim_win_all_states)])

# visualise ev results
mean(result_ev_all_states >= 270)
qplot(result_ev_all_states, geom="histogram", binwidth=30)


############################
# Building data for export #
############################

PredVote2 <- as.data.frame(colMeans(p)) %>%
  tibble::rownames_to_column("date") %>%
  subset(
    date < Sys.Date() | date == Electiondate
  )
PredVoteSD <- apply(p, c(2,3), FUN = sd)
PredVoteCI <- apply(PredVoteSD, c(1,2), {function(x) 1.96*x})
PredVoteCILeft <- as.data.frame(colMeans(p) + PredVoteCI) %>%
  tibble::rownames_to_column("date")
PredVoteCIRight <- as.data.frame(colMeans(p) - PredVoteCI) %>%
  tibble::rownames_to_column("date")


# Calculate probability of win for each state with polls
PredictedResults <- as.data.frame(colMeans(sim_win)) %>%
  tibble::rownames_to_column("state") %>%
  rename(
    pred = "colMeans(sim_win)"
  ) %>%
  select(state, pred) %>%
  mutate(polls = TRUE,
  )

# add probability for state with no polls
nopolls <- AllData %>%
  subset(
    state %in% states_no_polls
  ) %>%
  mutate(pred = ifelse(dem > 0.5, 1, 0),
         polls = FALSE) %>%
  select(state,pred, polls)

FinalResults <- merge(nopolls, PredictedResults, all=TRUE)


# wrange poll data for each state
Poll <- PollData %>%
  mutate(
    vote = dem/(dem+gop)
  ) %>%
  select(state, date, vote) %>%
  arrange(state) %>%
  pivot_wider(names_from = state, values_from = vote, values_fn = {mean})

# Adding Raw Poll Data for bullet points
PollR <- PollData %>%
  mutate(
    avg = dem-gop
  ) %>%
  select(state, date, avg) %>%
  arrange(state) %>%
  pivot_wider(names_from = state, values_from = avg, values_fn = {mean})

# build csv files for each state including polls, CIs and avg vote share
for (state in factor(states_w_polls)){
  CILeft <- PredVoteCILeft %>%
    select("date", state) %>%
    rename(
      "CILeft" = state
    ) %>%
    arrange(date)
  CIRight <- PredVoteCIRight %>%
    select("date", state) %>%
    rename(
      "CIRight" = state
    ) %>%
    arrange(date)
  Pred <- PredVote2 %>%
    select("date", state) %>%
    rename(
      "Pred" = state
    ) %>%
    arrange(date)
  Pollv <- Poll %>%
    select("date", state) %>%
    rename(
      "Poll" = state
    ) %>%
    mutate(
      date = as.character(date)
    ) %>%
    arrange(date)
  RawPolls <- PollR %>%
    select("date", state) %>%
    rename(
      "PollDiff" = state
    ) %>%
    mutate(
      date = as.character(date)
    ) %>%
    arrange(date)
  CIs <- merge(CILeft, CIRight, by="date")
  statefile <- merge(CIs, Pred, by="date", all.x=TRUE)
  statewpollsD <- merge(statefile, Pollv, by="date", all.x=TRUE)
  statewpolls <- merge(statewpollsD, RawPolls, by="date", all.x=TRUE)
  filename = paste("output//states//", state,".csv", sep="")
  write.csv(statewpolls, filename, row.names=FALSE)
  
}

#build csvs for ev distribution + state probabilities
results <- as.data.frame(result_ev_all_states)
write.csv(results, 'output//ev_prediction.csv', row.names = FALSE)
write.csv(FinalResults, 'output//FinalResults.csv', row.names = FALSE)

