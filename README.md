# 2024 March Madness Mania Competition

## [2nd Place Submission](https://www.kaggle.com/competitions/march-machine-learning-mania-2024/leaderboard)

[GITHUB Link](https://github.com/JackLich10/kaggle_2024)

```
Private Leaderboard Score: 0.05437
Private Leaderboard Place: 2nd
```

### Background

I am a 2022 graduate of Duke University with a degree in Data Science. I currently work for a sports analytics company in Washington, D.C.. This is my third year entering this competition. I entered again because I think it's super fun to forecast the results of such crazy basketball tournaments. Last year, I finished 7th in the competition, before that 100th. My men's model was first written 3 years ago for fun and I have updated both my men's and women's models this year to use the same methodologies.

### Methodology

All coding was done in R. The basic methodology behind both of my models is to:

1) generate team ratings via ridge (`glmnet`)/mixed effects (`lme4`) modeling predicting point difference, offense/defense efficiency, offense/defense pace, etc. (i.e. what is team A's impact on offense efficiency, score difference, etc. after controlling for opponent, home court, etc.?). I also fit a 'matchup adjustment' mixed effects model, which tries to predict if a team will play up or down to the competition.

2) use team ratings from 1) to predict team level offensive efficiency (points/possession) and pace (possessions) for each game (via XGBoost)

3) use team ratings from 1) and team level predictions from 2) to predict game level score difference (via XGBoost)

The most important features to predict the team level efficiencies and the game level score differences are the various team ratings from 1). Each of these models are trained after each day of each season. As such, it takes multiple days for the entirety of this modeling framework to run for the first time. A simple GLM converts the predicted game level score difference to predicted win probability. I predicted each possible matchup's win probabilitiy and then conducted 100k simulations of each tournament.

### Code

This [repository](https://github.com/JackLich10/kaggle_2024) has my men's model and women's code in their respective folders, `mens/` and `womens/`. I also have a common `models/` folder with each team/game level model that can be run for either men's or women's, depending on command line optionalities. Within each of `mens/` and `womens/`, the `run_all.sh` shell script specifies the order in which the files need to be run for the entire modeling framework to work. The `simulate.R` script takes the predictions and simulates the tournament, combining into the final submission file.


*NOTE*: My men's model uses public data via my own [gamezoneR](https://jacklich10.github.io/gamezoneR/index.html) R package and from the public [hoopR](https://github.com/sportsdataverse/hoopR/) R package. My women's model uses data from the public [wehoop](https://github.com/sportsdataverse/wehoop/) R package. Shout out sportsdataverse and ESPN for public data feeds!
