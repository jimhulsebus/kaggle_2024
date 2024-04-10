# cd "${0%/*}"
cd ~/Desktop/RStudio/college_basketball/
pwd

Rscript womens/load_data.R --season 2024
wait

Rscript womens/load_data.R --combine TRUE
wait

(trap 'kill 0' SIGINT; Rscript lagged_metrics.R --gender womens & Rscript models/lme4_poss_level.R --gender womens --rerun FALSE & Rscript models/ridge_poss_level.R --gender womens --rerun FALSE & Rscript models/ridge_game_level.R --gender womens --rerun FALSE & wait)
wait

Rscript models/lme4_matchup_adj.R --gender womens --rerun FALSE
wait

Rscript process.R --gender womens
wait

Rscript models/predict_team_level.R --gender womens --rerun FALSE
wait

Rscript models/predict_game_level.R --gender womens --rerun FALSE
wait

Rscript womens/kaggle.R
