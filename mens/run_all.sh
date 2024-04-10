# cd "${0%/*}"
cd ~/Desktop/RStudio/college_basketball/
pwd

# Rscript mens/load_data_gamezone.R --start_season 2017-18
Rscript mens/load_data.R --season 2024
wait

Rscript mens/load_data.R --combine TRUE
wait

Rscript mens/load_data.R --join TRUE
wait

(trap 'kill 0' SIGINT; Rscript lagged_metrics.R --gender mens & Rscript models/lme4_poss_level.R --gender mens --rerun FALSE & Rscript models/ridge_poss_level.R --gender mens --rerun FALSE & Rscript models/ridge_game_level.R --gender mens --rerun FALSE & wait)
wait

Rscript models/lme4_matchup_adj.R --gender mens --rerun FALSE
wait

Rscript process.R --gender mens
wait

Rscript models/predict_team_level.R --gender mens --rerun FALSE
wait

Rscript models/predict_game_level.R --gender mens --rerun FALSE
wait

Rscript mens/kaggle.R

