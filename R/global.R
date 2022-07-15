library(dplyr)
library(fivbvis)
library(gt)
library(plotly)
library(purrr)
library(reactable)
library(shiny)
library(shinyWidgets)
library(waiter)
# Clean ----
rm(list = ls())
# Default Value ----
v_caching(FALSE)
v_options(verbose = FALSE)
addPoolName <- TRUE

rv <- reactiveValues()

spike_limit <- 0.15
reception_limit <- 0.2
type_p5 <- c("c", "a", "b", "s", "d", "e", "r", "l")
type_p6 <- c("c", "a", "b", "s", "d", "e", "r", "f")
pos.color <- RColorBrewer::brewer.pal(6, "Set2")
names(pos.color) <- c("MB", "OP", "OH", "S", "L", "U")
plotlyColor <- list(
  bg = "#EBEBEB",
  grid = "#FFFFFF",
  medianline = "#FDDD60"
)

p.col <- c(No = "noShirt", Name = "name", "player.teamName", `Pos.` = "player.volleyPosition", Team = "team.code")
t.col <- c(Team = "team", "team.code")
scorer.col <- c(Attack = "spikePoint", Block = "blockPoint", Serve = "servePoint", Total = "pointTotal", `Avg. by set` = "pointPointAverageBySet")
attacker.col <- c(Attacks = "spikePoint", Faults = "spikeFault", Shots = "spikeContinue", Total = "spikeTotal", `Succ. %` = "spikePointPercentage", `Eff. %` = "spikeEfficiencyPercentage", `Avg. by set` = "spikePointAverageBySet", y = "spikePointOrgPercentage", x = "spikeFaultOrgPercentage")
blocker.col <- c(`Kill Blocks` = "blockPoint", Faults = "blockFault", Rebounds = "blockContinue", Total = "blockTotal", `KB. %` = "blockPointPercentage", `Eff. %` = "blockEfficiencyPercentage", `Avg. by set` = "blockPointAverageBySet", y = "blockPointOrgPercentage", x = "blockFaultOrgPercentage")
server.col <- c(`Aces` = "servePoint", Faults = "serveFault", `Serve Hits` = "serveContinue", Total = "serveTotal", `Ace %` = "servePointPercentage", `Eff. %` = "serveEfficiencyPercentage", `Avg. by set` = "servePointAverageBySet", y = "servePointOrgPercentage", x = "serveFaultOrgPercentage")
digger.col <- c(`Digs` = "digExcellent", Faults = "digFault", Receptions = "digContinue", Total = "digTotal", `Dig %` = "digExcellentPercentage", `Eff. %` = "digEfficiencyPercentage", `Avg. by set` = "digExcellentAverageBySet", y = "digExcellentOrgPercentage", x = "digFaultOrgPercentage")
setter.col <- c(`Running Sets` = "setExcellent", Faults = "setFault", `Still Sets` = "setContinue", Total = "setTotal", `RS. %` = "setExcellentPercentage", `Eff. %` = "setEfficiencyPercentage", `Avg. by set` = "setExcellentAverageBySet", y = "setExcellentOrgPercentage", x = "setFaultOrgPercentage")
receiver.col <- c(`Excellents` = "receptionExcellent", Faults = "receptionFault", `Serve Receptions` = "receptionContinue", Total = "receptionTotal", `Exc. %` = "receptionExcellentPercentage", `Eff. %` = "receptionEfficiencyPercentage", `Avg. by set` = "receptionExcellentAverageBySet", y = "receptionExcellentOrgPercentage", x = "receptionFaultOrgPercentage")
libero.col <- c(`Excellents` = "liberoExcellent", Faults = "liberoFault", `In Play` = "liberoContinue", Total = "liberoTotal", `Exc. %` = "liberoExcellentPercentage", `Eff. %` = "liberoEfficiencyPercentage", `Avg. by set` = "liberoExcellentAverageBySet", y = "liberoExcellentOrgPercentage", x = "liberoFaultOrgPercentage")
tf.col <- c(`Team Errors` = "teamFault", `Opp. Errors` = "opponentError", `Avg. by set` = "teamFaultAverageBySet", `Opp. Avg. by set` = "opponentErrorAverageBySet", y = "opponentErrorOrgAverageBySet", x = "teamFaultOrgAverageBySet")

# waiter
waiting_screen <- tagList(
  spin_fading_circles(),
  h4("Cool stuff loading...")
)

df.colDef <- colDef(
  header = function(value) {
    fivbvis:::first_to_upper(value)
  },
  headerVAlign = "bottom",
  minWidth = 70,
  sortNALast = TRUE
)
