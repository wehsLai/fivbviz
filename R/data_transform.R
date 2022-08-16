# get tournaments list
get_tournament_data <- function(no) {
  if (!missing(no)) {
    # selected tournament
    pl <- list(No = no, Fields = paste0(v_fields("Volleyball Tournament"), collapse = " "))
    tournament <- v_get_volley_tournament(parent = pl)

    matches <- tibble()
    teams <- tibble()
    players <- tibble()
    statistics <- list(
      Player = tibble(),
      Team = tibble()
    )

    # matches
    cl <- list(Filter = c(NoTournament = no))
    matches <- v_get_volley_match_list(children = cl)

    # teams
    cl <- list(Filter = c(NoTournament = no))
    teams <- v_get_volley_team_list(children = cl)
    if (nrow(teams) > 0) {
      teams <- teams %>% arrange(code)

      # players
      cl <- list(Filter = c(NoTournament = no), Relation = c(Name = "Team", Fields = "Code Name"))
      players <- v_get_volley_player_list(children = cl)
      if (nrow(players) > 0) {
        players <- players %>% arrange(team.code, noShirt)

        if (nrow(matches) > 0) {
          cal_no <- matches$no[matches$resultType == 0]
          # Statistics
          pl <- list(
            Fields = paste0(v_fields("Volleyball Statistic"), collapse = " "), SumBy = "Match"
          )
          cl <- list(
            Filter = c(NoTournaments = no, MatchesToUse = "MatchesStarted"),
            Relation = c(Name = "Match", Fields = "NoInTournament DateLocal TeamACode TeamBCode MatchResultText"),
            Relation = c(Name = "Player", Fields = "TeamName FirstName LastName VolleyPosition")
          )
          temp <- v_get_volley_statistic_list(parent = pl, children = cl)
          if (nrow(temp) > 0) {
            # select Player records only
            statistics$Player <- temp %>%
              filter(noMatch %in% cal_no & itemType == 45) %>%
              left_join(players[, c("noPlayer", "team.code", "team.name")], by = c("noItem" = "noPlayer"), suffix = c("", ".y"), keep = FALSE)
            # calculate Team records per match through Player records
            statistics$Team <- statistics$Player %>%
              group_by(noMatch, team.code, team.name) %>%
              summarise(
                nbSets = first(nbSets),
                pointTotal = sum(pointTotal), # abs point
                pointPointAverageBySet = Per(nbSets, pointTotal),
                spikePoint = sum(spikePoint),
                spikeFault = sum(spikeFault),
                spikeContinue = sum(spikeContinue),
                spikeTotal = sum(spikeTotal),
                spikePointPercentage = Percentage(spikeTotal, spikePoint), # note
                spikeEfficiencyPercentage = Efficiency(spikeTotal, spikePoint, spikeFault),
                spikePointAverageBySet = Per(nbSets, spikePoint),
                blockPoint = sum(blockPoint),
                blockFault = sum(blockFault),
                blockContinue = sum(blockContinue),
                blockTotal = sum(blockTotal),
                blockPointPercentage = Percentage(blockTotal, blockPoint),
                blockEfficiencyPercentage = Efficiency(blockTotal, blockPoint, blockFault),
                blockPointAverageBySet = Per(nbSets, blockPoint), # note
                servePoint = sum(servePoint),
                serveFault = sum(serveFault),
                serveContinue = sum(serveContinue),
                serveTotal = sum(serveTotal),
                servePointPercentage = Percentage(serveTotal, servePoint),
                serveEfficiencyPercentage = Efficiency(serveTotal, servePoint, serveFault),
                servePointAverageBySet = Per(nbSets, servePoint), # note
                digExcellent = sum(digExcellent),
                digFault = sum(digFault),
                digContinue = sum(digContinue),
                digTotal = sum(digTotal),
                digExcellentPercentage = Percentage(digTotal, digExcellent),
                digEfficiencyPercentage = Efficiency(digTotal, digExcellent, digFault),
                digExcellentAverageBySet = Per(nbSets, digExcellent), # note
                setExcellent = sum(setExcellent),
                setFault = sum(setFault),
                setContinue = sum(setContinue),
                setTotal = sum(setTotal),
                setExcellentPercentage = Percentage(setTotal, setExcellent),
                setEfficiencyPercentage = Efficiency(setTotal, setExcellent, setFault),
                setExcellentAverageBySet = Per(nbSets, setExcellent), # note
                receptionExcellent = sum(receptionExcellent),
                receptionFault = sum(receptionFault),
                receptionContinue = sum(receptionContinue),
                receptionTotal = sum(receptionTotal),
                receptionExcellentPercentage = Percentage(receptionTotal, receptionExcellent),
                receptionEfficiencyPercentage = Efficiency(receptionTotal, receptionExcellent, receptionFault), # note
                receptionExcellentAverageBySet = Per(nbSets, receptionExcellent),
              ) %>%
              ungroup()

            # total points of each team
            points <- matches %>% mutate(
              pointsTeamATotal = rowSums(select(matches, starts_with("pointsTeamA")), na.rm = TRUE),
              pointsTeamBTotal = rowSums(select(matches, starts_with("pointsTeamB")), na.rm = TRUE)
            )

            points <- bind_rows(
              select(points, no, team.code = teamACode, pointsTeamTotal = pointsTeamATotal),
              select(points, no, team.code = teamBCode, pointsTeamTotal = pointsTeamBTotal)
            )

            statistics$Team <- statistics$Team %>%
              left_join(points, by = c("noMatch" = "no", "team.code" = "team.code")) %>%
              group_by(noMatch) %>%
              mutate(
                opponentError = pointsTeamTotal - pointTotal,
                teamError = sum(opponentError) - opponentError
              ) %>%
              ungroup()
          }
        }
      }
    }

    out <- list(
      tournament = tournament,
      matches = matches,
      teams = teams,
      players = players,
      statistics = statistics
    )

    out
  } else {
    stop("wrong tournament no")
  }
}

Percentage <- function(total, performance, digits = 2, naRankZeroPf = FALSE) {
  ifelse(total == 0 | (performance == 0 & naRankZeroPf), NA, round((performance * 100) / total, digits))
}

Efficiency <- function(total, performance, fault, digits = 2, naRankZeroPf = FALSE) {
  ifelse(total == 0 | (performance == 0 & naRankZeroPf), NA, round(((performance - fault) * 100) / total, digits))
}

Per <- function(nb, performance, digits = 2, naRankZeroPf = FALSE) {
  ifelse(nb == 0 | (performance == 0 & naRankZeroPf), NA, round(performance / nb, digits))
}

OrgPoint <- function(total) {
  out <- ifelse(total == 0, NA, total)
  out
}

OrgPercentage <- function(total, performance, setLimit = FALSE, teamTotal, limit, naRankZeroPf = FALSE) {
  out <- ifelse(total == 0 | (performance == 0 & naRankZeroPf), NA, (performance * 100) / total)
  if (setLimit) out[total < teamTotal * limit] <- NA
  out
}

OrgEfficiency <- function(total, performance, fault, setLimit = FALSE, teamTotal, limit, naRankZeroPf = FALSE) {
  out <- ifelse(total == 0 | (performance == 0 & naRankZeroPf), NA, ((performance - fault) * 100) / total)
  if (setLimit) out[total < teamTotal * limit] <- NA
  out
}

OrgPer <- function(nb, performance, naRankZeroPf = FALSE) {
  ifelse(nb == 0 | (performance == 0 & naRankZeroPf), NA, performance / nb)
}

Ranking <- function(performance, isDesc = TRUE) {
  if (isDesc) {
    x <- desc(performance)
  } else {
    x <- performance
  }
  min_rank(x)
}

calculate_agg <- function(gpd_df, isTeam = TRUE) {
  if (isTeam) {
    out <- gpd_df %>% summarise(
      nbSets = sum(nbSets),
      teamError = sum(teamError),
      teamErrorAverageBySet = Per(nbSets, teamError),
      teamErrorOrgAverageBySet = OrgPer(nbSets, teamError, ),
      opponentError = sum(opponentError),
      opponentErrorAverageBySet = Per(nbSets, opponentError),
      opponentErrorOrgAverageBySet = OrgPer(nbSets, opponentError),
      pointTotal = sum(pointTotal), # abs point
      pointPointAverageBySet = Per(nbSets, pointTotal),
      spikePoint = sum(spikePoint),
      spikeFault = sum(spikeFault),
      spikeContinue = sum(spikeContinue),
      spikeTotal = sum(spikeTotal),
      spikePointPercentage = Percentage(spikeTotal, spikePoint), # note
      spikeEfficiencyPercentage = Efficiency(spikeTotal, spikePoint, spikeFault),
      spikePointAverageBySet = Per(nbSets, spikePoint),
      spikePointOrgPercentage = OrgPercentage(spikeTotal, spikePoint),
      spikeFaultOrgPercentage = OrgPercentage(spikeTotal, spikeFault),
      blockPoint = sum(blockPoint),
      blockFault = sum(blockFault),
      blockContinue = sum(blockContinue),
      blockTotal = sum(blockTotal),
      blockPointPercentage = Percentage(blockTotal, blockPoint),
      blockEfficiencyPercentage = Efficiency(blockTotal, blockPoint, blockFault),
      blockPointAverageBySet = Per(nbSets, blockPoint), # note
      blockPointOrgPercentage = OrgPercentage(blockTotal, blockPoint),
      blockFaultOrgPercentage = OrgPercentage(blockTotal, blockFault),
      servePoint = sum(servePoint),
      serveFault = sum(serveFault),
      serveContinue = sum(serveContinue),
      serveTotal = sum(serveTotal),
      servePointPercentage = Percentage(serveTotal, servePoint),
      serveEfficiencyPercentage = Efficiency(serveTotal, servePoint, serveFault),
      servePointAverageBySet = Per(nbSets, servePoint), # note
      servePointOrgPercentage = OrgPercentage(serveTotal, servePoint),
      serveFaultOrgPercentage = OrgPercentage(serveTotal, serveFault),
      digExcellent = sum(digExcellent),
      digFault = sum(digFault),
      digContinue = sum(digContinue),
      digTotal = sum(digTotal),
      digExcellentPercentage = Percentage(digTotal, digExcellent),
      digEfficiencyPercentage = Efficiency(digTotal, digExcellent, digFault),
      digExcellentAverageBySet = Per(nbSets, digExcellent), # note
      digExcellentOrgPercentage = OrgPercentage(digTotal, digExcellent),
      digFaultOrgPercentage = OrgPercentage(digTotal, digFault),
      setExcellent = sum(setExcellent),
      setFault = sum(setFault),
      setContinue = sum(setContinue),
      setTotal = sum(setTotal),
      setExcellentPercentage = Percentage(setTotal, setExcellent),
      setEfficiencyPercentage = Efficiency(setTotal, setExcellent, setFault),
      setExcellentAverageBySet = Per(nbSets, setExcellent), # note
      setExcellentOrgPercentage = OrgPercentage(setTotal, setExcellent),
      setFaultOrgPercentage = OrgPercentage(setTotal, setFault),
      receptionExcellent = sum(receptionExcellent),
      receptionFault = sum(receptionFault),
      receptionContinue = sum(receptionContinue),
      receptionTotal = sum(receptionTotal),
      receptionExcellentPercentage = Percentage(receptionTotal, receptionExcellent),
      receptionEfficiencyPercentage = Efficiency(receptionTotal, receptionExcellent, receptionFault), # note
      receptionExcellentAverageBySet = Per(nbSets, receptionExcellent),
      receptionExcellentOrgPercentage = OrgPercentage(receptionTotal, receptionExcellent),
      receptionFaultOrgPercentage = OrgPercentage(receptionTotal, receptionFault)
    )

    out$scoreRank <- Ranking(OrgPoint(out$pointTotal))
    out$spikeRank <- Ranking(OrgPercentage(out$spikeTotal, out$spikePoint, naRankZeroPf = TRUE)) # note
    out$blockRank <- Ranking(OrgPer(out$nbSets, out$blockPoint, naRankZeroPf = TRUE)) # note
    out$serveRank <- Ranking(OrgPer(out$nbSets, out$servePoint, naRankZeroPf = TRUE)) # note
    out$digRank <- Ranking(OrgPer(out$nbSets, out$digExcellent, naRankZeroPf = TRUE)) # note
    out$setRank <- Ranking(OrgPer(out$nbSets, out$setExcellent, naRankZeroPf = TRUE)) # note
    out$receptionRank <- Ranking(OrgEfficiency(out$receptionTotal, out$receptionExcellent, out$receptionFault, naRankZeroPf = TRUE)) # note
    out$tfRank <- Ranking(OrgPer(out$nbSets, out$teamError, naRankZeroPf = TRUE), isDesc = FALSE)
  } else {
    out <- gpd_df %>% summarise(
      # order by note, team.code, noShirt(player)
      nbSets = sum(nbSets),
      nbSets.t = first(nbSets.t),
      # timePlayed = as.period(seconds_to_period(sum(timePlayed)), unit="minutes"),
      # timePlayedAverageBySet = Per(nbSets.t, timePlayed, 0),
      pointTotal = sum(pointTotal), # abs point
      pointPointAverageBySet = Per(nbSets.t, pointTotal),
      spikePoint = sum(spikePoint),
      spikeFault = sum(spikeFault),
      spikeContinue = sum(spikeContinue),
      spikeTotal = sum(spikeTotal),
      spikePointPercentage = Percentage(spikeTotal, spikePoint), # note
      spikeEfficiencyPercentage = Efficiency(spikeTotal, spikePoint, spikeFault),
      spikePointAverageBySet = Per(nbSets.t, spikePoint),
      spikeTotal.t = first(spikeTotal.t),
      spikeLoad = Percentage(spikeTotal.t, spikeTotal),
      spikePointOrgPercentage = OrgPercentage(spikeTotal, spikePoint),
      spikeFaultOrgPercentage = OrgPercentage(spikeTotal, spikeFault),
      blockPoint = sum(blockPoint),
      blockFault = sum(blockFault),
      blockContinue = sum(blockContinue),
      blockTotal = sum(blockTotal),
      blockPointPercentage = Percentage(blockTotal, blockPoint),
      blockEfficiencyPercentage = Efficiency(blockTotal, blockPoint, blockFault),
      blockPointAverageBySet = Per(nbSets.t, blockPoint), # note
      blockTotal.t = first(blockTotal.t),
      blockLoad = Percentage(blockTotal.t, blockTotal),
      blockPointOrgPercentage = OrgPercentage(blockTotal, blockPoint),
      blockFaultOrgPercentage = OrgPercentage(blockTotal, blockFault),
      servePoint = sum(servePoint),
      serveFault = sum(serveFault),
      serveContinue = sum(serveContinue),
      serveTotal = sum(serveTotal),
      servePointPercentage = Percentage(serveTotal, servePoint),
      serveEfficiencyPercentage = Efficiency(serveTotal, servePoint, serveFault),
      servePointAverageBySet = Per(nbSets.t, servePoint), # note
      serveTotal.t = first(serveTotal.t),
      serveLoad = Percentage(serveTotal.t, serveTotal),
      servePointOrgPercentage = OrgPercentage(serveTotal, servePoint),
      serveFaultOrgPercentage = OrgPercentage(serveTotal, serveFault),
      digExcellent = sum(digExcellent),
      digFault = sum(digFault),
      digContinue = sum(digContinue),
      digTotal = sum(digTotal),
      digExcellentPercentage = Percentage(digTotal, digExcellent),
      digEfficiencyPercentage = Efficiency(digTotal, digExcellent, digFault),
      digExcellentAverageBySet = Per(nbSets.t, digExcellent), # note
      digTotal.t = first(digTotal.t),
      digLoad = Percentage(digTotal.t, digTotal),
      digExcellentOrgPercentage = OrgPercentage(digTotal, digExcellent),
      digFaultOrgPercentage = OrgPercentage(digTotal, digFault),
      setExcellent = sum(setExcellent),
      setFault = sum(setFault),
      setContinue = sum(setContinue),
      setTotal = sum(setTotal),
      setExcellentPercentage = Percentage(setTotal, setExcellent),
      setEfficiencyPercentage = Efficiency(setTotal, setExcellent, setFault),
      setExcellentAverageBySet = Per(nbSets.t, setExcellent), # note
      setTotal.t = first(setTotal.t),
      setLoad = Percentage(setTotal.t, setTotal),
      setExcellentOrgPercentage = OrgPercentage(setTotal, setExcellent),
      setFaultOrgPercentage = OrgPercentage(setTotal, setFault),
      receptionExcellent = sum(receptionExcellent),
      receptionFault = sum(receptionFault),
      receptionContinue = sum(receptionContinue),
      receptionTotal = sum(receptionTotal),
      receptionExcellentPercentage = Percentage(receptionTotal, receptionExcellent),
      receptionEfficiencyPercentage = Efficiency(receptionTotal, receptionExcellent, receptionFault), # note
      receptionExcellentAverageBySet = Per(nbSets.t, receptionExcellent),
      receptionTotal.t = first(receptionTotal.t),
      receptionLoad = Percentage(receptionTotal.t, receptionTotal),
      receptionExcellentOrgPercentage = OrgPercentage(receptionTotal, receptionExcellent),
      receptionFaultOrgPercentage = OrgPercentage(receptionTotal, receptionFault)
    )

    out <- out %>% mutate(
      liberoExcellent = ifelse(player.volleyPosition == "L", digExcellent + receptionExcellent, NA),
      liberoFault = ifelse(player.volleyPosition == "L", digFault + receptionFault, NA),
      liberoContinue = ifelse(player.volleyPosition == "L", digContinue + receptionContinue, NA),
      liberoTotal = ifelse(player.volleyPosition == "L", digTotal + receptionTotal, NA),
      liberoExcellentPercentage = ifelse(player.volleyPosition == "L", Percentage(liberoTotal, liberoExcellent), NA),
      liberoEfficiencyPercentage = ifelse(player.volleyPosition == "L", Efficiency(liberoTotal, liberoExcellent, liberoFault), NA), # note
      liberoExcellentAverageBySet = ifelse(player.volleyPosition == "L", Per(nbSets.t, liberoExcellent), NA), # note
      liberoExcellentOrgPercentage = OrgPercentage(liberoTotal, liberoExcellent),
      liberoFaultOrgPercentage = OrgPercentage(liberoTotal, liberoFault)
    )

    out$scoreRank <- Ranking(OrgPoint(out$pointTotal))
    out$spikeRank <- Ranking(OrgPercentage(out$spikeTotal, out$spikePoint, setLimit = TRUE, teamTotal = out$spikeTotal.t, limit = spike_limit, naRankZeroPf = TRUE)) # note
    out$blockRank <- Ranking(OrgPer(out$nbSets.t, out$blockPoint, naRankZeroPf = TRUE)) # note
    out$serveRank <- Ranking(OrgPer(out$nbSets.t, out$servePoint, naRankZeroPf = TRUE)) # note
    out$digRank <- Ranking(OrgPer(out$nbSets.t, out$digExcellent, naRankZeroPf = TRUE)) # note
    out$setRank <- Ranking(OrgPer(out$nbSets.t, out$setExcellent, naRankZeroPf = TRUE)) # note
    out$receptionRank <- Ranking(OrgEfficiency(out$receptionTotal, out$receptionExcellent, out$receptionFault, setLimit = TRUE, teamTotal = out$receptionTotal.t, limit = reception_limit, naRankZeroPf = TRUE)) # note
    out$liberoRank <- Ranking(OrgPer(out$nbSets.t, out$liberoExcellent)) # note
  }
  out
}

get_agg <- function(statistics) {
  team_agg <- tibble()
  player_agg <- tibble()

  if (nrow(statistics$Team) > 0) {
    team_agg <- statistics$Team %>%
      group_by(team.code, team.name, team = paste(team.code, team.name)) %>%
      calculate_agg(isTeam = TRUE) %>%
      ungroup()
    player_agg <- statistics$Player %>%
      left_join(team_agg[, c("team.code", "nbSets", "spikeTotal", "blockTotal", "serveTotal", "digTotal", "setTotal", "receptionTotal")],
        by = c("team.code" = "team.code"), suffix = c("", ".t"), keep = FALSE
      ) %>%
      group_by(team.code, team.name, noShirt, player.teamName, player.lastName, player.firstName, name = paste(player.lastName, player.firstName), player.volleyPosition) %>%
      calculate_agg(isTeam = FALSE) %>%
      ungroup()
  }
  out <- list(
    team_agg = team_agg,
    player_agg = player_agg
  )
}

get_p5 <- function(player_agg, type = "c", showAll = FALSE, number = 10) {
  if (missing(type) || !(type %in% c("c", "a", "b", "s", "d", "e", "r", "l"))) {
    stop("type not found")
  } else {
    type <- tolower(type)
    out <- NA
    switch(type,
      "c" = {
        out <- player_agg %>%
          mutate(showText = paste0(
            "<b>", name, "<br>",
            team.code, " ", noShirt, " ", player.volleyPosition, "</b><br>",
            "Rank: ", scoreRank, "<br>",
            "Attack: ", spikePoint, "<br>",
            "Block: ", blockPoint, "<br>",
            "Serve: ", servePoint, "<br>",
            "Total: ", pointTotal, "<br>",
            "Avg. by set: ", pointPointAverageBySet
          )) %>%
          select(Rk = "scoreRank", all_of(p.col), all_of(scorer.col), showText) %>%
          filter(Total > 0)
      },
      "a" = {
        out <- player_agg %>%
          mutate(showText = paste0(
            "<b>", name, "<br>",
            team.code, " ", noShirt, " ", player.volleyPosition, "</b><br>",
            "Rank: ", spikeRank, "<br>",
            "Attacks: ", spikePoint, "<br>",
            "Faults: ", spikeFault, "<br>",
            "Total: ", spikeTotal, "<br>",
            "Avg. by set: ", spikePointAverageBySet, "<br>",
            "Succ. %: ", spikePointPercentage, "<br>",
            "Eff. %: ", spikeEfficiencyPercentage, "<br>",
            "Load %: ", spikeLoad
          )) %>%
          select(Rk = "spikeRank", all_of(p.col), all_of(attacker.col), `Load %` = "spikeLoad", showText) %>%
          filter(Total > 0)
      },
      "b" = {
        out <- player_agg %>%
          mutate(showText = paste0(
            "<b>", name, "<br>",
            team.code, " ", noShirt, " ", player.volleyPosition, "</b><br>",
            "Rank: ", blockRank, "<br>",
            "Kill Blocks: ", blockPoint, "<br>",
            "Faults: ", blockFault, "<br>",
            "Total: ", blockTotal, "<br>",
            "Avg. by set: ", blockPointAverageBySet, "<br>",
            "KB. %: ", blockPointPercentage, "<br>",
            "Eff. %: ", blockEfficiencyPercentage, "<br>",
            "Load %: ", blockLoad
          )) %>%
          select(Rk = "blockRank", all_of(p.col), all_of(blocker.col), `Load %` = "blockLoad", showText) %>%
          filter(Total > 0)
      },
      "s" = {
        out <- player_agg %>%
          mutate(showText = paste0(
            "<b>", name, "<br>",
            team.code, " ", noShirt, " ", player.volleyPosition, "</b><br>",
            "Rank: ", serveRank, "<br>",
            "Aces: ", servePoint, "<br>",
            "Faults: ", serveFault, "<br>",
            "Total: ", serveTotal, "<br>",
            "Avg. by set: ", servePointAverageBySet, "<br>",
            "Ace %: ", servePointPercentage, "<br>",
            "Eff. %: ", serveEfficiencyPercentage, "<br>",
            "Load %: ", serveLoad
          )) %>%
          select(Rk = "serveRank", all_of(p.col), all_of(server.col), `Load %` = "serveLoad", showText) %>%
          filter(Total > 0)
      },
      "d" = {
        out <- player_agg %>%
          mutate(showText = paste0(
            "<b>", name, "<br>",
            team.code, " ", noShirt, " ", player.volleyPosition, "</b><br>",
            "Rank: ", digRank, "<br>",
            "Digs: ", digExcellent, "<br>",
            "Faults: ", digFault, "<br>",
            "Total: ", digTotal, "<br>",
            "Avg. by set: ", digExcellentAverageBySet, "<br>",
            "Dig %: ", digExcellentPercentage, "<br>",
            "Eff. %: ", digEfficiencyPercentage, "<br>",
            "Load %: ", digLoad
          )) %>%
          select(Rk = "digRank", all_of(p.col), all_of(digger.col), `Load %` = "digLoad", showText) %>%
          filter(Total > 0)
      },
      "e" = {
        out <- player_agg %>%
          mutate(showText = paste0(
            "<b>", name, "<br>",
            team.code, " ", noShirt, " ", player.volleyPosition, "</b><br>",
            "Rank: ", setRank, "<br>",
            "Running Sets: ", setExcellent, "<br>",
            "Faults: ", setFault, "<br>",
            "Total: ", setTotal, "<br>",
            "Avg. by set: ", setExcellentAverageBySet, "<br>",
            "RS. %: ", setExcellentPercentage, "<br>",
            "Eff. %: ", setEfficiencyPercentage, "<br>",
            "Load %: ", setLoad
          )) %>%
          select(Rk = "setRank", all_of(p.col), all_of(setter.col), `Load %` = "setLoad", showText) %>%
          filter(Total > 0)
      },
      "r" = {
        out <- player_agg %>%
          mutate(showText = paste0(
            "<b>", name, "<br>",
            team.code, " ", noShirt, " ", player.volleyPosition, "</b><br>",
            "Rank: ", receptionRank, "<br>",
            "Excellents: ", receptionExcellent, "<br>",
            "Faults: ", receptionFault, "<br>",
            "Total: ", receptionTotal, "<br>",
            "Avg. by set: ", receptionExcellentAverageBySet, "<br>",
            "Exc. %: ", receptionExcellentPercentage, "<br>",
            "Eff. %: ", receptionEfficiencyPercentage, "<br>",
            "Load %: ", receptionLoad
          )) %>%
          select(Rk = "receptionRank", all_of(p.col), all_of(receiver.col), `Load %` = "receptionLoad", showText) %>%
          filter(Total > 0)
      },
      "l" = {
        out <- player_agg %>%
          mutate(showText = paste0(
            "<b>", name, "<br>",
            team.code, " ", noShirt, " ", player.volleyPosition, "</b><br>",
            "Rank: ", liberoRank, "<br>",
            "Excellents: ", liberoExcellent, "<br>",
            "Faults: ", liberoFault, "<br>",
            "Total: ", liberoTotal, "<br>",
            "Avg. by set: ", liberoExcellentAverageBySet, "<br>",
            "Exc. %: ", liberoExcellentPercentage, "<br>",
            "Eff. %: ", liberoEfficiencyPercentage
          )) %>%
          select(Rk = "liberoRank", all_of(p.col), all_of(libero.col), showText) %>%
          filter(Total > 0)
      }
    )
    out <- out %>% arrange(Rk, Team, No)
    if (!showAll) out <- out %>% filter(Rk <= number)

    out
  }
}

get_p6 <- function(team_agg, type = "c", showAll = FALSE, number = 10) {
  if (missing(type) || !(type %in% c("c", "a", "b", "s", "d", "e", "r", "f"))) {
    stop("type not found")
  } else {
    type <- tolower(type)
    out <- NA
    switch(type,
      "c" = {
        out <- team_agg %>%
          mutate(showText = paste0(
            "<b>", team.code, " - ", team.name, "</b><br>",
            "Rank: ", scoreRank, "<br>",
            "Attack: ", spikePoint, "<br>",
            "Block: ", blockPoint, "<br>",
            "Serve: ", servePoint, "<br>",
            "Total: ", pointTotal, "<br>",
            "Avg. by set: ", pointPointAverageBySet
          )) %>%
          select(Rk = "scoreRank", all_of(t.col), all_of(scorer.col), showText)
      },
      "a" = {
        out <- team_agg %>%
          mutate(showText = paste0(
            "<b>", team.code, " - ", team.name, "</b><br>",
            "Rank: ", spikeRank, "<br>",
            "Attacks: ", spikePoint, "<br>",
            "Faults: ", spikeFault, "<br>",
            "Total: ", spikeTotal, "<br>",
            "Avg. by set: ", spikePointAverageBySet, "<br>",
            "Succ. %: ", spikePointPercentage, "<br>",
            "Eff. %: ", spikeEfficiencyPercentage
          )) %>%
          select(Rk = "spikeRank", all_of(t.col), all_of(attacker.col), showText)
      },
      "b" = {
        out <- team_agg %>%
          mutate(showText = paste0(
            "<b>", team.code, " - ", team.name, "</b><br>",
            "Rank: ", blockRank, "<br>",
            "Kill Blocks: ", blockPoint, "<br>",
            "Faults: ", blockFault, "<br>",
            "Total: ", blockTotal, "<br>",
            "Avg. by set: ", blockPointAverageBySet, "<br>",
            "KB. %: ", blockPointPercentage, "<br>",
            "Eff. %: ", blockEfficiencyPercentage
          )) %>%
          select(Rk = "blockRank", all_of(t.col), all_of(blocker.col), showText)
      },
      "s" = {
        out <- team_agg %>%
          mutate(showText = paste0(
            "<b>", team.code, " - ", team.name, "</b><br>",
            "Rank: ", serveRank, "<br>",
            "Aces: ", servePoint, "<br>",
            "Faults: ", serveFault, "<br>",
            "Total: ", serveTotal, "<br>",
            "Avg. by set: ", servePointAverageBySet, "<br>",
            "Ace %: ", servePointPercentage, "<br>",
            "Eff. %: ", serveEfficiencyPercentage
          )) %>%
          select(Rk = "serveRank", all_of(t.col), all_of(server.col), showText)
      },
      "d" = {
        out <- team_agg %>%
          mutate(showText = paste0(
            "<b>", team.code, " - ", team.name, "</b><br>",
            "Rank: ", digRank, "<br>",
            "Digs: ", digExcellent, "<br>",
            "Faults: ", digFault, "<br>",
            "Total: ", digTotal, "<br>",
            "Avg. by set: ", digExcellentAverageBySet, "<br>",
            "Dig %: ", digExcellentPercentage, "<br>",
            "Eff. %: ", digEfficiencyPercentage
          )) %>%
          select(Rk = "digRank", all_of(t.col), all_of(digger.col), showText)
      },
      "e" = {
        out <- team_agg %>%
          mutate(showText = paste0(
            "<b>", team.code, " - ", team.name, "</b><br>",
            "Rank: ", setRank, "<br>",
            "Running Sets: ", setExcellent, "<br>",
            "Faults: ", setFault, "<br>",
            "Total: ", setTotal, "<br>",
            "Avg. by set: ", setExcellentAverageBySet, "<br>",
            "RS. %: ", setExcellentPercentage, "<br>",
            "Eff. %: ", setEfficiencyPercentage
          )) %>%
          select(Rk = "setRank", all_of(t.col), all_of(setter.col), showText)
      },
      "r" = {
        out <- team_agg %>%
          mutate(showText = paste0(
            "<b>", team.code, " - ", team.name, "</b><br>",
            "Rank: ", receptionRank, "<br>",
            "Excellents: ", receptionExcellent, "<br>",
            "Faults: ", receptionFault, "<br>",
            "Total: ", receptionTotal, "<br>",
            "Avg. by set: ", receptionExcellentAverageBySet, "<br>",
            "Exc. %: ", receptionExcellentPercentage, "<br>",
            "Eff. %: ", receptionEfficiencyPercentage
          )) %>%
          select(Rk = "receptionRank", all_of(t.col), all_of(receiver.col), showText)
      },
      "f" = {
        out <- team_agg %>%
          mutate(showText = paste0(
            "<b>", team.code, " - ", team.name, "</b><br>",
            "Rank: ", tfRank, "<br>",
            "Team Errors: ", teamError, "<br>",
            "Opp. Errors: ", opponentError, "<br>",
            "Avg. by set: ", teamErrorAverageBySet, "<br>",
            "Opp. Avg. by set: ", opponentErrorAverageBySet
          )) %>%
          select(Rk = "tfRank", all_of(t.col), all_of(tf.col), showText)
      }
    )
    out <- out %>% arrange(Rk, Team)
    # Do not filter teams
    # if (!showAll) out <- out %>% filter(Rk <= number)
    out
  }
}

add_agg <- function(statistics) {
  agg <- get_agg(statistics)
  statistics$team_agg <- agg$team_agg
  statistics$player_agg <- agg$player_agg
  statistics
}

rankBySkillDf <- function(agg, type, isTeam = FALSE, showAll = TRUE, number = 10) {
  if (isTeam) {
    out <- get_p6(agg, type, showAll, number)
  } else {
    out <- get_p5(agg, type, showAll, number)
  }
  return(out)
}

rankBySkillshow <- function(f, type, isTeam = FALSE, limit = 0) {
  if (isTeam) {
    # team data
    if (type == "c") {
      data <- f %>%
        select(-team.code, -showText)
    } else {
      data <- f %>% select(-team.code, -y, -x, -showText)
    }
  } else {
    # player data
    if (type == "c") {
      data <- f %>%
        select(-player.teamName, -showText)
    } else if (type %in% c("a", "b", "s", "d", "e", "r")) {
      data <- f %>%
        filter(`Load %` >= limit) %>%
        select(-player.teamName, -y, -x, -showText)
    } else {
      data <- f %>%
        select(-player.teamName, -y, -x, -showText)
    }
  }
}

get_p3 <- function(tournamentName, matches, statistics, noMatch, type = "text") {
  if (is.null(noMatch)) {
    out <- ""
  } else {
    # rename
    p.col <- c(No = "noShirt", Name = "player.teamName", `Pos` = "player.volleyPosition")
    t.col <- c(Name = "team")
    a.col <- c(Atk = "spikePoint", Err = "spikeFault", Inp = "spikeContinue", Tot = "spikeTotal", `Succ.%` = "spikePointPercentage")
    b.col <- c(Blo = "blockPoint", Err = "blockFault", Inp = "blockContinue", Tot = "blockTotal", `Avg.pS` = "blockPointAverageBySet")
    s.col <- c(Ace = "servePoint", Err = "serveFault", Inp = "serveContinue", Tot = "serveTotal", `Avg.pS` = "servePointAverageBySet")
    d.col <- c(Dig = "digExcellent", Err = "digFault", Inp = "digContinue", Tot = "digTotal", `Avg.pS` = "digExcellentAverageBySet")
    e.col <- c(Run = "setExcellent", Err = "setFault", Inp = "setContinue", Tot = "setTotal", `Avg.pS` = "setExcellentAverageBySet")
    r.col <- c(Exc = "receptionExcellent", Err = "receptionFault", Inp = "receptionContinue", Tot = "receptionTotal", `Eff. %` = "receptionEfficiencyPercentage")

    m <- matches %>% filter(no == noMatch)
    m <- m %>% mutate(
      pointsTeamATotal = rowSums(select(m, starts_with("pointsTeamA")), na.rm = TRUE),
      pointsTeamBTotal = rowSums(select(m, starts_with("pointsTeamB")), na.rm = TRUE)
    )
    lv <- c(m$teamACode, m$teamBCode)

    t <- statistics$Team %>%
      filter(noMatch == m$no) %>%
      mutate(team = "Total Team")
    p <- statistics$Player %>%
      filter(noMatch == m$no) %>%
      left_join(t[, c("team.code", "nbSets", "spikeTotal", "blockTotal", "serveTotal", "digTotal", "setTotal", "receptionTotal")], by = c("team.code" = "team.code"), suffix = c("", ".t"), keep = FALSE) %>%
      group_by(team.code) %>%
      mutate(
        name = sprintf("%-20.20s", paste(player.lastName, player.firstName)),
        spikePointPercentage = Percentage(spikeTotal, spikePoint), # note
        blockPointAverageBySet = Per(nbSets.t, blockPoint), # note
        servePointAverageBySet = Per(nbSets.t, servePoint), # note
        digExcellentAverageBySet = Per(nbSets.t, digExcellent), # note
        setExcellentAverageBySet = Per(nbSets.t, setExcellent), # note
        receptionEfficiencyPercentage = Efficiency(receptionTotal, receptionExcellent, receptionFault), # note
        spikeRank = Ranking(OrgPercentage(spikeTotal, spikePoint, setLimit = TRUE, teamTotal = spikeTotal.t, limit = spike_limit, naRankZeroPf = TRUE)), # note
        blockRank = Ranking(OrgPer(nbSets.t, blockPoint, naRankZeroPf = TRUE)), # note
        serveRank = Ranking(OrgPer(nbSets.t, servePoint, naRankZeroPf = TRUE)), # note
        digRank = Ranking(OrgPer(nbSets.t, digExcellent, naRankZeroPf = TRUE)), # note
        setRank = Ranking(OrgPer(nbSets.t, setExcellent, naRankZeroPf = TRUE)), # note
        receptionRank = Ranking(OrgEfficiency(receptionTotal, receptionExcellent, receptionFault, setLimit = TRUE, teamTotal = receptionTotal.t, limit = reception_limit, naRankZeroPf = TRUE)) # note
      ) %>%
      ungroup()

    p$spikePointPercentage[is.na(p$spikeRank)] <- NA
    p$blockPointAverageBySet[is.na(p$blockRank)] <- NA
    p$servePointAverageBySet[is.na(p$serveRank)] <- NA
    p$digExcellentAverageBySet[is.na(p$digRank)] <- NA
    p$setExcellentAverageBySet[is.na(p$setRank)] <- NA
    p$receptionEfficiencyPercentage[is.na(p$receptionRank)] <- NA

    p$team.code <- factor(p$team.code, levels = lv)
    t$team.code <- factor(t$team.code, levels = lv)

    sp <- split(p, p$team.code)
    st <- split(t, t$team.code)
    # header
    header <- pandoc.header.return(paste0(tournamentName, "\n[P-3] Match players ranking"), 1, style = "setext") %>% gsub("^\n", "", .)
    # P2 Document
    p2url <- "[P-2]"
    if (!is.na(m$noDocumentP2)) p2url <- sprintf("%s https://www.fivb.org/vis2009/getdocument.asmx?no=%s", p2url, as.character(m$noDocumentP2))
    # Info
    endTime <- hms(m$endTime)
    if (is.na(m$beginDateTimeUtc)) {
      beginTime <- hms(m$timeLocal)
      durationTotal <- endTime - beginTime
    } else {
      durationTotal <- as.period(difftime(m$endDateTimeUtc, m$beginDateTimeUtc, units = "mins"))
      beginTime <- endTime - durationTotal
    }
    info <- sprintf(
      "Match: %s Date: %s Spectators: %s\nCity: %s\nHall: %s\nMatch duration: Start: %s End: %s Total: %s",
      m$noInTournament, m$dateLocal, format(m$nbSpectators, big.mark = ","),
      m$city, m$hall, parsePeridHM(beginTime), parsePeridHM(endTime), parsePeridHM(durationTotal)
    ) %>%
      stringr::str_replace_all(" NA", " ")

    # Score
    if (m$format != 2) {
      i <- 5
    } else {
      i <- 7
    }

    r1 <- m %>%
      select(teamACode, matchPointsA, all_of(paste0("pointsTeamASet", 1:i)), pointsTeamATotal) %>%
      setNames(c("Teams", "Sets", 1:i, "Total")) %>%
      map_df(as.character)
    r2 <- m %>%
      select(teamBCode, matchPointsB, all_of(paste0("pointsTeamBSet", 1:i)), pointsTeamBTotal) %>%
      setNames(c("Teams", "Sets", 1:i, "Total")) %>%
      map_df(as.character)
    r3 <- c("Duration:", NA_integer_, parsePeridHM(seconds_to_period(m[, paste0("durationSet", 1:i)])), parsePeridHM(seconds_to_period(m$durationTotal))) %>%
      setNames(c("Teams", "Sets", 1:i, "Total")) %>%
      map_df(as.character)
    score <- bind_rows(r1, r2, r3)

    # Stats
    p3_stats <- ""
    for (j in 1:2) {
      # Title Team
      titleTeam <- pandoc.table.return(paste(st[[j]]$team.code, st[[j]]$team.name, sep = " - "), style = "grid") %>% gsub("^\n\n|\n$", "", .)
      stats <- map(c("Attack", "Block", "Serve", "Dig", "Set", "Reception"), function(z) {
        # Title Skill
        switch(z,
          "Attack" = {
            titleSkill <- sprintf("%s (Limit: %0.f%%)", z, spike_limit * 100)
            rk <- c(Rk = "spikeRank")
            sk <- a.col
          },
          "Block" = {
            titleSkill <- z
            rk <- c(Rk = "blockRank")
            sk <- b.col
          },
          "Serve" = {
            titleSkill <- z
            rk <- c(Rk = "serveRank")
            sk <- s.col
          },
          "Dig" = {
            titleSkill <- z
            rk <- c(Rk = "digRank")
            sk <- d.col
          },
          "Set" = {
            titleSkill <- z
            rk <- c(Rk = "setRank")
            sk <- e.col
          },
          "Reception" = {
            titleSkill <- sprintf("%s (Limit: %0.f%%)", z, reception_limit * 100)
            rk <- c(Rk = "receptionRank")
            sk <- r.col
          }
        )

        # Body Player Stats
        body <- sp[[j]] %>%
          select(all_of(rk), all_of(p.col), all_of(sk)) %>%
          arrange(Rk, No) %>%
          bind_rows(st[[j]] %>% select(all_of(t.col), all_of(sk)))
        if (type == "text") {
          body[, tail(names(body), 1)] <- sprintf("%.2f", pull(body[, tail(names(body), 1)])) %>% stringr::str_replace_all("NA", "")
          body <- body %>%
            pandoc.table.return(style = "simple", missing = "", justify = c("right", "right", "left", "center", rep("right", 5))) %>%
            gsub("^\n\n", "", .)
          out <- paste0(pandoc.header.return(titleSkill, 1, style = "setext") %>% gsub("^\n", "", .), body)
        }
      })
      if (type == "text") {
        p3_stats <- paste(p3_stats, titleTeam, paste(stats, collapse = ""), sep = "\n")
      }
    }
    # finally combine all parts
    if (type == "text") {
      out <- paste0(
        header,
        info, "\n",
        pandoc.table.return(score, style = "grid", missing = "", justify = c("left", rep("right", i + 2))) %>% gsub("^\n\n|\n$", "", .),
        p2url, "\n",
        p3_stats
      )
    }
  }
  out
}
