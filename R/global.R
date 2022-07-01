library(downloadthis)
library(dplyr)
library(reactable)
library(fivbvis)
library(flexdashboard)
library(shiny)
library(shinyWidgets)
library(waiter)

# default value
v_caching(FALSE)
v_options(verbose = TRUE)
Sys.setlocale(category = 'LC_ALL', locale = 'English_United States.1252')
Y <- format(Sys.Date(), "%Y")

# waiter
waiting_screen <- tagList(
    spin_fading_circles(),
    h4("Cool stuff loading...")
) 

# get tournaments list
pl <- list(Fields = "No Season ShortNameOrName StartDate EndDate Gender OrganizerType Status")
# tournaments <- v_get_volley_tournament_list(parent = pl) %>% arrange(desc(startDate))
tournaments <- readRDS("data/tournaments.rds")

df.colDef <- colDef(
    header = function(value) {
        fivbvis:::first_to_upper(value)
    },
    headerVAlign = 'bottom',
    minWidth = 70
)

tl.colDef <- list(
    no = colDef(minWidth = 50, filterable = FALSE, align = "left"),
    season = colDef(minWidth = 50),
    shortNameOrName = colDef(name = "Tournament", minWidth = 240)
)

# get tournaments list
get_tournament_data <- function(no) {
    if (!missing(no)) {
        # selected tournament
        pl <- list(No = no, Fields = paste0(v_fields("Volleyball Tournament"), collapse = " "))
        tournament <- v_get_volley_tournament(parent = pl)

        # matches
        cl <- list(Filter = c(NoTournament = no))
        matches <- v_get_volley_match_list(children = cl)
        cal_no <- matches$no[matches$resultType == 0]
        
        # teams
        pl <- list(Fields = "No Code Name Version")
        cl <- list(Filter = c(NoTournament = no))
        teams <- v_get_volley_team_list(children = cl) %>% arrange(code)

        # players
        cl <- list(Filter = c(NoTournament = no))
        players <- v_get_volley_player_list(children = cl)
        
        # Statistics
        pl <- list(Fields = paste0(v_fields("Volleyball Statistic"), collapse = " "),
                   Include = "PlayersSumByTeam", SumBy="Match")
        cl <- list(Filter = c(NoTournaments = no, MatchesToUse="MatchesFinished"),
                   Relation = c(Name="Match", Fields="NoInTournament DateLocal TeamACode TeamBCode MatchResult"),
                   Relation = c(Name="Team", Fields="Code Name"),
                   Relation = c(Name="Player", Fields="TeamName FirstName LastName VolleyPosition")
        )
        statistics <- v_get_volley_statistic_list(parent = pl, children = cl) %>%
            split(as.factor(ifelse(is.na(.$team.code), "Player", "Team")))
        
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