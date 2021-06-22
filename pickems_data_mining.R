#' # Libraries
library(tidyverse)
library(rvest)

#' # Given you code, give me the leaderboard
which_board <- function (code) {
  if(!code %in% c("MM", "JJ", "SS", "CC", "all")) {"Please choose: MM, JJ, SS, CC, all"} else {
    if(code == "MM") {
      "https://pickem.overwatchleague.com/en-us/leaderboards/2021/may-melee"
    } else if (code == "JJ") {
      "https://pickem.overwatchleague.com/en-us/leaderboards/2021/june-joust"
    } else if (code == "SS") {
      "https://pickem.overwatchleague.com/en-us/leaderboards/2021/summer-showdown"
    } else if (code == "CC") {
      "https://pickem.overwatchleague.com/en-us/leaderboards/2021/countdown-cup"
    } else if (code == "all") {
      "https://pickem.overwatchleague.com/en-us/leaderboards/2021"
    }
  }
}
which_board("SS")

#' # Pulling sites for Top 100
site_list <- function(leader_frame, select_frame) {
  #leader_frame = "JJ"
  #select_frame = "MM"
  site <- case_when(leader_frame == "MM" ~ which_board("MM"),
                    leader_frame == "JJ" ~ which_board("JJ"),
                    leader_frame == "SS" ~ which_board("SS"),
                    leader_frame == "CC" ~ which_board("CC"))
  leaderboards_source <- read_html(site) %>% html_nodes(xpath = "body") %>% as.character()
  leaderboards_chunks <- strsplit(leaderboards_source, "placing h2")[[1]][2:101]
  sapply(c(1:100), function(index) {
    # index = 1
    selected_leaderboards_chunk <- leaderboards_chunks[grepl(paste0(">", index, "<"), leaderboards_chunks)]
    sites <- strsplit(selected_leaderboards_chunk, " ")[[1]]
    sites <- sites[grepl("https", strsplit(selected_leaderboards_chunk, " ")[[1]])]
    selected_site <- if(select_frame == "MM") {
      sites[1]
    } else if (select_frame == "JJ") {
      sites[2]
    } else if (select_frame == "SS") {
      sites[3]
    } else if (select_frame == "CC") {
      sites[4]
    }
    strsplit(selected_site, '"')[[1]][2]
  })
}

site_list("JJ", "MM")

#' # Pull info from each site




