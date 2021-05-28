#' # Library
library(tidyverse)

source("func.R")

#" # Overall DAG
overall_dag_data <- lapply(seq(1, nrow(owl_2021_custom_data(FALSE)), by = 2), function(x) {
  team1 <- owl_2021_custom_data(FALSE)[x,]
  team2 <- owl_2021_custom_data(FALSE)[x+1,]
  if (team1$Points > team2$Points) {
    data.frame(team.w = team1$Team, team.l = team2$Team)
  } else if (team1$Points < team2$Points) {
    data.frame(team.w = team2$Team, team.l = team1$Team)
  }
}) %>% bind_rows()

dagitty::dagitty(
  paste("dag {", paste(overall_dag_data$team.w, overall_dag_data$team.l, sep = " -> ", collapse = " "), "}")
) %>% ggdag::ggdag()

#' # Control Maps DAG 
control_map_dag_data <- lapply(seq(1, nrow(filter(owl_2021_custom_data(FALSE), !is.na(`C S`))), by = 2), function(x) {
  data <- owl_2021_custom_data(FALSE) %>% filter(!is.na(`C S`))
  team1 <- data[x,]
  team2 <- data[x+1,]
  output <- if(is.na(team1$`C2 S`) | is.na(team2$`C2 S`)) {
    if (team1$`C S` > team2$`C S`) {
      data.frame(team.w = team1$Team, team.l = team2$Team)
    } else if (team1$`C S` < team2$`C S`) {
      data.frame(team.w = team2$Team, team.l = team1$Team)
    }
  } else {
    c1 <- if (team1$`C S` > team2$`C S`) {
      data.frame(team.w = team1$Team, team.l = team2$Team)
    } else if (team1$`C S` < team2$`C S`) {
      data.frame(team.w = team2$Team, team.l = team1$Team)
    }
    c2 <- if (team1$`C2 S` > team2$`C2 S`) {
      data.frame(team.w = team1$Team, team.l = team2$Team)
    } else if(team1$`C2 S` < team2$`C2 S`) {
      data.frame(team.w = team2$Team, team.l = team1$Team)
    }
    bind_rows(c1, c2)
  }
}) %>% bind_rows()

dagitty::dagitty(
  paste("dag {", paste(control_map_dag_data$team.w, control_map_dag_data$team.l, sep = " -> ", collapse = " "), "}")
) %>% ggdag::ggdag()

#' # Hybrid Maps DAG
hybrid_map_dag_data <- lapply(seq(1, nrow(filter(owl_2021_custom_data(FALSE), !is.na(`H S`))), by = 2), function(x) {
  #x = 47
  data <- owl_2021_custom_data(FALSE) %>% filter(!is.na(`H S`))
  team1 <- data[x,]
  team2 <- data[x+1,]
  output <- if(is.na(team1$`H2 S`) | is.na(team2$`H2 S`)) {
    if (team1$`H S` > team2$`H S`) {
      data.frame(team.w = team1$Team, team.l = team2$Team)
    } else if (team1$`H S` < team2$`H S`) {
      data.frame(team.w = team2$Team, team.l = team1$Team)
    }
  } else {
    h1 <- if (team1$`H S` > team2$`H S`) {
      data.frame(team.w = team1$Team, team.l = team2$Team)
    } else if (team1$`H S` < team2$`H S`) {
      data.frame(team.w = team2$Team, team.l = team1$Team)
    }
    h2 <- if (team1$`H2 S` > team2$`H2 S`) {
      data.frame(team.w = team1$Team, team.l = team2$Team)
    } else if(team1$`H2 S` < team2$`H2 S`) {
      data.frame(team.w = team2$Team, team.l = team1$Team)
    }
    bind_rows(h1, h2)
  }
}) %>% bind_rows()

dagitty::dagitty(
  paste("dag {", paste(hybrid_map_dag_data$team.w, hybrid_map_dag_data$team.l, sep = " -> ", collapse = " "), "}")
) %>% ggdag::ggdag()

#' # Assault Maps DAG
assault_map_dag_data <- lapply(seq(1, nrow(filter(owl_2021_custom_data(FALSE), !is.na(`A S`))), by = 2), function(x) {
  data <- owl_2021_custom_data(FALSE) %>% filter(!is.na(`A S`))
  team1 <- data[x,]
  team2 <- data[x+1,]
  output <- if(is.na(team1$`A2 S`) | is.na(team2$`A2 S`)) {
    if (team1$`A S` > team2$`A S`) {
      data.frame(team.w = team1$Team, team.l = team2$Team)
    } else if (team1$`A S` < team2$`A S`) {
      data.frame(team.w = team2$Team, team.l = team1$Team)
    }
  } else {
    a1 <- if (team1$`A S` > team2$`A S`) {
      data.frame(team.w = team1$Team, team.l = team2$Team)
    } else if (team1$`A S` < team2$`A S`) {
      data.frame(team.w = team2$Team, team.l = team1$Team)
    }
    a2 <- if (team1$`A2 S` > team2$`A2 S`) {
      data.frame(team.w = team1$Team, team.l = team2$Team)
    } else if(team1$`A2 S` < team2$`A2 S`) {
      data.frame(team.w = team2$Team, team.l = team1$Team)
    }
    bind_rows(a1, a2)
  }
}) %>% bind_rows()

dagitty::dagitty(
  paste("dag {", paste(assault_map_dag_data$team.w, assault_map_dag_data$team.l, sep = " -> ", collapse = " "), "}")
) %>% ggdag::ggdag()

#' # Payload Maps DAG
payload_map_dag_data <- lapply(seq(1, nrow(filter(owl_2021_custom_data(FALSE), !is.na(`P S`))), by = 2), function(x) {
  data <- owl_2021_custom_data(FALSE) %>% filter(!is.na(`P S`))
  team1 <- data[x,]
  team2 <- data[x+1,]
  output <- if(is.na(team1$`P2 S`) | is.na(team2$`P2 S`)) {
    if (team1$`P S` > team2$`P S`) {
      data.frame(team.w = team1$Team, team.l = team2$Team)
    } else if (team1$`P S` < team2$`P S`) {
      data.frame(team.w = team2$Team, team.l = team1$Team)
    }
  } else {
    p1 <- if (team1$`P S` > team2$`P S`) {
      data.frame(team.w = team1$Team, team.l = team2$Team)
    } else if (team1$`P S` < team2$`P S`) {
      data.frame(team.w = team2$Team, team.l = team1$Team)
    }
    p2 <- if (team1$`P2 S` > team2$`P2 S`) {
      data.frame(team.w = team1$Team, team.l = team2$Team)
    } else if(team1$`P2 S` < team2$`P2 S`) {
      data.frame(team.w = team2$Team, team.l = team1$Team)
    }
    bind_rows(p1, p2)
  }
}) %>% bind_rows()

dagitty::dagitty(
  paste("dag {", paste(payload_map_dag_data$team.w, payload_map_dag_data$team.l, sep = " -> ", collapse = " "), "}")
) %>% ggdag::ggdag()

