# Application
# Nick Michalak (nickmm@umich.edu)

# Install and/or load packages
# install.packages("tidyverse")
# install.packages("ggrepel")
# install.packages("readxl")
# install.packages("lubridate")
# install.packages("shiny")
# install.packages("kableExtra")
# install.packages("formattable")
# install.packages("scales")
# install.packages("mgcv")
# install.packages("gratia")
# install.packages("effects")
# install.packages("DT")

library(tidyverse)
library(ggrepel)
library(readxl)
library(lubridate)
library(shiny)
library(kableExtra)
library(formattable)
library(scales)
library(mgcv)
library(gratia)
library(effects)
library(DT)

# Use percent from formattable
percent <- formattable::percent

# ggplot2 theme
theme_app <- theme_bw() +
  theme(plot.title = element_text(family = "Helvetica", size = rel(1.5), color = "grey20", face = "bold", hjust = 0),
        plot.subtitle = element_text(family = "Helvetica", size = rel(1), color = "grey25", face = "bold", hjust = 0),
        plot.caption = element_text(family = "Helvetica", size = rel(1.25), color = "grey25"),
        axis.title = element_blank(),
        axis.text = element_text(family = "Helvetica", size = rel(1.25), color = "grey50", face = "bold"),
        legend.title = element_text(family = "Helvetica", size = rel(1.25), color = "grey25", face = "bold"),
        legend.text = element_text(family = "Helvetica", size = rel(1.25), color = "grey25", face = "bold"),
        legend.position = "top",
        panel.border = element_blank())

# Read primary polling data from fivethirtyeight.com
president_primary_polls <- read_csv(file = "https://projects.fivethirtyeight.com/polls-page/president_primary_polls.csv")

# Add National level to state
president_primary_polls$state <- with(data = president_primary_polls, ifelse(is.na(state), "National", state))

# Convert character to factor variables
president_primary_polls <- president_primary_polls %>% 
  mutate_at(vars(c("pollster", "population", "candidate_name")), factor)

# Replace missing FiveThirtyEight Grade with "No Grade"
president_primary_polls$fte_grade <- with(data = president_primary_polls, ifelse(is.na(fte_grade), "No Grade", fte_grade))

# Reorder FiveThirtyEight Grade
president_primary_polls$fte_grade <- with(data = president_primary_polls, factor(fte_grade, levels = c("A+", "A", "A-", "A/B", "B+", "B", "B-", "B/C", "C+", "C", "C-", "C/D", "D+", "D", "D-", "No Grade"), ordered = TRUE))

# Reorder population levels
president_primary_polls$population <- with(president_primary_polls, factor(population, levels = c("a", "v", "rv", "lv")))

# Replace notes missing with "No notes"
president_primary_polls$notes <- with(data = president_primary_polls, ifelse(is.na(notes), "No notes", notes))

# Convert dates
president_primary_polls <- president_primary_polls %>% 
  # Convert poll date variables to date class
  mutate_at(c("start_date", "end_date"), mdy) %>% 
  rowwise() %>% 
  # Compute median date
  mutate(median_date = median(c(start_date, end_date))) %>% 
  ungroup()

# Create population rank variable
population_rank <- president_primary_polls %>% 
  count(poll_id, population) %>% 
  group_by(poll_id) %>% 
  mutate(population_rank = row_number()) %>% 
  ungroup() %>% 
  select(-n)

# Join with primary poll data
president_primary_polls <- president_primary_polls %>% 
  full_join(population_rank, by = c("poll_id", "population"))

# Convert percentage to proportion
president_primary_polls$ppt <- president_primary_polls$pct / 100

# Some polls have multiple questions; average questions x candidates within each population within each poll
president_primary_polls <- president_primary_polls %>% 
  # no head to head polls; democratic party polls
  filter(notes != "head-to-head poll" & party == "DEM") %>% 
  group_by(poll_id, population, candidate_name) %>% 
  mutate(ppt.avg = mean(ppt, na.rm = TRUE)) %>% 
  ungroup()

# Compute median_date as a numeric variables
president_primary_polls$median_date_dbl <- as.double(president_primary_polls$median_date)

# Save candidates character vector
candidates <- c("Amy Klobuchar", "Andrew Yang", "Bernard Sanders", "Beto O'Rourke", "Cory A. Booker", "Deval Patrick", "Elizabeth Warren", "Joseph R. Biden Jr.", "Julián Castro", "Kamala D. Harris", "Michael Bloomberg", "Pete Buttigieg", "Tom Steyer", "Tulsi Gabbard")

# Subset data
polls <- president_primary_polls %>% 
  # Averaged multiple questions within each poll above; remove duplicates with the filter
  filter(!duplicated(cbind(poll_id, candidate_name)))

# Read primary polling average data
primary_pollavrg_nr <- read_xlsx(path = "data/president_primary_pollavrg_name_recognition.xlsx", sheet = "clean", na = "-9999")

# Create political party factor
primary_pollavrg_nr$primary_party <- factor(primary_pollavrg_nr$primary_party)

# and contrasts
contrasts(primary_pollavrg_nr$primary_party) <- cbind(republican = c(-0.5, 0.5))

# Fit model for first half of the primary year (January - June)
glm.fit1 <- glm(nominee ~ poll_avrg1 * primary_party, family = binomial(link = "logit"), data = primary_pollavrg_nr)

# Fit model for first half of the primary year (January - June)
glm.fit2 <- glm(nominee ~ poll_avrg2 * primary_party, family = binomial(link = "logit"), data = primary_pollavrg_nr)

# Define UI
ui <- fluidPage(
  # App Title
  titlePanel("2020 Democratic Presidential Primary Polling Averages"),
  
  sidebarLayout(
    sidebarPanel(
      # Input State
      selectInput(inputId = "state", label = "Select National or a specific State", choices = sort(unique(president_primary_polls$state)), selected = "National"),
      
      # Input Candidates
      selectInput(inputId = "candidates", label = "Select Candidates", choices = candidates, selected = c("Bernard Sanders", "Elizabeth Warren", "Joseph R. Biden Jr.", "Pete Buttigieg"), multiple = TRUE),
      
      # Input FiveThirtyEight Grade
      selectInput(inputId = "pollgrade538", label = "Select FiveThirtyEight Poll Grade(s)", choices = levels(polls$fte_grade), selected = levels(polls$fte_grade), multiple = TRUE),
      
      # Input pollsters to remove
      selectInput(inputId = "pollster_remove", label = "Select Pollster(s) to remove", choices = levels(polls$pollster), selected = NA, multiple = TRUE),
      
      # Input date range
      dateRangeInput("date_range",
                     label = "Input range of polling Dates: YYYY-MM-DD",
                     start = min(polls$start_date) - 2, end = max(polls$end_date)),
      
      # Input basis for smooth term
      numericInput("basis_k",
                   label = "Input basis for smoothing (i.e., degrees of freedom)",
                   value = 10),
      helpText("Bigger values allow the smoothing curve to 'wiggle' more (i.e., it's more sensitive to changes)"),
      
      # Add Run button
      submitButton("Run Models", icon = icon("refresh")),
    ),
    
    # Panel Layout
    mainPanel(
      tabsetPanel(
        # Output GAMM Plot
        tabPanel("Plot", plotOutput(outputId = "model.plot", width = "100%", height = "500px")),
        # Output up-to-date GAMM or GLM fitted values
        tabPanel("Table (Final Values from Trend)", tableOutput(outputId = "fitted.value")),
        # Output 10 most recent polls per candidate
        tabPanel("Table (Recent Polls)", formattableOutput(outputId = "recent.poll")),
        # Averages from different recent periods
        tabPanel("Table (Period Averages)", tableOutput(outputId = "period.average")),
        # Output raw data
        tabPanel("Table (Raw Data)", dataTableOutput(outputId = "raw.data")),
        # Output GAMM Plot
        tabPanel("Nominee Probability (Historical)", plotOutput(outputId = "historic.prob", width = "100%", height = "500px")),
        # Methods via markdown
        tabPanel("Methods", withMathJax(includeMarkdown(path = "methods.md")))
      )
    ), position = "left", fluid = TRUE
  )
)

# Define server
server <- function(input, output) {
  # Generate dataset for use throughout server
  server_data <- reactive({
    server_data <- polls %>% 
      filter(state == input$state & candidate_name %in% input$candidates & fte_grade %in% input$pollgrade538 & !pollster %in% input$pollster_remove & start_date >= input$date_range[1] & end_date <= input$date_range[2]) %>% 
      # select the most inclusive polling population from each poll
      group_by(poll_id) %>% 
      mutate(highest_poprank = unique(population[population_rank == min(population_rank)])) %>% 
      filter(population == highest_poprank) %>% 
      ungroup()
  })
  
  # Generate model predictions for use throughout the server
  server_pred <- reactive({
    # Fit different models based on available data 
    if (max(count(server_data(), candidate_name)$n) >= 10) {
      # Fit GAMM
      input$candidates %>% 
        map_df(function(candidate_i) {
          # Subset data
          data_i <- server_data() %>%
            filter(candidate_name == candidate_i)
          
          # Fit GAMM model
          gamm.fit <- gamm(ppt.avg ~ s(median_date_dbl, bs = "tp", k = input$basis_k), correlation = corCAR1(form = ~ median_date_dbl | pollster:tracking / poll_id), family = binomial(link = "logit"), data = data_i, weights = sample_size, method = "REML")
          
          # Return predictions
          confint(gamm.fit$gam, parm = "median_date_dbl", type = "simultaneous", shift = TRUE, transform = function(x) exp(x) / (1 + exp(x))) %>% 
            as_tibble() %>% 
            mutate(median_date = as_date(median_date_dbl),
                   candidate_name = candidate_i)
        }
        )
    } else {
      # Subset data from the last 4 weeks
      input$candidates %>% 
        map_df(function(candidate_i) {
          data_i <- server_data() %>% 
            filter(candidate_name == candidate_i)
          
          # Fit intercept only logistic regression
          glm.fit <- tryCatch(
            glm(ppt.avg ~ median_date_dbl, family = binomial(link = "logit"), data = data_i, weights = sample_size), error = function(x) {
              glm(ppt.avg ~ 1, family = binomial(link = "logit"), data = data_i, weights = sample_size)
            }
          )
          
          # Create table of effects
          tryCatch(
            Effect(focal.predictors = "median_date_dbl", mod = glm.fit) %>% 
              as_tibble() %>% 
              mutate(median_date = as_date(median_date_dbl),
                     candidate_name = candidate_i) %>% 
              rename(est = fit), error = function(x) {
                       coefficients(summary(glm.fit)) %>% 
                         as_tibble() %>% 
                         mutate(median_date = data_i$median_date,
                                candidate_name = candidate_i,
                                lower = confint(glm.fit)[1], 
                                upper = confint(glm.fit)[2]) %>% 
                         set_names(c("est", "se", "z", "p", "median_date", "candidate_name", "lower", "upper")) %>% 
                         mutate_at(vars(c("est", "se", "lower", "upper")), plogis)
                     }
          )
        }
        )
    }
  }
  )
  
  # Render plot
  output$model.plot <- renderPlot(expr = {
    if (max(count(server_data(), candidate_name)$n) >= 10) {
      # Plot output
      server_data() %>%
        ggplot(mapping = aes(x = median_date, y = ppt.avg, fill = candidate_name, color = candidate_name)) +
        geom_ribbon(data = server_pred(), mapping = aes(y = est, ymin = lower, ymax = upper), alpha = 0.10, linetype = "dotted") +
        geom_line(data = server_pred(), mapping = aes(x = median_date, y = est), size = 1.5) +
        geom_point(mapping = aes(size = sample_size), alpha = 0.20) +
        scale_x_date(date_breaks = "1 month", date_labels = "%b") +
        scale_y_continuous(breaks = seq(0, 1, 0.10), labels = percent_format(accuracy = 1)) +
        scale_fill_manual(values = c("#0072B2", "#D55E00", "#B07AA1", "#009E73", "#E15759", "#999999", "#E69F00", "#56B4E9", "#F0E442", "#CC79A7", "#4E79A7", "#F28E2B", "#76B7B2", "#59A14F", "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC")) +
        scale_color_manual(values = c("#0072B2", "#D55E00", "#B07AA1", "#009E73", "#E15759", "#999999", "#E69F00", "#56B4E9", "#F0E442", "#CC79A7", "#4E79A7", "#F28E2B", "#76B7B2", "#59A14F", "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC")) +
        labs(fill = "Candidate", color = "Candidate", size = "Sample Size", x = "Median Date", title = paste0(input$state, " Polling Average"), caption = "Error ribbons represent 95% confidence intervals") +
        theme_app
    } else {
      # Plot output
      server_data() %>%
        ggplot(mapping = aes(x = median_date, y = ppt.avg, fill = candidate_name, color = candidate_name)) +
        geom_line(data = server_pred(), mapping = aes(x = median_date, y = est), size = 1.5) +
        geom_point(mapping = aes(size = sample_size), alpha = 0.20) +
        scale_x_date(date_breaks = "1 month", date_labels = "%b") +
        scale_y_continuous(breaks = seq(0, 1, 0.10), labels = percent_format(accuracy = 1)) +
        scale_fill_manual(values = c("#0072B2", "#D55E00", "#B07AA1", "#009E73", "#E15759", "#999999", "#E69F00", "#56B4E9", "#F0E442", "#CC79A7", "#4E79A7", "#F28E2B", "#76B7B2", "#59A14F", "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC")) +
        scale_color_manual(values = c("#0072B2", "#D55E00", "#B07AA1", "#009E73", "#E15759", "#999999", "#E69F00", "#56B4E9", "#F0E442", "#CC79A7", "#4E79A7", "#F28E2B", "#76B7B2", "#59A14F", "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC")) +
        labs(fill = "Candidate", color = "Candidate", size = "Sample Size", x = "Median Date", title = paste0(input$state, " Polling Average")) +
        theme_app
    }
  }
  )
  
  # Render Averages Table
  output$fitted.value <- function() {
    # Table Output
    server_pred() %>% 
      select(median_date, candidate_name, est, lower, upper) %>% 
      set_names(nm = c("To Date", "Candidate", "Average", "Lower", "Upper")) %>% 
      mutate("Width of Uncertainty" = Upper - Lower) %>% 
      group_by(Candidate) %>% 
      filter(`To Date` == max(`To Date`)) %>% 
      ungroup() %>% 
      arrange(desc(Average)) %>% 
      group_by(`To Date`) %>% 
      mutate(Behind = Average - ifelse(is.na(lag(Average)), Average, max(Average))) %>% 
      ungroup() %>% 
      mutate(Average = color_tile("white", "#56b4e9")(percent(round(Average, 3), digits = 1)),
             Lower = color_tile("white", "#56b4e9")(percent(round(Lower, 3), digits = 1)),
             Upper = color_tile("white", "#56b4e9")(percent(round(Upper, 3), digits = 1)),
             "Width of Uncertainty" = color_tile("white", "#cc79a7")(percent(round(`Width of Uncertainty`, 3), digits = 1)),
             Behind = color_tile("#d55e00", "white")(percent(round(Behind, 3), digits = 1))) %>% 
      kable("html", escape = FALSE, caption = paste0(input$state, " Polling Averages")) %>%
      add_footnote(label = c("Lower and Upper denote 95% confidence bounds.", "Width of Uncertainty = Upper - Lower"), notation = "symbol") %>% 
      kable_styling("hover", full_width = FALSE)
  }
  
  # Render Recent Poll Table
  output$recent.poll <- renderFormattable(expr = {
    server_data() %>% 
      # Arrange by most recent
      arrange(desc(end_date)) %>% 
      # Filter in only first ten unique (recent-date sorted) polls
      filter(poll_id %in% unique(poll_id)[1:10]) %>% 
      group_by(candidate_name) %>% 
      mutate(average = mean(ppt.avg, na.rm = TRUE)) %>% 
      ungroup() %>% 
      # Reorder the levels of candidate name in order of decreasing polling average in recent polls
      mutate(candidate_name = reorder(as.character(candidate_name), average),
             candidate_name = factor(candidate_name, levels = levels(candidate_name)[nlevels(candidate_name):1]),
             moe = (qnorm(p = 1 - 0.05 / 2) / sqrt(sample_size)) / 2) %>% 
      select(start_date, end_date, pollster, population, fte_grade, candidate_name, ppt.avg, moe) %>% 
      spread(key = candidate_name, ppt.avg) %>% 
      arrange(desc(end_date)) %>% 
      rename(Start = start_date, End = end_date, Pollster = pollster, Population = population, "538 Grade" = fte_grade, "Margin of Error" = moe) %>% 
      formattable(list(area(col = 6) ~ function(x) color_tile("white", "#cc79a7")(percent(x, digits = 1)),
                       area(col = 7:ncol(.)) ~ function(x) color_tile("white", "#56b4e9")(percent(x, digits = 0))), caption = paste0("Up to Ten Most Recent ", input$state, " Polls"))
  }
  )
  
  # Render Period Averages
  output$period.average <- function() {
    (polls %>% 
       filter(state == input$state & candidate_name %in% input$candidates & fte_grade %in% input$pollgrade538 & !pollster %in% input$pollster_remove) %>% 
       # select the most inclusive polling population from each poll
       group_by(poll_id) %>% 
       mutate(highest_poprank = unique(population[population_rank == min(population_rank)])) %>% 
       filter(population == highest_poprank) %>% 
       ungroup() %>% 
       pull(end_date) %>% 
       # 1 week, 2 weeks, 1 month, 2 months, 3 months 
       max() - c(7, 14, 28, 56, 84)) %>% 
      map_df(function(date_i) {
        # Subset data
        data_i <- server_data() %>% 
          filter(start_date >= date_i)
        
        # Fit model
        glm.fit <- glm(ppt.avg ~ 1 + candidate_name, family = binomial(link = "logit"), data = data_i, weights = sample_size)
        
        # Return averages
        Effect(glm.fit, focal.predictors = "candidate_name") %>% 
          as_tibble() %>% 
          mutate(Period = paste0("Average Since ", date_i)) %>% 
          arrange(Period, desc(fit)) %>% 
          select(Period, candidate_name, fit, lower, upper) %>% 
          set_names(c("Period", "Candidate", "Average", "Lower", "Upper")) %>% 
          mutate("Width of Uncertainty" = Upper - Lower) %>% 
          group_by(Period) %>% 
          mutate(Behind = Average - ifelse(is.na(lag(Average)), Average, max(Average))) %>% 
          ungroup()
      }) %>% 
      mutate(Average = color_tile("white", "#56b4e9")(percent(round(Average, 3), digits = 1)),
             Lower = color_tile("white", "#56b4e9")(percent(round(Lower, 3), digits = 1)),
             Upper = color_tile("white", "#56b4e9")(percent(round(Upper, 3), digits = 1)),
             "Width of Uncertainty" = color_tile("white", "#cc79a7")(percent(round(`Width of Uncertainty`, 3), digits = 1)),
             Behind = color_tile("#d55e00", "white")(percent(round(Behind, 3), digits = 1))) %>% 
      kable("html", escape = FALSE, caption = paste0(input$state, " Polling Averages per Period")) %>%
      add_footnote(label = c("Lower and Upper denote 95% confidence bounds.", "Width of Uncertainty = Upper - Lower"), notation = "symbol") %>% 
      kable_styling("hover", full_width = FALSE)
  }
  
  # Render Raw Data Table
  output$raw.data <- renderDataTable(datatable({
    polls %>% 
      filter(state == input$state & candidate_name %in% input$candidates & fte_grade %in% input$pollgrade538 & !pollster %in% input$pollster_remove) %>% 
      # select the most inclusive polling population from each poll
      group_by(poll_id) %>% 
      mutate(highest_poprank = unique(population[population_rank == min(population_rank)])) %>% 
      filter(population == highest_poprank) %>% 
      ungroup() %>% 
      arrange(desc(end_date))
  }))
  
  # Render Nominee Probability Plot based on historical data
  output$historic.prob <- renderPlot(expr = {
    # Subset National data
    render_data <- polls %>% 
      filter(state == "National" & candidate_name %in% input$candidates & fte_grade %in% input$pollgrade538 & !pollster %in% input$pollster_remove & start_date >= input$date_range[1] & end_date <= input$date_range[2]) %>% 
      # select the most inclusive polling population from each poll
      group_by(poll_id) %>% 
      mutate(highest_poprank = unique(population[population_rank == min(population_rank)])) %>% 
      filter(population == highest_poprank) %>% 
      ungroup()
    
    # Fit different models based on available data 
    # Try GAMM first
    pred <- if (max(count(render_data, candidate_name)$n) >= 10) {
      input$candidates %>% 
        map_df(function(candidate_i) {
          # Subset data
          data_i <- render_data %>%
            filter(candidate_name == candidate_i)
          
          # Fit GAMM model
          gamm.fit <- gamm(ppt.avg ~ s(median_date_dbl, bs = "tp", k = input$basis_k), correlation = corCAR1(form = ~ median_date_dbl | pollster:tracking / poll_id), family = binomial(link = "logit"), data = data_i, weights = sample_size, method = "REML")
          
          # Return predictions
          confint(gamm.fit$gam, parm = "median_date_dbl", type = "simultaneous", shift = TRUE, transform = function(x) exp(x) / (1 + exp(x))) %>% 
            as_tibble() %>% 
            mutate(median_date = as_date(median_date_dbl),
                   candidate_name = candidate_i)
        }
        )
    } else {
      # Try GLM next
      input$candidates %>% 
        map_df(function(candidate_i) {
          data_i <- render_data %>% 
            filter(candidate_name == candidate_i)
          
          # Fit intercept only logistic regression
          glm.fit <- tryCatch(
            glm(ppt.avg ~ median_date_dbl, family = binomial(link = "logit"), data = data_i, weights = sample_size), error = function(x) {
              glm(ppt.avg ~ 1, family = binomial(link = "logit"), data = data_i, weights = sample_size)
            }
          )
          
          # Create table of effects
          tryCatch(
            Effect(focal.predictors = "median_date_dbl", mod = glm.fit) %>% 
              as_tibble() %>% 
              mutate(median_date = as_date(median_date_dbl),
                     candidate_name = candidate_i) %>% 
              rename(est = fit), error = function(x) {
                       coefficients(summary(glm.fit)) %>% 
                         as_tibble() %>% 
                         mutate(median_date = data_i$median_date,
                                candidate_name = candidate_i,
                                lower = confint(glm.fit)[1], 
                                upper = confint(glm.fit)[2]) %>% 
                         set_names(c("est", "se", "z", "p", "median_date", "candidate_name", "lower", "upper")) %>% 
                         mutate_at(vars(c("est", "se", "lower", "upper")), plogis)
                     }
          )
        }
        )
    }
    
    # Generate Table of Predicted Probabilities per Candidate based on historical data (models fitted near the top of the file)
    sort(unique(pred$candidate_name)) %>% 
      map_df(function(candidate_i) {
        # (pre)January - June
        pred_table1 <- Effect(focal.predictors = c("poll_avrg1", "primary_party"), mod = glm.fit1, xlevels = list(poll_avrg1 = with(pred, est[median_date <= "2019-06-01" & candidate_name == candidate_i]))) %>% 
          as_tibble() %>% 
          filter(primary_party == "Democratic") %>%
          mutate(median_date = with(pred, median_date[median_date <= "2019-06-01" & candidate_name == candidate_i]),
                 candidate_name = candidate_i)
        
        # June - December
        pred_table2 <- Effect(focal.predictors = c("poll_avrg2", "primary_party"), mod = glm.fit2, xlevels = list(poll_avrg2 = with(pred, est[median_date > "2019-06-01" & candidate_name == candidate_i]))) %>% 
          as_tibble() %>% 
          filter(primary_party == "Democratic") %>% 
          mutate(median_date = with(pred, median_date[median_date > "2019-06-01" & candidate_name == candidate_i]),
                 candidate_name = candidate_i)
        
        # Return Table of Probabilities
        bind_rows(pred_table1, pred_table2)
      }) %>% 
      # Return Plot of Predicted Probabilities
      ggplot(mapping = aes(x = median_date, y = fit, fill = candidate_name, color = candidate_name)) +
      geom_hline(yintercept = 0.50, linetype = "dotted") +
      geom_ribbon(mapping = aes(ymin = lower, ymax = upper), alpha = 0.20, linetype = "dotted") +
      geom_line(size = 1.5) +
      geom_text_repel(data = function(dat) {
        dat %>% 
          group_by(candidate_name) %>% 
          filter(median_date == max(median_date)) %>% 
          ungroup()
      }, mapping = aes(x = median_date, y = fit, label = percent(fit, digits = 0)), size = 7.5, fontface = "bold", hjust = -2, show.legend = FALSE) + 
      scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = expand_scale(mult = c(0, 1 / 12))) +
      scale_y_continuous(breaks = seq(0, 1, 0.10), labels = percent_format(accuracy = 1)) +
      scale_fill_manual(values = c("#0072B2", "#D55E00", "#B07AA1", "#009E73", "#E15759", "#999999", "#E69F00", "#56B4E9", "#F0E442", "#CC79A7", "#4E79A7", "#F28E2B", "#76B7B2", "#59A14F", "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC")) +
      scale_color_manual(values = c("#0072B2", "#D55E00", "#B07AA1", "#009E73", "#E15759", "#999999", "#E69F00", "#56B4E9", "#F0E442", "#CC79A7", "#4E79A7", "#F28E2B", "#76B7B2", "#59A14F", "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC")) +
      labs(fill = "Candidate", color = "Candidate", x = "Median Date", title = "Historical Probability of Winning the Nomination", caption = "Error ribbons represent 95% confidence intervals") +
      theme_app
  })
}

# Luanch Shiny App
shinyApp(ui = ui, server = server)
