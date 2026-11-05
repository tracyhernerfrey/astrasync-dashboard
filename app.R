# Run once if needed: install.packages(c("shiny", "ggplot2", "triangle"))
# Open app.R in RStudio and click Run App.
library(shiny)
library(ggplot2)

months <- 18
rate <- 0.015
monthly_cost <- 1200 * 100
discount <- (1 + rate)^(-months)
annuity <- sum((1 + rate)^(-seq_len(months)))
bonus <- 800000
base_bonus_prob <- 0.70
base_win_prob <- 0.40

set.seed(2026)
n_sim <- 100000
# Savings and gain-share are in millions until converted to dollars.
savings <- triangle::rtriangle(n_sim, a = 3.2, b = 12.8, c = 5.6)
gain_share <- 0.20 * (pmax(savings - 4, 0) +
                      pmax(savings - 6, 0) +
                      pmax(savings - 8, 0))
mean_gain <- mean(gain_share)
median_gain <- median(gain_share)

payoff_a <- (152000 - monthly_cost) * annuity
b_no_bonus <- (125000 - monthly_cost) * annuity
b_with_bonus <- b_no_bonus + bonus * discount
# The win payoff averages over the simulated gain-share distribution.
# Losing contributes zero. Separate bid preparation costs are assumed zero.
c_win <- (150000 - monthly_cost) * annuity +
  mean_gain * 1000000 * discount

payoffs <- function(win_prob, bonus_prob) {
  c(A = payoff_a, B = b_no_bonus + bonus_prob * bonus * discount,
    C = win_prob * c_win)
}
baseline <- payoffs(base_win_prob, base_bonus_prob)
money <- function(x) paste0("$", formatC(x, format = "f", digits = 0, big.mark = ","))
pct <- function(x) sprintf("%.2f%%", 100 * x)

option_colors <- c(A = "#405985", B = "#6741A5", C = "#00796F")

payoff_card <- function(option, label, detail) {
  div(class = paste("payoff-card", paste0("option-", option)),
      span(class = "card-label", paste("OPTION", option)),
      h3(label),
      div(class = "card-value", money(baseline[option])),
      p(detail))
}

tree_outcome <- function(probability, label, payoff, expected = FALSE) {
  tags$li(
    span(class = "odds", probability),
    span(class = "outcome-label", label),
    strong(money(payoff)),
    if (expected) tags$small("Average payoff if won")
  )
}

tree_branch <- function(option, label, ...) {
  tags$li(class = paste("tree-branch", paste0("option-", option)),
    div(class = "branch-head",
      span(class = "branch-letter", option),
      strong(label),
      span(class = "branch-pv", paste("Expected PV", money(baseline[option])))),
    tags$ul(class = "outcomes", ...))
}

ui <- fluidPage(
  tags$head(tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
            tags$style(HTML("
    body {background:#eef3f9;color:#182744;font-size:16px;line-height:1.55}
    .dashboard {max-width:1120px;margin:36px auto 60px}
    .hero {background:linear-gradient(115deg,#101c39,#26244e 70%,#124c57);
      color:white;padding:42px 38px;border-radius:22px}
    .hero h1 {font-size:clamp(29px,5vw,49px);letter-spacing:-.04em;margin:8px 0 12px}
    .hero p {color:#e1e9f7;margin:0}
    .eyebrow,.card-label {font-size:12px;letter-spacing:.14em;font-weight:800}
    .eyebrow {color:#8ff3dc}
    section {background:white;padding:28px;margin:20px 0;border-radius:16px;
      border:1px solid #dae4f0;box-shadow:0 3px 12px #172a4610}
    h1,h2,h3 {font-weight:700}
    h2 {margin:0 0 14px;font-size:25px;letter-spacing:-.02em}
    .payoff-grid {display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:14px;margin-top:22px}
    .payoff-card {padding:17px;border:1px solid #dce5f0;border-top:4px solid var(--accent);
      border-radius:12px;background:#f8faff}
    .payoff-card h3 {font-size:17px;margin:6px 0}
    .card-value {font-size:clamp(24px,3.5vw,36px);font-weight:800;color:var(--accent)}
    .payoff-card p {margin:3px 0 0;color:#43536b;font-size:14px}
    .option-A {--accent:#405985} .option-B {--accent:#6741a5}
    .option-C {--accent:#00796f;background:#edf9f6}
    li {margin:0 0 8px}
    .irs-bar,.irs-single {background:#00796f!important}
    .irs-bar {border-color:#00796f!important}
    .irs-single:before {border-top-color:#00796f!important}
    .decision-call {font-size:clamp(21px,3vw,28px);color:#00685e;margin:0 0 5px}
    .decision-detail {color:#43536b;margin:0}
    .tree-root {display:inline-block;border-radius:9px;padding:9px 15px;
      background:#182744;color:white;font-weight:700}
    .tree-branches,.outcomes {list-style:none;margin:0;padding:0}
    .tree-branches {margin-left:16px;border-left:2px solid #cbd7e8;padding:14px 0 0 21px}
    .tree-branch {position:relative;margin-bottom:16px;background:transparent}
    .tree-branch::before {content:'';position:absolute;left:-22px;top:22px;
      width:22px;border-top:2px solid #cbd7e8}
    .branch-head {display:flex;align-items:center;flex-wrap:wrap;gap:8px 12px;
      background:#f1f5fb;padding:9px 12px;border-left:4px solid var(--accent);border-radius:8px}
    .option-C .branch-head {background:#e2f6f0}
    .branch-letter,.odds {display:inline-block;border-radius:6px;padding:2px 8px;
      font-weight:750;font-size:13px}
    .branch-letter {color:white;background:var(--accent)}
    .branch-pv {margin-left:auto;color:var(--accent);font-weight:700}
    .outcomes {margin-left:17px;border-left:2px solid #cbd7e8;padding:7px 0 0 16px}
    .outcomes li {position:relative;display:flex;flex-wrap:wrap;align-items:center;
      gap:4px 10px;margin:0;padding:6px 0}
    .outcomes li::before {content:'';position:absolute;left:-17px;top:19px;
      width:14px;border-top:2px solid #cbd7e8}
    .odds {background:#edf2f9;color:var(--accent)}
    .outcomes small {color:#43536b}
    @media(max-width:620px) {
      .dashboard {margin:16px auto} .hero,section {padding:20px}
      .payoff-grid {grid-template-columns:1fr}
    }
  "))),
  div(class = "dashboard",
    div(class = "hero",
      div(class = "eyebrow", "ASTRASYNC  /  DECISION ANALYSIS"),
      h1("Should AstraSync bid?"),
      p("18-month implementation  |  1.5% monthly discount rate  |  Payoffs in present-value dollars")),
    tags$section(h2("Summary"),
      h3(class = "decision-call", "Bid in the RFP."),
      p(class = "decision-detail",
        paste("Option C leads Option B by",
              money(baseline["C"] - baseline["B"]),
              "in expected present value, with a 40% chance of winning.")),
      div(class = "payoff-grid",
        payoff_card("A", "Fixed offer", "Guaranteed contribution"),
        payoff_card("B", "Bonus offer", "70% chance of earning the bonus"),
        payoff_card("C", "RFP bid", "Highest expected payoff at a 40% win probability"))),
    tags$section(h2("Analysis"),
      tags$ol(
        tags$li(strong("A. Accept fixed payments."),
          paste("Collect $152,000 monthly and earn $32,000 monthly after labor.",
                "Expected PV is", paste0(money(baseline["A"]), "."))),
        tags$li(strong("B. Accept the bonus offer."),
          paste("Collect $125,000 monthly and earn $5,000 monthly after labor.",
                "The $800,000 month-18 bonus has a 70% probability.",
                "Expected PV is", paste0(money(baseline["B"]), "."))),
        tags$li(strong("C. Bid in the RFP."),
          paste("A $150,000 monthly bid leaves $30,000 monthly after labor if AstraSync wins.",
                "The gain-share is paid in month 18 and averages",
                paste0(money(mean_gain * 1000000), "."),
                "The chance of winning is 40%. Expected PV is",
                paste0(money(baseline["C"]), ".")))),
      plotOutput("histogram", height = "340px")),
    tags$section(h2("Illustration"),
      p("Follow each decision to its possible outcomes. All payoffs are present values."),
      div(class = "tree-root", "Choose a contract path"),
      tags$ul(class = "tree-branches",
        tree_branch("A", "Accept the fixed offer",
          tree_outcome("100%", "Fixed payments", payoff_a)),
        tree_branch("B", "Accept the bonus offer",
          tree_outcome("70%", "Earn bonus", b_with_bonus),
          tree_outcome("30%", "Miss bonus", b_no_bonus)),
        tree_branch("C", "Bid in the RFP",
          tree_outcome("40%", "Win the bid", c_win, expected = TRUE),
          tree_outcome("60%", "Lose the bid", 0)))),
    tags$section(h2("Sensitivity analysis"),
      p("Change the win and bonus probabilities to see when the preferred alternative changes. Option A provides a fixed comparison."),
      fluidRow(
        column(6, sliderInput("win", "Option C win probability (%)", 0, 100, 40, step = 0.1)),
        column(6, sliderInput("bonus", "Option B bonus probability (%)", 0, 100, 70, step = 0.1))),
      tableOutput("scenario"),
      textOutput("switch_points")),
    tags$section(h2("Conclusion"),
      p("At the stated probabilities, C maximizes expected present value. Because the advantage is small, a modest change in the win or bonus probability can change the decision. A provides the most predictable contribution if AstraSync prioritizes certainty."))
  )
)

server <- function(input, output) {
  output$histogram <- renderPlot({
    ggplot(data.frame(gain_share), aes(gain_share)) +
      geom_histogram(binwidth = 0.15, boundary = 0,
                     fill = "#9ab4d7", color = "white") +
      geom_vline(aes(xintercept = mean_gain, color = "Mean"), linewidth = 1) +
      geom_vline(aes(xintercept = median_gain, color = "Median"),
                 linewidth = 1, linetype = "dashed") +
      scale_color_manual(name = NULL, values = c(Mean = option_colors[["B"]], Median = option_colors[["C"]])) +
      labs(title = "Option C gain-share distribution",
           subtitle = paste(format(n_sim, big.mark = ",", scientific = FALSE),
                            "simulations conditional on winning the RFP"),
           x = "Month-18 gain-share (millions of dollars)", y = "Simulations") +
      theme_minimal(base_size = 14)
  })

  output$scenario <- renderTable({
    current <- payoffs(input$win / 100, input$bonus / 100)
    data.frame(Option = names(current), "Expected PV" = money(current),
               Decision = ifelse(current == max(current), "Highest expected PV", ""),
               check.names = FALSE)
  }, width = "100%")

  output$switch_points <- renderText({
    best_non_c <- max(payoff_a, payoffs(0, input$bonus / 100)["B"])
    best_non_b <- max(payoff_a, payoffs(input$win / 100, 0)["C"])
    bonus_cutoff <- (best_non_b - b_no_bonus) / (bonus * discount)
    bonus_text <- if (bonus_cutoff > 1) {
      "B cannot lead even with a guaranteed bonus at this win probability."
    } else {
      paste("B ties the best competing option at a bonus probability of",
            paste0(pct(bonus_cutoff), "."))
    }
    paste("C ties the best competing option at a win probability of",
          paste0(pct(best_non_c / c_win), "."), bonus_text)
  })
}

shinyApp(ui, server)
