library(shiny)
library(shinythemes)
library(data.table)
library(RMySQL)
library(qcc)
library(xts)
library(dygraphs)
library(dplyr)
library(qicharts)
library(tidyverse)
library(summarytools)
conn = dbConnect(MySQL(), user = 'root', password = '1234',
                 dbname = 'store', host = 'localhost')
emp = dbSendQuery(conn, "SELECT * FROM employee")
a=fetch(emp)
pro = dbSendQuery(conn, "SELECT * FROM product")
b= fetch(pro)
sup = dbSendQuery(conn, "SELECT * FROM supplier")
c= fetch(sup)
values = qcc.groups(c$stockdelivered,c$gender)

about_page <- tabPanel(
  title = "About",
  titlePanel("About"),
  "Created with R Shiny",
  br(),
  "2021 April"
)

main_page <- tabPanel(
  title = "Analysis",
  tabsetPanel(
    tabPanel(
      title = "Plot",
      plotOutput("plot")
    ),
    tabPanel(
      title = "C Chart",
      plotOutput("plotc")
    ),
    tabPanel(
      title = "X Chart",
      plotOutput("plotx")
    ),
    tabPanel(
      title = "Cumsum",
      plotOutput("plotcus")
    ),
    tabPanel(
      title = "S chart",
      plotOutput("plots")
    ),
    tabPanel(
      title = "Decriptive Stat",
      tableOutput("mytable")
    ),
    tabPanel(
      title = "Anova BoxPlot",
      plotOutput('boxplot')    )

  )
)

ui <- navbarPage(
  title = "Data Analyser",
  theme = shinytheme('cerulean'),
  main_page,
  about_page
)
server <- function(input, output, session) {

  output$plot <- renderPlot({

    #Plot
    barplot(a$empage)

  })
  output$plotc <- renderPlot({

    qcc(b$price, type = "c", xlab="Dates", ylab="Values")
  })

  output$plotx <- renderPlot({
    sub_div <- c(b$pid)
    annual <- c(b$price)
    jan_feb <- c(b$dicount)
    new_df <- data.frame(sub_div,annual,jan_feb)
    colnames(new_df) <- c('empid','x1','x2')
    head(new_df)
    tail(new_df)
    new_df[,2:3]
    new_df$x.bar <- rowMeans(new_df[,2:3])
    head(new_df)
    qic(new_df$x.bar,chart = 'i',main = 'x bar chart',ylab = 'sample range', xlab = 'samples')

  })
  output$plotcus <- renderPlot({
    id <- (a$empid)
    salary <- (a$empsalary)
    length(salary)
    length(id)
    cf=c()
    for (x in 0:length(salary))
    {
      a <- salary[x] + salary[x+1]
      cf <- append(cf,a)
    }
    newdata <- data.frame(id,salary,cf) # new data created from existing.
    summation <- sum(newdata$salary)
    summation
    num <- nrow(newdata)
    central_line <- (summation/num)
    central_line
    deviation <- sd(newdata$salary)
    mean <- mean(newdata$salary)
    ucl <- (central_line + 3*sqrt(central_line))
    lcl <- (central_line - 3*sqrt(central_line))
    if (lcl <= 0) {
      lcl <- (0)
    }
    newdata
    csx <- cumsum(newdata$cf)
    plot(x = newdata$id,
         y = csx,
         main = "Cusum Chart",
         xlab = "empid",
         ylab = "Cumulative Sum Score",
         type = "o")
    abline(h = c(ucl,lcl) ,col = "red")


  })
  output$plots <- renderPlot({

    qcc(values, type ="S", xlab="Gender", ylab="Stocks")

  })
  output$mytable <- renderTable({
    m <- descr(a$empsalary,
               stats = c("mean", "sd", "min", "q1", "med", "q3", "max", "iqr", "cv"),
               transpose = FALSE)
    class(m) <-"matrix"
    m %>% as_tibble(rownames="Statistic")
  })


  output$boxplot <- renderPlot({
    boxplot(a$empsalary ~ a$empgender,data = a, main="Salary of Employee", xlab="GENDER",ylab = "SALARY")

  })


}

shinyApp(ui = ui, server = server)
