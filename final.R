library(shinydashboard)
library(DT)
library(readr)
library(ijtiff)
library(raster)
library(imager)
library(shiny)
library(shinyWidgets)
library(RMySQL)
library(DBI)

makereactivetrigger <- function() {
  rv <- reactiveValues(a = 0)
  list(
    depend = function() {
      rv$a
      invisible()
    },
    trigger = function() {
      rv$a <- isolate(rv$a + 1)
    }
  )
}
dbtrigger <- makereactivetrigger()
con <- dbConnect(MySQL(), user = 'root', password = '1234',
                 dbname = 'store', host = 'localhost')

ui<-dashboardPage(skin="red",
                  dashboardHeader(title="Store Management"),
                  dashboardSidebar(
                    width=200,
                    sidebarMenu(
                      menuItem(h4("Home") , tabName = "Home", icon = icon("")),
                      menuItem(h4("Product") , tabName = "Product", icon = icon("")),
                      menuItem(h4("Employee") , tabName = "Employee", icon = icon("")),
                      menuItem(h4("Customer"), tabName = "Customer", icon = icon("")),
                      menuItem(h4("Supplier"), tabName = "Supplier", icon = icon(""))


                    )),
                  dashboardBody(


                    tabItems(
                      tabItem("Home",
                              fluidPage(
                                h1("Welcome to Mobile Store",""),
                                h3("We do sales and Service for all type of mobile phones....",""),

                              )),
                      tabItem("Product",
                              fluidPage(
                                numericInput("pid", "Product ID",""),
                                textInput("pname", "Product Name",""),
                                textInput("stock", "Stock on Hand",""),
                                numericInput("price","Product Price",""),
                                numericInput("discount","Discount",""),
                                actionButton("writetoproduct", "Next Product")

                              )
                      ),

                      tabItem("Employee",
                              fluidPage(
                                numericInput("empid", "Employee ID",""),
                                textInput("empname", "Employee Name",""),
                                textInput("empage", "Employee Age",""),
                                numericInput("empsalary","Emplyee Salary",""),
                                radioButtons("empgender", "Employee gender",
                                             c("None selected" = "",
                                               "male",
                                               "female" ,
                                               "other"
                                             )),
                                actionButton("writetoemployee", "Next Employee")

                              )

                      ),

                      tabItem("Customer",
                              fluidPage(
                                numericInput("cid", "Customer ID",""),
                                textInput("cname", "Customer Name",""),
                                textInput("cage", "Customer Age",""),
                                numericInput("cpid","Product Id",""),
                                numericInput("noofmobile","No of mobiles Bought",""),
                                actionButton("writetocustomer", "Next Customer")

                              )

                      ),
                      tabItem("Supplier",
                              fluidPage(
                                numericInput("sid", "Supplier ID",""),
                                textInput("sname", "Supplier Name",""),
                                numericInput("spid","Product Id",""),
                                numericInput("stockdelivered","No of stock delivered",""),
                                actionButton("writetosupplier", "Next Supplier")

                              ),



                      )

                        ),
                      )
)

server <- function(input, output,session) {

  observeEvent(input$writetoproduct,{
    sql1 = "INSERT INTO product (pid, pname, stock, price, dicount) VALUES (?pid, ?pname, ?stock, ?price, ?discount)"
    sql <- sqlInterpolate(con, sql1, pid=input$pid, pname=input$pname, stock=input$stock, price=input$price, discount=input$discount)
    dbExecute(con, sql)
    dbtrigger$trigger()

    updateTextInput(session,"pid", "Product ID","")
    updateTextInput(session,"pname", "Product Name","")
    updateTextInput(session,"stock", "Stock","")
    updateTextInput(session,"price","Product Price","")
    updateTextInput(session,"discount","Discount","")
  })
  observeEvent(input$writetoemployee,{
    sql2 = "INSERT INTO employee (empid, empname, empage, empsalary, empgender) VALUES (?empid, ?empname, ?empage, ?empsalary, ?empgender)"
    sql3 <- sqlInterpolate(con, sql2, empid=input$empid, empname=input$empname, empage=input$empage, empsalary=input$empsalary, empgender=input$empgender)
    dbExecute(con, sql3)
    dbtrigger$trigger()

    updateTextInput(session,"empid", "Product ID","")
    updateTextInput(session,"empname", "Product Name","")
    updateTextInput(session, "empage","Employee Age","")
    updateTextInput(session,"empsalary", "Stock","")
    updateRadioButtons(session,"empgender", "Employee gender",
                       c("None selected" = "",
                         "male",
                         "female" ,
                         "other"
                         ))
  })
  observeEvent(input$writetocustomer,{
    sql4 = "INSERT INTO customer (cid, cname, cage, pid, noofmobile) VALUES (?cid, ?cname, ?cage, ?pid, ?noofmobile)"
    sql5 <- sqlInterpolate(con, sql4, cid=input$cid, cname=input$cname, cage=input$cage, pid=input$cpid, noofmobile=input$noofmobile)
    dbExecute(con, sql5)
    dbtrigger$trigger()

    updateTextInput(session,"cid", "Customer ID","")
    updateTextInput(session,"cname", "Customer Name","")
    updateTextInput(session, "cage","Customer Age","")
    updateTextInput(session,"cpid", "Product ID","")
    updateTextInput(session,"noofmobile","No oF Mobile","")
  })
  observeEvent(input$writetosupplier,{
    sql6 = "INSERT INTO supplier (sid, sname,pid,stockdelivered) VALUES (?sid, ?sname,?pid, ?stockdelivered)"
    sql7 <- sqlInterpolate(con, sql6, sid=input$sid, sname=input$sname, pid=input$spid, stockdelivered=input$stockdelivered)
    dbExecute(con, sql7)
    dbtrigger$trigger()

    updateTextInput(session,"sid", "Supplier ID","")
    updateTextInput(session,"sname", "Supplier Name","")
    updateTextInput(session,"spid", "Product ID","")
    updateTextInput(session,"stockdelivered","No of Stock Delivered","")
  })
}

shinyApp(ui, server)
