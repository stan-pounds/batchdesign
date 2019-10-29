#install.packages(shiny)
library(shiny)

###############################################
# Define a function to generate a study design

design.study=function(n.tot,              # total number of subjects
                      n.per.batch,        # number of subjects that can be evaluated per batch
                      trts=c("A","B"))    # list of treatments to be evaluated
  
{ # begin function
  ID=sample(n.tot)                          # randomly order subjects
  n.trt=length(trts)                        # number of treatments
  n.batch=ceiling(n.tot/n.per.batch)        # number of batches
  btch.lbls=rep(1:n.batch,each=n.per.batch) # generate batch labels
  btch.lbls=btch.lbls[1:n.tot]              # truncate batch labels to match sample size   
  trt.lbls=rep("",n.tot)                    # initialize treatment labels for subjects
  for (i in 1:n.batch)                      # for each batch, do the following
  {                                         # begin loop over batches
    in.batch=(btch.lbls==i)                       # identify subjects assigned to this batch
    N.batch=sum(in.batch)                         # determine number of subjects for this batch
    tr.lbls=rep(trts,each=ceiling(N.batch/n.trt)) # initial set of treatment labels for this batch
    tr.ord=sample(length(tr.lbls))[1:N.batch]     # order of treatment assignments for this batch
    trt.lbls[in.batch]=tr.lbls[tr.ord]            # treatment assignments for this batch
  }                                         # end loop over batches
  
  # create a data.frame with the study design
  res=cbind.data.frame(ID=ID,
                       batch=btch.lbls,
                       treatment=trt.lbls)
  
  # return the result
  return(res)
  
} # end function

############################
# User Interface

ui=fluidPage(
  HTML("<H1>Study Design: Treatment and Batch Allocation</H1>"),                # Page Title     
  sidebarLayout(
                sidebarPanel(                                     # Begin Side Bar Panel
                             numericInput(inputId="totN",         # Input Box for Total Sample size
                                          "Total Sample Size",
                                          value=12),
                             
                             numericInput(inputId="batchSize",    # Input Box for Batch Size
                                          "Number per Batch",
                                         value=4),
                             
                            numericInput(inputId="Ntreat",        # Input Box for Number of Treatments
                                         "Number of Treatments",
                                         value=2),
                            
                            actionButton("go","Go")),             # Go button
              
                # Define the main panel for the user interface  
              mainPanel(HTML("<H3>Design Summary: Cross-Tabulation</H3>"),
                        tableOutput('cx.tab'),                                # Main panel has two tables: batch-treatment cross-tab
                        HTML("<H3>Design Details: Subject Assignments</H3>"),
                        tableOutput('design.table'))                          # The overall design table
              ))                                                              # End main panel & ui
            
###################################
# Back-end server actions

server=function(input,  # list of data elements coming from UI
                output) # list of data elements going to UI
{
  observeEvent(input$go,                                               # After go button is pressed, do the following:
               {design.table=design.study(input$totN,                    # Design the study
                                        input$batchSize,
                                        trt=1:input$Ntreat)          
               
               cx.table=table(Batch=design.table$batch,                  # cross-tabulate batch and treatment
                              Treatment=design.table$treatment)
               
               cx.dframe=as.data.frame(cx.table)                         # convert to data frame for rendering
               colnames(cx.dframe)[3]="Number of Subjects"
               

               colnames(design.table)=c("Subject Number",                # column names for rendering on UI
                                        "Batch Number",
                                        "Treatment Number")
               
               output$design.table=renderTable(design.table,             # render the design table as output$design.table
                                               align='c')
               
               output$cx.tab=renderTable(cx.dframe,align='c')            # render the cross-tabulation as ouput$cx.tab
               })
}

# Create and launch the app
shinyApp(ui=ui,server=server)