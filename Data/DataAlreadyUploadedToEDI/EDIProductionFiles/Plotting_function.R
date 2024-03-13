# Title: Functions for Sensors Visual Inspection 
# Use in the markdown to create plots and for plotly to work
# Author: Adrienne Breef-Pilz
# First developed: 28 December 24
# Last edited: 7 March 24

all_plot<-function(
    Var, 
    y_lab,  # This label can take an expression aka have the proper degrees C, 
    y_lab2, # This label is for the plotly function which can not handle expression argument. 
    Depth=F,  # Do you want depth as a factor
    Water=T, # Are these plots for inwater streaming sensors?
    Raw_file = T, # Do you have access to raw files to compare to. This is only for streaming sensors. 
    Use_plotly = F)  # Do you want to produce plotly interactive plots?
{ 
  
  # Start off with the extra plot as false and it will change to true if there are extra plots
  Extra_plot=FALSE
  
  # Are there raw files. This is only for streaming sensors where we have access to the raw files before qaqc
  if(Raw_file == T){
    switch_raw=T
    
    # If there are raw files to compare to then we only want to keep both observations if they are different.
    # It makes the file smaller. 
    # Let's just look at the current
    # only keep observations that are distinct for that variable
    
    df_unique <- current_plot_df[!duplicated(current_plot_df[c('DateTime', Var)]),]
    
    # create layers
    qaqc_current <- df_unique%>%filter(type=="qaqc")
    
    raw_current <- df_unique%>%filter(type=="raw")
    
  }else{
    switch_raw=F
    
    # rename the file
    qaqc_current <- current
  }
  
  
  
  if(Water==T){
    # Get the name of the reservoir
    Res = current_df[1, "Reservoir"]
    if(Res=="FCR" & Var %in% c("ThermistorTemp_C_5","ThermistorTemp_C_9")){
      Extra_plot = TRUE
      if (Var=="ThermistorTemp_C_5"){
        Var2="RDOTemp_C_5"
        Switch=F # Used to switch on and off the layer for the pressure sensor
      }else if (Var=="ThermistorTemp_C_9"){
        Var2="RDOTemp_C_9"
        Var3="LvlTemp_C_9"
        Switch=T # Used to switch on and off the layer for the pressure sensor
      } 
    }else if (Res=="BVR" & Var %in% c("ThermistorTemp_C_6","ThermistorTemp_C_13")){
      Extra_plot=TRUE
      if (Var=="ThermistorTemp_C_6"){
        Var2="RDOTemp_C_6"
        Switch=F # Used to switch on and off the layer for the pressure sensor
      }else if (Var=="ThermistorTemp_C_13"){
        Var2="RDOTemp_C_13"
        Var3="LvlTemp_C_13"
        Switch=T # Used to switch on and off the layer for the pressure sensor
      }
    }else if (Res=="CCR" & Var =="ThermistorTemp_C_13"){
      Extra_plot=TRUE
      Var2 = "LvlTemp_C_13"
      Switch=F # Used to switch on and off the layer for the pressure sensor
      
    }
    if(Extra_plot==TRUE){
      # Plot alltemp sensors at the same depth on top of each other
      # graph all
      com_all<-ggplot() +
        geom_scattermore(data=current, aes(x=DateTime, y=.data[[Var]], color="Therm"))+ 
        geom_scattermore(data=current, aes(x=DateTime, y=.data[[Var2]], color="RDO"))+
        {if(Switch)geom_scattermore(data=current, aes(x=DateTime, y=.data[[Var3]], color="Pressure"))}+ # print layer if Switch is TRUE
        ggtitle(paste0("All Water Temp from"," ",Var,", ",Var2)) +
        labs(y = y_lab,
             color = "Legend") +
        scale_color_manual(values = colors2)+
        theme_bw()
      
      # Plots of just the current year with all temperatures reading from the same depth at different sensors
      
      com_curr <- ggplot() +
        geom_scattermore(data = qaqc_current, aes(x = DateTime, y=.data[[Var]], color="Therm"), pointsize = 3)+ 
        geom_scattermore(data = qaqc_current, aes(x = DateTime, y=.data[[Var2]], color="RDO"), pointsize = 3)+
        {if(Switch)geom_scattermore(data = qaqc_current, aes(x = DateTime, y=.data[[Var3]],
                                                             color="Pressure"), pointsize = 3)}+ # print layer if Switch is TRUE
        ggtitle(paste0("Current Water Temp from"," ",Var,", ",Var2)) +
        labs(y = y_lab2,
             color = "Legend") +
        scale_color_manual(values = colors2)+
        theme_bw()
    }
    
  } 
  
  # Plot all of the observations
  if (Depth==T){
    
    all<- ggplot() +
      geom_scattermore(data=current_df, aes(x=DateTime, y=.data[[Var]], color=as.factor(Depth_m)), pointsize = 2)+
      ggtitle("All QAQCd",Var) +
      labs(y = y_lab,
           color = "Legend") +
      #scale_color_manual(values = colors)+
      theme_bw()
    
  }else {
    geom_scattermore(data=current_df, aes(x=DateTime, y=.data[[Var]], color="qaqc"))+
      ggtitle("All QAQCd",Var) +
      labs(y = y_lab,
           color = "Legend") +
      scale_color_manual(values = colors)+
      theme_bw()
    
  }
  
  
  # Create the current plotly so we can get the date of points out of range 
  
  # Battery and cable power don't need to be interactive
  
  if(grepl("Battery_V|power_V", Var)|Use_plotly==FALSE){
    
    if(Depth==T){
      cur <- ggplot()+
        {if(switch_raw)geom_scattermore(data= raw_current, aes(x=DateTime, y=.data[[Var]], 
                                                               color=as.factor(Depth_m),shape=type), pointsize = 3)}+
        geom_scattermore(data= qaqc_current, aes(x=DateTime, y=.data[[Var]], 
                                                 color=as.factor(Depth_m), shape=type), pointsize = 3)+
        ggtitle(paste0("Current: ",Var)) +
        labs(y = y_lab2,
             color = "Legend") +
        #scale_color_manual(values = colors)+
        theme_bw()
    }else{
      cur <- ggplot()+
        {if(switch_raw)geom_scattermore(data= raw_current, aes(x=DateTime, y=.data[[Var]], 
                                                               color=type), pointsize = 3)}+
        geom_scattermore(data= qaqc_current, aes(x=DateTime, y=.data[[Var]], 
                                                 color=type), pointsize = 3)+
        ggtitle(paste0("Current: ",Var)) +
        labs(y = y_lab2,
             color = "Legend") +
        scale_color_manual(values = colors)+
        theme_bw()
    }
  }else{
    
    if(Depth==T){
      cur <- {ggplot()+
          {if(switch_raw) geom_point(data= raw_current, aes(x=DateTime, y=.data[[Var]], 
                                                            color=as.factor(Depth_m),shape=type))}+
          geom_point(data= qaqc_current, aes(x=DateTime, y=.data[[Var]], 
                                             color=as.factor(Depth_m),shape=type))+
          ggtitle(paste0("Current: ",Var)) +
          labs(y = y_lab2,
               color = "Legend") +
          #scale_color_manual(values = colors)+
          theme_bw()}%>% ggplotly%>% as_widget
    }else{
      cur <- {ggplot()+
          {if(switch_raw) geom_point(data= raw_current, aes(x=DateTime, y=.data[[Var]], 
                                                            color=type))}+
          geom_point(data= qaqc_current, aes(x=DateTime, y=.data[[Var]], 
                                             color=type))+
          ggtitle(paste0("Current: ",Var)) +
          labs(y = y_lab2,
               color = "Legend") +
          scale_color_manual(values = colors)+
          theme_bw()}%>% ggplotly%>% as_widget
      
    }
  }
  
  # density plot
  if(Depth==T){
    den <-ggplot(data = daily, aes(x = .data[[Var]], group = Year, fill = Year))+
      geom_density(alpha=0.5)+
      xlab("Daily avg.")+
      ggtitle("All",Var) +
      theme_bw()+
      facet_wrap(~ as.factor(Depth_m), scale="free")
    
  }else{
    den <-ggplot(data = daily, aes(x = .data[[Var]], group = Year, fill = Year))+
      geom_density(alpha=0.5)+
      xlab("Daily avg.")+
      ggtitle("All",Var) +
      theme_bw()
  }
  
  
  # box plot
  if(Depth==T){
    box <-ggplot(data = daily, aes(x = Year, y = .data[[Var]], group = Year, fill = Year))+
      geom_boxplot()+
      ylab(y_lab)+
      ggtitle("Boxplot",Var) +
      theme_bw()+
      facet_wrap(~ as.factor(Depth_m), scales="free")
  }else{
    box <-ggplot(data = daily, aes(x = Year, y = .data[[Var]], group = Year, fill = Year))+
      geom_boxplot()+
      ylab(y_lab)+
      ggtitle("Boxplot",Var) +
      theme_bw()
    
  }
  # Add extra plots for chla and blue greens
  # Might need to Take out NA
  # data_comp <- daily[!is.na(daily$EXOChla_ugL_1), ] 
  
  if(grepl("Chla|BGAPC", Var)){
    
    daily_plot <- ggplot() +
      geom_point(data=current_df, aes(x=DateTime, y=.data[[Var]]))+
      {if(grepl("Chla", Var))geom_line(data=daily, aes(x=DateTime, y=.data[[Var]]), col="green")}+# print layer if Switch is TRUE
      {if(grepl("BGAPC", Var))geom_line(data=daily, aes(x=DateTime, y=.data[[Var]]), col="blue")}+
      ylab(y_lab)+
      ggtitle(paste0(Var, "Compare 10 minute observations to Daily mean"))+
      theme_bw()
  }
  
  
  
  # Make a list of the plots. 
  # For Depths with multiple temps let's add other plots
  
  if(Extra_plot==TRUE){
    newlist <- list(cur,all,den,box,com_curr,com_all) # have to list all outputs under one name
    
  }else if (grepl("Chla|BGAPC",Var)){
    
    newlist <- list(cur,all,den,box,daily_plot)
    
  }else{
    newlist <- list(cur,all,den,box)
  }
  
  # Prints the plots where they should be. 
  # Plotly would always print out of order. 
  # This is how I could get them working for each Variable
  
  
  for(j in 1:length(newlist)){
    x <- newlist[[j]]
    
    if (inherits(x, "plotly")) {
      # print the html piece of the htmlwidgets
      cat(renderTags(x)$html)
    } else {
      # print the ggplot graphs
      print(x)
      
    }
  }
  
  return(newlist)
}
