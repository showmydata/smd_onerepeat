# Repeated Measures for two measures
# Git info: https://carpentries.github.io/sandpaper-docs/github-pat.html

library(shiny)
library(stringr)
library(tidyr)
library(readr)
library("gsheet")
library(mvtnorm) # for rmvnorm in generating a random sample
library(psych) # for geometric.mean
library(sadists) # for qlambdap the lambda-prime distribution
source("functions/d_and_cis.R") 
# My scripts
source("functions/make_url.R") 
source("functions/parse_url.R") 
source("functions/add_data_link_to_url.R")
source("functions/get_data_from_url.R")
source("functions/ci_functions.R")
source("functions/equate_zscored_axis_ranges.R") 
source("functions/perc_rank_rm.R") 
source("functions/jitter_by_percent_min_wn2.R") 
source("functions/process_label.R")

shinyServer(  # Initiate the shiny server
  function(input, output, session) { # Create the function -- added 'session' for URL project 3/22/24

    output$ui_plot <- renderUI({
      if (input$dots_or_slopes=='slopes') {plotheight=input$plotheight_s; plotwidth=input$plotwidth_s} 
      else if (input$dots_or_slopes=='dots') {plotheight=input$plotsize; plotwidth=input$plotsize} 
      plotOutput("contents", width = plotwidth*8, height = plotheight*8)
      })
    output$contents <- renderPlot( { # Call Shiny function that makes the plot
      
      ### DATA INPUT ###
      if(input$myData>"") {
        # Next 3 lines added 8/15/23
        v=unlist(strsplit(input$myData,"\n")); v=unlist(strsplit(v[1],"\t")); # Read 'header' exactly, regardless of characters
        if(!all(is.na(as.numeric(v)))) for (i in 1:length(v)) v[i]=paste("column ",i); # If 'header' has any numbers (is not all words), replace with "column i"
        d0=gsub(",","",input$myData); d0=gsub("'","",d0); d0=gsub("‘","",d0); d0=gsub("’","",d0); d0=gsub('"',"",d0); d0=gsub("“","",d0); d0=gsub("”","",d0) # Replace various characters that produce errors
        for (i in 1:length(v)) { vv=v[i]; # For each variable label
        if (nchar(vv)>20) { # If the variable label is >20 length, add a carriage return at the last space before the 20th character
          b=unlist(gregexpr(' ', vv)); c=max(b[b<20]); vv=paste(substr(vv,1,c-1), "\n", substr(vv,c+1,nchar(vv)), sep=""); v[i]=vv
        }}
        
        d <- read.table(text = d0, sep = '\t', header = FALSE)
        d2 <- read.table(text = d0, sep = '\t', header = TRUE); 
        # v=as.character(colnames(d2)); v=gsub("\\.", " ", v) # Formerly got column names produced by read.table, now replaced by new lines above
        v=strtrim(v, 45)
      } else {
        d=attitude[,c(3,2)]; v=c("Knowledge\ntime 1","Knowledge\ntime 2"); colnames(d) <- v
        d=as.data.frame(get_data_from_url(d,session,input$datalink)); v=colnames(d)
      }
      d=as.data.frame(d);
      # 3/27/24 -- copy-pasted from TMB version of app -- hoping it will deal with periods and other characters in google sheet
      v=gsub(".", " ", v, fixed=TRUE); v=gsub(",","",v); v=gsub("'","",v); v=gsub("‘","",v); v=gsub("’","",v); v=gsub('"',"",v); v=gsub("“","",v); v=gsub("”","",v) # Replace various characters that produce errors
      for (i in 1:length(v)) { vv=v[i]; # For each variable label
      if (nchar(vv)>20) { # If the variable label is >20 length, add a carriage return at the last space before the 20th character
        b=unlist(gregexpr(' ', vv)); c=max(b[b<20]); vv=paste(substr(vv,1,c-1), "\n", substr(vv,c+1,nchar(vv)), sep=""); v[i]=vv
      }}
      
      
      ## DEAL WITH >2 COLUMNS OF DATA ##
      d=d[rowSums(d=="") != ncol(d), ] # Exclude completely blank rows from data set
      isdata=FALSE;
        dkeep=d;          # keep this copy of d for its labels
      for (i in 1:length(d)) { # for each column, set to numeric then see if it is at least 20% of numbers
        d[,i]=as.numeric(d[,i]); isdata[i]=FALSE; if(sum(!is.na(d[,i]))/length(d[,i])>.2) {isdata[i]=TRUE}
      }
      dolabels=FALSE      # set default to no labels
      if (length(d)>2) {  # if more than two columns
        dolabels=TRUE     # plan to assign one to labels
        w1=which(isdata)  # find the columns with data
        w2=which(!isdata) # find the columns without data
        dd=d[,w1[1:2]]    # get the first two data columns
        v=v[w1[1:2]]      # get the labels for the first two data columns
        if (length(w2)==0) thelabels=dkeep[,w1[3]] else thelabels=dkeep[,w2[1]] # get the labels column from one of two places
        thelabels[1]=NA
      } else {
        dd=d[,1:2]; # if column number wasn't greater than 2, just graph the two columns
      }
      cc=complete.cases(dd); # find the complete cases (excludes NAs in first row left by labels)
      if (length(d)>2) thelabels=thelabels[cc] # If there are labels, exclude incomplete rows
      d=dd[cc,];             # Exclude incomplete rows in data
      d=as.data.frame(d);
      
      ### DATA PROCESSING -- SLOPES ###
      if (input$dots_or_slopes=='slopes') {
      x_s=d[,1]; y_s=d[,2];
      # Transform data into percentile ranks
      if (input$spearman_s==TRUE) {temp_s=cbind(x_s,y_s); temp_s=perc_rank_rm(temp_s); x_s=temp_s[,1]; y_s=temp_s[,2]}
      # Jitter data percent of minimum difference between points for each column
      x1_s=jitter_by_percent_min_wn2(x_s,input$xjitter_s,ranked=FALSE)
      y1_s=jitter_by_percent_min_wn2(y_s,input$yjitter_s,ranked=FALSE)
      # Compute means, medians, and CIs
      dig_s=as.numeric(input$digits_s)
      xmean_s=mean(x_s)
      ymean_s=mean(y_s)
      xmedian_s=median(x_s)
      ymedian_s=median(y_s)
      z_s=y_s-x_s
      mean_difference_s=round(mean(z_s),dig_s)
      t_s=t.test(y_s, x_s, paired = TRUE)
      ci2_s=t_s$conf.int
      moe_s=(ci2_s[2]-ci2_s[1])/2
      n_s=sum(cc)
      r_s=cor(x_s,y_s)
      ci_r_s=round(CIr(r_s,n=n_s,level=.95),dig_s)
      # Old approach to d and CIs
      meansd_s=round(sqrt((sd(x_s)^2+sd(y_s)^2)/2),dig_s)
      standardized_mean_difference_raw_s=round(mean_difference_s/meansd_s,dig_s)
      standardized_ci2_raw_s=round(ci2_s/meansd_s,dig_s)
      # New approach to d and CIs
      standardized_mean_difference_s=round(dp(cbind(x_s,y_s)),dig_s)
      standardized_ci2_s=round(adjustedlambdaprime(standardized_mean_difference_s,cbind(x_s,y_s)),dig_s)
      if (standardized_mean_difference_s<0) {standardized_mean_difference_s=-standardized_mean_difference_s; standardized_ci2_s=c(-standardized_ci2_s[2],-standardized_ci2_s[1])}
      # T-test
      tt_s=t.test(x_s, y_s, paired=TRUE)
      # Find good axis ranges
      xmin_s=min(x_s); xmax_s=max(x_s); ymin_s=min(y_s); ymax_s=max(y_s); # Finds min and max values
      min_s=min(c(xmin_s,ymin_s)); if ((ymean_s-moe_s)<min_s) min_s=ymean_s-moe_s
      max_s=max(c(xmax_s,ymax_s)); if ((ymean_s+moe_s)>max_s) max_s=ymean_s+moe_s
      xlim_s=c(min_s, max_s)
      ylim_s=c(min_s, max_s)
      # Take specified range
      if(input$axisranges_s=="") stuff=1 else {rng_s=input$axisranges_s; rng_s=unlist(strsplit(rng_s,",")); 
                            rng_s=as.numeric(rng_s); ylim_s[1:length(rng_s)]=rng_s; if (length(ylim_s)>2) ylim_s=ylim_s[1:2]}
      # Get variable labels and measure label
      # measurelabel=input$measurelabel_s
      v1_s=process_label(input$xvariablelabel_s,v[1]); xlabel_s=v1_s; 
      v2_s=process_label(input$yvariablelabel_s,v[2]); ylabel_s=v2_s;
#      if (input$xvariablelabel_s=="") {
#        if (nchar(xlabel_s)>17) xlabel_s=gsub(" ", "\n", xlabel_s)
#        }
#      if (input$yvariablelabel_s=="") {
#        if (nchar(ylabel_s)>17) ylabel_s=gsub(" ", "\n", ylabel_s)
#        }
      extra_margin_s=max(str_count(xlabel_s,"\n"),str_count(ylabel_s,"\n"),str_count(input$measurelabel_s,"\n")+1) # max carriage returns in a label (to adjust plot margins)
      title_extra_s=str_count(input$graphtitle_s,"\n") # carriage returns in the title
      # Compute statistics
      if (input$addmean_s) output$meantext_s = renderText(paste("Means (x = ",round(xmean_s,dig_s),", y = ",round(ymean_s,dig_s),")", ", n = " , n_s, "<br>",sep=""))
      if (input$addmedian_s) output$mediantext_s = renderText(paste("Medians (x = ",round(xmedian_s,dig_s),", y = ",round(ymedian_s,dig_s),")","<br>",sep=""))
      output$meandifftext_s = renderText(paste("Mean difference in original units = ",mean_difference_s,", 95% CI [",round(ci2_s[1],dig_s),", ",round(ci2_s[2],dig_s),"]<br>",sep=""))
      output$cohensdtext_raw_s = renderText(paste("Mean difference in SD units (Cohen's d) = ",standardized_mean_difference_raw_s,", 95% CI [",standardized_ci2_raw_s[1],", ",standardized_ci2_raw_s[2],"]<br>",sep=""))
      output$cohensdtext_unbiased_s = renderText(paste("Bias-corrected mean difference in SD units (Cohen's d unbiased) = ",standardized_mean_difference_s,", 95% CI [",standardized_ci2_s[1],", ",standardized_ci2_s[2],"]<br>",sep=""))
      output$corrtext_s = renderText(paste("r = ",round(r_s,dig_s),", 95% CI [",ci_r_s[1],", ",ci_r_s[2],"]<br>",sep=""))
      output$ttext_s = renderText(paste("t(", n_s-1, ") = ", signif(-tt_s$statistic,dig_s), ", ", "p = ", signif(tt_s$p.value,dig_s), "<br><br>",sep=""))
      } ### END DATA PROCESSING -- SLOPES ###
      
      
      ### DATA PROCESSING -- DOTS
      if (input$dots_or_slopes=='dots') {
      x=d[,1]; y=d[,2];
      # Transform data into percentile ranks
      if (input$spearman==TRUE) {temp=cbind(x,y); temp=perc_rank_rm(temp); x=temp[,1]; y=temp[,2]}
      # Jitter data percent of minimum difference between points for each column
      x1=jitter_by_percent_min_wn2(x,input$xjitter,ranked=FALSE)
      y1=jitter_by_percent_min_wn2(y,input$yjitter,ranked=FALSE)
      # Compute means, medians, and CIs
      dig=as.numeric(input$digits)
      xmean=mean(x)
      ymean=mean(y)
      xmedian=median(x)
      ymedian=median(y)
      z=y-x
      mean_difference=round(mean(z),dig)
      t=t.test(y, x, paired = TRUE)
      ci2=t$conf.int
      moe=(ci2[2]-ci2[1])/2
      n=sum(cc)
      r=cor(x,y)
      ci_r=round(CIr(r,n=n,level=.95),dig)
      # Old approach to d and CIs
      meansd=round(sqrt((sd(x)^2+sd(y)^2)/2),dig)
      standardized_mean_difference_raw=round(mean_difference/meansd,dig)
      standardized_ci2_raw=round(ci2/meansd,dig)
      # New approach to d and CIs
      standardized_mean_difference=round(dp(cbind(x,y)),dig)
      standardized_ci2=round(adjustedlambdaprime(standardized_mean_difference,cbind(x,y)),dig)
      if (standardized_mean_difference<0) {standardized_mean_difference=-standardized_mean_difference; standardized_ci2=c(-standardized_ci2[2],-standardized_ci2[1])}
      # T-test
      tt=t.test(x, y, paired=TRUE)
      # Find good axis ranges
      xmin=min(x); xmax=max(x); ymin=min(y); ymax=max(y); # Finds min and max values
      min=min(c(xmin,ymin)); if ((ymean-moe)<min) min=ymean-moe
      max=max(c(xmax,ymax)); if ((ymean+moe)>max) max=ymean+moe; if (dolabels) {labelnudge=(max-min)/60; max=max+labelnudge}
      xlim=c(min, max)
      ylim=c(min, max)
      # Take specified range
      if(input$axisranges=="") stuff=1 else {rng=input$axisranges; rng=unlist(strsplit(rng,",")); rng=as.numeric(rng); xlim[1:length(rng)]=rng; ylim[1:length(rng)]=rng}
      # Get variable labels
      v1=process_label(input$xvariablelabel,v[1]); xlabel=v1; 
      v2=process_label(input$yvariablelabel,v[2]); ylabel=v2;
      extra_margin=max(str_count(v1,"\n"),str_count(v2,"\n")) # max carriage returns in a label (to adjust plot margins)
      title_extra=str_count(input$graphtitle,"\n") # carriage returns in the title
      # Compute statistics
      if (input$addmean) output$meantext = renderText(paste("Means (x = ",round(xmean,dig),", y = ",round(ymean,dig),")", ", n = ", n, "<br>",sep=""))
      if (input$addmedian) output$mediantext = renderText(paste("Medians (x = ",round(xmedian,dig),", y = ",round(ymedian,dig),")","<br>",sep=""))
      output$meandifftext = renderText(paste("Mean difference = ",mean_difference,", 95% CI [",round(ci2[1],dig),", ",round(ci2[2],dig),"]<br>",sep=""))
      output$cohensdtext_raw = renderText(paste("Raw mean difference in SD units (Cohen's d raw) = ",standardized_mean_difference_raw,", 95% CI [",standardized_ci2_raw[1],", ",standardized_ci2_raw[2],"]<br>",sep=""))
      output$cohensdtext_unbiased = renderText(paste("Unbiased mean difference in SD units (Cohen's d unbiased) = ",standardized_mean_difference,", 95% CI [",standardized_ci2[1],", ",standardized_ci2[2],"]<br>",sep=""))
      output$corrtext = renderText(paste("r = ",round(r,dig),", 95% CI [",ci_r[1],", ",ci_r[2],"]<br>",sep=""))
      output$ttext = renderText(paste("t(", n-1, ") = ", signif(tt$statistic,dig), ", ", "p = ", signif(tt$p.value,dig),"<br><br>",sep=""))
      } ### END DATA PROCESSING -- DOTS ###
      
      # # If axis numbers are specified and the user specified an axis range, then get those axis numbers and add them to the plot, expanding the range. 
      # if (input$axisnums_s!="" & userspecifiedaxisrange) {axisnums_s=as.numeric(unlist(strsplit(input$axisnums_s,","))); 
      #   pl = pl + scale_y_continuous(expand = c(0, 0), labels=function(n){format(n, big.mark = ",", scientific = FALSE)}, breaks=axisnums_s)}
      # # Else if axis numbers are specified (and the user did not specify an axis range), then get those axis numbers and add them. 
      # else if (input$axisnums_s!="") {axisnums_s=as.numeric(unlist(strsplit(input$axisnums_s,","))); 
      #   pl = pl + scale_y_continuous(breaks=axisnums_s) }
      # # Else if just the range is specified, then just expand the range. 
      # else if (userspecifiedaxisrange) 
      #   pl <- pl + scale_y_continuous(expand = c(0, 0), labels=function(n){format(n, big.mark = ",", scientific = FALSE)})
      
      yaxt_flag <- if (input$axisnums_s!="") "n" else "s"
      
      ### DRAW SLOPE PLOT IN WINDOW ###
      if (input$dots_or_slopes=='slopes') {
      c_s=col2rgb(input$color_dot_s)/255
      makemyplot_s <- function() {
        #par(pty="s") # Forces the scatterplot to be square
        par(mar = c(5 + extra_margin_s*2, 4 + extra_margin_s*2, 4 + title_extra_s*3, 2) + 0.1) # adjust default axis margin for multiline labels: par(mar = c(lower,left,top,right)) as par(mar = c(5,4,4,2) + 0.1)
        #par(mar = c(4+extra_margin_s*2,6+extra_margin_s*2,4,2) + 0.1) # adjust default axis margin for multiline labels: par(mar = c(lower,left,top,right)) as par(mar = c(5,4,4,2) + 0.1)
        plot(0, 0, col = "white", xlab = "", ylab = input$measurelabel_s, xlim=c(0,3), ylim=ylim_s, cex.main=3, las = 1,
               cex.axis=input$numbersize/50, cex.lab=input$labelsize/40, xaxt="n", yaxt=yaxt_flag, frame=FALSE, main=input$graphtitle_s, mgp=c(4,1,0))
          axis(1, at = c(.5, 2.5), labels = c(xlabel_s, ylabel_s), cex.axis=input$labelsize/50, mgp=c(3,2.5+extra_margin_s*2,0))
          axis(side = 2, at = ylim_s, labels = FALSE)
          if (input$axisnums_s!="") {axisnums_s=as.numeric(unlist(strsplit(input$axisnums_s,","))); 
            axis(side = 2, at = c(axisnums_s), labels = c(axisnums_s), cex.axis = input$numbersize/50, las=1)}
          if (input$addgrid_s & input$axisnums_s=="") grid(lty=1, nx=NA, ny=NULL, lwd = 1, col = "lightgray")
          else if (input$addgrid_s) abline(h = c(axisnums_s), lty = 1, lwd = 1, col = "lightgray")
          segments(x0=numeric(length(x1_s))+.5, y0=x1_s, x1=numeric(length(y1_s))+2.5, y1=y1_s, 
                   lwd=input$ind_linewidths/10,  # line width
                   col=rgb(red=c_s[1], green=c_s[2], blue=c_s[3], alpha=input$lineopacity/100))      # dot color
          if (input$adddots) {
            points(numeric(length(x1_s))+.5, x1_s,
                   pch=19, cex=input$s_dotsize/20,
                   col=rgb(red=c_s[1], green=c_s[2], blue=c_s[3], alpha=input$s_dotopacity/100))
            points(numeric(length(y1_s))+2.5, y1_s,
                   pch=19, cex=input$s_dotsize/20,
                   col=rgb(red=c_s[1], green=c_s[2], blue=c_s[3], alpha=input$s_dotopacity/100))
          }

        #mtext(v1_s, side = 1, line = str_count(v1_s,"\n")*2+3, cex=2)
        # Add mean, median, 95% CI
        if (input$addmedian_s==TRUE) {
          points(.5,xmedian_s,pch=15,col=input$color_median_s,cex=input$mediansize_s/20)
          points(2.5,ymedian_s,pch=15,col=input$color_median_s,cex=input$mediansize_s/20)
          segments(x0=numeric(length(x1_s))+.5, y0=xmedian_s, x1=numeric(length(y1_s))+2.5, y1=ymedian_s,
                   lwd=input$median_linewidth_s/10,  # line width
                   col=input$color_median_s)
        } 
        if (input$addmean_s==TRUE) {
          #if (input$add95ci_s==TRUE) {
            #lines(c(.5,.5),c(xmean_s-moe_s,xmean_s+moe_s),col=input$color_ci_s, lwd=input$ci_lw_s/10)
            #lines(c(2.5,2.5),c(ymean_s-moe_s,ymean_s+moe_s),col=input$color_ci_s, lwd=input$ci_lw_s/10)
          #}
          points(.5,xmean_s,pch=17,col=input$color_mean_s,cex=input$meansize_s/20)
          points(2.5,ymean_s,pch=17,col=input$color_mean_s,cex=input$meansize_s/20)
          segments(x0=.5, y0=xmean_s, +2.5, y1=ymean_s,
                   lwd=input$mean_linewidth_s/10,  # line width
                   col=input$color_mean_s)
        }
        if (dolabels) {text(.45, x1_s, labels=thelabels, cex=0.9, pos=2, col=rgb(0,0,0,.5))}
        if (dolabels) {text(2.55, y1_s, labels=thelabels, cex=0.9, pos=4, col=rgb(0,0,0,.5))}
          #if (input$showstats) mtext(text=paste("     ",outputtext,sep=""), side=3, cex=1.25, adj=0, line=-1, font=2)
      }
      makemyplot_s()
      }
            
      ### DRAW DOTS PLOT IN WINDOW ###
      if (input$dots_or_slopes=='dots') {
      c=col2rgb(input$color_dot)/255
      makemyplot <- function() {
        par(pty="s") # Forces the scatterplot to be square
        par(mar = c(5 + extra_margin*2, 4 + extra_margin*2, 4 + title_extra*3, 2) + 0.1) # adjust default axis margin for multiline labels: par(mar = c(lower,left,top,right)) as par(mar = c(5,4,4,2) + 0.1)
        plot(0,0,col="white",
             main=input$graphtitle, xlab="", ylab=ylabel, cex.main=3, cex.axis=1.5,       # title & axis labels & font sizes
             xlim=xlim, ylim=ylim, frame=FALSE, cex.lab=2)                              # axis ranges & no frame
        if (input$addgrid) grid(lty=1, nx=NULL, ny=NULL, lwd = 1)
        points(x1, y1,
            pch=as.numeric(input$dottype), cex=input$dotsize/20, lwd=input$ringlw/10,    # dot type & size & line width
            col=rgb(red=c[1], green=c[2], blue=c[3], alpha=input$dotopacity/100))        # dot color
        mtext(v1, side = 1, line = str_count(v1,"\n")*2+3, cex=2)
        if(input$lsline==TRUE) abline(lm(y~x), col=input$lsline_color, lwd=input$lw_lsline/10)   # draw least-squares line
        if(input$xyline==TRUE) abline(a = 0, b = 1, col=input$xyline_color, lwd=input$lw_xyline/10) # Draws xy line
        if (input$rug==TRUE) {
          rug(x1,side=1,quiet=TRUE,ticksize=.01); rug(y1,side=2,quiet=TRUE,ticksize=.01)
        }
        # Add mean, median, 95% CI
        if (input$addmedian==TRUE) {
          points(xmedian,ymedian,pch=15,col=input$color_median,cex=input$mediansize/20)
        }
        if (input$addmean==TRUE) {
          if (input$add95ci==TRUE) {
            lines(c(xmean,xmean),c(ymean-moe,ymean+moe),col=input$color_ci, lwd=input$ci_lw/10)
          }
          points(xmean,ymean,pch=15,col=input$color_mean,cex=input$meansize/20)
        }
        if (dolabels) {text(x1, y1+labelnudge, labels=thelabels, cex=0.9, pos=3, col=rgb(0,0,0,.5))}
        if (input$showdifferences) apply(cbind(x1,x1,y1,y1-(y1-x1)),1,function(coords){lines(coords[1:2],coords[3:4],col=input$xyline_color)}) # plots lsline residual lines
        #if (input$showstats) mtext(text=paste("     ",outputtext,sep=""), side=3, cex=1.25, adj=0, line=-1, font=2)
      }
      makemyplot()
      }
        settings=reactiveValuesToList(input);
        theurl=make_url(settings, get_all=FALSE, 
                        datalink=input$datalink, 
                        appurl="https://showmydata.shinyapps.io/onerepeat"); 
        theurl=gsub("\\n","\n",theurl,fixed=TRUE); theurl=gsub("\n","newline",theurl,fixed=TRUE); #NEW
        output$clip <- renderUI({ rclipButton(inputId = "clipbtn", icon = icon("clipboard"), 
                                              label = "Copy link with current settings", 
                                              clipText = theurl)}) 

      ### WRITE SLOPE PLOT TO DEVICE OR TO DOWNLOAD ###
      if (input$dots_or_slopes=='slopes') {
      # Save as 'filename' the 'content'
      output$down_s <- downloadHandler(
        filename =  function() {
          paste("myplot", input$filetype_s, sep=".")
        },
        # content is a function with argument file. content writes the plot to the device
        content = function(file) {
          if(input$filetype_s == "png")
            png(file, units="in", width=input$plotwidth_s/9, height=input$plotheight_s/9, res=500) # make png file
          else if(input$filetype_s == "pdf")
            pdf(file, width=input$plotwidth_s/9, height=input$plotheight_s/9) # open the pdf device
          makemyplot_s()
          dev.off()  # turn the device off
        })
      }
      
            
      ### WRITE DOT PLOT TO DEVICE OR TO DOWNLOAD ###
      if (input$dots_or_slopes=='dots') {
      # Save as 'filename' the 'content'
      output$down <- downloadHandler(
        filename =  function() {
          paste("myplot", input$filetype, sep=".")
        },
        # content is a function with argument file. content writes the plot to the device
        content = function(file) {
          if(input$filetype == "png")
            png(file, units="in", width=input$plotsize/9, height=input$plotsize/9, res=500) # make png file
          else if(input$filetype == "pdf")
            pdf(file, width=input$plotsize/9, height=input$plotsize/9) # open the pdf device
          makemyplot()
          dev.off()  # turn the device off
        })
      }
      
    })
    
    # Get link, Make link, Add URL
    observe({ urlstring=session$clientData$url_search; if (urlstring!="") session <- parse_url(urlstring, session) }) # updates session
    
  })