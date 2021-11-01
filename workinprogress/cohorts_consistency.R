# internal consistency  {{{
plotInternalConsistency <-  function(idx,log.scales=TRUE,
                                     cols=c("white", "yellow", "red"),use.rsq=TRUE,mark.significant=FALSE,...)
{
  
  # Define colour function
  if(!length(cols)>0) stop("Colour definitions do not contain sufficient number of colours (at least one required)")
  if(length(cols)==1) cols <- rep(cols,2)
  colFn <- colorRamp(colors=cols)
  
  #Number of ages
  ages <- dimnames(idx@index)[[1]]
  
  #Convert to Cohorts, reshape into appropriate format for splom
  flc <-  if(log.scales) {log10(idx@index)} 
  else {idx@index}
  flc  <- as.data.frame(FLCohort(flc))
  flc.wide <-  reshape(flc,direction="wide",timevar=names(flc)[1],idvar=names(flc)[2:6])
  names(flc.wide) <-  gsub("data.","",names(flc.wide))
  
  #Default plot settings
  plot.args <- list(~flc.wide[ages],data=flc.wide, pscales=0,varname.font=2,varname.cex=1.5,
                    xlab = if(log.scales) {expression(paste(Log[10]," (Index Value)"))} 
                    else {"Index Value"},
                    ylab = if(log.scales) {expression(paste(Log[10]," (Index Value)"))}
                    else { "Index Value"},
                    sub=list(if(use.rsq) {expression(paste("Lower right panels show the Coefficient of Determination (",italic(r^2),")"))}
                             else { expression(paste("Lower right panels show the Coefficient of Correlation (",italic(r),")"))},cex=0.7),
                    upper.panel=function(x,y,...)
                    {
                      # Filter out NAs
                      both.points  <-  is.finite(x) & is.finite(y)
                      x.filtered <-  x[both.points]
                      y.filtered <-  y[both.points]
                      # Only plot lmline if there is more than one point - colour panel according to rsq.
                      if(length(x.filtered)>2)
                      {
                        r <-  cor(y.filtered,x.filtered)    
                        if(use.rsq) {
                          panel.colour <- r^2           #Colour & number panel based on the coefficient of determination (r^2)
                        } else {
                          panel.colour <- 0.5*r+0.5     #Colour & number panel based on the correlation coefficient (r)
                        }
                        if(is.numeric(mark.significant) | identical(TRUE,mark.significant) ) {
                          lm.model <- lm(y.filtered ~ x.filtered)
                          p.value  <- summary(lm.model)$coefficients["x.filtered",4]/2    #Halve the p-value, as we are doing a one sided test, not a two
                          slope    <- summary(lm.model)$coefficients["x.filtered",1]
                          signif.level <- 0.05
                          if(is.numeric(mark.significant)) signif.level <- mark.significant 
                          if(p.value < signif.level & slope >0) {  #If marking significance, only fill panel and draw line when its significant
                            number.format <- "%4.3f*"      #If its a significant correlation, mark with a *
                            panel.fill(col = rgb(colFn(panel.colour),maxColorValue=255))   #Colour panel based on the coefficient of determination (r^2)
                            panel.lmline(x.filtered,y.filtered,lwd=2)
                          }                
                        } else {  #If not marking significance, always fill panel and draw best fit line
                          panel.fill(col = rgb(colFn(panel.colour),maxColorValue=255))   #Colour panel based on the coefficient of determination (r^2)
                          panel.lmline(x.filtered,y.filtered,lwd=2)
                        }
                      }
                      panel.splom(x.filtered,y.filtered,col="black",...)
                    },
                    lower.panel=function(x, y, ...)
                    {
                      #Filter out NAs
                      both.points  <-  is.finite(x) & is.finite(y)
                      x.filtered <-  x[both.points]
                      y.filtered <-  y[both.points]
                      
                      #Calculate r squared - but only if there is enough data to do so
                      if(length(x.filtered)>2)
                      {
                        r <-  cor(y.filtered,x.filtered)
                        if(use.rsq) {
                          panel.colour <- r^2           #Colour & number panel based on the coefficient of determination (r^2)
                          panel.number <- round(r^2,3)
                        } else {
                          panel.colour <- 0.5*r+0.5  #Colour & number panel based on the correlation coefficient (r)
                          panel.number <- round(r,3)
                        }
                        number.format <- "%4.3f"
                        if(is.numeric(mark.significant) | identical(TRUE,mark.significant) ) {
                          lm.model <- lm(y.filtered ~ x.filtered)
                          p.value  <- summary(lm.model)$coefficients["x.filtered",4]/2
                          slope    <- summary(lm.model)$coefficients["x.filtered",1]
                          signif.level <- 0.05
                          if(is.numeric(mark.significant)) signif.level <- mark.significant 
                          if(p.value < signif.level & slope > 0) {  #If marking significance, only fill panel when its significant & positive
                            number.format <- "%4.3f*"      #If its a significant correlation, mark with a *
                            panel.fill(col = rgb(colFn(panel.colour),maxColorValue=255))   #Colour panel based on the coefficient of determination (r^2)
                          }                
                        } else {  #If not marking significance, always fill panel 
                          panel.fill(col = rgb(colFn(panel.colour),maxColorValue=255))   #Colour panel based on the coefficient of determination (r^2)
                        }
                        grid::grid.text(label =sprintf(number.format,panel.number),x = unit(0.5, "npc"),
                                        y = unit(0.5,"npc"),just="center")}})
  
  #Passed settings
  passed.args   <- list(...)
  plot.args[names(passed.args)] <- passed.args
  
  #Do plot
  p <- do.call(splom,plot.args)
  print(p)
  return(p)
}