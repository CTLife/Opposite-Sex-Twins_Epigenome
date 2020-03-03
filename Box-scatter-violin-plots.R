library("ggplot2") 

MyTheme_1_g <- function(textSize1=14, hjust1=1, vjust1=1,  angle1=30) {    # "hjust=1, vjust=1, angle=30" for some boxplots.
  theme(  
    line  = element_line(colour="black",  size=1.0,   linetype=1,      lineend=NULL),                                                                                        ## all line elements.          局部优先总体,下面3个也是,只对非局部设置有效.   所有线属性.
    rect  = element_rect(colour="black",  size=1.0,   linetype=1,      fill="transparent" ),                                                                                 ## all rectangluar elements.    hjust=1: 靠右对齐.   所有矩形区域属性.
    text  = element_text(family="serif",  face="plain",  colour="black",  size=textSize1, hjust=0.5, vjust=0.5,   angle=0, lineheight=1.0,  margin = NULL, debug = NULL),    ## all text elements.           "serif" for a serif font. 所有文本相关属性.
    title = element_text(family="serif",  face="plain",  colour="black",  size=textSize1, hjust=0.5, vjust=0.5,   angle=0, lineheight=1.0,  margin = NULL, debug = NULL),    ## all title elements: plot, axes, legends.    hjust:水平对齐的方向.  所有标题属性.
    ## aspect.ratio = 1,   ##高宽比
    
    axis.title    = element_text(family="serif", face="plain", colour="black", size=textSize1,    hjust=0.5,    vjust=0.5,    angle=0,       lineheight=1.0,  margin = NULL, debug = NULL),       ## label of axes (element_text; inherits from text).  horizontal: 水平的, 水平线 
    axis.title.x  = element_text(family="serif", face="plain", colour="black", size=textSize1,    hjust=0.5,    vjust=0.5,    angle=0,       lineheight=1.0,  margin = NULL, debug = NULL),       ## x axis label (element_text; inherits from axis.title)
    axis.title.y  = element_text(family="serif", face="plain", colour="black", size=textSize1,    hjust=0.5,    vjust=0.5,    angle=90,      lineheight=1.0,  margin = NULL, debug = NULL),       ## y axis label (element_text; inherits from axis.title)
    axis.text     = element_text(family="serif", face="plain", colour="black", size=textSize1,    hjust=0.5,    vjust=0.5,    angle=0,       lineheight=1.0,  margin = NULL, debug = NULL),       ## tick labels along axes (element_text; inherits from text). 坐标轴刻度的标签的属性.                                                         
    axis.text.x   = element_text(family="serif", face="plain", colour="black", size=textSize1,    hjust=hjust1, vjust=vjust1, angle=angle1,  lineheight=1.0,  margin = NULL, debug = NULL),       ## x axis tick labels (element_text; inherits from axis.text)
    axis.text.y   = element_text(family="serif", face="plain", colour="black", size=textSize1,    hjust=0.5,    vjust=0.5,    angle=0,       lineheight=1.0,  margin = NULL, debug = NULL),       ## y axis tick labels (element_text; inherits from axis.text)
    
    axis.ticks        = element_line(colour="black", size=0.5, linetype=1, lineend=NULL),          ## tick marks along axes (element_line; inherits from line). 坐标轴刻度线.
    axis.ticks.x      = element_line(colour="black", size=0.5, linetype=1, lineend=NULL),          ## x axis tick marks (element_line; inherits from axis.ticks)
    axis.ticks.y      = element_line(colour="black", size=0.5, linetype=1, lineend=NULL),          ## y axis tick marks (element_line; inherits from axis.ticks)
    axis.ticks.length = grid::unit(2.0,   "mm",   data=NULL),                                      ## length of tick marks (unit), ‘"mm"’ Millimetres.  10 mm = 1 cm.  刻度线长度
    axis.line         = element_line(colour="transparent", size=0.3, linetype=1, lineend=NULL), 	 ## lines along axes (element_line; inherits from line). 坐标轴线
    axis.line.x       = element_line(colour="transparent", size=0.3, linetype=1, lineend=NULL), 	 ## line along x axis (element_line; inherits from axis.line)
    axis.line.y       = element_line(colour="transparent", size=0.3, linetype=1, lineend=NULL),	   ## line along y axis (element_line; inherits from axis.line)
    
    legend.background    = element_rect(colour="transparent", size=1, linetype=1, fill="transparent" ), 	      ## background of legend (element_rect; inherits from rect)
    legend.spacing       = grid::unit(1, "mm", data=NULL), 	                                                    ## extra space added around legend (unit). linetype=1指的是矩形边框的类型.
    legend.key           = element_rect(colour="transparent", size=2, linetype=1, fill="transparent" ), 	      ## background underneath legend keys. 图例符号. size=1指的是矩形边框的大小.
    legend.key.size      = grid::unit(6,   "mm", data=NULL) , 	                                                ## size of legend keys   (unit; inherits from legend.key.size)
    legend.key.height    = grid::unit(6.5, "mm", data=NULL) , 	                                                ## key background height (unit; inherits from legend.key.size)
    legend.key.width     = grid::unit(8,   "mm", data=NULL) ,                                                   ## key background width  (unit; inherits from legend.key.size)
    legend.text          = element_text(family="serif", face=NULL, colour="black", size=textSize1, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL), 	##legend item labels. 图例文字标签.
    legend.text.align    = 0, 	                    ## alignment of legend labels (number from 0 (left) to 1 (right))
    legend.title         = element_blank(),   	    ## title of legend (element_text; inherits from title)
    legend.title.align   = 0, 	                    ## alignment of legend title (number from 0 (left) to 1 (right))
    legend.position      = "right", 	              ## the position of legends. ("left", "right", "bottom", "top", or two-element numeric vector)
    legend.direction     = "vertical",        	    ## layout of items in legends  ("horizontal" or "vertical")   图例排列方向
    legend.justification = "center",      	        ## anchor point for positioning legend inside plot ("center" or two-element numeric vector)  图例居中方式
    legend.box           = NULL, 	                  ## arrangement of multiple legends ("horizontal" or "vertical")  多图例的排列方式
    legend.box.just      = NULL, 	                  ## justification of each legend within the overall bounding box, when there are multiple legends ("top", "bottom", "left", or "right")  多图例的居中方式
    
    panel.background   = element_rect(colour="transparent", size=0.0, linetype=1, fill="transparent" ),   	## background of plotting area, drawn underneath plot (element_rect; inherits from rect)
    panel.border       = element_rect(colour="black", size=0.5, linetype=1, fill=NA ), 	                    ## border around plotting area, drawn on top of plot so that it covers tick marks and grid lines. This should be used with fill=NA (element_rect; inherits from rect)
    panel.spacing      = grid::unit(1, "mm", data=NULL) , 	                                                ## margin around facet panels (unit)  分面绘图区之间的边距
    panel.spacing.x    = grid::unit(1, "mm", data=NULL) ,
    panel.spacing.y    = grid::unit(1, "mm", data=NULL) ,
    panel.grid         = element_blank(), 	                                                                ## grid lines (element_line; inherits from line)  绘图区网格线
    panel.grid.major   = element_line(colour="transparent", size=NULL, linetype=NULL, lineend=NULL) , 	    ## major grid lines (element_line; inherits from panel.grid)  主网格线
    panel.grid.minor   = element_line(colour="transparent", size=NULL, linetype=NULL, lineend=NULL) ,     	## minor grid lines (element_line; inherits from panel.grid)  次网格线
    panel.grid.major.x = element_line(colour="transparent", size=NULL, linetype=NULL, lineend=NULL) , 	    ## vertical major grid lines (element_line; inherits from panel.grid.major)
    panel.grid.major.y = element_line(colour="transparent", size=NULL, linetype=NULL, lineend=NULL) ,     	## horizontal major grid lines (element_line; inherits from panel.grid.major)
    panel.grid.minor.x = element_line(colour="transparent", size=NULL, linetype=NULL, lineend=NULL) ,     	## vertical minor grid lines (element_line; inherits from panel.grid.minor)
    panel.grid.minor.y = element_line(colour="transparent", size=NULL, linetype=NULL, lineend=NULL) ,     	## horizontal minor grid lines (element_line; inherits from panel.grid.minor)
    
    plot.background	= element_rect(colour="transparent", size=NULL, linetype=NULL, fill="transparent" ),                                                ## background of the entire plot (element_rect; inherits from rect)  整个图形的背景
    plot.title      = element_text(family="serif", face=NULL, colour="black", size=textSize1, hjust=0.5, vjust=0.5,   angle=NULL, lineheight=NULL),     ## plot title (text appearance) (element_text; inherits from title)  图形标题
    plot.margin     = grid::unit(c(5, 5, 5, 5), "mm", data=NULL), 	                                                                                    ## margin around entire plot (unit with the sizes of the top, right, bottom, and left margins)
    
    strip.background = element_rect(colour=NULL,    size=NULL, linetype=NULL, fill=NULL ), 	                                                      ## background of facet labels (element_rect; inherits from rect)  分面标签背景
    strip.text       = element_text(family="serif", face=NULL, colour=NULL, size=NULL, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL), 	    ## facet labels (element_text; inherits from text)
    strip.text.x     = element_text(family="serif", face=NULL, colour=NULL, size=NULL, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL), 	    ## facet labels along horizontal direction (element_text; inherits from strip.text)
    strip.text.y     = element_text(family="serif", face=NULL, colour=NULL, size=NULL, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL)   	  ## facet labels along vertical direction (element_text; inherits from strip.text) 
  ) 
} 


MySaveGgplot2_1_g <- function(ggplot2Figure1,  path1, fileName1,  height1, width1) {
  SVG1 <- paste(path1,  "/",  "SVG",  sep = "",  collapse = NULL)
  PNG1 <- paste(path1,  "/",  "PNG",  sep = "",  collapse = NULL)
  PDF1 <- paste(path1,  "/",  "PDF",  sep = "",  collapse = NULL)
  EPS1 <- paste(path1,  "/",  "EPS",  sep = "",  collapse = NULL)
  if( ! file.exists(SVG1) ) { dir.create(SVG1) }
  if( ! file.exists(PNG1) ) { dir.create(PNG1) }
  if( ! file.exists(PDF1) ) { dir.create(PDF1) }
  if( ! file.exists(EPS1) ) { dir.create(EPS1) }
  ggsave( filename = paste(SVG1,  "/",  fileName1,  ".svg",  sep="",  collapse=NULL),     height=height1,    width=width1,      dpi = 1200 )
  ggsave( filename = paste(PNG1,  "/",  fileName1,  ".png",  sep="",  collapse=NULL),     height=height1,    width=width1,      dpi = 1200 )
  ggsave( filename = paste(PDF1,  "/",  fileName1,  ".pdf",  sep="",  collapse=NULL),     height=height1,    width=width1,      dpi = 1200 )
  ggsave( filename = paste(EPS1,  "/",  fileName1,  ".eps",  sep="",  collapse=NULL),     height=height1,    width=width1,      dpi = 1200,   device=cairo_ps)         
}


## 1 feature as type
MyBoxViolinPlot_1_f <- function(vector2,   sampleType2,  colours2,   path2,   fileName2,  title2,  xLab2,  yLab2,    height2=4,   width2=4,   Ymin2=0, Ymax2=3) { 
  vector2[vector2>Ymax2] <- Ymax2
  vector2[vector2<Ymin2] <- Ymin2
  DataFrame_Local  <- data.frame(   sampleType=sampleType2,   yAxis=vector2    ) 
  
  FigureTemp1 <- ggplot( DataFrame_Local, aes(x=sampleType, y=yAxis ) ) +  
    geom_boxplot( outlier.shape=NA, outlier.size=0, notch=FALSE,  notchwidth = 0.1,  alpha=1  ) +   
    stat_summary( position=position_dodge(width=0.9), fun.y=mean,  color="yellow4",  geom="point", shape=19, size=0.5, show.legend = FALSE) + 
    xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=12, hjust1=1, vjust1=1,  angle1=30 ) + ylim(Ymin2, Ymax2 )
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp1,  path1=path2, fileName1=paste(fileName2, "_boxPlot_NoFacet",   sep="",  collapse=NULL),  height1=height2, width1=width2-1)
  
  FigureTemp1a <- ggplot( DataFrame_Local, aes(x=sampleType, y=yAxis, fill=sampleType ) ) +  
    geom_boxplot( outlier.shape=NA, outlier.size=0, notch=FALSE,  notchwidth = 0.1,  alpha=1  ) +   
    stat_summary( position=position_dodge(width=0.9), fun.y=mean,  color="yellow4",  geom="point", shape=19, size=0.5, show.legend = FALSE) + 
    xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=12, hjust1=1, vjust1=1,  angle1=30 ) + ylim(Ymin2, Ymax2 )
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp1a,  path1=path2, fileName1=paste(fileName2, "_boxPlot_NoFacet2",   sep="",  collapse=NULL),  height1=height2, width1=width2)
  
  FigureTemp1b <- ggplot( DataFrame_Local, aes(x=sampleType, y=yAxis, fill=sampleType ) ) +  
    geom_boxplot( outlier.shape=NA, outlier.size=0, notch=FALSE,  notchwidth = 0.1,  alpha=1  ) +   
    stat_summary( position=position_dodge(width=0.9), fun.y=mean,  color="yellow4",  geom="point", shape=19, size=0.5, show.legend = FALSE) + 
    scale_fill_manual( values = colours2 ) + 
    xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=12, hjust1=1, vjust1=1,  angle1=30 ) + ylim(Ymin2, Ymax2 )
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp1b,  path1=path2, fileName1=paste(fileName2, "_boxPlot_NoFacet3",   sep="",  collapse=NULL),  height1=height2, width1=width2)
  
  FigureTemp1 <- ggplot( DataFrame_Local, aes(x=sampleType, y=yAxis ) ) +  
    geom_boxplot( outlier.size=0.1, notch=FALSE,  notchwidth = 0.1,  alpha=1  ) +   
    stat_summary( position=position_dodge(width=0.9), fun.y=mean,  color="yellow4",  geom="point", shape=19, size=0.5, show.legend = FALSE) + 
    xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=12, hjust1=1, vjust1=1,  angle1=30 ) + ylim(Ymin2, Ymax2 )
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp1,  path1=path2, fileName1=paste(fileName2, "_boxPlot_NoFacet_withOutlier",   sep="",  collapse=NULL),  height1=height2, width1=width2-1)                             
  
  FigureTemp1 <- ggplot( DataFrame_Local, aes(x=sampleType, y=yAxis, fill=sampleType  ) ) +  
    geom_boxplot( outlier.size=0.1, notch=FALSE,  notchwidth = 0.1,  alpha=1  ) +   
    stat_summary( position=position_dodge(width=0.9), fun.y=mean,  color="yellow4",  geom="point", shape=19, size=0.5, show.legend = FALSE) + 
    xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=12, hjust1=1, vjust1=1,  angle1=30 ) + ylim(Ymin2, Ymax2 )
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp1,  path1=path2, fileName1=paste(fileName2, "_boxPlot_NoFacet_withOutlier2",   sep="",  collapse=NULL),  height1=height2, width1=width2)
  
  FigureTemp1 <- ggplot( DataFrame_Local, aes(x=sampleType, y=yAxis, fill=sampleType  ) ) +  
    geom_boxplot( outlier.size=0.1, notch=FALSE,  notchwidth = 0.1,  alpha=1  ) +   
    stat_summary( position=position_dodge(width=0.9), fun.y=mean,  color="yellow4",  geom="point", shape=19, size=0.5, show.legend = FALSE) + 
    scale_fill_manual( values = colours2 ) + 
    xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=12, hjust1=1, vjust1=1,  angle1=30 ) + ylim(Ymin2, Ymax2 )
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp1,  path1=path2, fileName1=paste(fileName2, "_boxPlot_NoFacet_withOutlier3",   sep="",  collapse=NULL),  height1=height2, width1=width2)
  
  
  FigureTemp4a <- ggplot( DataFrame_Local, aes(x=sampleType, y=yAxis ) ) +  
    geom_violin(  colour = "red", fill="red", adjust=1.5  ) + 
    geom_boxplot( outlier.shape=NA, outlier.size=0, size=0.5,    width=0.6,     alpha=0.00001, position=position_dodge(width=0.5)    ) +   
    stat_summary( position=position_dodge(width=0.9), fun.y=mean,  color="yellow4",  geom="point", shape=19, size=0.5, show.legend = FALSE) + 
    xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=12, hjust1=1, vjust1=1,  angle1=30 ) + ylim(Ymin2, Ymax2 )
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp4a,  path1=path2, fileName1=paste(fileName2, "_violinBoxPlot_NoFacet",   sep="",  collapse=NULL),  height1=height2, width1=width2-1)
  
  FigureTemp4a <- ggplot( DataFrame_Local, aes(x=sampleType, y=yAxis, fill=sampleType ) ) +  
    geom_violin(  colour = NA  , adjust=1.5 ) + 
    geom_boxplot( outlier.shape=NA, outlier.size=0, size=0.5,    width=0.6,     alpha=0.00001, position=position_dodge(width=0.5)    ) +   
    stat_summary( position=position_dodge(width=0.9), fun.y=mean,  color="yellow4",  geom="point", shape=19, size=0.5, show.legend = FALSE) + 
    xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=12, hjust1=1, vjust1=1,  angle1=30 ) + ylim(Ymin2, Ymax2 )
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp4a,  path1=path2, fileName1=paste(fileName2, "_violinBoxPlot_NoFacet2",   sep="",  collapse=NULL),  height1=height2, width1=width2)
  
  FigureTemp4a <- ggplot( DataFrame_Local, aes(x=sampleType, y=yAxis, fill=sampleType ) ) +  
    geom_violin(  colour = NA , adjust=1.5  ) + 
    geom_boxplot( outlier.shape=NA, outlier.size=0, size=0.5,    width=0.6,     alpha=0.00001, position=position_dodge(width=0.5)    ) +   
    stat_summary( position=position_dodge(width=0.9), fun.y=mean,  color="yellow4",  geom="point", shape=19, size=0.5, show.legend = FALSE) + 
    scale_fill_manual( values = colours2 ) +
    xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=12, hjust1=1, vjust1=1,  angle1=30 ) + ylim(Ymin2, Ymax2 )
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp4a,  path1=path2, fileName1=paste(fileName2, "_violinBoxPlot_NoFacet3",   sep="",  collapse=NULL),  height1=height2, width1=width2)
  
  
}  


## Add scatter plot
MyBoxViolinPlot_2 <- function(vector2,   sampleType2,    path2,   fileName2,    title2,  xLab2,  yLab2,   height2=4,  width2=4,   Ymin2=0, Ymax2=3, alpha2=0.4) {                                                     
  vector2[vector2>Ymax2] <- Ymax2
  vector2[vector2<Ymin2] <- Ymin2
  DataFrame1  <- data.frame(   sampleType=sampleType2,   yAxis=vector2 )  
  
  FigureTemp1 <- ggplot( DataFrame1, aes(x=sampleType) ) +   
    geom_jitter(aes(y=yAxis), size=0.1, colour="grey1", alpha=alpha2, position = position_jitter(width=0.25) ) +
    geom_boxplot( alpha=0, width=0.7,   aes(y=yAxis),  outlier.size=0,  size=0.7,  fill=NA, colour="red") +    
    xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=14, hjust1=1, vjust1=1,  angle1=30 )  + ylim(Ymin2, Ymax2 )
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp1,  path1=path2, fileName1=paste(fileName2, "-ScatterBoxPlot",        sep="",  collapse=NULL),  height1=height2, width1=width2)
  
  FigureTemp2 <- ggplot(DataFrame1, aes(x=sampleType) ) +   
    geom_jitter(aes(y=yAxis), size=0.1, colour="grey1", alpha=alpha2, position = position_jitter(width=0.25) ) +
    geom_violin(aes(y=yAxis), fill = NA, colour = "blue", adjust = 3,  alpha=0, size=0.6) +  
    geom_boxplot( alpha=0, width=0.3,   aes(y=yAxis),  outlier.size=0,  size=0.6,  fill=NA, colour="red" ) +    
    stat_summary(   aes(y=yAxis),   fun.y=mean, colour="yellow3", geom="point", shape=19, size=0.0, show.legend = FALSE) + 
    xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=14, hjust1=1, vjust1=1,  angle1=30 )  + ylim(Ymin2, Ymax2 )
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp2,  path1=path2, fileName1=paste(fileName2, "-ViolinPlot-3adjust",    sep="",  collapse=NULL),  height1=height2, width1=width2)
  
  FigureTemp3 <- ggplot(DataFrame1, aes(x=sampleType) ) +  
    geom_jitter(aes(y=yAxis), size=0.1, colour="grey1", alpha=alpha2, position = position_jitter(width=0.25) ) +
    geom_violin(aes(y=yAxis), fill = NA, colour = "blue",   alpha=0, size=0.6) +  
    geom_boxplot( alpha=0, width=0.3,   aes(y=yAxis),  outlier.size=0,  size=0.6,  fill=NA, colour="red" ) +    
    stat_summary(   aes(y=yAxis),   fun.y=mean, colour="yellow3", geom="point", shape=19, size=0.0, show.legend = FALSE) + 
    xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=14, hjust1=1, vjust1=1,  angle1=30 )  + ylim(Ymin2, Ymax2 )
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp3,  path1=path2, fileName1=paste(fileName2, "-ViolinPlot-noAdjust",   sep="",  collapse=NULL),  height1=height2, width1=width2)
}  



## Add scatter plot
MyBoxViolinPlot_3 <- function(vector2,   sampleType2,    path2,   fileName2,    title2,  xLab2,  yLab2,   height2=4,  width2=4,   Ymin2=0, Ymax2=3, alpha2=0.4) {                                                     
  vector2[vector2>Ymax2] <- Ymax2
  vector2[vector2<Ymin2] <- Ymin2
  DataFrame1  <- data.frame(   sampleType=sampleType2,   yAxis=vector2 )  
  
  FigureTemp1 <- ggplot( DataFrame1, aes(x=sampleType) ) +   
    geom_jitter(aes(y=yAxis), size=0.3, colour="grey1", alpha=alpha2, position = position_jitter(width=0.25) ) +
    geom_boxplot( alpha=0, width=0.7,   aes(y=yAxis),  outlier.size=0,  size=0.7,  fill=NA, colour="red") +    
    xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=14, hjust1=1, vjust1=1,  angle1=30 )  + ylim(Ymin2, Ymax2 )
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp1,  path1=path2, fileName1=paste(fileName2, "-ScatterBoxPlot",        sep="",  collapse=NULL),  height1=height2, width1=width2)
  
  FigureTemp2 <- ggplot(DataFrame1, aes(x=sampleType) ) +   
    geom_jitter(aes(y=yAxis), size=0.3, colour="grey1", alpha=alpha2, position = position_jitter(width=0.25) ) +
    geom_violin(aes(y=yAxis), fill = NA, colour = "blue", adjust = 3,  alpha=0, size=0.6) +  
    geom_boxplot( alpha=0, width=0.3,   aes(y=yAxis),  outlier.size=0,  size=0.6,  fill=NA, colour="red" ) +    
    stat_summary(   aes(y=yAxis),   fun.y=mean, colour="yellow3", geom="point", shape=19, size=0.0, show.legend = FALSE) + 
    xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=14, hjust1=1, vjust1=1,  angle1=30 )  + ylim(Ymin2, Ymax2 )
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp2,  path1=path2, fileName1=paste(fileName2, "-ViolinPlot-3adjust",    sep="",  collapse=NULL),  height1=height2, width1=width2)
  
  FigureTemp3 <- ggplot(DataFrame1, aes(x=sampleType) ) +  
    geom_jitter(aes(y=yAxis), size=0.3, colour="grey1", alpha=alpha2, position = position_jitter(width=0.25) ) +
    geom_violin(aes(y=yAxis), fill = NA, colour = "blue",   alpha=0, size=0.6) +  
    geom_boxplot( alpha=0, width=0.3,   aes(y=yAxis),  outlier.size=0,  size=0.6,  fill=NA, colour="red" ) +    
    stat_summary(   aes(y=yAxis),   fun.y=mean, colour="yellow3", geom="point", shape=19, size=0.0, show.legend = FALSE) + 
    xlab(xLab2 ) + ylab( yLab2 ) + ggtitle( title2 )  + MyTheme_1_g(textSize1=14, hjust1=1, vjust1=1,  angle1=30 )  + ylim(Ymin2, Ymax2 )
  MySaveGgplot2_1_g(ggplot2Figure1=FigureTemp3,  path1=path2, fileName1=paste(fileName2, "-ViolinPlot-noAdjust",   sep="",  collapse=NULL),  height1=height2, width1=width2)
}  




## T test and Wilcoxon test  (unpaired).
MyHypothesisTest_1_f <- function(vector1, vector2, file1) {
  myTempFunction <- function() {
    
    sink(file=file1)
    print("######################## Apply continuity correction in the normal approximation for the p-value. ###############################################")
    
    print("##################################################################################################################################for comparing boxplot")
    wilcoxTest_2 <- wilcox.test(x=vector1, y=vector2, alternative="two.sided",  mu=0,   paired=FALSE,  exact=FALSE, correct=TRUE,  conf.int=TRUE,  conf.level=0.95)
    print( wilcoxTest_2  )
    cat( "\n\nExact p-value:", wilcoxTest_2$p.value, "\n\n\n\n\n" ) 
    
    print("##################################################################################################################################")
    wilcoxTest_4 <- wilcox.test(x=vector1, y=vector2, alternative="less",       mu=0,   paired=FALSE,  exact=FALSE, correct=TRUE,  conf.int=TRUE,  conf.level=0.95)
    print( wilcoxTest_4  )
    cat( "\n\nExact p-value:", wilcoxTest_4$p.value, "\n\n\n\n\n" )
    print("##################################################################################################################################")
    
    print("##################################################################################################################################")
    wilcoxTest_6 <- wilcox.test(x=vector1, y=vector2, alternative="greater",    mu=0,   paired=FALSE,  exact=FALSE, correct=TRUE,  conf.int=TRUE,  conf.level=0.95)
    print( wilcoxTest_6  )
    cat( "\n\nExact p-value:", wilcoxTest_6$p.value, "\n\n\n\n\n\n" )
    
    
    print("######################## Don't apply continuity correction in the normal approximation for the p-value. ###############################################")
    print("##################################################################################################################################")
    
    print("##################################################################################################################################")
    wilcoxTestB_2 <- wilcox.test(x=vector1, y=vector2, alternative="two.sided",  mu=0,   paired=FALSE,  exact=FALSE, correct=FALSE,  conf.int=TRUE,  conf.level=0.95)
    print( wilcoxTestB_2  )
    cat( "\n\nExact p-value:", wilcoxTestB_2$p.value, "\n\n\n\n\n" ) 
    print("##################################################################################################################################")
    
    print("##################################################################################################################################")
    wilcoxTestB_4 <- wilcox.test(x=vector1, y=vector2, alternative="less",       mu=0,   paired=FALSE,  exact=FALSE, correct=FALSE,  conf.int=TRUE,  conf.level=0.95)
    print( wilcoxTestB_4  )
    cat( "\n\nExact p-value:", wilcoxTestB_4$p.value, "\n\n\n\n\n" )
    print("##################################################################################################################################")
    
    print("##################################################################################################################################")
    wilcoxTestB_6 <- wilcox.test(x=vector1, y=vector2, alternative="greater",    mu=0,   paired=FALSE,  exact=FALSE, correct=FALSE,  conf.int=TRUE,  conf.level=0.95)
    print( wilcoxTestB_6  )
    cat( "\n\nExact p-value:", wilcoxTestB_6$p.value, "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n" )
    
    
    
    print("######################## T-test, var.equal=FALSE. ###############################################")
    print("##################################################################################################################################")
    
    print("##################################################################################################################################")
    tTest_2 <- t.test(x=vector1, y=vector2, alternative="two.sided",  mu=0,   paired=FALSE,   var.equal=FALSE,  conf.level=0.95)
    print( tTest_2  )
    cat( "\n\nExact p-value:", tTest_2$p.value, "\n\n\n\n\n" )
    print("##################################################################################################################################")
    
    print("##################################################################################################################################")
    tTest_4 <- t.test(x=vector1, y=vector2, alternative="less",       mu=0,   paired=FALSE,   var.equal=FALSE,  conf.level=0.95)
    print( tTest_4  )
    cat( "\n\nExact p-value:", tTest_4$p.value, "\n\n\n\n\n" )
    print("##################################################################################################################################")
    
    print("##################################################################################################################################")
    tTest_6 <- t.test(x=vector1, y=vector2, alternative="greater",    mu=0,   paired=FALSE,   var.equal=FALSE,  conf.level=0.95)
    print( tTest_6  )
    cat( "\n\nExact p-value:", tTest_6$p.value, "\n\n\n\n\n\n" )
    
    
    print("######################## T-test, var.equal=TRUE. ##############################################################################################")
    
    print("##################################################################################################################################")
    tTestB_2 <- t.test(x=vector1, y=vector2, alternative="two.sided",  mu=0,   paired=FALSE,   var.equal=TRUE,  conf.level=0.95)
    print( tTestB_2  )
    cat( "\n\nExact p-value:", tTestB_2$p.value, "\n\n\n\n\n" )
    print("##################################################################################################################################")
    
    print("##################################################################################################################################")
    tTestB_4 <- t.test(x=vector1, y=vector2, alternative="less",       mu=0,   paired=FALSE,   var.equal=TRUE,  conf.level=0.95)
    print( tTestB_4 )
    cat( "\n\nExact p-value:", tTestB_4$p.value, "\n\n\n\n\n" )
    
    print("##################################################################################################################################")
    tTestB_6 <- t.test(x=vector1, y=vector2, alternative="greater",    mu=0,   paired=FALSE,   var.equal=TRUE,  conf.level=0.95)
    print( tTestB_6  )
    cat( "\n\nExact p-value:", tTestB_6$p.value, "\n\n\n\n\n" )
    
    sink()
    
  }
  tryCatch(
    myTempFunction(),
    error = function(err){"descriptiveStatistics_f_9999999999999"}
  ) 
  
}





###########################################################################################################################################################################
rawMatrix_1 <- read.table("pearson_rcorr.format.kept.txt", header=TRUE,   sep="\t" )  
dim(rawMatrix_1 )
rawMatrix_1[1:3,]
summary(rawMatrix_1[,3])



AllResults_g <- "Z-FinalFigures"
if( ! file.exists(AllResults_g) ) { dir.create(path=AllResults_g, recursive = TRUE) }

 
MyBoxViolinPlot_1_f(vector2=rawMatrix_1$cor,   sampleType2=rawMatrix_1$type,  
                                colours2=c( rep("red", 10) ) ,   
                                path2=AllResults_g,   fileName2="all_1",  
                                title2="",  xLab2="pairs",  yLab2="Pearson correlation coefficient",    
                                height2=3.5,   width2=6,  Ymin2=0.978, Ymax2=0.995  )
 
MyBoxViolinPlot_3(vector2=rawMatrix_1$cor,   sampleType2=rawMatrix_1$type,  
                     path2=AllResults_g,   fileName2="all_2",  
                    title2="",  xLab2="paires",  yLab2="Pearson correlation coefficient",    
                    height2=3.5,   width2=6,   Ymin2=0.978, Ymax2=0.995, alpha2=0.5)


rawMatrix_1$cor[rawMatrix_1$cor>0.990] = 0.990
rawMatrix_1$cor[rawMatrix_1$cor<0.980] = 0.980
MyBoxViolinPlot_1_f(vector2=rawMatrix_1$cor,   sampleType2=rawMatrix_1$type,  
                    colours2=c( rep("red", 10) ) ,   
                    path2=AllResults_g,   fileName2="all_3",  
                    title2="",  xLab2="pairs",  yLab2="Pearson correlation coefficient",    
                    height2=3.5,   width2=6,  Ymin2=0.980, Ymax2=0.990  )



my1 =  rawMatrix_1[rawMatrix_1$type=="FF_FF",3]
my2 =  rawMatrix_1[rawMatrix_1$type=="FF_FM-boy",3]
my3 =  rawMatrix_1[rawMatrix_1$type=="FF_MM",3]
my4 =  rawMatrix_1[rawMatrix_1$type=="FM-boy_FM-boy",3]
my5 =  rawMatrix_1[rawMatrix_1$type=="FM-boy_MM",3]
my6 =  rawMatrix_1[rawMatrix_1$type=="FM-girl_FF",3]
my7 =  rawMatrix_1[rawMatrix_1$type=="FM-girl_FM-boy",3]
my8 =  rawMatrix_1[rawMatrix_1$type=="FM-girl_FM-girl",3]
my9 =  rawMatrix_1[rawMatrix_1$type=="FM-girl_MM",3]
my10=  rawMatrix_1[rawMatrix_1$type=="MM_MM",3]


MyHypothesisTest_1_f(vector1=my1, vector2=my2, file1=paste(AllResults_g, "1-2.txt", sep="/") )
MyHypothesisTest_1_f(vector1=my1, vector2=my3, file1=paste(AllResults_g, "1-3.txt", sep="/") )
MyHypothesisTest_1_f(vector1=my1, vector2=my4, file1=paste(AllResults_g, "1-4.txt", sep="/") )
MyHypothesisTest_1_f(vector1=my1, vector2=my5, file1=paste(AllResults_g, "1-5.txt", sep="/") )
MyHypothesisTest_1_f(vector1=my1, vector2=my6, file1=paste(AllResults_g, "1-6.txt", sep="/") )
MyHypothesisTest_1_f(vector1=my1, vector2=my7, file1=paste(AllResults_g, "1-7.txt", sep="/") )
MyHypothesisTest_1_f(vector1=my1, vector2=my8, file1=paste(AllResults_g, "1-8.txt", sep="/") )
MyHypothesisTest_1_f(vector1=my1, vector2=my9, file1=paste(AllResults_g, "1-9.txt", sep="/") )
MyHypothesisTest_1_f(vector1=my1, vector2=my10,file1=paste(AllResults_g, "1-10.txt", sep="/") )




MyHypothesisTest_1_f(vector1=my2, vector2=my3, file1=paste(AllResults_g, "2-3.txt", sep="/") )
MyHypothesisTest_1_f(vector1=my2, vector2=my4, file1=paste(AllResults_g, "2-4.txt", sep="/") )
MyHypothesisTest_1_f(vector1=my2, vector2=my5, file1=paste(AllResults_g, "2-5.txt", sep="/") )
MyHypothesisTest_1_f(vector1=my2, vector2=my6, file1=paste(AllResults_g, "2-6.txt", sep="/") )
MyHypothesisTest_1_f(vector1=my2, vector2=my7, file1=paste(AllResults_g, "2-7.txt", sep="/") )
MyHypothesisTest_1_f(vector1=my2, vector2=my8, file1=paste(AllResults_g, "2-8.txt", sep="/") )
MyHypothesisTest_1_f(vector1=my2, vector2=my9, file1=paste(AllResults_g, "2-9.txt", sep="/") )
MyHypothesisTest_1_f(vector1=my2, vector2=my10,file1=paste(AllResults_g, "2-10.txt", sep="/") )

MyHypothesisTest_1_f(vector1=my3, vector2=my4, file1=paste(AllResults_g, "3-4.txt", sep="/") )
MyHypothesisTest_1_f(vector1=my3, vector2=my5, file1=paste(AllResults_g, "3-5.txt", sep="/") )
MyHypothesisTest_1_f(vector1=my3, vector2=my6, file1=paste(AllResults_g, "3-6.txt", sep="/") )
MyHypothesisTest_1_f(vector1=my3, vector2=my7, file1=paste(AllResults_g, "3-7.txt", sep="/") )
MyHypothesisTest_1_f(vector1=my3, vector2=my8, file1=paste(AllResults_g, "3-8.txt", sep="/") )
MyHypothesisTest_1_f(vector1=my3, vector2=my9, file1=paste(AllResults_g, "3-9.txt", sep="/") )
MyHypothesisTest_1_f(vector1=my3, vector2=my10,file1=paste(AllResults_g, "3-10.txt", sep="/") )

MyHypothesisTest_1_f(vector1=my4, vector2=my5, file1=paste(AllResults_g, "4-5.txt", sep="/") )
MyHypothesisTest_1_f(vector1=my4, vector2=my6, file1=paste(AllResults_g, "4-6.txt", sep="/") )
MyHypothesisTest_1_f(vector1=my4, vector2=my7, file1=paste(AllResults_g, "4-7.txt", sep="/") )
MyHypothesisTest_1_f(vector1=my4, vector2=my8, file1=paste(AllResults_g, "4-8.txt", sep="/") )
MyHypothesisTest_1_f(vector1=my4, vector2=my9, file1=paste(AllResults_g, "4-9.txt", sep="/") )
MyHypothesisTest_1_f(vector1=my4, vector2=my10,file1=paste(AllResults_g, "4-10.txt", sep="/") )

MyHypothesisTest_1_f(vector1=my5, vector2=my6, file1=paste(AllResults_g, "5-6.txt", sep="/") )
MyHypothesisTest_1_f(vector1=my5, vector2=my7, file1=paste(AllResults_g, "5-7.txt", sep="/") )
MyHypothesisTest_1_f(vector1=my5, vector2=my8, file1=paste(AllResults_g, "5-8.txt", sep="/") )
MyHypothesisTest_1_f(vector1=my5, vector2=my9, file1=paste(AllResults_g, "5-9.txt", sep="/") )
MyHypothesisTest_1_f(vector1=my5, vector2=my10,file1=paste(AllResults_g, "5-10.txt", sep="/") )

MyHypothesisTest_1_f(vector1=my6, vector2=my7, file1=paste(AllResults_g, "6-7.txt", sep="/") )
MyHypothesisTest_1_f(vector1=my6, vector2=my8, file1=paste(AllResults_g, "6-8.txt", sep="/") )
MyHypothesisTest_1_f(vector1=my6, vector2=my9, file1=paste(AllResults_g, "6-9.txt", sep="/") )
MyHypothesisTest_1_f(vector1=my6, vector2=my10,file1=paste(AllResults_g, "6-10.txt", sep="/") )

MyHypothesisTest_1_f(vector1=my7, vector2=my8, file1=paste(AllResults_g, "7-8.txt", sep="/") )
MyHypothesisTest_1_f(vector1=my7, vector2=my9, file1=paste(AllResults_g, "7-9.txt", sep="/") )
MyHypothesisTest_1_f(vector1=my7, vector2=my10,file1=paste(AllResults_g, "7-10.txt", sep="/") )

MyHypothesisTest_1_f(vector1=my8, vector2=my9, file1=paste(AllResults_g, "8-9.txt", sep="/") )
MyHypothesisTest_1_f(vector1=my8, vector2=my10,file1=paste(AllResults_g, "8-10.txt", sep="/") )

MyHypothesisTest_1_f(vector1=my9, vector2=my10,file1=paste(AllResults_g, "9-10.txt", sep="/") )



