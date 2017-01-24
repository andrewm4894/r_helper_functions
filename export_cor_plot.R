##########################################
## FUNCTION TO EXPORT CORPLOT
##########################################

export_cor_plot <- function(df,output_dir) {  
  mycor <- melt(cor(df))
  mycor <- sqldf('select * from mycor order by Var1, Var2')
  write.table(mycor, file=paste(sep='',output_dir,'cor_table.csv'), sep=',', row.names = FALSE)
  mycorplot <- qplot(x=Var1, y=Var2, data=mycor, fill=value, geom="tile") + scale_fill_gradient2(limits=c(-1, 1)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  mycorplotly <- ggplotly(mycorplot,width=1800,height=1200)
  htmlwidgets::saveWidget(mycorplotly, paste(sep='',output_dir,'cor_plot.html'))
}
