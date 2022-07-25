plotexport_fxn <-
function(plotname, plottype=c("wc","bp"), 
                           filename, filetype=".png"){
  if(plottype=="wc"){
    htmlwidgets::saveWidget(widget=plotname,"temp.html", selfcontained=FALSE)
    webshot("temp.html", paste0("figures/", filename, filetype), 
            vwidth=800, vheight=800, delay=30)
    
    unlink("temp_files", recursive=TRUE)
    unlink("temp.html")
  }
  if(plottype=="bp"){
    png(paste0("figures/", filename, filetype), width=600, height=600)
    print(plotname)
    dev.off()
  }
}
