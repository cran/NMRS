`Met.FID` <-
function(){


require(tcltk)#Packages
  require(tkrplot)
require(Rwave)
tt <- tktoplevel(background="white")#Main windows
  tkwm.title(tt,"FID Import Interface")

fid.out<-Fid()


######Plot Parameters:

xlim=c(fid.out$PPM[length(fid.out$PPM)],0)
parPlotSize <- c()
  usrCoords <- c()
indexLabeled<-c()
  labeledPoints <- list()
intensity=100
pivot=7
#######################################################

######Plot function

plotFunction1 <- function()
  {
  params <- par(bg="white")
  plot(fid.out$PPM,Re(fid.out$spectrum),"l", xlim=xlim,xlab="PPM",
ylim=range(Re(fid.out$spectrum)/(0.01*intensity)),ylab="")
    if (length(indexLabeled)>0)
    for (i in (1:length(indexLabeled)))
    {
      indexClosest <- indexLabeled[i]
    }
  parPlotSize <<- par("plt")
  usrCoords   <<- par("usr")
  par(params)
}
########################################################
  
img1 <- tkrplot(tt,fun=plotFunction1,hscale=2,vscale=1.5)
  tkgrid(img1,columnspan=6)
SliderValue1 <- tclVar(0)
SliderValue2 <- tclVar(0)
phase.correction<-function()
{
phc0<-as.numeric(tclvalue(SliderValue1))
phc1<-as.numeric(tclvalue(SliderValue2))
fid.out<<-phase(fid.out,phc0,phc1,pivot)
tkrreplot(img1,plotFunction1())
}

######Zoom Subfunction

Zoom<-function()
  {
dlg <- tktoplevel()
  tkwm.deiconify(dlg)
  tkgrab.set(dlg)
  tkfocus(dlg)
  tkwm.title(dlg,"Zoom")
SliderValue1 <- tclVar(round(fid.out$PPM[1]))
SliderValue2 <- tclVar(round(fid.out$PPM[length(fid.out$PPM)]))
SliderValueLabel1 <- tklabel(dlg,text=as.character(tclvalue(SliderValue1)))
SliderValueLabel2 <- tklabel(dlg,text=as.character(tclvalue(SliderValue2)))
tkconfigure(SliderValueLabel1,textvariable=SliderValue1)
tkconfigure(SliderValueLabel2,textvariable=SliderValue2)
slider1 <- tkscale(dlg, from=fid.out$PPM[length(fid.out$PPM)], to=0,showvalue=F, variable=SliderValue1,
                   resolution=0.01, orient="horizontal",length=300)
slider2 <- tkscale(dlg,  from=fid.out$PPM[length(fid.out$PPM)], to=0,showvalue=F, variable=SliderValue2,
                   resolution=0.01, orient="horizontal",length=300)
tkgrid(tklabel(dlg,text="       "))
tkgrid(tklabel(dlg,text="Zoom"),sticky="w")
tkgrid(tklabel(dlg,text="       "))
tkgrid(tklabel(dlg,text="From : "),SliderValueLabel1,slider1)
tkgrid(tklabel(dlg,text="To : "),SliderValueLabel2,slider2)
tkgrid(tklabel(dlg,text="       "))
  tkgrid(tklabel(dlg,text="       "))
  
onOK <- function()
  {
if(as.numeric(tclvalue(SliderValue1))>as.numeric(tclvalue(SliderValue2)))
{
xlim<<-c(as.numeric(tclvalue(SliderValue1)),
as.numeric(tclvalue(SliderValue2)))
}
if(as.numeric(tclvalue(SliderValue1))<as.numeric(tclvalue(SliderValue2)))
{
xlim<<-c(as.numeric(tclvalue(SliderValue2)),as.numeric(tclvalue(SliderValue1)))
}
    tkdestroy(dlg)
tkrreplot(img1,plotFunction1())
}
 onCancel <- function()
  {
    ReturnVal <<- 0
    tkdestroy(dlg)
   }
  OK.but     <-tkbutton(dlg,text="   OK   ",command=onOK)
  Cancel.but <-tkbutton(dlg,text=" Cancel ",command=onCancel)
  tkgrid(OK.but,Cancel.but,sticky="e")
  tkgrid(tklabel(dlg,text="    "))
  tkraise(dlg)
  tkfocus(dlg)
  tkbind(dlg, "<Destroy>", function() {tkgrab.release(dlg);tkfocus(dlg)})
  }
#########################################################

############TSP Reference Subfunction

Met.reference<- function(entryWidth=5,returnValOnCancel="ID_CANCEL")
  {
  dlg <- tktoplevel()
  tkwm.title(dlg,"Reference")
  textEntryVarTcl <- tclVar(paste(" "))
  textEntryWidget <- tkentry(dlg,width=paste(entryWidth),textvariable=textEntryVarTcl)
  textEntryVarTcl2 <- tclVar(paste(format(xPlotCoord,digits=3)))
  textEntryWidget2 <- tkentry(dlg,width=paste(entryWidth),textvariable=textEntryVarTcl2,state="disabled")
  tkgrid(tklabel(dlg,text="       "))
  tkgrid(tklabel(dlg,text="Old Shift:   "),textEntryWidget2)
  tkgrid(tklabel(dlg,text="       "))
  tkgrid(tklabel(dlg,text="New Shift:   "),textEntryWidget)
  tkgrid(tklabel(dlg,text="       "))
  ReturnVal <- returnValOnCancel
  onOK <- function()
  {
    Reference <- as.numeric(tclvalue(textEntryVarTcl))-as.numeric(tclvalue(textEntryVarTcl2))
    fid.out$PPM=fid.out$PPM+Reference
fid.out<<-fid.out
tkrreplot(img1,plotFunction1())
    tkdestroy(dlg)
  }
onCancel <- function()
  {
    ReturnVal <<- 0
    tkgrab.release(dlg)
    tkdestroy(dlg)
  }
  OK.but     <-tkbutton(dlg,text="   OK   ",command=onOK)
  Cancel.but <-tkbutton(dlg,text=" Cancel ",command=onCancel)
  tkgrid(OK.but,Cancel.but)
  tkgrid(tklabel(dlg,text="    "))
  tkraise(dlg)
  tkfocus(dlg)
  tkbind(dlg, "<Destroy>", function() {tkgrab.release(dlg);tkfocus(dlg)})
  tkbind(textEntryWidget, "<Return>", onOK)
  tkwait.window(dlg)
  }


  labelClosestPoint <- function(xClick,yClick,imgXcoords,imgYcoords)
  {
  squared.Distance <- (xClick-imgXcoords)^2 + (yClick-imgYcoords)^2
  indexClosest <- which.min(squared.Distance)
  indexLabeled <<- c(indexLabeled,indexClosest)
  tkrreplot(img1)
  }

  OnLeftClick <- function(x,y)
  {
  xClick <- x
yClick <- y
  require(tcltk)
  width  <- as.numeric(tclvalue(tkwinfo("reqwidth",img1)))
  height <- as.numeric(tclvalue(tkwinfo("reqheight",img1)))
xMin <- parPlotSize[1] * width
xMax <- parPlotSize[2] * width
yMin <- parPlotSize[3] * height
yMax <- parPlotSize[4] * height
rangeX <- usrCoords[2] - usrCoords[1]
rangeY <- usrCoords[4] - usrCoords[3]

  imgXcoords <- (fid.out$PPM-usrCoords[1])*(xMax-xMin)/rangeX + xMin
  imgYcoords <- (fid.out$PPM-usrCoords[3])*(yMax-yMin)/rangeY + yMin

  xClick <- as.numeric(xClick)+0.5
  yClick <- as.numeric(yClick)+0.5
  yClick <- height - yClick

  xPlotCoord <<- usrCoords[1]+(xClick-xMin)*rangeX/(xMax-xMin)
  yPlotCoord <- usrCoords[3]+(yClick-yMin)*rangeY/(yMax-yMin)

#a<<-xPlotCoord

  #msg <- paste("Label the point closest to these approximate plot coordinates: \n",
      #         "x =",format(xPlotCoord,digits=3),",   y =",format(yPlotCoord,digits=3))
  #mbval<- tkmessageBox(title="Label Point Closest to These Approximate Plot Coordinates",
      #                 message=msg,type="ok",icon="info")

  #if (tclvalue(mbval)=="yes")
  #  labelClosestPoint(xClick,yClick,imgXcoords,imgYcoords)
      Met.reference()
}
TSP.reference<-function()
{
tkbind(img1, "<Button-1>",OnLeftClick)
tkconfigure(img1,cursor="crosshair")
}

#########################################################

######Pivot

Pivot<-function()
  {
dlg <- tktoplevel()
  tkwm.deiconify(dlg)
  tkgrab.set(dlg)
  tkfocus(dlg)
  tkwm.title(dlg,"Phase correction. Pivot")
SliderValue4 <- tclVar(7)
SliderValueLabel4 <- tklabel(dlg,text=as.character(tclvalue(SliderValue4)))

slider4 <- tkscale(dlg, from=fid.out$PPM[length(fid.out$PPM)], to=fid.out$PPM[1],showvalue=F, variable=SliderValue4,
             resolution=0.01, orient="horizontal",bigincrement=1,length=300)
tkconfigure(SliderValueLabel4,textvariable=SliderValue4)

tkgrid(tklabel(dlg,text="Pivot (ppm) : "),SliderValueLabel4,slider4)
  
onOK <- function()
  {

pivot<<-c(as.numeric(tclvalue(SliderValue4)))
tkdestroy(dlg)
phase.correction()
}
 onCancel <- function()
  {
    ReturnVal <<- c(7)
    tkdestroy(dlg)
   }
  OK.but     <-tkbutton(dlg,text="   OK   ",command=onOK)
  Cancel.but <-tkbutton(dlg,text=" Cancel ",command=onCancel)
  tkgrid(OK.but,Cancel.but,sticky="e")
  tkgrid(tklabel(dlg,text="    "))
  tkraise(dlg)
  tkfocus(dlg)
  #tkbind(dlg, "<Destroy>", function() {tkgrab.release(dlg);tkfocus(dlg)})
  }
###########################################################################
######Intensity Subfunction


f.intensity<-function()
  {
  intensity<<-as.numeric(tclvalue(SliderValue3))
tkrreplot(img1,plotFunction1())
  }
 SliderValue3 <- tclVar(100)
  SliderValueLabel3 <- tklabel(tt,text=as.character(tclvalue(SliderValue3)),background="white")
slider3 <- tkscale(tt, from=25, to=2000,showvalue=F, variable=SliderValue3,
             resolution=25, orient="horizontal",bigincrement=25)

#########################################################

######PDF

  CopyToClip <- function()
  {
fileName<-tclvalue(tkgetSaveFile())
pdf(file = fileName)
plotFunction1()
dev.off()
windows()
plotFunction1()
  }
#########################################################

###### onOk & onCancel

 onok <- function()
 {
spect.fid<<-cbind(fid.out$PPM,Re(fid.out$spectrum))
nexfid<<-"yes"
tkdestroy(tt)
  }
onCancel<-function()
{
nexfid<<-"no"
tkdestroy(tt)
}
#########################################################

Menu <- tkmenu(tt,borderwidth=40)
  tkconfigure(tt, menu=Menu)
tkadd(Menu, "command", label="Zoom",
        command=function() Zoom())
tkadd(Menu, "command", label="TSP Reference",
        command=function() TSP.reference())
tkadd(Menu, "command", label="Phase Correction. Pivot",
        command=function() Pivot())


  SliderValueLabel1 <- tklabel(tt,text=as.character(tclvalue(SliderValue1)),background="white")
slider1 <- tkscale(tt, from=0, to=360,showvalue=F, variable=SliderValue1,
             resolution=1, orient="horizontal",bigincrement=1)

  SliderValueLabel2 <- tklabel(tt,text=as.character(tclvalue(SliderValue2)),background="white")
slider2 <- tkscale(tt, from=0, to=360,showvalue=F, variable=SliderValue2,
             resolution=1, orient="horizontal",bigincrement=1)

tkgrid(tklabel(tt,text="Phase adjustment", bg="white"),row=1,column=2,columnspan=2)
tkgrid(tklabel(tt,text="Intensity", bg="white"),row=1,column=1,sticky="w")
tkgrid(slider3,row=2,column=1,sticky="w")
tkgrid(tklabel(tt,text="Phc0", bg="white"),row=2,sticky="e",column=2)
tkgrid(tklabel(tt,text="Phc1", bg="white"),row=3,sticky="e",column=2)
tkgrid(slider1,row=2,column=3,sticky="w")
tkgrid(slider2,row=3,column=3,sticky="w")

copy.but <- tkbutton(tt,text="Copy to pdf",command=CopyToClip)
ok.but <- tkbutton(tt,text="Ok",command=onok)
cancel.but <- tkbutton(tt,text="Cancel",command=onCancel)

#tkgrid(tklabel(tt,text="  ", bg="white"),row=4)
tkgrid(copy.but,row=1,column=4,columnspan=2)
tkgrid(ok.but,row=3,column=4,sticky="e")
tkgrid(cancel.but,row=3,column=5,sticky="w")
#tkgrid(tklabel(tt,text="  ", bg="white"),row=6)
tkbind(slider1,"<ButtonRelease-1>" ,phase.correction)
tkbind(slider2,"<ButtonRelease-1>" ,phase.correction)
tkbind(slider3,"<ButtonRelease-1>" ,f.intensity)
#tkpack(img1,side="top")
#tkpack(tklabel(tt,text="Phase adjustment", bg="white"),side="left",padx=5)
#tkpack(tklabel(tt,text="Phc0", bg="white"),slider1,side="left",padx=5)
#tkpack(tklabel(tt,text="Phc1", bg="white"),slider2,side="left",padx=5)
tkwait.window(tt)
}

